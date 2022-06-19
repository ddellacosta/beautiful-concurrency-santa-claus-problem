{-# LANGUAGE FlexibleContexts #-}

module SantaClaus.Actions
  (
    santa
  ,
  )
where

import Control.Concurrent (forkIO, threadDelay, ThreadId)
import Control.Monad (forever, join)
import Control.Monad.Reader (MonadIO (..), MonadReader (..))
import Control.Monad.STM (STM)
import qualified Data.Text as T
import Data.Text (Text)
import qualified SantaClaus.Logger as Logger
import SantaClaus.Group (awaitGroup, joinGroup, newGroup, Group)
import SantaClaus.Gate (operateGate, passGate, Gate)
import SantaClaus.Logger (Logger)
import SantaClaus.Monad (Env (..), MonadLogger (..))
import System.Random (getStdRandom, randomR)
import UnliftIO (atomically, orElse, MonadUnliftIO)
import UnliftIO.Async (async, asyncThreadId)

groupTask
  :: (MonadIO m, MonadLogger m, MonadReader Env m, MonadUnliftIO m)
  => (Int -> m ())
  -> Group
  -> Int
  -> m ThreadId
groupTask task gp id = do
  let task' = doGroupTask gp (task id) >> randomDelay
  asyncThreadId <$> async (forever task')
  where
    doGroupTask :: (MonadLogger m) => Group -> m () -> m ()
    doGroupTask group doTask = joinGroup group >>=
      \(inGate, outGate) ->
        passGate inGate >> doTask >> passGate outGate

randomDelay :: (MonadIO m) => m ()
randomDelay = do
  waitTime <- getStdRandom $ randomR (1, 1000000)
  liftIO $ threadDelay waitTime

santa' :: (MonadIO m, MonadLogger m) => Group -> Group -> m ()
santa' elfGroup reinGroup = do
  logMsg "----------" 
  choose
    [ (awaitGroup reinGroup, run "deliver toys")
    , (awaitGroup elfGroup, run "meet in my study")
    ]
  where
    run :: (MonadLogger m, MonadIO m) => Text -> (Gate, Gate) -> m ()
    run task (inGate, outGate) = do
      logMsg $ "Ho! Ho! Ho! Let's " <> task
      operateGate inGate
      operateGate outGate

choose :: (MonadLogger m, MonadIO m) => [(STM a, a -> m ())] -> m ()
choose choices = join $ atomically $ foldr1 orElse actions
  where 
    actions =
      [ choiceAction <$> guard | (guard, choiceAction) <- choices ]

santa :: (MonadIO m, MonadLogger m, MonadReader Env m, MonadUnliftIO m) => m ()
santa = do
  elfGroup <- liftIO $ newGroup 3
  sequence_ [ elf elfGroup n | n <- [1..10] ]
  reinGroup <- liftIO $ newGroup 9
  sequence_ [ reindeer reinGroup n | n <- [1..9] ]
  forever $ santa' elfGroup reinGroup
  where
    elf = groupTask meetInStudy
    reindeer = groupTask deliverToys

    meetInStudy, deliverToys :: (MonadLogger m) => Int -> m ()
    meetInStudy id = do 
      logMsg $ T.pack $ "Elf " <> show id <> " meeting in the study"
    deliverToys id = do
      logMsg $ T.pack $ "Reindeer " <> show id <> " delivering toys"
