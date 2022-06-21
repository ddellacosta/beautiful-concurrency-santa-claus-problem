{-# LANGUAGE FlexibleContexts #-}

module SantaClaus.Actions
  ( santaAction
  ,
  )
where

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Monad (forever, join)
import Control.Monad.Reader (MonadIO (..), MonadReader (..))
import Control.Monad.STM (STM)
import Data.Text (Text)
import qualified Data.Text as T
import SantaClaus.Gate (Gate, operateGate, passGate)
import SantaClaus.Group (Group, awaitGroup, joinGroup, newGroup)
import SantaClaus.Logger (Logger)
import qualified SantaClaus.Logger as Logger
import SantaClaus.Monad (Env (..), MonadLogger (..))
import System.Random (getStdRandom, randomR)
import UnliftIO (MonadUnliftIO, atomically, orElse)
import UnliftIO.Async (async, asyncThreadId)

runReadySubAction :: (MonadIO m, MonadLogger m) => Group -> Group -> m ()
runReadySubAction elfGroup reinGroup = do
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
    actions = [choiceAction <$> guard | (guard, choiceAction) <- choices]

groupTask ::
  (MonadIO m, MonadLogger m, MonadReader Env m, MonadUnliftIO m) =>
  (Int -> m ()) ->
  Group ->
  Int ->
  m ThreadId
groupTask task gp id = do
  let task' = doGroupTask gp (task id) >> randomDelay
  asyncThreadId <$> async (forever task')
  where
    doGroupTask :: (MonadLogger m) => Group -> m () -> m ()
    doGroupTask group doTask =
      joinGroup group >>= \(inGate, outGate) ->
        passGate inGate >> doTask >> passGate outGate

    randomDelay :: (MonadIO m) => m ()
    randomDelay = liftIO . threadDelay =<< getStdRandom (randomR (1, 1000000))

santaAction ::
  (MonadIO m, MonadLogger m, MonadReader Env m, MonadUnliftIO m) => m ()
santaAction = do
  elfGroup <- liftIO $ newGroup 3
  sequence_ [elf elfGroup n | n <- [1 .. 10]]
  reinGroup <- liftIO $ newGroup 9
  sequence_ [reindeer reinGroup n | n <- [1 .. 9]]
  forever $ runReadySubAction elfGroup reinGroup
  where
    elf = groupTask meetInStudy
    reindeer = groupTask deliverToys

    meetInStudy, deliverToys :: (MonadLogger m) => Int -> m ()
    meetInStudy id = do
      logMsg $ T.pack $ "Elf " <> show id <> " meeting in the study"
    deliverToys id = do
      logMsg $ T.pack $ "Reindeer " <> show id <> " delivering toys"
