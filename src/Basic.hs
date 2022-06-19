module Basic where

import Control.Concurrent (forkIO, threadDelay, ThreadId)
import Control.Concurrent.STM (atomically, isEmptyTQueue, newTQueue, newTVar, readTQueue, readTVar, writeTQueue, writeTVar, STM, TVar, TQueue)
import Control.Monad (forever, join)
import Control.Monad.STM (check, orElse)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import System.Random (getStdRandom, randomR)

data Group = MkGroup Int (TVar (Int, Gate, Gate))

newGroup n = atomically $ do
  g1 <- newGate n
  g2 <- newGate n
  tv <- newTVar (n, g1, g2)
  pure $ MkGroup n tv

joinGroup :: Group -> IO (Gate, Gate)
joinGroup (MkGroup n tv) = atomically $ do
  (nLeft, g1, g2) <- readTVar tv
  check $ nLeft > 0
  writeTVar tv (nLeft - 1, g1, g2)
  pure (g1, g2)

awaitGroup :: Group -> STM (Gate, Gate)
awaitGroup (MkGroup n tv) = do
  (nLeft, g1, g2) <- readTVar tv
  check $ nLeft == 0
  --
  -- original, definitely simplest and probably best for this number
  -- of gates
  -- newG1 <- newGate n
  -- newG2 <- newGate n
  -- writeTVar tv (n, newG1, newG2)
  --
  let [newG1, newG2] = [ newGate n | _ <- [1..2] ]
  -- or
  -- let [newG1, newG2] = take 2 $ repeat $ newGate n
  --
  writeTVar tv =<< (n,,) <$> newG1 <*> newG2
  pure (g1, g2)

data Gate = MkGate Int (TVar Int)

newGate :: Int -> STM Gate
newGate n = MkGate n <$> newTVar 0

passGate :: Gate -> IO ()
passGate (MkGate n tv) = atomically $ do
  nLeft <- readTVar tv
  check $ nLeft > 0
  writeTVar tv (nLeft - 1)

operateGate :: Gate -> IO ()
operateGate (MkGate n tv) = do
  atomically $ writeTVar tv n
  atomically $ do
    nLeft <- readTVar tv 
    check $ nLeft == 0

newtype Logger = Logger (TQueue Text)

newLogger :: IO Logger
newLogger = do
  tq <- atomically $ newTQueue
  pure $ Logger tq

initLogger :: Logger -> IO ThreadId
initLogger (Logger tq) = forkIO $ forever $ do
  msg <- atomically $ do
    check . not <$> isEmptyTQueue tq
    readTQueue tq
  TIO.putStrLn msg

logMsg :: Logger -> Text -> IO ()
logMsg (Logger tq) msg = atomically $ writeTQueue tq msg

randomDelay :: IO ()
randomDelay = do
  waitTime <- getStdRandom $ randomR (1, 1000000)
  threadDelay waitTime


-- Domain

meetInStudy :: Logger -> Int -> IO ()
meetInStudy logger id = logMsg logger $
  T.pack $ "Elf " <> show id <> " meeting in the study"

deliverToys :: Logger -> Int -> IO ()
deliverToys logger id = logMsg logger $
  T.pack $ "Reindeer " <> show id <> " delivering toys"

helper1 :: Group -> IO () -> IO ()
helper1 group doTask = joinGroup group >>=
  \(inGate, outGate) ->
    passGate inGate >> doTask >> passGate outGate

elf1, reindeer1 :: Logger -> Group -> Int -> IO ()
elf1 logger gp id = helper1 gp (meetInStudy logger id)
reindeer1 logger gp id = helper1 gp (deliverToys logger id)

elf, reindeer :: Logger -> Group -> Int -> IO ThreadId
elf logger gp id = forkIO $ forever $
  elf1 logger gp id >> randomDelay
reindeer logger gp id = forkIO $ forever $
  reindeer1 logger gp id >> randomDelay

--
-- first version, before he abstracts out `choose`
--
-- santa :: Logger -> Group -> Group -> IO ()
-- santa logger elfGroup reinGroup = do
--   logMsg logger "----------" 
--   (task, (inGate, outGate)) <- atomically $
--     orElse
--       (chooseGroup reinGroup "deliver toys")
--       (chooseGroup elfGroup "meet in my study")
--   logMsg logger $ "Ho! Ho! Ho! Let's " <> task
--   operateGate inGate
--   -- now the helpers do their task
--   operateGate outGate
-- 
--   where
--     chooseGroup :: Group -> Text -> STM (Text, (Gate, Gate))
--     chooseGroup gp task = do
--       gates <- awaitGroup gp
--       pure (task, gates)
--

santa :: Logger -> Group -> Group -> IO ()
santa logger elfGroup reinGroup = do
  logMsg logger "----------" 
  choose
    [ (awaitGroup reinGroup, run "deliver toys")
    , (awaitGroup elfGroup, run "meet in my study")
    ]

  where
    run :: Text -> (Gate, Gate) -> IO ()
    run task (inGate, outGate) = do
      logMsg logger $ "Ho! Ho! Ho! Let's " <> task
      operateGate inGate
      operateGate outGate

choose :: [(STM a, a -> IO ())] -> IO ()
choose choices = join $ atomically $ foldr1 orElse actions
  where 
    actions :: [STM (IO ())]
    actions =
      [ choiceAction <$> guard | (guard, choiceAction) <- choices ]

main :: IO ()
main = do
  logger <- newLogger
  initLogger logger
  elfGroup <- newGroup 3
  sequence_ [ elf logger elfGroup n | n <- [1..10] ]
  reinGroup <- newGroup 9
  sequence_ [ reindeer logger reinGroup n | n <- [1..9] ]
  forever $ santa logger elfGroup reinGroup
