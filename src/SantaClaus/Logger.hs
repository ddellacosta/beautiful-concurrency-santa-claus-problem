module SantaClaus.Logger
  ( Logger (..)
  , initLogger
  , logMsg
  , newLogger
  )
where

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM (TQueue, atomically, isEmptyTQueue, newTQueue, readTQueue, writeTQueue)
import Control.Monad (forever)
import Control.Monad.STM (check)
import Data.Text (Text)
import qualified Data.Text.IO as TIO

newtype Logger = Logger (TQueue Text)

newLogger :: IO Logger
newLogger = Logger <$> (atomically newTQueue)

initLogger :: Logger -> IO ThreadId
initLogger (Logger tq) = forkIO $
  forever $ do
    msg <- atomically $ do
      check . not <$> isEmptyTQueue tq
      readTQueue tq
    TIO.putStrLn msg

logMsg :: Logger -> Text -> IO ()
logMsg (Logger tq) msg = atomically $ writeTQueue tq msg
