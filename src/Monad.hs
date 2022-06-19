{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-} 

module Monad
  (
    runIt
  ,
  )
  where

import Control.Concurrent.STM (atomically, isEmptyTQueue, newTQueue, readTQueue, writeTQueue, TQueue)
import Control.Monad (forever)
import Control.Monad.Reader (runReaderT, ReaderT, MonadIO (..), MonadReader (..))
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Logger (initLogger, logMsg, newLogger, Logger (..))

data Env = Env { logger :: Logger }

newtype SantaMonad a = SantaMonad { runSanta :: ReaderT Env IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

foo :: (MonadReader Env a, MonadIO a) => a ()
foo = do
  (Env logger) <- ask
  liftIO $ logMsg logger "hey"

runIt :: IO ()
runIt = do
  logger <- newLogger
  initLogger logger
  runReaderT foo (Env logger)

