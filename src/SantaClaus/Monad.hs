{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-} 

module SantaClaus.Monad
  (
    Env (..)
  , MonadLogger (..)
  , runSanta
  ,
  )
  where

import Control.Concurrent.STM (atomically, isEmptyTQueue, newTQueue, readTQueue, writeTQueue, TQueue)
import Control.Monad (forever)
import Control.Monad.Reader (runReaderT, ReaderT, MonadIO (..), MonadReader (..))
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified SantaClaus.Logger as Logger
import SantaClaus.Logger (Logger)
import UnliftIO (MonadUnliftIO (..))

data Env = Env { logger :: Logger }

newtype SantaMonad a = SantaMonad { unSanta :: ReaderT Env IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadUnliftIO)

class (Monad m, MonadReader Env m, MonadIO m) => MonadLogger m where
  logMsg :: Text -> m ()

instance MonadLogger SantaMonad where
  logMsg t = do
    (Env logger) <- ask
    liftIO $ Logger.logMsg logger t

runSanta :: SantaMonad () -> Env -> IO ()
runSanta sm env = runReaderT (unSanta sm) env
