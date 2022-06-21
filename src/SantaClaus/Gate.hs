module SantaClaus.Gate
  ( Gate
  , operateGate
  , passGate
  , newGate
  )
where

import Control.Concurrent.STM (STM, TVar, atomically, newTVar, readTVar, writeTVar)
import Control.Monad.Reader (MonadIO (..))
import Control.Monad.STM (check)

data Gate = MkGate Int (TVar Int)

newGate :: Int -> STM Gate
newGate n = MkGate n <$> newTVar 0

passGate :: (MonadIO m) => Gate -> m ()
passGate (MkGate n tv) = liftIO $
  atomically $ do
    nLeft <- readTVar tv
    check $ nLeft > 0
    writeTVar tv (nLeft - 1)

operateGate :: (MonadIO m) => Gate -> m ()
operateGate (MkGate n tv) = do
  liftIO $ atomically $ writeTVar tv n
  liftIO $ atomically $ do
    nLeft <- readTVar tv
    check $ nLeft == 0
