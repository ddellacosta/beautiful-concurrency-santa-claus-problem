module SantaClaus.Group
  (
    Group
  , awaitGroup
  , joinGroup
  , newGroup
  , 
  )
  where

import Control.Concurrent.STM (atomically, newTVar, readTVar, writeTVar, STM, TVar)
import Control.Monad.Reader (MonadIO (..))
import Control.Monad.STM (check)
import SantaClaus.Gate (newGate, Gate)

data Group = MkGroup Int (TVar (Int, Gate, Gate))

newGroup n = atomically $ do
  g1 <- newGate n
  g2 <- newGate n
  tv <- newTVar (n, g1, g2)
  pure $ MkGroup n tv

joinGroup :: (MonadIO m) => Group -> m (Gate, Gate)
joinGroup (MkGroup n tv) = liftIO $ atomically $ do
  (nLeft, g1, g2) <- readTVar tv
  check $ nLeft > 0
  writeTVar tv (nLeft - 1, g1, g2)
  pure (g1, g2)

awaitGroup :: Group -> STM (Gate, Gate)
awaitGroup (MkGroup n tv) = do
  (nLeft, g1, g2) <- readTVar tv
  check $ nLeft == 0
  let [newG1, newG2] = [ newGate n | n <- [1..2] ]
  writeTVar tv =<< (n,,) <$> newG1 <*> newG2
  pure (g1, g2)
