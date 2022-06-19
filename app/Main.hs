module Main where

import Control.Monad.Reader (runReaderT)
import SantaClaus.Logger (initLogger, newLogger)
import SantaClaus.Monad (runSanta, Env (..))
import qualified SantaClaus.Actions

main :: IO ()
main = do
  logger <- newLogger
  initLogger logger
  runSanta SantaClaus.Actions.santa (Env logger)
