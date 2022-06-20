module Main where

import Control.Monad.Reader (runReaderT)
import SantaClaus.Logger (initLogger, newLogger)
import SantaClaus.Monad (runSanta, Env (..))
import SantaClaus.Actions (santaAction)

main :: IO ()
main = do
  logger <- newLogger
  initLogger logger
  runSanta santaAction (Env logger)
