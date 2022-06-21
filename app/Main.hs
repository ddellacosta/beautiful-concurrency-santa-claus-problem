module Main where

import Control.Monad.Reader (runReaderT)
import SantaClaus.Actions (santaAction)
import SantaClaus.Logger (initLogger, newLogger)
import SantaClaus.Monad (Env (..), runSanta)

main :: IO ()
main = do
  logger <- newLogger
  initLogger logger
  runSanta santaAction (Env logger)
