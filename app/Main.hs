module Main where

import RIO
import qualified System.IO
import qualified Services.Cli.Main as Cli
import qualified System.Log.Formatter as Logger
import qualified System.Log.Handler as Handler
import qualified System.Log.Handler.Simple as Logger
import qualified System.Log.Logger as Logger

main :: System.IO.IO ()
main = do
  logger  <- Logger.getRootLogger
  handler <- Logger.streamHandler stdout Logger.INFO
  let formatter = Logger.simpleLogFormatter "[$time : $loggername :$prio] $msg"
  Logger.saveGlobalLogger $
    logger
      & Logger.setHandlers [Handler.setFormatter handler formatter]
      & Logger.setLevel Logger.INFO
  Cli.main
