module Services.Log.Main
  ( run
  , collect
  , update
  ) where

import RIO
import qualified RIO.Map as Map
import qualified System.IO
import qualified Data.Time.Clock.POSIX as Time
import qualified Models.Build.Main as Build
import qualified Models.BuildState.Main as BuildState
import qualified Models.Log.Main as Log
import qualified Models.LogCollection.Main as LogCollection
import qualified Models.LogCollectionStatus.Main as LogCollectionStatus
import qualified Modules.Docker.Models.LogGetOptions.Main as LogGetOptions
import qualified Modules.Docker.Models.LogGetResponse.Main as LogGetResponse
import qualified Services.Log.Modules.Main as Module

run :: Build.Build -> LogCollection.LogCollection -> Module.Module -> System.IO.IO (LogCollection.LogCollection, [Log.Log])
run bx cx env = do
  now  <- Time.getPOSIXTime
  logs <- collect now cx env
  return $ (update now bx.state cx, logs)

collect :: Time.POSIXTime -> LogCollection.LogCollection -> Module.Module -> System.IO.IO [Log.Log]
collect until cx env = do
  map <- flip Map.traverseWithKey cx $ \step status ->
    case status of
      LogCollectionStatus.Ready             -> return []
      LogCollectionStatus.Complete          -> return []
      LogCollectionStatus.Running cid since -> do
        response <- env.docker.getLogs $ LogGetOptions.LogGetOptions
          { LogGetOptions.containerID = cid
          , LogGetOptions.since       = since
          , LogGetOptions.until       = until
          }
        return $
          [ Log.Log
              { Log.step   = step
              , Log.output = LogGetResponse.raw response
              }
          ]
  return $ concat $ Map.elems map

update :: Time.POSIXTime -> BuildState.BuildState -> LogCollection.LogCollection -> LogCollection.LogCollection
update tx bs cx =
  flip Map.mapWithKey cx $ \step status ->
    case status of
      LogCollectionStatus.Ready       ->
        case bs of
          BuildState.Running bs ->
            if bs.step == step
              then LogCollectionStatus.Running bs.containerID 0
              else LogCollectionStatus.Ready
          _                     ->
            LogCollectionStatus.Ready
      LogCollectionStatus.Running _ _ ->
        case bs of
          BuildState.Running bs ->
            if bs.step == step
              then LogCollectionStatus.Running bs.containerID tx
              else LogCollectionStatus.Complete
          _ ->
            LogCollectionStatus.Complete
      LogCollectionStatus.Complete    ->
        LogCollectionStatus.Complete
