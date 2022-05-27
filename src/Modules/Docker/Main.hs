module Modules.Docker.Main
  ( createContainer
  , startContainer
  , checkContainer
  , createVolume
  , createImage
  , getLogs
  , clean
  ) where

import RIO
import qualified System.IO
import qualified System.Process.Typed
import qualified Network.HTTP.Simple as HTTP
import qualified Modules.Socket.Main as Socket
import qualified Modules.Timestamp.Main as Timestamp
import qualified Modules.Docker.Models.Image.Main as Image
import qualified Modules.Docker.Models.Response.Main as Response
import qualified Modules.Docker.Models.ContainerID.Main as ContainerID
import qualified Modules.Docker.Models.ContainerCheckOptions.Main as ContainerCheckOptions
import qualified Modules.Docker.Models.ContainerCheckResponse.Main as ContainerCheckResponse
import qualified Modules.Docker.Models.ContainerCreateOptions.Main as ContainerCreateOptions
import qualified Modules.Docker.Models.ContainerCreateResponse.Main as ContainerCreateResponse
import qualified Modules.Docker.Models.ContainerStartOptions.Main as ContainerStartOptions
import qualified Modules.Docker.Models.ContainerStartResponse.Main as ContainerStartResponse
import qualified Modules.Docker.Models.ImageCreateOptions.Main as ImageCreateOptions
import qualified Modules.Docker.Models.LogGetOptions.Main as LogGetOptions
import qualified Modules.Docker.Models.LogGetResponse.Main as LogGetResponse
import qualified Modules.Docker.Models.Volume.Main as Volume
import qualified Modules.Docker.Models.VolumeCreateOptions.Main as VolumeCreateOptions
import qualified Modules.Docker.Models.VolumeCreateResponse.Main as VolumeCreateResponse

createContainer :: ContainerCreateOptions.ContainerCreateOptions -> System.IO.IO ContainerCreateResponse.ContainerCreateResponse
createContainer options = do
  manager <- Socket.createManager "/var/run/docker.sock"
  let body = ContainerCreateOptions.toJSON options
      req  = HTTP.defaultRequest
           & HTTP.setRequestManager manager
           & HTTP.setRequestPath "/v1.40/containers/create"
           & HTTP.setRequestMethod "POST"
           & HTTP.setRequestBodyJSON body
  HTTP.httpBS req >>= Response.parse ContainerCreateResponse.fromJSON

startContainer :: ContainerStartOptions.ContainerStartOptions -> System.IO.IO ()
startContainer options = do
  manager <- Socket.createManager "/var/run/docker.sock"
  let path = "/v1.40/containers/" <> ContainerID.toText options.containerID <> "/start"
      req  = HTTP.defaultRequest
           & HTTP.setRequestManager manager
           & HTTP.setRequestPath (encodeUtf8 path)
           & HTTP.setRequestMethod "POST"
  void $ HTTP.httpBS req

checkContainer :: ContainerCheckOptions.ContainerCheckOptions -> System.IO.IO ContainerCheckResponse.ContainerCheckResponse
checkContainer options = do
  manager <- Socket.createManager "/var/run/docker.sock"
  let path = "/containers/" <> ContainerID.toText options.containerID <> "/json"
      req  = HTTP.defaultRequest
           & HTTP.setRequestManager manager
           & HTTP.setRequestPath (encodeUtf8 path)
  HTTP.httpBS req >>= Response.parse ContainerCheckResponse.fromJSON

createVolume :: VolumeCreateOptions.VolumeCreateOptions -> System.IO.IO VolumeCreateResponse.VolumeCreateResponse
createVolume options = do
  manager <- Socket.createManager "/var/run/docker.sock"
  let path = "/volumes/create"
      req  = HTTP.defaultRequest
           & HTTP.setRequestManager manager
           & HTTP.setRequestPath (encodeUtf8 path)
           & HTTP.setRequestBodyJSON (VolumeCreateOptions.toJSON options)
           & HTTP.setRequestMethod "POST"
  HTTP.httpBS req >>= Response.parse VolumeCreateResponse.fromJSON

createImage :: ImageCreateOptions.ImageCreateOptions -> System.IO.IO ()
createImage options = do
  manager <- Socket.createManager "/var/run/docker.sock"
  let path = "/images/create?" <> ImageCreateOptions.toUrlQuery options
      req  = HTTP.defaultRequest
           & HTTP.setRequestManager manager
           & HTTP.setRequestPath (encodeUtf8 path)
           & HTTP.setRequestMethod "POST"
  void $ HTTP.httpBS req

getLogs :: LogGetOptions.LogGetOptions -> System.IO.IO LogGetResponse.LogGetResponse
getLogs options = do
  manager <- Socket.createManager "/var/run/docker.sock"
  let path = "/containers/" <> ContainerID.toText options.containerID <> "/logs?" <> LogGetOptions.toUrlQuery options
      req  = HTTP.defaultRequest
           & HTTP.setRequestManager manager
           & HTTP.setRequestPath (encodeUtf8 path)
  HTTP.httpBS req <&> LogGetResponse.fromResponse

clean :: System.IO.IO ()
clean = void do
  System.Process.Typed.readProcessStdout "docker rm -f $(docker ps -aq --filter \"label=hircle-ci\")"
  System.Process.Typed.readProcessStdout "docker volume rm -f $(docker volume ls -q --filter \"label=hircle-ci\")"

