 module Modules.Socket.Main
  ( createManager
  ) where

import RIO
import qualified System.IO
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.Internal as Client.Internal
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket.ByteString

createManager :: FilePath -> System.IO.IO Client.Manager
createManager fp = Client.newManager $ Client.defaultManagerSettings
  { Client.managerRawConnection = pure createSocket
  }
  where
    createSocket _ _ _ = do
      s <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
      Socket.connect s (Socket.SockAddrUnix fp)
      Client.Internal.makeConnection
        (Socket.ByteString.recv s 8096)
        (Socket.ByteString.sendAll s)
        (Socket.close s)

