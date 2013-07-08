{-# LANGUAGE ScopedTypeVariables #-}
module Server (serve, ServerConfig(..)) where
import Prelude hiding (catch)
import Agent
import Network (accept)
import Network.Socket hiding (accept)
import System.Process
import System.IO
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Exception
import Control.Monad
import Control.Concurrent

data ServerConfig = ServerConfig { port :: Int
                                 , command :: String
                                 , numConnections :: Int}

type ServerM = ReaderT ServerConfig IO

newSocket :: ServerM Socket
newSocket = do
  port <- asks port
  numConnections <- asks numConnections
  liftIO $ do sock <- socket AF_INET Stream 0
              setSocketOption sock ReuseAddr 1
              bindSocket sock (SockAddrInet (fromIntegral port) iNADDR_ANY)
              listen sock numConnections
              return sock

spawnProcess :: Socket -> ServerM Agent
spawnProcess sock = do
  cmd <- asks command
  liftIO $ do (hIn, hOut, _, process) <- runInteractiveCommand cmd
              agent <- makeAgent hIn hOut
              forkIO $ do waitForProcess process
                          killAgent agent
              return agent

serveM :: ServerM ()
serveM = do
  sock <- newSocket
  processAgent <- spawnProcess sock
  liftIO $ let go isFirstConn = do
                 putStrLn "waiting for connection"
                 (h, _, _) <- accept sock
                 putStrLn "socket accepted"
                 bufferedMessage <- readAgentBuffer processAgent
                 clientAgent <- makeAgent h h
                 unless isFirstConn $ writeAgent clientAgent bufferedMessage
                 conn <- connectAgents processAgent clientAgent
                 select [(clientAgent, killAgentConnection conn >> go False),
                         (processAgent, sClose sock)]
           in go True

serve :: ServerConfig  -> IO ()
serve conf = runReaderT serveM conf
