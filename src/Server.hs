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
import Control.Concurrent.STM
import Control.Monad
import Control.Concurrent

data ServerConfig = ServerConfig { port :: Int
                                 , command :: String }

type ServerM = ReaderT ServerConfig IO

newSocket :: ServerM Socket
newSocket = do
  port <- asks port
  liftIO $ do sock <- socket AF_INET Stream 0
              setSocketOption sock ReuseAddr 1
              bindSocket sock (SockAddrInet (fromIntegral port) iNADDR_ANY)
              listen sock 2
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
  liftIO $ let go = do putStrLn "waiting for connection"
                       (h, _, _) <- accept sock
                       putStrLn "socket accepted"
                       clientAgent <- makeAgent h h
                       connectAgents processAgent clientAgent
                       join . atomically $ select [(clientAgent, go),
                                                   (processAgent, sClose sock)]
           in go

serve :: ServerConfig  -> IO ()
serve conf = runReaderT serveM conf
