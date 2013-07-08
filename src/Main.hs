module Main where
import System.Environment
import Network.Socket (withSocketsDo)
import Server
import Client

defaultPort = 2002
defaultNumConnections = 2

main = withSocketsDo $ do
  args <- getArgs
  case args of
    cmd:_ -> let conf = ServerConfig defaultPort cmd defaultNumConnections
             in serve conf
    [] -> let conf = ClientConfig defaultPort
          in clientConnect conf
