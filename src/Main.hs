module Main where
import System.Environment
import Network.Socket (withSocketsDo)
import Server
import Client

defaultPort = 3344

main = withSocketsDo $ do
  args <- getArgs
  case args of
    cmd:_ -> let conf = ServerConfig defaultPort cmd
             in serve conf
    [] -> let conf = ClientConfig defaultPort
          in clientConnect conf
