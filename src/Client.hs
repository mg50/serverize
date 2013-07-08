module Client where
import Network
import System.IO
import Agent
import Control.Monad

data ClientConfig = ClientConfig { port :: Int }

clientConnect conf = do
  h <- connectTo "localhost" $ PortNumber (fromIntegral $ port conf)
  serverAgent <- makeAgent h h
  clientAgent <- makeAgent stdout stdin
  connectAgents clientAgent serverAgent
  select [(clientAgent, return ()),
          (serverAgent, putStrLn "server stopped responding; exiting")]
