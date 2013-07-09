{-# LANGUAGE ScopedTypeVariables #-}
module Agent (Agent,
              makeAgent,
              writeAgent,
              readAgent,
              killAgent,
              readAgentBuffer,
              connectAgents,
              killAgentConnection,
              select)
       where
import Prelude hiding (catch)
import System.IO
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TMVar
import Control.Exception

data Agent = Agent { agentRead :: TChan String
                   , agentWrite :: TChan String
                   , agentClose :: TMVar ()
                   , agentBuffer :: TVar String }

data AgentConnection = AgentConnection (MVar ()) (MVar ())

makeAgent :: Handle -> Handle -> IO Agent
makeAgent hIn hOut = do readChan <- newTChanIO
                        writeChan <- newTChanIO
                        done <- newEmptyTMVarIO
                        buf <- newTVarIO ""

                        let safely m = m `catch` \(e :: SomeException) ->
                              atomically $ putTMVar done ()

                        readTid <- forkIO . safely . forever $ do
                          c <- hGetChar hOut
                          atomically $ do writeTChan readChan [c]
                                          bufferedMsg <- readTVar buf
                                          case bufferedMsg of
                                            h:_ | h == '\n' -> writeTVar buf [c]
                                            _ -> modifyTVar buf (c:)

                        writeTid <- forkIO . safely . forever $ do
                          msg <- atomically $ readTChan writeChan
                          hPutStr hIn msg
                          hFlush hIn

                        forkIO $ do atomically $ takeTMVar done
                                    killThread readTid
                                    killThread writeTid

                        return $ Agent readChan writeChan done buf

writeAgent (Agent _ x _ _) = atomically . writeTChan x
readAgent (Agent x _ _ _) = atomically $ readTChan x
killAgent (Agent _ _ done _) = atomically (putTMVar done ())
readAgentBuffer agent = atomically $ do
  empty <- isEmptyTChan (agentRead agent)
  if empty
     then do msg <- readTVar (agentBuffer agent)
             return $ Just (reverse msg)
     else return Nothing

connectAgents ag1 ag2 = do disc <- newEmptyMVar
                           finishedDisc <- newEmptyMVar
                           let disconnect = \(e :: SomeException) -> putMVar disc ()
                           let wire x y = forever $ do msg <- readAgent x
                                                       writeAgent y msg
                           tid1 <- forkIO $ wire ag1 ag2 `catch` disconnect
                           tid2 <- forkIO $ wire ag2 ag1 `catch` disconnect

                           forkIO $ do takeMVar disc
                                       killThread tid1
                                       killThread tid2
                                       putMVar finishedDisc ()
                           return $ AgentConnection disc finishedDisc

killAgentConnection (AgentConnection kill done) = putMVar kill () >> takeMVar done

waitTMVar v = takeTMVar v >>= putTMVar v

select :: [(Agent, IO ())] -> IO ()
select pairs = join . atomically $ foldr1 orElse actions
  where actions = map toStm pairs
        toStm (Agent _ _ done _, io) = waitTMVar done >> return io
