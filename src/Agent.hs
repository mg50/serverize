{-# LANGUAGE ScopedTypeVariables #-}
module Agent (Agent,
              makeAgent,
              writeAgent,
              readAgent,
              killAgentSync,
              connectAgents,
              select)
       where
import Prelude hiding (catch)
import System.IO
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TMVar
import Control.Exception

data Agent = Agent { agentRead :: TChan String
                   , agentWrite :: TChan String
                   , agentClose :: (TMVar (), TMVar ()) }

makeAgent hIn hOut = do readChan <- newTChanIO
                        writeChan <- newTChanIO
                        done1 <- newEmptyTMVarIO
                        done2 <- newEmptyTMVarIO

                        let safely m = m `catch` \(e :: SomeException) ->
                              atomically $ putTMVar done1 ()

                        readTid <- forkIO . safely . forever $ do
                          msg <- hGetChar hOut
                          atomically $ writeTChan readChan [msg]

                        writeTid <- forkIO . safely . forever $ do
                          msg <- atomically $ readTChan writeChan
                          hPutStr hIn msg
                          hFlush hIn

                        forkIO $ forever $ do atomically $ takeTMVar done1
                                              killThread readTid
                                              killThread writeTid
                                              atomically $ putTMVar done2 ()

                        return $ Agent readChan writeChan (done1, done2)

writeAgent (Agent _ x _) = atomically . writeTChan x
readAgent (Agent x _ _) = atomically $ readTChan x
killAgentSync (Agent _ _ (d1, d2)) = atomically (putTMVar d1 ()) >> atomically (takeTMVar d2)

connectAgents ag1 ag2 = do disc <- newEmptyMVar
                           let disconnect = \(e :: SomeException) -> putMVar disc ()
                           let wire x y = forever $ do msg <- readAgent x
                                                       writeAgent y msg
                           tid1 <- forkIO $ wire ag1 ag2 `catch` disconnect
                           tid2 <- forkIO $ wire ag2 ag1 `catch` disconnect

                           forkIO $ do takeMVar disc
                                       killThread tid1
                                       killThread tid2

-- waitForAgent (Agent _ _ (_, d)) = modifyMVar_ d return

waitTMVar v = takeTMVar v >>= putTMVar v

select :: [(Agent, IO ())] -> STM (IO ())
select pairs = foldr1 orElse actions
  where actions = map toStm pairs
        toStm (Agent _ _ (_, done), io) = waitTMVar done >> return io
