import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Serializable
import Control.Distributed.Process.Node
import Network.Transport.TCP
import Network.Transport hiding (send)
import Data.ByteString.Char8 (pack)
import System.Environment
import Data.Maybe
import Message

main = do
    args <- getArgs
    case args of
        [ "slave", port ] -> do
            Right t <- createTransport "127.0.0.1" port defaultTCPParameters --Startup the network layer
            let addr = EndPointAddress (pack "127.0.0.1:44444:0") --Hardcoded namenode serverAddr
                                                                  --This should be the only Address that needs to be hardcoded
                nodeid = NodeId addr --define NodeId for the namenode
            node <- newLocalNode t initRemoteTable --Startup a local node using the network layer
            runProcess node $ client nodeid --run the client process

    
client :: NodeId -> Process ()
client nid = do
    whereisRemoteAsync nid "namenodePID" --Send a WhereIs to the namenode.
                                         --at this point the namenode should already be online
                                         --and have registered its pid under the name namenodePID
    WhereIsReply _ mpid <- expect :: Process WhereIsReply --Wait for the WhereIsReply, should probably use expectTimeOut
    say "received whereis" --Some logging for debugging
    (s,r) <- newChan --We create a Channel to communicate with the namenode
                     --It is possible to use it for two-way communication, but
                     --in that case there should be a ordering of the messages, because
                     --both the namenode and client will have the same receiveport so both are able
                     --to receive the message and we don't want either of them to send messages to themselves
    send (fromJust mpid) (SP s) --Send the sendport to the namenode so he can send a message to the client
    say "sent sendport"
    m <- receiveChan r --Wait for a message from the namenode
    say $ show m
    _ <- expect :: Process M --Only doing this so that "say" actually can get time to print out messages
    return ()
    