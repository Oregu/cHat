module Main where

import Prelude hiding (catch)

import System.IO
import Network
import Control.OldException (finally, catch)
import Control.Concurrent
import Monad

portn = 14003

main = withSocketsDo $ do
	servSock <- listenOn $ PortNumber portn
	putStrLn $ "cHat server: listening on port " ++ show portn
	chan <- newChan
	forkIO $ acceptLoop servSock chan `finally` sClose servSock
	dispatchProc servSock chan []

type Nick = String
type From = String
data Client = Client Handle Nick
data Command = Connect Client | Msg From String | Disconnect Client

acceptLoop :: Socket -> Chan Command -> IO ()
acceptLoop servSock chan = do
	(handle, server, port) <- accept servSock
	hSetBuffering handle NoBuffering
	let client = Client handle (show port)
	forkIO $ (processClient client chan) `catch` (\_ -> writeChan chan $ Disconnect client)
	writeChan chan $ Connect client
	acceptLoop servSock chan

processClient :: Client -> Chan Command -> IO ()
processClient c@(Client h n) chan =
	liftM (Msg n) (hGetLine h) >>= writeChan chan >> processClient c chan

dispatchProc :: Socket -> Chan Command -> [Client] -> IO ()
dispatchProc sock chan cls = do
	cmd <- readChan chan
	case cmd of
		(Connect cl@(Client _ nick)) -> do
			let cls' = cl:cls
			putStrLn $ "Total " ++ (show $ length cls') ++ " clients"
			broadcastFrom nick "Client connected!" cls'
			dispatchProc sock chan cls'
		(Msg who str) -> do
			broadcastFrom who str cls
			dispatchProc sock chan cls
		(Disconnect (Client dh nick)) -> do
			hClose dh
			let cls' = filter (\(Client h _) -> dh /= h) cls
			putStrLn "Client disconnected!"
			broadcastFrom nick "Client disconnected!" cls'
			dispatchProc sock chan cls'

broadcastFrom :: From -> String -> [Client] -> IO ()
broadcastFrom from msg = broadcast $ from ++ " >> " ++ msg

broadcast :: String -> [Client] -> IO ()
broadcast msg = mapM_ $ send msg

send :: String -> Client -> IO ()
send msg (Client h n) = (hPutStrLn h msg >> hFlush h) `catch` (\e -> putStrLn $ show e)

