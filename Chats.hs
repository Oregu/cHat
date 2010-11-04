module Main where

import Prelude hiding (catch)

import System.IO
import Network
import Control.Exception (finally, catch)
import Control.Concurrent

main = withSocketsDo $ do
	servSock <- listenOn $ PortNumber 14003
	putStrLn $ "listening on: " ++ show 14003
	acceptLoop servSock `finally` sClose servSock

acceptLoop :: Socket -> IO ()
acceptLoop servSock = do
	conn <- accept servSock
	forkIO $ processClient conn
	acceptLoop servSock

processClient :: (Handle, HostName, PortNumber) -> IO ()
processClient (handle, host, port) = do
	putStrLn $ "(" ++ show host ++ ":" ++ show port ++ ") new client"
	clStr <- hGetLine handle
	hPutStrLn handle $ "Hello, Client!\nYou said: " ++ clStr
	hFlush handle
	hClose handle

