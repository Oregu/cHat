module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Network
import System.IO
import Monad
import Control.Concurrent

port = PortNumber 14003
host = "localhost"

main = do
	initGUI
	window <- createGUI
	widgetShowAll window
	mainGUI

createGUI :: IO Window
createGUI = do
	Just xml <- xmlNew "cHat.glade"
	sendButton <- xmlGetWidget xml castToButton "sendButton"
	messagesView <- xmlGetWidget xml castToTextView "messagesView"
	messageEntry <- xmlGetWidget xml castToEntry "messageEntry"
	serverConn <- connect
	hSetBuffering serverConn LineBuffering
	forkIO $ listenServer serverConn messagesView
	onClicked sendButton $ sendClicked serverConn messageEntry
	window   <- xmlGetWidget xml castToWindow "chatWindow"
	onDestroy window $ hClose serverConn >> mainQuit
	return window

listenServer :: Handle -> TextView -> IO ()
listenServer sock tv = do
	putStrLn "Waiting for server"
	msgs <- liftM (++ "\n") $ hGetLine sock
	putStrLn $ "readed " ++ msgs ++ " from socket"
	if (null msgs)
		then listenServer sock tv
		else do
			tb <- textViewGetBuffer tv
			endIter <- textBufferGetEndIter tb
			textBufferInsert tb endIter msgs
			listenServer sock tv

sendClicked :: Handle -> Entry -> IO ()
sendClicked conn entry = do
	text <- get entry entryText
	unless (null text) $ sendToServer conn text

connect :: IO Handle
connect = connectTo host port

sendToServer :: Handle -> String -> IO ()
sendToServer h text = hPutStrLn h text >> hFlush h 

