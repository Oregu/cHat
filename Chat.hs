module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

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
	onClicked sendButton $ sendClickedTest messagesView
	window   <- xmlGetWidget xml castToWindow "chatWindow"
	onDestroy window mainQuit
	return window

sendClickedTest :: TextView -> IO ()
sendClickedTest tv = do
	tb <- textViewGetBuffer tv
	endIter <- textBufferGetEndIter tb
	textBufferInsert tb endIter "Hello world!\n"

