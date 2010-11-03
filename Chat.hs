module Main where

import Graphics.UI.Gtk

main = do
	initGUI
	window <- createGUI
	widgetShowAll window
	mainGUI

createGUI :: IO Window
createGUI = do
	window <- windowNew
	button <- buttonNew
	entry <- entryNew
	textView <- textViewNew
	hpane <- hPanedNew
	rootpane <- vPanedNew
	panedAdd1 hpane entry
	panedAdd2 hpane button
	panedAdd1 rootpane textView
	panedAdd2 rootpane hpane
	set textView [ widgetHeightRequest := 300
				  ,widgetWidthRequest := 400 ]
	set entry [ widgetWidthRequest := 200 ]
	set window [ containerBorderWidth := 5
				,containerChild := rootpane ]
	set button [ buttonLabel := "Hello World" ]
	onClicked button clickEvent
	onDestroy window mainQuit
	return window

clickEvent :: IO ()
clickEvent = putStrLn "Hello World"
