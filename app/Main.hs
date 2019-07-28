{-# LANGUAGE OverloadedStrings #-}

module Main where

import SDL    
import Lib
import SDL.Time (ticks)
import Control.Monad (unless)
import Control.Lens
import Data.Maybe (catMaybes)


main :: IO ()
main = do
    initializeAll
    window <- createWindow "My SDL Application" defaultWindow
    renderer <- createRenderer window (-1) defaultRenderer
    initialTick <- ticks
    appLoop (initialState initialTick) renderer
  
appLoop :: State -> Renderer -> IO ()
appLoop state renderer = do
    events <- pollEvents
    tick <- ticks
    let qPressed = any eventIsQPress events
        updates = concat [
          if tick - view lastTick state > 1000 then [Tick tick] else [],
          catMaybes (map (action . eventPayload) events)
          ]
        newState = foldl (\s u -> update u s) state updates
    -- putStrLn $ "Events: " ++ show updates    
    -- putStrLn $ "New State: " ++ show (view active newState) 
    renderState newState renderer
    unless qPressed (appLoop newState renderer)
   
eventIsQPress :: Event -> Bool   
eventIsQPress event =
  case eventPayload event of
    KeyboardEvent keyboardEvent ->
      keyboardEventKeyMotion keyboardEvent == Pressed &&
      keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
    _ -> False

action :: EventPayload -> Maybe Update
action (KeyboardEvent keyboardEvent)
  | keyboardEventKeyMotion keyboardEvent == Pressed = extractEvent (keysymKeycode (keyboardEventKeysym keyboardEvent))
  | otherwise = Nothing
  where
    extractEvent KeycodeLeft  = Just MoveLeft
    extractEvent KeycodeRight = Just MoveRight
    extractEvent KeycodeUp    = Just Rotate
    extractEvent KeycodeDown  = Just Drop
    extractEvent _            = Nothing

action _ = Nothing    
      
