{-# LANGUAGE OverloadedStrings #-}

module Main where
import qualified SDL
import qualified SDL.Font as SDLFont
import qualified SDL.Time as SDLTime
import Lib
import Control.Monad (unless)
import Control.Lens
import Data.Maybe (catMaybes)


main :: IO ()
main = do
    initLibs
    window <- SDL.createWindow "My SDL Application"  SDL.defaultWindow
    renderer <-  SDL.createRenderer window (-1)  SDL.defaultRenderer
    initialTick <- SDLTime.ticks
    appLoop (initialState initialTick) renderer
  
initLibs :: IO ()
initLibs = do
  SDL.initializeAll
  SDLFont.initialize

shutdown :: IO ()
shutdown = do
  SDLFont.quit
  SDL.quit

appLoop :: State -> SDL.Renderer -> IO ()
appLoop state renderer = do
    events <-  SDL.pollEvents
    tick <- SDLTime.ticks
    let qPressed = any eventIsQPress events
        updates = concat [
          if tick - view lastTick state > 1000 then [Tick tick] else [],
          catMaybes (map (action .  SDL.eventPayload) events)
          ]
        newState = foldl (\s u -> update u s) state updates
    renderState newState renderer
    unless qPressed (appLoop newState renderer)
   
eventIsQPress :: SDL.Event -> Bool   
eventIsQPress event =
  case  SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent ->
      SDL.keyboardEventKeyMotion keyboardEvent ==  SDL.Pressed &&
      SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) ==  SDL.KeycodeQ
    _ -> False

action :: SDL.EventPayload -> Maybe Update
action (SDL.KeyboardEvent keyboardEvent)
  |  SDL.keyboardEventKeyMotion keyboardEvent ==  SDL.Pressed = extractEvent ( SDL.keysymKeycode ( SDL.keyboardEventKeysym keyboardEvent))
  | otherwise = Nothing
  where
    extractEvent SDL.KeycodeLeft  = Just MoveLeft
    extractEvent SDL.KeycodeRight = Just MoveRight
    extractEvent SDL.KeycodeUp    = Just Rotate
    extractEvent SDL.KeycodeDown  = Just Drop
    extractEvent _            = Nothing

action _ = Nothing    
      
