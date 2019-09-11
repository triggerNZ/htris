{-# LANGUAGE OverloadedStrings #-}

module Repl where

import Data.IORef
import Control.Concurrent

import qualified SDL
import qualified SDL.Font as SDLFont
import qualified SDL.Time as SDLTime

import System.IO.Unsafe (unsafePerformIO)

import Lib

type SRef = IORef State

initLibs :: IO ()
initLibs = do
    SDL.initializeAll
    SDLFont.initialize
    
shutdown :: IO ()
shutdown = do
    SDLFont.quit
    SDL.quit

demo :: SRef -> IO ()
demo ref = do
    initLibs
    window <- SDL.createWindow "REPL DEMO"  SDL.defaultWindow
    renderer <-  SDL.createRenderer window (-1)  SDL.defaultRenderer
    replLoop ref renderer
    shutdown
  
replLoop :: SRef -> SDL.Renderer -> IO ()
replLoop ref renderer = do
    st <- readIORef ref
    renderState st renderer
    replLoop ref renderer

startDemo :: IO ThreadId
startDemo = do
    thread <- forkIO (demo stateRef)
    return $ thread

stateRef :: SRef
stateRef = unsafePerformIO (newIORef (initialState 0))

modState :: (State -> State) -> IO ()
modState f = modState' stateRef f

modState' :: SRef -> (State -> State) -> IO ()
modState' ref f = modifyIORef ref f