module MonadPoint.Base( 
  Config(..),
  runPresentation,
  ) where

import Control.Exception
import Control.Monad.State

import Data.IORef

import System.Exit

import Graphics.UI.GLUT as GLUT

import MonadPoint.Rendering
import MonadPoint.Presentation as P
import MonadPoint.Config

--

runPresentation :: Config -> Presentation a -> IO ()
runPresentation cfg m = do
  (_, _) <- getArgsAndInitialize
  
  initialDisplayMode $= [RGBAMode, DoubleBuffered]
  initialWindowSize $= defaultWindowSize
  _ <- createWindow "MonadPoint"
  
  comp <- execPresentation m
  stat <- initRenderer cfg (P.render comp)
  stat <- execStateT (P.initialize comp) stat
  stat <- newIORef stat

  kmsr  <- liftM kmStat $ readIORef stat
  
  displayCallback $= displayProc stat
  keyboardMouseCallback $= Just (kmProc kmsr)
  addTimerCallback 0 $ timerProc $ displayProc stat
  
  attachMenu RightButton
    (Menu [
        MenuEntry "&Full" toggleScreen,
        MenuEntry "&Exit" exitLoop])
    
  mainLoop

exitLoop :: IO a
exitLoop = throwIO ExitSuccess

toggleScreen :: IO ()
toggleScreen = fullScreen

initMatrix :: IO ()
initMatrix = do
  wsize <- GLUT.get windowSize
  viewport $= (Position 0 0, wsize)
  matrixMode $= Projection
  
timerProc m = do
  addTimerCallback (1000`div`60) (timerProc m)
  m

displayProc stat = do
  rs <- readIORef stat
  rs <- execStateT renderPage rs
  writeIORef stat rs
