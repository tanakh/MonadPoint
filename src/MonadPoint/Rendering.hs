{-# LANGUAGE ExistentialQuantification #-}

module MonadPoint.Rendering where

import Control.Monad.State
import Control.Monad.Writer hiding (All)

import Data.IORef
import Data.List
import Data.Maybe

import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.FTGL hiding (AlignLeft, AlignCenter, AlignRight)
import Graphics.UI.GLUT as GLUT hiding (Font)

import Codec.Image.STB as STB

import qualified MonadPoint.Config as Config

--

defaultWindowSize = Size 800 600
fontSize = 128 :: Int

type Rendering a = StateT RenderState IO a

data RenderState =
  RenderState
  {
    datDir :: String,
    
    aspect :: Double, -- アスペクト比
    font :: Font, -- フォント
    fixFont :: Font, -- 等幅フォント
    
    imgs :: [(String, TextureObject)], -- 画像
    
    kmStat :: IORef KeyMouseState, -- キー・マウス情報
    
    tick :: Double, -- ページ切り替え率
    curPage :: Rendering (), -- PageData, -- 現在のページ
    nextPage :: Maybe (Rendering ()), -- Maybe PageData, -- 遷移先ページ
    direction :: Bool, -- 遷移方向
    
    plugins :: [Rendering ()] -- プラグイン
  }

data KeyMouseState =
  KeyMouseState
  { prevKeys :: [Key]
  , curKeys  :: [Key]
  }

kmProc stat key keyState mod pos = do
  case keyState of
    Down -> do
      modifyIORef stat $ \s -> s { curKeys = curKeys s \\ [key] }
    Up -> do
      modifyIORef stat $ \s -> s { curKeys = nub $ key : curKeys s }

kmsIsKeyPress :: Key -> KeyMouseState -> Bool
kmsIsKeyPress k stat =
  not (k`elem`prevKeys stat) && (k`elem`curKeys stat)

isKeyPress :: Key -> Rendering Bool
isKeyPress key = do
  kmsr <- gets kmStat
  kms  <- liftIO $ readIORef kmsr
  return $ kmsIsKeyPress key kms

initRenderState dir (Size w h) font ffont kmsr startPage =
  RenderState
  { datDir = dir 
  , aspect = 600/800
  , font = font
  , fixFont = ffont
  , imgs = []
  , kmStat = kmsr
  , tick = 0
  , curPage = startPage
  , nextPage = Nothing
  , direction = False
  , plugins = []
  }

initRenderer :: Config.Config -> Rendering a -> IO RenderState
initRenderer cfg m = do 
  blend $= Enabled
  -- blendEquation $= FuncAdd
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  
  textureFunction $= Replace
  texture Texture2D $= Enabled
  
  -- multisample $= Enabled
  -- polygonSmooth $= Enabled
  -- hint PolygonSmooth $= Nicest
  
  let datdir = Config.datDir cfg ++ "/"
  let pfont = Config.pFont cfg
  let ffont = Config.fFont cfg

  font <- createPolygonFont (datdir ++ pfont)
  setFontFaceSize font fontSize fontSize
  setFontDepth font 1.0
  
  ffont <- createPolygonFont (datdir ++ ffont)
  setFontFaceSize ffont fontSize fontSize
  setFontDepth ffont 1.0
  
  kmsr <- newIORef $ KeyMouseState [] []

  return $ initRenderState datdir defaultWindowSize font ffont kmsr (m >> return ())
  
bg :: Double -> Rendering()
bg alpha = do
  liftIO $ renderPrimitive Polygon $ do
    color $ Color4 0.4 0.4 (0.6 :: GLdouble) (realToFrac alpha)
    vertex $ Vertex2 ( 0) ( 0::GLdouble)
    vertex $ Vertex2 ( 1) ( 0::GLdouble)
    color $ Color4 0.1 0.1 (0.1 :: GLdouble) (realToFrac alpha)
    vertex $ Vertex2 ( 1) ( 1::GLdouble)
    vertex $ Vertex2 ( 0) ( 1::GLdouble)

renderPage :: Rendering ()
renderPage = do
  liftIO $ clearColor $= Color4 0 0 0 0
  liftIO $ clear [ColorBuffer]
  
  Size ww hh <- liftIO $ GLUT.get windowSize
  modify $ \s -> s { aspect = fromIntegral hh / fromIntegral ww }
  
  liftIO $ loadIdentity
    
  liftIO $ do
    translate $ Vector3 (-1) (-1) (0 :: GLdouble)
    scale 2.0 2.0 (1 :: GLdouble)
  
  bg 1
  
  cp <- gets curPage
  np <- gets nextPage
  d  <- gets direction
  case np of
    Nothing -> do
      cp
      return ()
    Just np -> do
      tc <- gets tick
      if d
        then do
        preserving $ do
          transl (-tc*1) 0
          cp
          bg $ min 1 (tc*2)
        preserving $ do
          transl (1-tc*1) 0
          np
          bg $ min 1 (2-tc*2)
        else do
        preserving $ do
          transl (-(1-tc)*1) 0
          np
          bg $ min 1 ((1-tc)*2)
        preserving $ do
          transl (1-(1-tc)*1) 0
          cp
          bg $ min 1 (2-(1-tc)*2)
      if (tc+1/20>1)
        then do
        modify $ \s -> s { tick = 0, curPage = np, nextPage = Nothing }
        else do
        modify $ \s -> s { tick = tc+1/20 }

  pins <- gets plugins
  sequence_ pins

  kmsr <- gets kmStat
  kms  <- liftIO $ readIORef kmsr
  liftIO $ writeIORef kmsr $ kms { prevKeys = curKeys kms }

  liftIO $ swapBuffers

class Page a where
  render :: a -> Rendering ()
  
data PageData = forall a. (Page a) => PageData a

instance Page PageData where
  render (PageData p) = render p

preserving :: Rendering a -> Rendering a
preserving m = do
  a <- gets aspect
  liftIO glPushMatrix
  ret <- m
  liftIO glPopMatrix
  modify $ \s -> s { aspect = a }
  return ret

scale2 :: Double -> Double -> Rendering ()
scale2 w h = do
  liftIO $ scale (realToFrac w) (realToFrac h) (1 :: GLdouble)
  modify $ \s -> s { aspect = aspect s * h / w }

transl :: Double -> Double -> Rendering ()
transl w h = do
  liftIO $ translate $ Vector3 (realToFrac w) (realToFrac h) (0 :: GLdouble)

square :: IO a -> Rendering a
square m = do
  a <- gets (realToFrac . aspect)
  liftIO $ preservingMatrix $ do
    scale a 1 (1 :: GLdouble)
    m

getImage :: String -> Rendering TextureObject
getImage fname = do
  is <- gets imgs
  case lookup fname is of
    Just tex -> return tex
    Nothing -> do
      datdir <- gets datDir
      tex <- liftIO $ loadTexture (datdir ++ fname)
      modify $ \s -> s { imgs = is ++ [(fname, tex)] }
      return tex

loadTexture :: String -> IO TextureObject
loadTexture fname = do
  rs <- loadImage fname
  case rs of
    Left err -> error err
    Right img -> do
      withImage img $ \ptr (w, h) comp -> do
        -- print (w, h, comp)
        [tex] <- genObjectNames 1
        textureBinding Texture2D $= Just tex              
        textureWrapMode Texture2D S $= (Repeated, Repeat)
        textureWrapMode Texture2D T $= (Repeated, Repeat)
        textureFilter Texture2D $= ((Linear', Nothing), Linear')
        texImage2D Nothing NoProxy 0 RGBA' (TextureSize2D w h) 0 $
          PixelData (if comp==4 then RGBA else RGB) UnsignedByte ptr
        return tex

changePage :: Rendering a -> Rendering ()
changePage p = do
  np <- gets nextPage
  when (isNothing np) $ do
    modify $ \s -> s
      { tick = 0
      , nextPage = Just $ p >> return ()
      , direction = True
      }

backwordPage :: Rendering a -> Rendering ()
backwordPage p = do
  np <- gets nextPage
  when (isNothing np) $ do
    modify $ \s -> s
      { tick = 0
      , nextPage = Just $ p >> return ()
      , direction = False
      }

installPlugin :: Rendering () -> Rendering ()
installPlugin p = do
  modify $ \s -> s { plugins = p : plugins s }
