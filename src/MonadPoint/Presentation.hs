{-# LANGUAGE ExistentialQuantification #-}

module MonadPoint.Presentation where

import Control.Monad.State
import Control.Monad.Writer hiding (All)

import Debug.Trace
  
import Graphics.Rendering.FTGL hiding (AlignLeft, AlignCenter, AlignRight)
import Graphics.UI.GLUT as GLUT hiding (Font, initialize)

import Codec.Binary.UTF8.String as U8

import MonadPoint.Rendering hiding (render)
import MonadPoint.Font

import Prelude hiding (seq)

--

zero :: GLdouble
zero = 0

type PresentationM c a = WriterT [c] IO a
type Presentation a = PresentationM Component a

execPresentation :: Presentation a -> IO Component
execPresentation p = do
  cs <- execWriterT p
  return $ seqs cs

class IsComponent c where
  initialize :: c -> Rendering ()
  initialize _ = return ()

  render :: c -> Rendering ()
  render _ = return ()

data Component = forall a . (IsComponent a) => Component a

instance IsComponent Component where
  initialize (Component c) =
    initialize c
  render (Component c) =
    render c

tellc :: IsComponent c => c -> Presentation ()
tellc c = tell [ Component c ]

--

instance IsComponent ()

--

data Seq = Seq Component Component

instance IsComponent Seq where
  initialize (Seq c d) = do
    initialize c
    initialize d
  
  render (Seq c d) = do
    render c
    render d

seq :: (IsComponent a, IsComponent b) => a -> b -> Component
seq c d = Component $ Seq (Component c) (Component d)

seqs :: [Component] -> Component
seqs cs = foldl1 (MonadPoint.Presentation.seq) cs

--

data Scaled = Scaled Double Double Component

instance IsComponent Scaled where
  render (Scaled w h c) = do
    preserving $ do
      transl ((1-w)/2) ((1-h)/2)
      scale2 w h
      render c

scale :: Double -> Presentation a -> Presentation a
scale scl p = do
  censor (\ls -> [Component $ Scaled scl scl $ seqs ls]) p

scalev :: Double -> Presentation a -> Presentation a
scalev scl p = do
  censor (\ls -> [Component $ Scaled scl 1 $ seqs ls]) p

scaleh :: Double -> Presentation a -> Presentation a
scaleh scl p = do
  censor (\ls -> [Component $ Scaled 1 scl $ seqs ls]) p

scalevl :: Double -> Presentation a -> Presentation a
scalevl scl p = do
  censor (\ls -> [ vert scl (seqs ls) () ]) p

scalevr :: Double -> Presentation a -> Presentation a
scalevr scl p = do
  censor (\ls -> [ vert (1-scl) () (seqs ls) ]) p

scalehu :: Double -> Presentation a -> Presentation a
scalehu scl p = do
  censor (\ls -> [ holz scl (seqs ls) () ]) p

scalehd :: Double -> Presentation a -> Presentation a
scalehd scl p = do
  censor (\ls -> [ holz (1-scl) () (seqs ls) ]) p

--

data Vertical = Vertical Double Component Component

instance IsComponent Vertical where
  render (Vertical r u l) = do
    preserving $ do
      scale2 r 1
      render u
    preserving $ do
      liftIO $ translate $ Vector3 (realToFrac r) 0 (0 :: GLdouble)
      scale2 (1 - r) 1
      render l

vert :: (IsComponent a, IsComponent b) => Double -> a -> b -> Component
vert d a b = Component $ Vertical d (Component a) (Component b)
      
--

data Holizontal = Holizontal Double Component Component

instance IsComponent Holizontal where
  render (Holizontal r u l) = do
    preserving $ do
      liftIO $ translate $ Vector3 0 (1 - realToFrac r) (0 :: GLdouble)
      scale2 1 r
      render u
    preserving $ do
      scale2 1 (1 - r)
      render l

holz :: (IsComponent a, IsComponent b) => Double -> a -> b -> Component
holz d a b = Component $ Holizontal d (Component a) (Component b)

holstack :: Presentation a -> Presentation a
holstack m =
  censor (\ls -> [ layoutH ls ]) m
  where
    layoutH [] = Component ()
    layoutH ccs@(c:cs) = do
      holz (1/fromIntegral tot) c (layoutH cs)
      where
        tot = length ccs

--

data TextComponent = TextComponent Bool Alignment String

data Alignment = AlignLeft | AlignCenter | AlignRight

instance IsComponent TextComponent where
  render (TextComponent f algn s) = do
    let us = U8.encodeString s
    f <- gets (if f then fixFont else font)
    a <- gets (realToFrac . aspect)
    liftIO $ color $ Color3 1 1 (1 :: GLdouble)
    preserving $ do
      bb <- liftIO $ GLUT.get blend
      liftIO $ blend $= Enabled
      (w, h) <- getFontSize f us
      --liftIO $ print (w, h)
      --liftIO $ print a
      transl 0 0.15
      if h/w <= a
        then do
        liftIO $ translate $ Vector3 (0) ((a-h/w)/a/2) (0 :: GLdouble)
        scale2 (1/realToFrac w) (1/realToFrac w)
        else do
        case algn of
          AlignLeft   -> transl 0 0
          AlignCenter -> transl (realToFrac $ (1/a-w/h)/(1/a)/2) 0
          AlignRight  -> transl (realToFrac $ (1/a-w/h)/(1/a)) 0
        scale2 (realToFrac a/realToFrac h) (realToFrac a/realToFrac h)
      scale2 (1.0/fromIntegral fontSize) (1.0/fromIntegral fontSize)
      square $ renderFont f us All
      liftIO $ blend $= bb

txt algn str = tellc $ TextComponent False algn str
txtl str = txt AlignLeft str
txtc str = txt AlignCenter str
txtr str = txt AlignRight str

--
      
{-
data Box = Box

instance IsComponent Box where
  render Box = do
    liftIO $ color $ Color3 1 0 (0 :: GLdouble)
    liftIO $ renderPrimitive LineLoop $ do
      vertex $ Vertex2 ( 0) ( 0::GLdouble)
      vertex $ Vertex2 ( 1) ( 0::GLdouble)
      vertex $ Vertex2 ( 1) ( 1::GLdouble)
      vertex $ Vertex2 ( 0) ( 1::GLdouble)

box = tellc Box
-}

--

data Picture = Picture String

instance IsComponent Picture where
  render (Picture name) = do
    img <- getImage name
    liftIO $ do
      textureBinding Texture2D $= Just img
      preservingMatrix $ do
        renderPrimitive Quads $ do
          texCoord $ TexCoord2 0 (1 :: GLdouble)
          vertex   $ Vertex3   0  0 zero
          texCoord $ TexCoord2 0 (0 :: GLdouble)
          vertex   $ Vertex3   0  1 zero
          texCoord $ TexCoord2 1 (0 :: GLdouble)
          vertex   $ Vertex3   1  1 zero
          texCoord $ TexCoord2 1 (1 :: GLdouble)
          vertex   $ Vertex3   1  0 zero
      textureBinding Texture2D $= Nothing

pict :: String -> Presentation ()
pict fname = tellc $ Picture fname

--

page :: Presentation a -> Presentation Component
page p = do
  (_, [r]) <- listens id $ censor (\ls -> [seqs ls]) p
  return r

--

data ListItem =
  ListItem [String] |
  UList [ListItem]
  deriving (Show)

li :: String -> PresentationM ListItem ()
li str =
  tell [ ListItem (lines str) ]

ul :: PresentationM ListItem a -> PresentationM ListItem a
ul m =
  censor (\ls -> [ UList ls ]) m

list :: PresentationM ListItem a -> Presentation a
list m =
  mapWriterT f m
  where
    f m = do
      (r, w) <- m
      return (r, [layoutList w])

layoutList :: [ListItem] -> Component
layoutList ls
  | hul >= 5 = ret
  | otherwise = holz (hul/5) ret ()
  where
    decay = 0.8
    leftMergin = 0.075
    
    (hul, ret) = layoutLists ls
    
    layoutLists ls =
      let (heights, comps) = unzip $ map layoutOne ls in
      (sum heights, layoutH heights comps)
    
    layoutOne (UList ls) =
      let (height, comp) = layoutLists ls in
      (height*decay, vert leftMergin () comp)
    
    layoutOne (ListItem ss) =
      (fromIntegral len,
       vert leftMergin
       (layoutH (replicate len 1) [ tcomp AlignLeft $ if ix==1 then "・" else " " | ix <- [1..len] ])
       (layoutH (replicate len 1) $ map (tcomp AlignLeft) ss))
      where
        len = length ss
        tcomp algn s = Component $ TextComponent False algn s

    layoutH [] [] = Component ()
    layoutH hhs@(h:hs) (c:cs) = do
      holz (h/tot) c (layoutH hs cs)
      where
        tot = sum hhs

--

data Box = Box (Color3 GLdouble) (Color3 GLdouble) (Color3 GLdouble) (Color3 GLdouble)

instance IsComponent Box where
  render (Box c1 c2 c3 c4) = do
    liftIO $ renderPrimitive Polygon $ do
      color c1
      vertex $ Vertex2 ( 0) ( 0::GLdouble)
      color c2
      vertex $ Vertex2 ( 1) ( 0::GLdouble)
      color c3
      vertex $ Vertex2 ( 1) ( 1::GLdouble)
      color c4
      vertex $ Vertex2 ( 0) ( 1::GLdouble)

box c1 c2 c3 c4 =
  tellc $ Box c1 c2 c3 c4

txtarea ls = do
  box (Color3 0.1 0.1 0.1) (Color3 0.1 0.1 0.1) (Color3 0.2 0.2 0.2) (Color3 0.2 0.2 0.2)
  MonadPoint.Presentation.scale 0.9 $ do
    tellc $
      foldr (\(l, sb) r -> holz (1/fromIntegral (h-sb)) (TextComponent True AlignLeft l) r)
      (Component ()) $
      zip mls [0..]
  where
    h = length ls
    ml = maximum (map length ls)
    mls = map (\l -> take ml $ l ++ cycle " ") ls

--

data KeyEvent = KeyEvent [Key] (Rendering ())

instance IsComponent KeyEvent where
  render (KeyEvent key r) = do
    ps <- mapM isKeyPress key
    when (or ps) $ r

onKey :: [Key] -> Rendering () -> Presentation ()
onKey key r =
  tellc $ KeyEvent key r

next :: Component -> Component
next c = Component $ KeyEvent ks $ change c
  where
    ks = [MouseButton LeftButton, SpecialKey KeyRight]

prev :: Component -> Component
prev c = Component $ KeyEvent ks $ backword c
  where
    ks = [SpecialKey KeyLeft]


change :: Component -> Rendering ()
change c = changePage $ render c

backword :: Component -> Rendering ()
backword c = backwordPage $ render c

--

data Plugin = Plugin (Rendering ())

instance IsComponent Plugin where
  initialize (Plugin r) = do
    -- liftIO $ putStrLn "install"
    installPlugin r

plugin :: Rendering () -> Presentation ()
plugin p = tellc $ Plugin p

--
      
pages :: Presentation a -> Presentation Component
pages m = do
  (_, [r]) <- listens id $ censor (\ls -> [pageConcat ls]) m
  return r

pageConcat :: [Component] -> Component
pageConcat ps = head ret
  where
    totl = length ps
    qs = map (\(p, i) -> seq p $ scl 0.9 $ sclrd 0.05 0.05 $ 
                         TextComponent False AlignLeft $
                         show i ++ "/"++ show totl) $
         zip ps [1..]
    
    ret = [ prp (nxp p (ix+1)) (ix-1)
          | (p, ix) <- zip qs [0..]]
    
    nxp p ix | ix<length ret = seq p $ next $ ret !! ix
             | otherwise = p

    prp p ix | ix>=0 = seq p $ prev $ ret !! ix
             | otherwise = p

    scl d m = Component $ Scaled d d m
    sclrd w h m =
      holz (1-h) () $
      vert (1-w) () m
