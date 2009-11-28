module MonadPoint.Font (
  getFontSize
  ) where

import Control.Monad.State

import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.FTGL hiding (AlignLeft, AlignCenter, AlignRight)
import Graphics.UI.GLUT as GLUT hiding (Font)
import MonadPoint.Rendering

--

getFontSize :: Font -> String -> Rendering (GLdouble, GLdouble)
getFontSize f s = do
  a <- gets (realToFrac . aspect)
  preserving $ liftIO $ do
    mat <- GLUT.get currentMatrix
    nc <- getMatrixComponents RowMajor (mat :: GLmatrix GLdouble)
    GLUT.scale (1.0/fromIntegral fontSize*a) (1.0/fromIntegral fontSize) (1 :: GLdouble)
    mat <- GLUT.get currentMatrix
    mc <- getMatrixComponents RowMajor (mat :: GLmatrix GLdouble)
    [x1,y1,z1,x2,y2,z2] <- liftM (map realToFrac) $ getFontBBox f s
    --print [x1, y1, z1]
    --print [x2, y2, z2]
    --mapM_ print $ take 4 $ map (take 4) $ iterate (drop 4) mc
    let [ax,ay,az,aw] = matMult mc [x1,0,z1,1]
        [bx,by,bz,bw] = matMult mc [x2,fromIntegral fontSize,z2,1]
        [cx,cy,cz,cw] = matMult nc [0,0,0,1]
        [dx,dy,dz,dw] = matMult nc [1,1,0,1]
    --print [ax, ay, az, aw]
    --print [bx, by, bz, bw]
    --print [cx, cy, cz, cw]
    --print [dx, dy, dz, dw]
    return (abs $ (bx/bw - ax/aw)/(dx/dw - cx/cw),
            abs $ (by/bw - ay/aw)/(dx/dw - cx/cw))

matMult [a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4] [x,y,z,w] =
  [ sum $ zipWith (*) [a1,a2,a3,a4] [x,y,z,w]
  , sum $ zipWith (*) [b1,b2,b3,b4] [x,y,z,w]
  , sum $ zipWith (*) [c1,c2,c3,c4] [x,y,z,w]
  , sum $ zipWith (*) [d1,d2,d3,d4] [x,y,z,w]]
