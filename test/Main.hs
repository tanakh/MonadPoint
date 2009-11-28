module Main(main) where

import MonadPoint

--

titlePage title name =
  scale 0.9 $ do
    scaleh 0.65 $ do
      scalehu 0.7 $ do
        holstack $ mapM_ txtc $ lines title
      scalehd 0.2 $ do
        txtc name

tmpl title m =
  page $ do
    scale 0.9 $ do
      scalehu 0.2 $ do
        txtc title
      scalehd 0.75 $ do
        m

--

myPresen = pages $ do
  titlePage
    "Super Presentation"
    "hoge"

  tmpl "Introduction..." $ do
    list $ do
      li "I love Haskell."
      li "I always use Haskell."
      li "About Haskel"
      ul $ do
        li "functional"
        li "lazy"
        li "super language!"

  tmpl "continue..." $ do
    list $ do
      li "continue..."

  return ()


--

cfg = Config "./dat" "ARISAKA.ttf" "ARISAKA_fix.ttf"

main :: IO ()
main = runPresentation cfg myPresen
