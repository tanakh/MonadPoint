module MonadPoint.Config(
  Config(..),
  ) where

data Config =
  Config
  { datDir :: String
  , pFont :: String
  , fFont :: String
  }
