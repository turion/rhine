{- | Common definitions for all @gloss@ backends.
-}

module FRP.Rhine.Gloss.Common
  ( module FRP.Rhine.Gloss.Common
  , module X
  ) where

-- gloss
import qualified Graphics.Gloss as X
import qualified Graphics.Gloss.Interface.Pure.Game as X
import Graphics.Gloss.Interface.Pure.Game


data GlossSettings = GlossSettings
  { display         :: Display      -- ^ Display mode (e.g. 'InWindow' or 'FullScreen').
  , backgroundColor :: Color        -- ^ Background color.
  , stepsPerSecond  :: Int          -- ^ Number of simulation steps per second of real time.
  }

defaultSettings :: GlossSettings
defaultSettings = GlossSettings
  { display         = InWindow "rhine-gloss" (400, 400) (10, 10)
  , backgroundColor = greyN 0.3
  , stepsPerSecond  = 30
  }
