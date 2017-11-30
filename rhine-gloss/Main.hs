{- | Example application for the @gloss@ wrapper. -}


-- rhine-gloss
import FRP.Rhine.Gloss


-- | Calculate a gear wheel rotated by a certain angle.
gears :: Float -> Picture
gears angle = color green $ pictures
  $ circleSolid 60
  : map (rotate angle) [ rotate (45 * n) $ rectangleSolid 20 150 | n <- [0..3] ]

-- | Rotate the gear with a constant angular velocity.
mainSyncSF :: GlossSyncSF a
mainSyncSF = timeInfoOf sinceStart >>> arr (* 50) >>> arr gears

main :: IO ()
main = flowGloss (InWindow "sonnen" (400, 400) (10, 10)) (greyN 0.3) 30
     $ buildGlossRhine Just mainSyncSF
