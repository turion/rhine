# README

This package provides a simple wrapper for the `gloss` library,
or rather the function `Graphics.Gloss.play`,
enabling you to write `gloss` applications as signal functions.
An example "gears" program, which you can run as `cabal run rhine-gloss-gears`
or `stack build && stack exec rhine-gloss-gears`,
now becomes as simple as:

```haskell
module README where

import FRP.Rhine.Gloss


-- | Calculate a gear wheel rotated by a certain angle.
gears :: Float -> Picture
gears angle = color green $ pictures
  $ circleSolid 60
  : map (rotate angle) [ rotate (45 * n) $ rectangleSolid 20 150 | n <- [0..3] ]

-- | Rotate the gear with a constant angular velocity.
mainClSF :: GlossClSF
mainClSF = timeInfoOf sinceInit >>> arr (* 50) >>> arr gears

main :: IO ()
main = flowGloss defaultSettings mainClSF
```

## Installation

Since `gloss` is based on `OpenGL`,
you will need to install the same dependencies as for that.
Typically, this are the `GL`, `GLU` and `GLUT` libraries.
(On Debian-like systems, `sudo apt-get install libgl1-mesa-dev libglu1-mesa-dev freeglut3-dev` should suffice.)

When building with `stack`, it is assumed that you have `nix-shell` installed,
which will automatically get these dependencies for you.
