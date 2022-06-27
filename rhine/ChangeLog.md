# Revision history for rhine

The version numbering follows the package `dunai`.
Since `rhine` reexports modules from `dunai`,
every major version in `dunai` triggers a major version in `rhine`.

## 0.8.0.0

* Documentation improvements
* Support for GHC 9.0.2
* Updated to `dunai-0.8`
* Added functions to pre-/post-compose SNs and Rhines with ClSFs
* Added flake & stack support on CI.
  Thank you, Miguel Negrão and Jun Matsuhita!

## 0.7.0

* Replaced old reactimation mechanism by clock erasure
* Dropped GHC support for < 8.4
* Reworked `gloss` backends.
  There are now two pure backends and an `IO` backend.
* Relaxed all upper version bounds

## 0.6.0

* Synced with `dunai` version numbers
* Supported GHC 8.8.3

## 0.5.1.0

* Synced with `dunai` version numbers
* Supported GHC 8.6
* Added support for randomness with `RandT` in `ClSF`s

## 0.5.0.0

* Deprecated GHC 7.*
* Big module reorganisation:
  * Renamed `SyncSF` to `ClSF` and many other renames
    (https://github.com/turion/rhine/issues/45)
  * `FRP.Rhine` by default exports all components
    (signal functions, clocks, schedules, resampling buffers)
* Refactored some fixed step clocks
* Added interpolation buffers

Note that this is the first release that is not in sync
with `dunai`'s version numbers.
`rhine-0.5` depends on `dunai-0.4`.

## 0.4.0.0 -- 2017.12.04

* Documentation typos fixed
* Added `ChangeLog.md`

## 0.3.0.0

* Version bump
* Documentation typos fixed (Thanks to Gabor Greif)

## 0.2.0.0

* Travis CI support
* Removed several utilities that are now in `dunai`
* Extended averaging functions

## 0.1.1.0

* Added `FRP.Rhine.Clock.Realtime.Stdin` (console keyboard event clock)
* Added `FRP.Rhine.Clock.Select` (event selection clock)
* Added `FRP.Rhine.ClSF.Except` (synchronous exception handling)

## 0.1.0.0

* Initial version
