# Revision history for rhine-gloss

## Upcoming

* Remove dependency on `monad-schedule` because of performance problems.
  See https://github.com/turion/rhine/issues/377.
* Revert scheduling in the `IO` backend to `IO`.

## 1.4

* Use `FreeAsyncT` in the gloss IO backend for fairer concurrency.
  See https://hackage.haskell.org/package/monad-schedule-0.2/docs/Control-Monad-Schedule-FreeAsync.html.
* Improvements, utilities, and hardenings for the IO backend

## 1.3

* Dropped `dunai` dependency in favour of state automata.
  See [the versions readme](./versions.md) for details.
* Support GHC 9.6 and 9.8

## 1.1

* dunai-0.11 compatibility
* Fixed bug in IO backend

## 1.0

* Removed schedules. See the [page about changes in version 1](/version1.md).
* Simplified `gloss` backends. Renamed `launchGlossThread` to `launchInGlossThread`.

## 0.7.0

* Reworked `gloss` backends.
  There are now two pure backends and an `IO` backend.

## 0.4.0.0 -- 2017.12.04

* Version bump

## 0.3.0.0  -- 2017-11-30

* Added simple example.

## 0.2.0.0  -- 2017-11-29

* First version. Version numbers follow rhine.
