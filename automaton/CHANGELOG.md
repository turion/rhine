# Revision history for automaton

## 1.6

* Fix `lastS`. Thanks to Sebastian Wålinder for reporting.
* Add `FilterAutomaton`
* Extend the automaton library by many useful functions and instances
* Improve runtime performance considerably in many places by inlining
* Add [`ChangesetT`](https://hackage.haskell.org/package/changeset) support
* Add `AccumT` example

## 1.5

* Fixed naming Final vs. Recursive vs. Coalgebraic
* Added `forever` utility for recursion in `AutomatonExcept`
* Generalised `concatS`, added `throwOnMaybe`, added `mapOutput`
* Fixed some docs

## 1.4

* Added `Data.Automaton.Trans.Accum`
* Added `unfold_`
* Backwards compatible to 1.3, but to keep version numbers in sync with rhine, the automaton version has also been bumped

## 1.3

* Initial version
