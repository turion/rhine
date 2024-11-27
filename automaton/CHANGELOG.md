# Revision history for automaton

## Upcoming

* Added `Data.Automaton.Schedule` module with a new `MonadSchedule` class that
  works natively on `Automaton` values instead of monadic actions.
  Instances are provided for common monad transformers.
* Added `Data.Automaton.Schedule.Trans` module with `ScheduleT` and related transformers
  providing free waiting and scheduling effects
  (previously in `monad-schedule`).
* Removed the `monad-schedule` package dependency from `automaton`

## 1.7

* Add `safely`, `forever` and `foreverE` exception handling functions for streams
* Use `TimeDomain Seconds` extensively
* Add `|-|` and `||-||` resampling buffer utilities

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
