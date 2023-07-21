# Breaking changes from versions 0.* to 1.*

## Summary

In version 1.0, the `Schedule` type was removed.
Many combinators and type signatures are simplified as a consequence,
and `Rhine`s are built only of three ingredients instead of four:
`ClSF`s, `Clock`s, and `ResamplingBuffer`s.
You can now remove all the scheduling code from your project.

The scheduling functionality now comes from the [`monad-schedule`](https://hackage.haskell.org/package/monad-schedule/) library,
and to build up composite `Rhine`s using `ParallelClock` or `SequentialClock`,
the monad needs to be an instance of [`MonadSchedule`](https://hackage.haskell.org/package/monad-schedule/docs/Control-Monad-Schedule-Class.html#t:MonadSchedule).

## What you need to do

There are several simplifications that you may have to apply to your code in order to make it compile again:

| Versions 0.*                          | Versions 1.*                |
| ------------------------------------- | --------------------------- |
| `rhine1 \|\|@ sched @\|\| rhine2`     | `rhine1 \|@\| rhine2`       |
| `rhine1 ++@ sched @++ rhine2`         | `rhine1 +@+ rhine2`         |
| `rhine1 >-- buf -@- sched --> rhine2` | `rhine1 >-- buf --> rhine2` |
| `ParallelClock m cl1 cl2`             | `ParallelClock cl1 cl2`     |
| `SequentialClock m cl1 cl2`           | `SequentialClock cl1 cl2`   |

You can delete all functions of type `Schedule m cl1 cl2`.

You might have to implement the type class `MonadSchedule` for a custom monad you're using.
If it is only a newtype around some known transformers,
you will probably be done quickly by wrapping and mapping the constructor.
(Use e.g. [`GlossConcT`](https://hackage.haskell.org/package/rhine-gloss/docs/FRP-Rhine-Gloss-IO.html#t:GlossConcT) for inspiration.
 Unfortunately it seems that a `derive newtype` clause does not work.)

If it is a more complex monad, you will have to write this instance yourself.
Feel free to ping me (@turion) to help doing this.
Also, feel free open an issue here to support any kind of monad you encounter in a Haskell library on Hackage.

## Motivation

Schedules were cumbersome because of several reasons:

1. They were not composable across monad transformers.
   Given a schedule for a monad `m`, it was usually not possible to lift it into `t1 m` where `t1` is a transformer.
   Yet it was clear that there was some concept to abstract here, because their definitions were often repetitive.
   This was problematic in particular because it would sometimes force users to re-implement and wrap schedules for custom monads.
2. Several schedules were clock-specific, and basically re-implemented the constituent clocks.
   This was error prone code repetition.
3. They made some type signatures like `ParallelClock` and `SequentialClock` unnecessarily complicated, adding a monad parameter for no apparent reason.

## Technical solution

The problem is, at heart, about finding the right concurrency abstraction.
Clocks are allowed to block until a certain time is reached,
so scheduling several clocks means allowing them to block in parallel without disturbing each other.

The improvement was to formulate this abstraction in a separate library, [`monad-schedule`](https://hackage.haskell.org/package/monad-schedule/),
and reuse it in `rhine`.
It defines a type class [`MonadSchedule`](https://hackage.haskell.org/package/monad-schedule/docs/Control-Monad-Schedule-Class.html#t:MonadSchedule),
which contains a function, `schedule`, which defines what it means for a monad `m` to support scheduling:

```haskell
schedule :: NonEmpty (m a) -> m (NonEmpty a, [m a])
```

Essentially, `schedule actions` will start all `actions` (each of which has type `m a`),
and block until the first (or possibly several) has finished.
Then it returns a list of continuation actions which can each be run until it finishes,
or which can be scheduled again.

For example, `IO` is an instance of `MonadSchedule`,
and it is implemented by launching a separate thread for each action,
and creating an `MVar` to collect all results.
But there are also implementations for purely algebraic monads,
which is very useful for deterministic scheduling guarantees.

Finally, `MonadSchedule` is extensible,
e.g. there exist instances for most standard transformers.
Notably, there is no instance for `StateT`,
which is evident since a global mutable state variable only makes sense single threaded.
(In case you do need global state, have a look into [`AccumT`](https://hackage.haskell.org/package/transformers/docs/Control-Monad-Trans-Accum.html),
which has an instance.)

## Steps taken

To account for this change in the library, all library code, example code, and documentation was updated, and the major version bumped.
The original article describing `rhine` has not been updated, though.
It remains a historical artifact describing the design choices at the time,
while the library is constantly adapting.
