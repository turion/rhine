# `monad-schedule`

## What

A new, simple, composable concurrency abstraction.

### The class

```haskell
class MonadSchedule m where
  -- | Run the actions concurrently,
  --   and return the result of the first finishers,
  --   together with completions for the unfinished actions.
  schedule :: NonEmpty (m a) -> m (NonEmpty a, [m a])
```

A monad `m` is said to allow scheduling if you can pass a number of actions `m a` to it,
and those can be executed at the same time concurrently.
You can observe the result of the actions after some time:
Some actions will complete first, and the results of these are returned then as a list `NonEmpty a`.
Other actions are still running, and for these you will receive continuations of type `m a`,
which you can further run or schedule to completion as you like.

### Example: Thread pool

The typical example for this is a thread pool.
You launch several threads and run one action in each thread.
Then `schedule actions` blocks until at least one action has finished.
This can be done in `IO` with lightweight GHC runtime threads (one per action),
or a fixed number of OS threads, see `Control.Monad.Schedule.OSThreadPool`.

### Composability

The `MonadSchedule` interface is very general.
This allows a lot of monads to be instances of `MonadSchedule`.
For example, most transformers such as `ReaderT`, `WriterT`, `ExceptT` can be added on top of an existing `MonadSchedule` instance.

#### Why not `StateT`?

State is inherently single-threaded,
and thus there is no good way to schedule several actions that manipulate it.

Imagine two threads running, both wanting to write to that single state.
It's not obvious which one should succeed.
The last one?
Should the first one be restarted, or its state be discarded?

You might want to merge the state modifications of the two threads,
and this can often be done by using `AccumT`:
The two threads are launched with the same initial state,
return a modification in form of a `Monoid`,
which is then appended to the global state instead of replacing it.
For example, typical map-reduce algorithms can be performed that way.
As another example, the monoid `Last` gives last-write-wins semantics.

But caution: Some common `AccumT` use cases have the typical concurrency pitfalls.
For example, if you use the internal state as a unique key resource, or a random generator seed,
uniqueness will not be guaranteed across threads.
