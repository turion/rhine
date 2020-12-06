# README
--------

[![Build Status](https://travis-ci.org/turion/rhine.svg?branch=master)](https://travis-ci.org/turion/rhine)
[![Version on Hackage](https://img.shields.io/hackage/v/rhine.svg)](https://hackage.haskell.org/package/rhine)

Rhine is a library for synchronous and asynchronous Functional Reactive Programming (FRP).
It separates the aspects of clocking, scheduling and resampling
from each other, and ensures clock-safety on the type level.

Complex reactive programs often process data at different rates.
For example, games, GUIs and media applications
may output audio and video signals, or receive
user input at unpredictable times.
Coordinating these different rates is a hard problem in general.
If not enough care is taken, buffer underruns and overflows, space and time leaks,
accidental synchronisation of independent sub-systems,
and concurrency issues, such as deadlocks, may all occur.

Rhine tackles these problems by annotating
the signal processing components with clocks,
which hold the information when data will be
input, processed and output.
Different components of the signal network
will become active at different times, or work
at different rates. If components running under different clocks need to communicate, it
has to be decided when each component becomes
active ("scheduling"), and how data is
transferred between the different rates ("resampling").
Rhine separates all these aspects from each
other, and from the individual signal processing of each subsystem.
It offers a flexible API to all of them and implements several
reusable standard solutions. In the places
where these aspects need to intertwine, typing
constraints on clocks come into effect, enforcing clock safety.

## Example

A typical example,
which can be run as `cd rhine-examples/ && cabal run Demonstration`,
would be:

```haskell
  -- | Create a simple message containing the time stamp since initialisation,
  --   for each tick of the clock.
  --   Since 'createMessage' works for arbitrary clocks (and doesn't need further input data),
  --   it is a 'Behaviour'.
  --   @time@ is the 'TimeDomain' of any clock used to sample,
  --   and it needs to be constrained in order for time differences
  --   to have a 'Show' instance.
  createMessage
    :: (Monad m, Show (Diff time))
    => String
    -> Behaviour m time String
  createMessage str
    =   timeInfoOf sinceInit >-> arr show
    >-> arr (("Clock " ++ str ++ " has ticked at: ") ++)

  -- | Output a message /every second/ (= every 1000 milliseconds).
  --   Let us assume we want to assure that 'printEverySecond'
  --   is only called every second,
  --   then we constrain its type signature with the clock @Millisecond 1000@.
  printEverySecond :: Show a => ClSF IO (Millisecond 1000) a ()
  printEverySecond = arrMCl print

  -- | Specialise 'createMessage' to a specific clock.
  ms500 :: ClSF IO (Millisecond 500) () String
  ms500 = createMessage "500 MS"


  ms1200 :: ClSF IO (Millisecond 1200) () String
  ms1200 = createMessage "1200 MS"

  -- | Create messages every 500 ms and every 1200 ms,
  --   collecting all of them in a list,
  --   which is output every second.
  main :: IO ()
  main = flow $
    ms500 @@ waitClock            --  a Rhine = a ClSF in the context of a Clock
    ||@ scheduleMillisecond @||   --  Compose 2 rhines in parallel
    ms1200 @@ waitClock           --  a Rhine
    >-- collect                   --  buffer results from both Rhines into a list
        -@- concurrently          --  the next Rhine executes in its own thread
    -->
    printEverySecond @@ waitClock --  the final Rhine

  -- | Uncomment the following for a type error (the clocks don't match):

  -- typeError = ms500 >>> printEverySecond
```

## This repository

* `rhine/`: The main library, which is also mirrored on hackage.
* `rhine-gloss/`: A wrapper library to [gloss](https://hackage.haskell.org/package/gloss), a functional OpenGL library.
* `rhine-examples/`: Different examples as a starting point to learn Rhine.

## Documentation

The best way to learn about Rhine is currently the [article _Rhine: FRP with Type-Level Clocks_](https://www.manuelbaerenz.de/files/Rhine.pdf).

For a quick reference of the most important concepts,
see the [cheatsheet](https://github.com/turion/rhine/blob/master/CHEATSHEET.md).

### Additional documentation

* [`stackage`](https://www.stackage.org/package/rhine)
* [`hackage`](https://hackage.haskell.org/package/rhine)
* https://github.com/turion/rhine-tutorial: Presentation and tutorial app
* https://github.com/turion/sonnendemo: Demo application


## FAQ

* Why does my blocking code, eg `arrMCl readLn`, behave [erratically](https://github.com/turion/rhine/issues/153)?

Clocks must be the only thing that blocks the thread, not `ClSF`s. So for example, you can fix:

```haskell
arrMCl readLn
```

by using:

```haskell
tagS >>> arr read :: ClSF IO StdinClock () Int
```

`tagS` contains the string that the `StdinClock` grabbed from `stdin`, and only the clock has been allowed to block the thread!


* Can a sampling schedule dynamically change, eg depend on a signal?

Yes, for instance you could implement a distance-dependent [collision detector](https://github.com/turion/rhine/issues/152).


* How to handle slow computations, IE computations that take longer than the sample rate?

Several [strategies exist](https://github.com/turion/rhine/issues/151) and it depends on your use case. For `FixedStep` clocks, it won't matter since the execution of the program isn't tied to a realtime clock. For `ClSF`s running on `UTCTime` clocks, you can execute the slow code in a separate thread and coordinate merging the results back into the signal network.

## Development

See [`Contributing.md`](./Contributing.md) for details.

* Rhine usually follows up-to-date GHC versions.
* Contributions are welcome!
  There are always a few issues labelled `help needed`,
  in case you're looking for an easy way to get started.
* Rhine is a beginner-friendly Haskell project!
  Even if you're new to Haskell and FRP, you can contribute.
  This is a good place to start contributing to open-source projects.
  Have a look at issues labelled `good first issue`.
  If you have questions, don't hesitate to ask on Github.

## Related projects

* https://github.com/turion/rhine-tutorial: Presentation and tutorial app
* https://github.com/fphh/rhine-ghcjs/:
  A little browser game written with Rhine and `react-hs`,
  compiles with `GHCJS` to JavaScript.
* https://github.com/turion/sonnendemo:
  An interactive simulation with a GUI version and a console version,
  using `rhine-gloss`.
