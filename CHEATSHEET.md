# `rhine` cheatsheet

The most common operators, functions and types,
with short examples and explanations.
If you miss something, feel free to open an issue or pull request.

## Architecture
|              | Time          | Data               |
|--------------|---------------|--------------------|
| Synchronous  | Atomic clocks | Signal functions   |
| Asynchronous | Scheduling    | Resampling buffers |

## Types
`m` is always the monad in which side effects can take place at every step.

| Type                   | Long name               | Explanation                                                                                    |
|------------------------|-------------------------|------------------------------------------------------------------------------------------------|
| `ClSF m cl a b`        | Clocked Signal Function | Input signal `a` at rate `cl`, output signal `b` at the same rate                              |
| `BehaviourF m td a b`  | Behaviour function      | A signal function that can run at any rate or clock, in a time domain `td`.                    |
| `ResBuf m cl1 cl2 a b` | Resampling buffer       | Input signal `a` at rate `cl1`, output signal `b` at rate `cl2`                                |
| `SN m cl a b`          | Signal network          | Process data internally at rate `cl`. Input `a` at rate `In cl`, and data `b` at rate `Out cl` |
| `Rhine m cl a b`       | Rhine                   | Combination of a signal network and a type-matching clock value.                               |

## Combinators

### General naming guideline
* `@` means roughly "at this rate"
* `*` means roughly "in data-parallel"
* `|` roughly means "in time-parallel"
* `+` is as `|`, but has the topology of a "fan-in", in analogy to [`Control.Arrow.ArrowChoice`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Arrow.html#v:-124--124--124-)
* `>` and `-` are in the direction of data flow
* `-` means that a `ClSF` or `ResBuf` is involved
* `^` composes with a pure function

### Common combinators

#### `SN` composition

##### Sequential composition
| Operator        | Description                                                                                | Simplified Type Signature                                                |
|-----------------|--------------------------------------------------------------------------------------------|--------------------------------------------------------------------------|
| `>>>` and `<<<` | Arrow composition                                                                          | `ClSF m cl a b -> ClSF m cl b c -> ClSF m cl a c`                        |
| `>->` and `<-<` | Same as `>>>` and `<<<` but with higher operator precedence                                |                                                                          |
| `^>>>`          | Precompose a signal network with a pure function                                           | `(a -> b) -> SN m cl b c -> SN m cl a c`                                 |
| `>>>^`          | Postcompose a signal network with a pure function                                          | `SN m cl a b -> (b -> c) -> SN m cl a c`                                 |
| `^-->` | Precompose a signal network with a `ClSF` | `ClSF m (In cl) a b -> SN m cl b c -> SN m cl a c` |
| `>--^` | Postcompose a signal network with a `ClSF` | `SN m cl a b -> ClSF m (Out cl) b c -> SN m cl a c` |

##### Parallel composition
| Operator   | Clock Type | Input Type | Output Type | Resulting Type                             |
|------------|------------|------------|-------------|--------------------------------------------|
| `****`     | Same       | Different  | Different   | `SN m cl (a, c) (b, d)`                    |
| `++++`     | Different  | Same       | Different   | `SN m (ParClock m clL clR) a (Either b c)` |
| `\|\|\|\|` | Different  | Same       | Same        | `SN m (ParClock m clL clR) a b`            |

#### `Rhine` composition

##### Sequential composition
| Operator        | Description                                                                                | Simplified Type Signature                                                |
|-----------------|--------------------------------------------------------------------------------------------|--------------------------------------------------------------------------|
| `^>>@`          | Precompose a `Rhine` with a pure function                                           | `(a -> b) -> Rhine m cl b c -> Rhine m cl a c`                                 |
| `@>>^`          | Postcompose a `Rhine` with a pure function                                          | `Rhine m cl a b -> (b -> c) -> Rhine m cl a c`                                 |
| `^->@` | Precompose a `Rhine` with a `ClSF` | `ClSF m (In cl) a b -> Rhine m cl b c -> Rhine m cl a c` |
| `@>-^` | Postcompose a `Rhine` with a `ClSF` | `Rhine m cl a b -> ClSF m (Out cl) b c -> Rhine m cl a c` |

##### Parallel composition

| Operator   | Resulting Type                                   |
|------------|--------------------------------------------------|
| `\|@\|`    | `Rhine m (ParallelClock clL clR) a b`            |
| `+@+`      | `Rhine m (ParallelClock clL clR) a (Either b c)` |

#### Common combinators in action

##### Sequential signal composition
```
clsf @@ cl >-- resBuf --> rh :: Rhine m (SequentialClock cl cl') a b

clsf    -- This is a clocked signal function.
     @@    -- Run the clocked signal function on...
        cl    -- ...this clock. Together, they form a "rhine".
           >--    -- We're resampling the signal function's output...
               resBuf    -- ...with this buffer.
                      -->    -- The output of the resampling is forwarded to...
                          rh    -- ...another rhine.
```

##### Data-parallel signal composition

```
sn1 **** sn2 :: SN m cl (a, c) (b, d)

sn1    -- This is a signal network with input `a` and output `b`, on clock `cl`. (It is not a rhine, it has no clock value.)
    ****    -- We are going to compose it with...
         sn2    -- ...another signal function that runs at the same time, but with input `c` and output `d`.
```

##### Time-parallel signal composition

```
rhL |@| rhR :: Rhine m (ParallelClock clL clR) a b

rhL    -- A rhine that inputs some data `a` and outputs some data `b`, on some clock `clL`.
    |@|    -- We are going to execute the first rhine in parallel with another one.
        rhR    -- The other rhine. It also inputs `a` and outputs `b`, but on another clock, e.g. `clR`.
```

## Common functions

### Clocked signal functions (`ClSF`s)

Automata in [`automaton`](http://hackage.haskell.org/package/automaton) are usually valid clocked signal functions.
Here are some of the most used:

| Name         | Type (abbreviated)                                   | Meaning                                           |
|--------------|------------------------------------------------------|---------------------------------------------------|
| `arr`        | `(a -> b) -> ClSF m cl a b`                          | Perform pure function every tick                  |
| `arrMCl`     | `(a -> m b) -> ClSF m cl a b`                        | Perform side effect every tick                    |
| `integral`   | `VectorSpace v => BehaviorF m td v v`                | Numerical Euler integral                          |
| `derivative` | `VectorSpace v => BehaviorF m td v v`                | Numerical Euler integral                          |
| `timeInfo`   | `ClSF m cl a (TimeInfo cl)`                          | Find out what time it is                          |
| `average`    | `VectorSpace v => Diff td -> BehaviourF m td v v`    | Average/low-pass a signal over a given time scale |
| `timer`      | `Diff td -> BehaviorF (ExceptT () m) td a (Diff td)` | Throw an exception when time is up                |
| `delayBy`    | `Diff td -> BehaviorF m td a a`                      | Delay a signal by a certain time                  |
| `keepFirst`  | `ClSF m cl a a`                                      | Hold the first input value                        |

### Signal networks and rhines

| Name         | Type (abbreviated)                                   | Meaning                                           |
|--------------|------------------------------------------------------|---------------------------------------------------|
| `flow`       | `Rhine m cl () () -> m ()`                           | Initialise clock and run top-level rhine          |

## Arrow syntax for clocked signal functions

```
clockedSignalFunction :: ClSF IO cl a b
clockedSignalFunction = proc a -> do    -- `a` is the input to the clocked signal function
  i <- anotherClSF  -< a + 23           -- Call other signal functions
  _ <- arrMCl print -< i                -- Call a side effect in the monad
  let b = i * 42                        -- Define local variable
  if b > 100                            -- Transient control flow. `case` expressions work as well.
    then returnA -< b                   -- Return output value
    else returnA -< b * 2
```

## Monad interface for control flow

```
exceptionThrowingClSF :: ClSF (ExceptT String m) Integer Integer
exceptionThrowingClSF = proc n -> do
  if n < 1000                       -- Transient control flow: Switch between the two branches every tick.
    then returnA -< 2 * n           -- Happy path. Value is output.
    else throwS  -< "Too large!"    -- An exception is thrown. No value is output. The exception needs to be handled first.

clockedSignalFunction :: ClSF m cl a b
clockedSignalFunction = safely $ do    -- There are no exceptions left to handle.
  e <- try $ clsf1                     -- `clsf1 :: ClSF (ExceptT e m) cl a b` threw an exception `e`.
  x <- once someMonadicAction e        -- While handling the exception, produce some side effect.
  try $ someHandler e x                -- `somehandler` produces a new clocked signal function, we switch to it _permanently_. It throws an exception `()`.
  safe clsfFinal                       -- Switch to a signal function `clsfFinal` which does not throw exceptions.
```
