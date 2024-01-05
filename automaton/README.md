# `automaton`: Effectful streams and automata in initial encoding

This library defines effectful streams and automata, in initial encoding.
They are useful to define effectful automata, or state machines, transducers, monadic stream functions and similar streaming abstractions.
In comparison to most other libraries, they are implemented here with explicit state types,
and thus are amenable to GHC optimizations, often resulting in dramatically better performance.

## What?

The core concept is an effectful stream in initial encoding:
```haskell
data StreamT m a = forall s.
  StreamT
  { state :: s
  , step :: s -> m (s, a)
  }
```
This is an stream because you can repeatedly call `step` on the `state` and produce output values `a`,
while mutating the internal state.
It is effectful because each step performs a side effect in `m`, typically a monad.

The definitions you will most often find in the wild is the "final encoding":
```haskell
data StreamT m a = StreamT (m (StreamT m a, a))
```
Semantically, there is no big difference between them, and in nearly all cases you can map the initial encoding onto the final one and vice versa.
(For the single edge case, see [the section in `Data.Automaton` about recursive definitions](hackage.haskell.org/package/automaton/docs/Data.Automaton.html).)
But when composing streams,
the initial encoding will often be more performant that than the final encoding because GHC can optimise the joint state and step functions of the streams.

### How are these automata?

Effectful streams are very versatile, because you can change the effect type `m` to get a number of different concepts.
When `m` contains a `Reader` effect, you get automata!
From the effectful stream alone, a side effect, a state transition and an output value is produced at every step.
If this effect includes reading an input value, you have all ingredients for an automaton (also known as a Mealy state machine, or a transducer).

Automata can be composed in many useful ways, and are very expressive.
A lot of reactive programs can be written with them,
by composing a big program out of many automaton components.

## Why?

Mostly, performance.
When composing a big automaton out of small ones, the final encoding is not very performant, as mentioned above:
Each step of each component contains a closure, which is basically opaque for the compiler.
In the initial encoding, the step functions of two composed automata are themselves composed, and the compiler can optimize them just like any regular function.
This often results in massive speedups.

### But really, why?

To serve as the basic building block in [`rhine`](https://hackage.haskell.org/package/rhine),
a library for Functional Reactive Programming.

## Doesn't this exist already?

Not quite.
There are many streaming libraries ([`streamt`](https://hackage.haskell.org/package/streamt), [`streaming`](https://hackage.haskell.org/package/streaming)),
and state machine libraries ([`machines`](https://hackage.haskell.org/package/machines)) that implement effectful streams.
Prominently, [`dunai`](https://hackage.haskell.org/package/dunai) implements monadic stream functions
(which are essentially effectful state machines)
and has inspired the design and API of this package to a great extent.
(Feel free to extend this list by other notable libraries.)
But all of these are implemented in the final encoding.

I am aware of only two fleshed-out implementations of effectful automata in the initial encoding,
both of which have been a big inspiration for this package:

* [`essence-of-live-coding`](https://hackage.haskell.org/package/essence-of-live-coding) restricts the state type to be serializable, gaining live coding capabilities, but sacrificing on expressivity.
* https://github.com/lexi-lambda/incremental/blob/master/src/Incremental/Fast.hs is unfortunately not published on Hackage, and doesn't seem maintained.
