# Major major version bumps

This document lists those major version bumps that are more likely to have broken your Rhine-depending code,
as well as guidance on how to fix these breakages.
You might still want to consult the changelogs of the individual packages, since only the biggest breakages are documented here.

## 1.2 -> 1.3: Removed dunai dependency

Rhine doesn't depend on [`dunai`](https://hackage.haskell.org/package/dunai) anymore.
Instead, its components are internally implemented as automata (a.k.a. state machines, transducers, Mealy machines, ...).
This doesn't make a big difference semantically, but it allows GHC to optimize the code substantially,
resulting in much faster programs, especially when the program consists of many components.

This change is purely a change of the internal representation, it nearly doesn't affect the API of Rhine.
Where Rhine did in the past re-export symbols from `dunai`,
it now defines those names in a new package with the same semantics, [`automaton`](https://hackage.haskell.org/package/automaton).

Naming and module structure of Rhine have staid largely the same,
a few changes are highlighted further below.

You probably don't need to change anything if your code doesn't have a direct dependency on `dunai`.
There is only one tiny special case you need to be aware of, recursive definitions.

### Direct `dunai` dependency in your code: Replace by `automaton`

One reason you might have a dependency on `dunai` is because you wrote your own clock.
Else, you might have needed special combinators that Rhine didn't reexport, or defined your own `MSF` somewhere.

If so, you need to replace the `MSF` type from `Data.MonadicStreamFunction` by the `Automaton` type from `Data.Automaton`.
This is typically done by just removing the `dunai` dependency from your code.
`Data.Automaton` is automatically re-exported in `FRP.Rhine`.

A lot of code written for a `dunai` `MSF` will continue to work for a Rhine `Automaton`,
but there are a few cases where it doesn't, most prominently:

* `iPre` is renamed to `delay`
* `morphS` is renamed to `hoistS`
* `morphGS` is renamed to `morph`
* You cannot build an `MSF` directly in continuation style. Consider this in `dunai` style:
  ```haskell
  myMSF s = MSF $ \a -> do
    (b, s') <- doSomething a s
    return (b, myMSF s')
  ```
  You have to write this in "coalgebraic encoding", making the state explicit:
  ```haskell
  automaton = unfoldM s $ \a s -> do
    (b, s') <- doSomething a s
    return $! Result s' b
  ```
  In those rare cases where you really need the continuation style, have a look at `Data.Automaton.Recursive`.

### Avoid recursive definitions of `MSF`s

One thing that doesn't work with the new representation is a recursion in the definition of an automaton itself.
Consider e.g. this construction that you can write in @dunai@:

```haskell
myParallely :: Monad m => MSF m a b -> MSF m [a] [b]
myParallely msf = proc as -> do
  case as of
    [] -> returnA -< []
    (a : as') -> do
      b <- msf -< a
      bs <- myParallely msf -< as
      returnA -< b : bs
```
The trouble here is that `myParallely` is used in the definition of itself.
In @dunai@, this is fine.
In @automaton@, this will loop at runtime, making the program unresponsive.
(The reason for this is that automata have an internal existential state type, which mustn't be recursive.)

For the rare cases where you might want to define an `Automaton` like this,
you will typically find a function that does the job for you.
For example, in this case you probably would have wanted to use `parallely`,
depending on what your intended semantics was.
In other situations, you might want to use a specific fixpoint operator like `fixA`.

In the most general case, you can follow this mechanical process to rewrite a recursive definition:

1. Rewrite your definition as the fixpoint of a function `f :: AutomatonT m a -> AutomatonT m a`.
   For example, if you wanted to define `many a = ((:) <$> a <*> many a) <|> return []`,
   then your function is `f x = ((:) <$> a <*> x) <|> return []`.
   (Note that in this case, `a` is an external parameter to the fixpoint.)
2. Evaluate `f` completely, on a generic automaton.
3. Recognise how `f` transforms the state type of the automaton, and define a datatype that captures this transformation.
4. Use a fixpoint operator such as `fixStream` to define the recursion.

For examples, see the definitions of `fixA`, `many`, or `parallely`.

## 0.9 -> 1.0: Removed explicit schedules

As a big simplification and breaking change,
explicit schedules were removed in version 1.0.
For an overview of the required changes, see [this page](/version1.md).
