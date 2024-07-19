{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{- | This module provides exception handling, and thus control flow,
to synchronous signal functions.

The API presented here closely follows @automaton@'s "Data.Automaton.Trans.Except",
and reexports everything needed from there.
-}
module FRP.Rhine.ClSF.Except (
  module FRP.Rhine.ClSF.Except,
  module X,
  safe,
  safely,
  exceptS,
  runAutomatonExcept,
  currentInput,
)
where

-- base
import Control.Category qualified as Category

-- transformers
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except as X
import Control.Monad.Trans.Reader

-- automaton
import Data.Automaton.Trans.Except hiding (once, once_, throwOn, throwOn', throwS, try)
import Data.Automaton.Trans.Except qualified as AutomatonE

-- rhine
import FRP.Rhine.ClSF.Core
import FRP.Rhine.ClSF.Except.Util
import FRP.Rhine.SN.Tick
import Data.SOP (All)

-- * Throwing exceptions

-- | Immediately throw the incoming exception.
throwS :: (Monad m) => ClSF (ExceptT e m) cl e a
throwS = arrMCl throwE

-- | Immediately throw the given exception.
throw :: (Monad m) => e -> Automaton (ExceptT e m) a b
throw = constM . throwE

-- | Do not throw an exception.
pass :: (Monad m) => Automaton (ExceptT e m) a a
pass = Category.id

-- | Throw the given exception when the 'Bool' turns true.
throwOn :: (Monad m) => e -> ClSF (ExceptT e m) cl Bool ()
throwOn e = proc b -> throwOn' -< (b, e)

-- | Variant of 'throwOn', where the exception can vary every tick.
throwOn' :: (Monad m) => ClSF (ExceptT e m) cl (Bool, e) ()
throwOn' = proc (b, e) ->
  if b
    then throwS -< e
    else returnA -< ()

-- | Throw the exception 'e' whenever the function evaluates to 'True'.
throwOnCond :: (Monad m) => (a -> Bool) -> e -> ClSF (ExceptT e m) cl a a
throwOnCond cond e = proc a ->
  if cond a
    then throwS -< e
    else returnA -< a

{- | Variant of 'throwOnCond' for Kleisli arrows.
   Throws the exception when the input is 'True'.
-}
throwOnCondM :: (Monad m) => (a -> m Bool) -> e -> ClSF (ExceptT e m) cl a a
throwOnCondM cond e = proc a -> do
  b <- arrMCl (lift . cond) -< a
  if b
    then throwS -< e
    else returnA -< a

-- | When the input is @Just e@, throw the exception @e@.
throwMaybe :: (Monad m) => ClSF (ExceptT e m) cl (Maybe e) (Maybe a)
throwMaybe = proc me -> case me of
  Nothing -> returnA -< Nothing
  Just e -> throwS -< e

-- * Monad interface

{- | A synchronous exception-throwing signal function.

It is based on a @newtype@ from @automaton@, 'AutomatonExcept',
to exhibit a monad interface /in the exception type/.
`return` then corresponds to throwing an exception,
and `(>>=)` is exception handling.
(For more information, see the documentation of 'AutomatonExcept'.)

* @cl@: The clock on which the signal function ticks
* @a@:  The input type
* @b@:  The output type
* @m@:  The monad that the signal function may take side effects in
* @e@:  The type of exceptions that can be thrown
-}
type ClSFExcept cls a b m e = AutomatonExcept a b (ReaderT (Tick cls) m) e

{- | A clock polymorphic 'ClSFExcept',
or equivalently an exception-throwing behaviour.
Any clock with time domain @time@ may occur.
-}
type BehaviourFExcept time a b m e =
  forall cls. (All (HasTimeDomain time) cls) => ClSFExcept cls a b m e

-- | Compatibility to U.S. american spelling.
type BehaviorFExcept time a b m e = BehaviourFExcept time a b m e

-- | Leave the monad context, to use the 'ClSFExcept' as an 'Arrow'.
runClSFExcept :: (Monad m) => ClSFExcept cl a b m e -> ClsSF (ExceptT e m) cl a b
runClSFExcept = hoistS commuteExceptReader . runAutomatonExcept

{- | Enter the monad context in the exception
   for 'ClSF's in the 'ExceptT' monad.
   The 'ClSF' will be run until it encounters an exception.
-}
try :: (Monad m) => ClsSF (ExceptT e m) cl a b -> ClSFExcept cl a b m e
try = AutomatonE.try . hoistS commuteReaderExcept

{- | Within the same tick, perform a monadic action,
   and immediately throw the value as an exception.
-}
once :: (Monad m) => (a -> m e) -> ClSFExcept cl a b m e
once f = AutomatonE.once $ lift . f

-- | A variant of 'once' without input.
once_ :: (Monad m) => m e -> ClSFExcept cl a b m e
once_ = once . const

{- | Advances a single tick with the given Kleisli arrow,
   and then throws an exception.
-}
step :: (Monad m) => (a -> m (b, e)) -> ClSFExcept cl a b m e
step f = AutomatonE.step $ lift . f
