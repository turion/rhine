{-# LANGUAGE CPP #-}

{- |
Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
License    : BSD3
Maintainer : ivan.perez@keera.co.uk

'MSF's with a 'State' monadic layer.

This module contains functions to work with 'MSF's that include a 'State'
monadic layer. This includes functions to create new 'MSF's that include an
additional layer, and functions to flatten that layer out of the 'MSF`'s
transformer stack.

It is based on the _strict_ state monad 'Control.Monad.Trans.State.Strict',
so when combining it with other modules such as @mtl@'s,
the strict version has to be included, i.e. 'Control.Monad.State.Strict'
instead of 'Control.Monad.State' or 'Control.Monad.State.Lazy'.
-}
module Data.Automaton.MSF.Trans.State (
  module Control.Monad.Trans.State.Strict,

  -- * 'State' 'MSF' running and wrapping
  stateS,
  runStateS,
  runStateS_,
  runStateS__,
)
where

-- External imports
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

import Control.Arrow (arr, (>>>))
import Control.Monad.Trans.State.Strict hiding (
  liftCallCC,
  liftCatch,
  liftListen,
  liftPass,
 )
import Data.Tuple (swap)

-- Internal imports

import Data.Automaton.MSF (MSF, feedback, morph)
import Data.Automaton.Result (Result (..))

-- * 'State' 'MSF' running and wrapping

{- | Build an 'MSF' in the 'State' monad from one that takes the state as an
extra input. This is the opposite of 'runStateS'.
-}
stateS :: (Functor m, Monad m) => MSF m (s, a) (s, b) -> MSF (StateT s m) a b
stateS = morph $ \f a -> StateT $ \s ->
  (\(Result s' (s, b)) -> (Result s' b, s))
    <$> f (s, a)

{- | Build an 'MSF' that takes a state as an extra input from one on the
'State' monad. This is the opposite of 'stateS'.
-}
runStateS :: (Functor m, Monad m) => MSF (StateT s m) a b -> MSF m (s, a) (s, b)
runStateS = morph $ \f (s, a) ->
  (\(Result s' b, s) -> Result s' (s, b))
    <$> runStateT (f a) s

{- | Build an 'MSF' /function/ that takes a fixed state as additional input,
from an 'MSF' in the 'State' monad, and outputs the new state with every
transformation step.
-}
runStateS_ ::
  (Functor m, Monad m) =>
  MSF (StateT s m) a b ->
  s ->
  MSF m a (s, b)
runStateS_ msf s =
  feedback s $
    arr swap >>> runStateS msf >>> arr (\(s', b) -> ((s', b), s'))

{- | Build an 'MSF' /function/ that takes a fixed state as additional input,
from an 'MSF' in the 'State' monad.
-}
runStateS__ :: (Functor m, Monad m) => MSF (StateT s m) a b -> s -> MSF m a b
runStateS__ msf s = runStateS_ msf s >>> arr snd
