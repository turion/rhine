{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Combinators to create 'Rhine's (main programs) from basic components
such as 'ClSF's, clocks, 'ResamplingBuffer's and 'Schedule's.

The combinator names are often mixed of the symbols @, @*@ and @>@,
and several other symbols.
The general mnemonic for combinator names is:

* @ annotates a data processing unit such as a signal function, network or buffer
  with temporal information like a clock or a schedule.
* @*@ composes parallely.
* @>@ composes sequentially.
-}
module FRP.Rhine.Reactimation.Combinators where

-- rhine
import FRP.Rhine.ClSF.Core
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy
import FRP.Rhine.ResamplingBuffer
import FRP.Rhine.SN
import FRP.Rhine.SN.Combinators
import FRP.Rhine.Type
import Control.Arrow.Free (FreeAlgebra2(..))

-- * Combinators and syntactic sugar for high-level composition of signal networks.

(>@>) :: Rhine m td cls1 a b -> Rhine m td cls2 b c -> Rhine m td (Append cls1 cls2) a c
Rhine clocks1 sn1 >@> Rhine clocks2 sn2 =
  let clocks = appendClocks clocks1 clocks2
   in Rhine
        { clocks
        , sn = appendClocksSN clocks2 sn1 >>> prependClocksSN clocks1 sn2
        }

infix 5 @@

{- | Create a synchronous 'Rhine' by combining a clocked signal function with a matching clock.
   Synchronicity is ensured by requiring that data enters (@In cl@)
   and leaves (@Out cl@) the system at the same as it is processed (@cl@).
-}
(@@) :: (Clock m cl, GetClockProxy cl) => ClSF m cl a b -> cl -> Rhine m (Time cl) '[cl] (At cl a) (At cl b)
clsf @@ cl =
  Rhine
    { clocks = Clocks {getClocks = ClassyClock cl :* Nil}
    , sn = synchronous clsf
    }
{-# INLINE (@@) #-}

-- FIXME Everything needs to be inlined

{- | A purely syntactical convenience construction
   enabling quadruple syntax for sequential composition, as described below.
-}
data RhineAndResamplingBuffer m td cls clC a c
  = forall clB b.
    (Clock m clB) =>
    RhineAndResamplingBuffer (Position clB cls) (Rhine m td cls a (At clB b)) (ResamplingBuffer m clB clC b c)

infix 2 >--
-- | Syntactic sugar for 'RhineAndResamplingBuffer'.
(>--) :: (Clock m clB, HasClock clB cls) => Rhine m td cls a (At clB b) -> ResamplingBuffer m clB clC b c -> RhineAndResamplingBuffer m td cls clC a c
(>--) = RhineAndResamplingBuffer position

infixr 1 -->
{- | The combinators for sequential composition allow for the following syntax:

@
rh1   :: Rhine            m      cl1           a b
rh1   =  ...

rh2   :: Rhine            m               cl2      c d
rh2   =  ...

rb    :: ResamplingBuffer m (Out cl1) (In cl2)   b c
rb    =  ...

rh    :: Rhine m (SequentialClock cl1 cl2) a d
rh    =  rh1 >-- rb --> rh2
@
-}
(-->) :: (HasClock clC cls2) => RhineAndResamplingBuffer m td cls1 clC a c -> Rhine m td cls2 (At clC c) d -> Rhine m td (Append cls1 cls2) a d
RhineAndResamplingBuffer positionB (Rhine cls1 sn1) rb --> Rhine cls2 sn2 =
  let positionC = position
   in Rhine
        { clocks = appendClocks cls1 cls2
        , sn =
            appendClocksSN cls2 sn1
              >>> SN (liftFree2 (Resampling (orderedPositionsInAppend cls1 cls2 positionB positionC) rb))
              >>> prependClocksSN cls1 sn2
        }

-- FIXME Research question how to make this work best

-- {- | The combinators for parallel composition allow for the following syntax:

-- @
-- rh1   :: Rhine m                clL      a         b
-- rh1   =  ...

-- rh2   :: Rhine m                    clR  a           c
-- rh2   =  ...

-- rh    :: Rhine m (ParallelClock clL clR) a (Either b c)
-- rh    =  rh1 +\@+ rh2
-- @
-- -}
-- infix 3 +@+
-- (+@+) ::
--   Rhine m td         cls1       a         b ->
--   Rhine m td              cls2  a           c ->
--   Rhine m td (Append cls1 cls2) a (Either b c)
-- (+@+) = wire $ \cls1 cls2 snab snac -> proc a -> do
--   b <- snab -< a
--   c <- snac -< a
--   returnA -< _

{- | The combinators for parallel composition allow for the following syntax:

@
rh1   :: Rhine m                clL      a b
rh1   =  ...

rh2   :: Rhine m                    clR  a b
rh2   =  ...

rh    :: Rhine m (ParallelClock clL clR) a b
rh    =  rh1 |\@| rh2
@
-}
-- infix 3 |@|

-- (|@|) ::
--   Rhine td m         cls1       a b ->
--   Rhine td m              cls2  a b ->
--   Rhine td m (Append cls1 cls2) a b
-- (|@|) = wire $ \cls1 cls2 sn1 sn2 -> _

infix 2 *@*

(*@*) ::
  Monad m =>
  Rhine m td         cls1        a      b ->
  Rhine m td              cls2      c      d ->
  Rhine m td (Append cls1 cls2) (a, c) (b, d)
(*@*) = wire $ const $ const (****)


-- FIXME these should become profunctor instances

-- | Postcompose a 'Rhine' with a pure function.
(@>>^) ::
  Monad m =>
  Rhine m td cls a b       ->
                  (b -> c) ->
  Rhine m td cls a      c
Rhine cls sn @>>^ f = Rhine cls $ sn >>>^ f

-- | Precompose a 'Rhine' with a pure function.
(^>>@) ::
  Monad m =>
                (a -> b)  ->
  Rhine m td cls      b c ->
  Rhine m td cls a      c
f ^>>@ Rhine cls sn = Rhine cls $ f ^>>> sn

-- | Postcompose a 'Rhine' with a 'ClSF'.
(@>-^) ::
  (HasClock cl cls, Monad m, Clock m cl
  ) =>
  Rhine m td cls  a b   ->
  ClSF  m cl        b        c ->
  Rhine m td cls  a   (At cl c)
Rhine cls sn @>-^ clsf = Rhine cls $ sn >--^ clsf

-- | Precompose a 'Rhine' with a 'ClSF'.
(^->@) ::
  ( HasClock cl cls, Clock m cl
  ) =>
  ClSF  m cl a b   ->
  Rhine m td     cls    (At cl b) c ->
  Rhine m td     cls  (At cl a)   c
clsf ^->@ Rhine cls sn = Rhine cls $ clsf ^--> sn
{- FOURMOLU_ENABLE -}
