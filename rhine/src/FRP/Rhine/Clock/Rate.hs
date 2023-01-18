{- | Enriches a clock such that its current tick rate is recorded in a state monad.
-}

-- FIXME Maybe short example in the module docs?

module FRP.Rhine.Clock.Rate where

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.Reactimation.ClockErasure (eraseClockClSF)

-- | Record and update frame rates of several clocks in a joint state of type @s@.
newtype RateT s m a = RateT { getRateT :: StateT s m a }
  deriving (Functor, Applicative, Monad)

-- FIXME Should I implement `MonadReader s`?

-- | Special case of 'RateT' where only the rate of one clock needs to be recorded.
type SimpleRateT cl m a = RateT (Diff (Time cl)) m a

-- | A clock type where the rate of the clock is recorded in 'RateT'.
newtype RateClock s rate cl = RateClock
  { getRateClock :: cl
  , setRate :: ClSF m cl () (s -> s)
  -- | From the global state, get the current clock rate.
  --   May return 'Nothing' if the rate has not been initialized yet,
  --   or if the clock does not add its rate to the state (see 'noRate').
  , getRate :: s -> Maybe rate
  }

instance Clock m cl => Clock (RateT s m) (RateClock s cl) where
  type Time (RateClock s cl) = Time cl
  type Tag (RateClock s cl) = Tag cl
  initClock RateClock { getRateClock, setRate, getRate } = do
    (runningClock, time) <- lift $ initClock getRateClock
    return $ (, time) $ proc () -> do
      (time, tag) <- liftTransS runningClock -< ()
      setRateFunction <- liftTransS $ eraseClockClSF (getClockProxy getRateClock) time setRate -< (time, tag, ())
      arrM $ RateT . modify -< setRateFunction
      returnA -< (time, tag)

-- | Record the difference of a single clock between the last two timestamps.
simpleRateClock :: cl -> RateClock (Diff (Time cl)) (Diff (Time cl)) cl
simpleRateClock getRateClock = RateClock
  { getRateClock
  , setRate = sinceLastS >>> arrMCl put
  , getRate = Just . fmap get
  }

-- FIXME Integrate with lenses?
-- | Record the difference of this clock between the last two timestamps, given a lens into the global state.
diffRateClockBy ::
  -- | The clock value
  cl ->
  -- The setter, often something like @\diffTime myState -> myState { diffMyClock = Just diffTime }@.
  ((Diff (Time cl)) -> s -> s) ->
  -- The getter, often a record accessor.
  (s -> Maybe (Diff (Time cl))) ->
  RateClock s (Diff (Time cl)) cl

-- | Lift a clock to 'RateT', but don't record its rate.
noRate :: cl -> RateClock s Void cl
noRate getRateClock = RateClock
  { getRateClock
  , setRate = arr_ id
  , getRate = const Nothing
  }

-- | Get the rate of a particular clock.
--
--   Can be 'Nothing' if the rate of this clock has not been initialized yet,
--   or if the clock does not add its rate to the state (see 'noRate').
getRateOf :: RateClock s rate cl -> RateT s m (Maybe rate)
getRateOf RateClock { getRate } = RateT $ gets getRate

-- | A generic state type that records the clock rates of several clocks.
data Rates [Type] where
  RatesClock :: Maybe (Diff (Time cl)) -> Rates '[cl]
  RatesClocks :: Maybe (Diff (Time cl)) -> Rates cls -> Rates (cl ': cls)

-- FIXME Do I want to generalise to arbitrary rate types?
-- | State types where rate setter and getter functions can be derived from the type.
class MakeRateClock s cl where
  defaultSetRate :: Diff (Time cl) -> s -> s
  defaultGetRate :: s -> Maybe (Diff (Time cl))

  -- | Initialize the global state, it may be empty.
  initState :: s

-- | Construct a 'RateClock' with derived getter and setter functions.
makeRateClock :: MakeRateClock s cl => cl -> RateClock s (Diff (Time cl)) cl
makeRateClock cl = diffRateClockBy cl defaultSetRate defaultGetRate

-- | For a single clock, the rate can be updated
instance MakeRateClock (Rates '[cl]) cl where
  defaultSetRate = const . RatesClock . Just
  defaultGetRate (RatesClock diffTimeMaybe) = diffTimeMaybe
  initState = RatesClock Nothing

-- | For the topmost clock in a list of clocks, the rate can be updated
instance MakeRateClock (Rates cls) cl' => MakeRateClock (Rates (cl ': cls)) cl where
  defaultSetRate diffTime (RatesClocks _ rates) = RatesClocks (Just difftime) rates
  defaultGetRate (RatesClocks diffTimeMaybe _) = diffTimeMaybe
  initState = RatesClocks Nothing initState

-- | If a clock is deeper in the list of clocks, its rate can still be updated recursively
instance MakeRateClock (Rates cls) cl => MakeRateClock (Rates (cl' ': cls)) cl where
  defaultSetRate diffTime (RatesClocks rate rates) = RatesClocks rate $ defaultSetRate diffTime rates
  defaultGetRate (RatesClocks _ rates) = defaultGetRate rates
  initState = RatesClocks Nothing initState
