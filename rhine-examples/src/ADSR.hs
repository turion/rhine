{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- | * Attack-Decay-Sustain-Release hull

This module demonstrates the use of timers and exceptions
to create a simple Attack-Decay-Sustain-Release (ADSR) hull signal,
as commonly used in audio processing.
(See e.g. https://en.wikipedia.org/wiki/Synthesizer#Attack_Decay_Sustain_Release_(ADSR)_envelope.)

They can be used to create a wide range of electronic musical instruments,
by combining them with a wave generator (e.g. sine, sawtooth or square).
The ADSR hull is multiplied with the wave signal
to create a tone that first rises in loudness ("attack"),
reaches a peak and becomes quieter again ("decay"),
then stays at that level for a while ("sustain")
until it drops back to zero ("release").

The interpolation happens linearly.
The attack period starts as soon as the user presses a key.
The sustain level, as well as the time spans
for attack, decay and release can be preset,
while the time span for the sustain period ends
when the user stops pressing the key.
-}


-- rhine
import FRP.Rhine

-- * The definition of an ADSR

-- | Collects the parameters an ADSR hull depends on.
--   An ADSR is specified by three time spans, and a number between 0 and 1.
data ADSR time s = ADSR
  { a :: Diff time -- ^ The attack time (for how long the level increases)
  , d :: Diff time -- ^ The decay time (for how long the level decreases)
  , s :: s         -- ^ The sustain level (a 'Fractional' between 0 and 1)
  , r :: Diff time -- ^ The release time (how long the level needs to decay)
  }

-- | Some sample settings for an 'ADSR'.
myADSR :: ADSR UTCTime Double
myADSR = ADSR
  { a = 0.05
  , d = 0.2
  , s = 0.3
  , r = 0.2
  }


{- |
Runs a given 'ADSR' parameter set as a 'BehaviourF',
i.e. a /signal/ which can be sampled on an arbitrary clock,
and depends on another input signal.
The output is a 'Fractional' number representing the current envelope amplitude.
The input is a 'Bool' representing the current state of the key.
('True' represents "pressed", 'False' represents "not pressed")

The amplitude of the ADSR goes through five states,
as follows:

* The ADSR hull curve is constantly at 0,
  until the input (representing a key) becomes 'True'.
* The amplitude then increases linearly,
  until it has reached the level 1 after time 'a',
  or the input becomes 'False'.
* If the amplitude reached 1, it then decreases linearly,
  until it has reached the level 's' after time 'd',
  or the input becomes 'False'.
* If the amplitude reached 's', it then sustains it
  until the input finally becomes 'False'.
* When the input becomes 'False', the amplitude decreases to 0 within time 'r'.
  The system then returns to the initial zero amplitude state.
-}
runADSR
  :: ( Monad m, TimeDomain time
     , Ord amplitude, Fractional amplitude, Diff time ~ amplitude )
  => ADSR time amplitude -> BehaviourF m time Bool amplitude
runADSR ADSR {..} = safely $ do
  _ <- try $ sustain 0 `till` keyPressed
  adsrFrom 0
    where
      adsrFrom attackAmplitude = do
        (_, releaseAmplitude) <- try $ (`till` keyReleased) $ safely $ do
          overdue <- try $ attack a attackAmplitude
          _       <- try $ decay d s overdue
          safe $ sustain s
        (_, attackAmplitude') <- try $ (`till` keyPressed) $ safely $ do
          _ <- try $ release r releaseAmplitude
          safe $ sustain 0
        adsrFrom attackAmplitude'


-- * The different states of the ADSR

-- | Interpolate between two values linearly within a given time span,
--   throwing an exception when the time is up.
--   The exception contains the "overdue" time,
--   i.e. how long before the tick the time was up.
linearly
  :: ( Monad m, TimeDomain time
     , Ord amplitude, Fractional amplitude, Diff time ~ amplitude )
  => Diff time -- ^ The time span, in which the amplitude will interpolate
  -> amplitude -- ^ The initial amplitude
  -> amplitude -- ^ The final amplitude
  -> Diff time -- ^ How far overdue the interpolation already is
  -> Behaviour (ExceptT (Diff time) m) time amplitude
linearly timeSpan initialAmplitude finalAmplitude overdue = proc _ -> do
  time <- (overdue +) ^<< sinceStart -< ()
  let
    remainingTime = timeSpan - time
    currentLevel  = ( initialAmplitude * remainingTime
                    + finalAmplitude   * time          )
                  / timeSpan
  _ <- throwOn' -< (remainingTime < 0, remainingTime)
  returnA       -< currentLevel


-- | The period in which the amplitude rises initially from 0 to 1,
--   and then an exception is thrown.
attack
  :: ( Monad m, TimeDomain time
     , Ord amplitude, Fractional amplitude, Diff time ~ amplitude )
  => Diff time -- ^ The attack time, in which the amplitude will rise from 0 to 1.
  -> amplitude -- ^ The initial amplitude
  -> Behaviour (ExceptT (Diff time) m) time amplitude
attack a amplitude = linearly a amplitude 1 0

-- | The period in which the amplitude falls from 1 to the sustain level,
--   and then an exception is thrown.
decay
  :: ( Monad m, TimeDomain time
     , Ord amplitude, Fractional amplitude, Diff time ~ amplitude )
  => Diff time -- ^ The decay time, in which the amplitude will fall from 1 to...
  -> amplitude -- ^ ...the sustain level.
  -> Diff time -- ^ How far overdue the decay period already is.
  -> Behaviour (ExceptT (Diff time) m) time amplitude
decay d = linearly d 1

-- | A period in which a given amplitude is sustained indefinitely.
sustain :: Monad m => amplitude -> Behaviour m time amplitude
sustain = arr . const

-- | The period in which the level falls from the sustain level to 0.
--   and then an exception is thrown.
release
  :: ( Monad m, TimeDomain time
     , Ord amplitude, Fractional amplitude, Diff time ~ amplitude )
  => Diff time -- ^ The release time, in which the amplitude will fall from...
  -> amplitude -- ^ ...the sustain level to 0.
  -> Behaviour (ExceptT (Diff time) m) time amplitude
release r s = linearly r s 0 0


-- * The main program

-- | A signal that alternates between 'False' and 'True' on every console newline.
key :: Rhine IO StdinClock () Bool
key = (count >>^ odd) @@ StdinClock

-- | Output the current amplitude of the ADSR hull on the console,
--   every 0.03 seconds.
consoleADSR :: Rhine IO (Millisecond 30) Bool ()
consoleADSR = runADSR myADSR >-> arrMCl print @@ waitClock

-- | Runs the main program, where you have the choice between console output
--   and pulse output.
main :: IO ()
main = flow $ key >-- keepLast False --> consoleADSR


-- * Utilities

-- | Raises an exception when the input becomes 'True',
--   i.e. the key is pressed.
keyPressed :: Monad m => BehaviourF (ExceptT () m) time Bool ()
keyPressed = throwOn ()

-- | Raises an exception when the input becomes 'False',
--   i.e. the key isn't pressed anymore.
keyReleased :: Monad m => BehaviourF (ExceptT () m) time Bool ()
keyReleased = arr not >>> keyPressed


-- | Executes the first 'ClSF' in parallel with a second 'ClSF',
--   only forwarding the output of the first,
--   until the second one raises an exception.
--   That exception is returned,
--   together with the current output of the first 'ClSF'.
till
  :: Monad m
  => ClSF                 m  cl a b
  -> ClSF (ExceptT  e     m) cl a   c
  -> ClSF (ExceptT (e, b) m) cl a b
till clsf clsfe = proc a -> do
  b <- liftClSF clsf                -< a
  _ <- runClSFExcept clsfeAndOutput -< (b, a)
  returnA -< b
    where
      clsfeAndOutput = do
        e      <- try $ clsfe <<< arr snd
        (b, _) <- currentInput
        return (e, b)
