-- FIXME Clocked a cl or Clocked cl a?
data Clocked = Clocked Type Type

data SN (inClocks :: [Clocked]) (internalClocks :: [Type]) (outClocks :: [Clocked]) m e where
  SynchronousExcept :: ClSFExcept m cl a b e -> SN '[Clocked a cl] '[cl] '[Clocked b cl] m e
  Resampling ::
    ResBuf m cl1 cl2 a b ->
    SN (Clocked b cl2 ': inClocks) internalClocks () (Clocked a cl1 ': outClocks) ->
    SN inClocks internalClocks outClocks
  -- TODO Alternatively, could make Synchronous a cons-like operator
  Combine ::
    SN inClocks1 internalClocks1 outClocks1 ->
    SN inClocks2 internalClocks2 outClocks2 ->
    SN (Concat inClocks1 inClocks2) (Concat internalClocks1 internalClocks2) (Concat outClocks1 outClocks2)
  Stop :: e
  -- This is cool because it means I can rewire internally!
  Catch :: SN inClocks internalClocks outClocks m e1 -> (e1 -> SN inClocks internalClocks outClocks m e2) -> SN inClocks internalClocks outClocks m e2
  PermuteInClocks
    SN inClocksBefore internalClocks outClocks ->
    Permutation inClocksBefore inClocksAfter ->
    SN inClocksAfter internalClocks outClocks ->
  PermuteInternalClocks
    SN inClocks internalClocksBefore outClocks ->
    Permutation internalClocksBefore internalClocksAfter ->
    SN inClocks internalClocksAfter outClocks ->
  PermuteOutClocks
    SN inClocks internalClocks outClocksBefore ->
    Permutation outClocksBefore outClocksAfter ->
    SN inClocks internalClocks outClocksAfter ->
  DoneIn ::
    SN (Clocked () cl ': inClocks) internalClocks outClocks m e ->
    SN inClocks internalClocks outClocks m e
  -- TODO Or do we want to be able to erase arbitrary output?
  DoneOut ::
    SN inClocks internalClocks (Clocked () cl ': outClocks) m e ->
    SN inClocks internalClocks outClocks m e
  ConcatIn2 ::
    SN (Clocked a cl1 ': Clocked b cl2 ': inClocks) internalClocks outClocks ->
    SN (Clocked (HList '[a, b]) (Clocks '[cl1, cl2])) internalClocks outClocks

data Clocks = Clocks [Type]

data HList (as :: [Type]) where
  HNil :: HList '[]
  HCons :: a -> HList as -> HList (a ': as)


data Permutation (before :: [a]) (after :: [a]) where
  Swap :: Permutation (a1 ': a2 ': as) (a2 ': a1 ': as)
  Push :: Permutation before after -> Permutation (a ': before) (a ': after)
  Compose :: Permutation before intermediate -> Permutation intermediate after -> Permutation before after
