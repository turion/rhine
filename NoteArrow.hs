newtype Clocked cl a = Clocked { unClocked :: Maybe a }

data SNArrow m a b where
  Arr :: (a -> b) -> SNArrow m a b
  ArrM :: (a -> m b) -> SNArrow m a b
  IntoClocked :: SNArrow m a (Clocked a) -- FIXME Maybe not even needed?
  OutOfClocked :: SNArrow m (Clocked a) (Maybe a) -- FIXME Maybe not even needed?
  ClSF :: ClSF m cl a b -> SNArrow m (Clocked cl a) (Clocked cl b)
  ResBuf :: ResBuf m cl1 cl2 a b -> SNArrow m (Clocked cl1 a) (Clocked cl2 b)
  First :: SNArrow m a b -> SNArrow m (a, c) (b, c)
  Clock :: cl -> SNArrow m a (Clocked cl (TimeInfo cl, Tag cl))
  Comp :: SNArrow m a b -> SNArrow m b c -> SNArrow m a c

clockErasure :: SNArrow m a b -> MSF m a b
clockErasure (Comp (Arr f) (Arr g)) = arr f >>> arr g
clockErasure (Comp (Clock cl1) sna) = -- Something time parallel here!
-- This is really hard because the arrow desugarer will by default use sequential desugaring.
-- Maybe this should be improved in GHC...
