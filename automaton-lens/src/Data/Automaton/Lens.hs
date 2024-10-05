module Data.Automaton.Lens where

-- lens
import Control.Lens.Combinators

-- profunctors
import Data.Profunctor.Traversing (Traversing (..))

-- automaton
import Data.Automaton
import Data.Stream.Result (ResultStateT)
import Control.Arrow

{- HLINT ignore applyLensLike "Eta reduce" -} -- I think deep subsumption prevents this hint from typechecking
applyLensLike :: Functor m => (forall state m . LensLike (ResultStateT state m) s t a b) -> Automaton m a b -> Automaton m s t
applyLensLike lensLike = handleStatefully lensLike

applyTraversal :: Monad m => Traversal s t a b -> Automaton m a b -> Automaton m s t
applyTraversal = wander

{- HLINT ignore applyLens "Eta reduce" -}
applyLens :: Functor m => Lens s t a b -> Automaton m a b -> Automaton m s t
applyLens lens = handleStatefully lens

applyPrism_ :: Monad m => Prism s t a b -> Automaton m a b -> Automaton m s t
applyPrism_ prism automaton = prism (automaton >>> arr Identity) >>> arr runIdentity

applyPrism :: (Monad m, Applicative f) => Prism s t a b -> Automaton m a (f b) -> Automaton m s (f t)
applyPrism prism = prism

{-
* Simple lenses ~ apply only one end?
-}
