module Data.Stream.Recursive.Except where

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except (ExceptT, runExceptT)

-- automaton
import Data.Stream.Recursive (Recursive (..))
import Data.Stream.Result (mapResultState)

handleExceptT :: (Monad m) => Recursive (ExceptT e1 m) b -> (e1 -> Recursive (ExceptT e2 m) b) -> Recursive (ExceptT e2 m) b
handleExceptT recursive handler = go recursive
  where
    go recursive = Recursive $ do
      resultOrException <- lift $ runExceptT $ getRecursive recursive
      case resultOrException of
        Right result -> return $! mapResultState go result
        Left e -> getRecursive $ handler e
