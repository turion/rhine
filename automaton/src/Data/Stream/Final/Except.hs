module Data.Stream.Final.Except where

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except (ExceptT, runExceptT)

-- automaton
import Data.Stream.Final (Final (..))
import Data.Stream.Result (mapResultState)

handleExceptT :: (Monad m) => Final (ExceptT e1 m) b -> (e1 -> Final (ExceptT e2 m) b) -> Final (ExceptT e2 m) b
handleExceptT final handler = go final
  where
    go final = Final $ do
      resultOrException <- lift $ runExceptT $ getFinal final
      case resultOrException of
        Right result -> return $! mapResultState go result
        Left e -> getFinal $ handler e
