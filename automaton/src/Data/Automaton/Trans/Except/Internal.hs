module Data.Automaton.Trans.Except.Internal where

-- transformers
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Reader

commuteReader :: ReaderT r (ExceptT e m) a -> ExceptT e (ReaderT r m) a
commuteReader = ExceptT . ReaderT . fmap runExceptT . runReaderT

commuteReaderBack :: ExceptT e (ReaderT r m) a -> ReaderT r (ExceptT e m) a
commuteReaderBack = ReaderT . fmap ExceptT . runReaderT . runExceptT
