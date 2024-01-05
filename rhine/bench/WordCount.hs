{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Count the number of words in the complete works of Shakespeare.
module WordCount where

-- base
import Control.Exception
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Monoid (Sum (..))
import GHC.IO.Handle hiding (hGetContents)
import System.IO (IOMode (ReadMode), openFile, stdin, withFile)
import System.IO.Error (isEOFError)
import Prelude hiding (getContents, getLine, words)

-- text
import Data.Text (words)
import Data.Text.IO (getLine)
import Data.Text.Lazy qualified as Lazy
import Data.Text.Lazy.IO (hGetContents)

-- criterion
import Criterion.Main

-- dunai
import Control.Monad.Trans.MSF.Except qualified as Dunai
import Data.MonadicStreamFunction qualified as Dunai

-- rhine
import FRP.Rhine
import FRP.Rhine.Clock.Except (
  DelayIOError,
  ExceptClock (..),
  delayIOError,
 )
import Paths_rhine

-- * Top level benchmarks

benchmarks :: Benchmark
benchmarks =
  bgroup
    "WordCount"
    [ bench "rhine" $ nfIO rhineWordCount
    , bench "dunai" $ nfIO dunaiWordCount
    , bgroup
        "Text"
        [ bench "IORef" $ nfIO textWordCount
        , bench "no IORef" $ nfIO textWordCountNoIORef
        , bench "Lazy" $ nfIO textLazy
        ]
    ]

-- * Benchmark helpers

-- | The path to Shakespeare's complete works
testFile :: IO FilePath
testFile = getDataFileName "bench/pg100.txt"

-- | Provide Shakespeare's complete works on stdin
withInput :: IO b -> IO b
withInput action = do
  inputFileName <- testFile
  withFile inputFileName ReadMode $ \stdinFile -> do
    hDuplicateTo stdinFile stdin
    action

-- * Frameworks specific implementations of word count

-- | Idiomatic Rhine implementation with a single clock
rhineWordCount :: IO Int
rhineWordCount = do
  Left (Right nWords) <- withInput $ runExceptT $ flow $ wc @@ delayIOError (ExceptClock StdinClock) Left
  return nWords
  where
    wc :: ClSF (ExceptT (Either IOError Int) IO) (DelayIOError (ExceptClock StdinClock IOError) (Either IOError Int)) () ()
    wc = proc _ -> do
      lineOrStop <- tagS -< ()
      nWords <- mappendS -< either (const 0) (Sum . length . words) lineOrStop
      throwOn' -< (either isEOFError (const False) lineOrStop, Right $ getSum nWords)

{- | Idiomatic dunai implementation.

Compared to Rhine, this doesn't have the overhead of clocks,
but it's implemented with continuations and not explicit state machines.
-}
dunaiWordCount :: IO Int
dunaiWordCount = do
  Left (Right nWords) <- withInput $ runExceptT $ Dunai.reactimate wc
  return nWords
  where
    wc = proc () -> do
      lineOrEOF <- Dunai.constM $ liftIO $ Control.Exception.try getLine -< ()
      nWords <- Dunai.mappendS -< either (const 0) (Sum . length . words) lineOrEOF
      case lineOrEOF of
        Right _ -> returnA -< ()
        Left e ->
          Dunai.throwS -< if isEOFError e then Right $ getSum nWords else Left e

-- ** Reference implementations in Haskell

{- | The fastest line-based word count implementation that I could think of.

Except for the way the IORef is handled,
this is what 'rhineWordCount' would reduce to roughly if all possible optimizations kick in,
and automata don't add any overhead.
-}
textWordCount :: IO Int
textWordCount = do
  wcOut <- newIORef (0 :: Int)
  catch (withInput $ go wcOut) $ \(e :: IOError) ->
    if isEOFError e
      then return ()
      else throwIO e
  readIORef wcOut
  where
    go wcOut = do
      line <- getLine
      modifyIORef' wcOut (+ length (words line))
      go wcOut

{- | The fastest line-based word count implementation that I could think of, not using IORefs.

This is what 'rhineWordCount' would reduce to roughly, if all possible optimizations kick in.
It is a bit slower than the version with IORef.
-}
textWordCountNoIORef :: IO Int
textWordCountNoIORef = do
  withInput $ go 0
  where
    processLine n = do
      line <- getLine
      return $ Right $ n + length (words line)
    go n = do
      n' <- catch (processLine n) $
        \(e :: IOError) ->
          if isEOFError e
            then return $ Left n
            else throwIO e
      either return go n'

-- | Just for fun the probably most readable but not the fastest way to count the number of words.
textLazy :: IO Int
textLazy = do
  inputFileName <- testFile
  h <- openFile inputFileName ReadMode
  length . Lazy.words <$> hGetContents h
