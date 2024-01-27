{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Count the number of words in the complete works of Shakespeare.
module WordCount where

-- base
import Control.Exception
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
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
import Data.MonadicStreamFunction qualified as Dunai

-- rhine
import FRP.Rhine
import Paths_rhine

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

testFile :: IO FilePath
testFile = getDataFileName "bench/pg100.txt"

withInput :: IO b -> IO b
withInput action = do
  inputFileName <- testFile
  withFile inputFileName ReadMode $ \stdinFile -> do
    hDuplicateTo stdinFile stdin
    action

-- FIXME the StdinClock should really throw something in Except instead.
-- Or there should be utilities to transform an IO exception into ExceptT, in rhine
rhineWordCount :: IO Int
rhineWordCount = do
  wcOut <- newIORef (0 :: Int)
  catch (withInput $ flow (wc wcOut @@ StdinClock) >> readIORef wcOut) $ \(e :: IOError) ->
    if isEOFError e
      then readIORef wcOut
      else throwIO e
  where
    wc :: IORef Int -> ClSF IO StdinClock () ()
    wc wcOut = proc _ -> do
      line <- tagS -< ()
      words <- mappendS -< Sum $ length $ words line
      arrMCl $ writeIORef wcOut -< getSum words
      returnA -< ()

dunaiWordCount :: IO Int
dunaiWordCount = do
  wcOut <- newIORef (0 :: Int)
  catch (withInput $ Dunai.reactimate (wc wcOut) >> readIORef wcOut) $ \(e :: IOError) ->
    if isEOFError e
      then readIORef wcOut
      else throwIO e
  readIORef wcOut
  where
    wc wcOut = proc () -> do
      line <- Dunai.constM getLine -< ()
      words <- Dunai.mappendS -< Sum $ length $ words line
      Dunai.arrM $ writeIORef wcOut -< getSum words
      returnA -< ()

{- | This is what 'rhineWordCount' should reduce to roughly (except the way the IORef is handled).

Everything that it takes longer is Rhine overhead.
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

textWordCountNoIORef :: IO Int
textWordCountNoIORef = do
  withInput $ go 0
  where
    step n = do
      line <- getLine
      return $ Right $ n + length (words line)
    go n = do
      n' <- catch (step n) $
        \(e :: IOError) ->
          if isEOFError e
            then return $ Left n
            else throwIO e
      either return go n'

textLazy :: IO Int
textLazy = do
  inputFileName <- testFile
  handle <- openFile inputFileName ReadMode
  length . Lazy.words <$> hGetContents handle
