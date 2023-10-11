module FRP.Rhine.Clock.File where

-- base
import Control.Monad.IO.Class
import Data.Bifunctor (first)
import Data.Void
import System.IO
import System.IO.Error

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy

-- | A clock that opens a file in read mode and extracts data of type @a@ from it.
data File e a = File
  { filename :: FilePath
  -- ^ The path of the file to be opened
  , action :: Handle -> IO (Either e a)
  -- ^ The action to be performed on the file handle,
  --   e.g. a line being read
  }

{- | Read a line of text from a text file.

For higher performance, you will typically want to use a 'Text' or 'ByteString' version,
see https://github.com/turion/rhine/issues/257.
-}
type TextFile = File Void String

{- | Create a 'TextFile' from a file path.

It ticks at every line of the file.
Its 'Tag' will be 'String', the current line.
-}
textFile :: FilePath -> TextFile
textFile filename =
  File
    { filename
    , action = fmap Right . hGetLine
    }

instance GetClockProxy (File e a)

{- | The only non-error exception that the 'File' clock can throw.

It is thrown when the file reaches its end.

To handle this exception outside of @rhine@,
lift all other signal components to the 'ExceptT' transformer,
call 'flow' on the whole 'Rhine',
and then 'runExceptT'.

To handle this exception inside of @rhine@,
you will probably want to use 'eraseClock' on the 'Rhine' containing the 'File',
and then add the result to another signal network.
-}
data FileException = EndOfFile
  deriving (Show, Eq)

instance MonadIO m => Clock (ExceptT (Either e FileException) m) (File e a) where
  type Time (File e a) = Integer
  type Tag (File e a) = a
  initClock File {filename, action} = lift $ do
    handle <- liftIO $ openFile filename ReadMode
    let getLineHandle = arrM $ const $ ExceptT $ liftIO $ do
          catchIOError (Data.Bifunctor.first Left <$> action handle) $ \e -> do
            hClose handle
            if isEOFError e
              then return $ Left $ Right EndOfFile
              else ioError e
    return (count &&& getLineHandle, 0)
