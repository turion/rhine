module Main (main) where

-- transformers
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (WriterT (..), tell)

-- bytestring
import Data.ByteString
import Data.ByteString qualified as ByteString

-- directory
import System.Directory (removeFile)

-- hspec
import Test.Hspec

-- vector
import Data.Vector (Vector)

-- cassava
import Data.Csv (Record)

-- rhine
import FRP.Rhine
import FRP.Rhine.Clock.File (FileException (..))

-- rhine-cassava
import FRP.Rhine.Clock.Csv

main :: IO ()
main = hspec $ do
  it "Can read a file" $ do
    let bytestring = "hi,this\nis,dog\n" :: ByteString
        filepath = "testfile.csv"
    ByteString.writeFile filepath bytestring
    Right result <- runExceptT @(Either String FileException) $ (flip embed [(), ()] =<<) $ eraseClock $ tagS @@ csvClock filepath
    removeFile filepath
    result `shouldBe` ([Just [["hi", "this"]], Just [["is", "dog"]]] :: [Maybe (Vector Record)])

  it "Throws EndOfFile when run for more ticks than the file is long" $ do
    let bytestring = "hi,this\nis,dog\n" :: ByteString
        filepath = "testfile.csv"
    ByteString.writeFile filepath bytestring
    Left e <- runExceptT @(Either String FileException) $ flow $ clId @@ csvClock filepath
    removeFile filepath
    e `shouldBe` Right EndOfFile

  it "Parses the CSV before throwing EndOfFile when run for more ticks than the file is long" $ do
    let bytestring = "hi,this\nis,dog\n" :: ByteString
        filepath = "testfile.csv"
        writerClock = HoistClock (csvClock filepath) (mapExceptT lift) :: HoistClock (ExceptT (Either String FileException) IO) (ExceptT (Either String FileException) (WriterT (Vector Record) IO)) CsvClock
    ByteString.writeFile filepath bytestring
    (Left e, contents) <- runWriterT $ runExceptT @(Either String FileException) $ flow $ tagS >-> arrMCl (lift . tell) @@ writerClock
    removeFile filepath
    e `shouldBe` Right EndOfFile
    contents `shouldBe` ([["hi", "this"], ["is", "dog"]] :: Vector Record)

  it "Throws EndOfFile when given incorrect separator" $ do
    let bytestring = "oh,that\ncat,is,rude\n" :: ByteString
        filepath = "testfile.csv"
    ByteString.writeFile filepath bytestring
    Left e <- runExceptT @(Either String FileException) $ flow $ clId @@ csvClock filepath
    removeFile filepath
    e `shouldBe` Left "hi"

  it "Throws EndOfFile when given incorrect separator" $ do
    let bytestring = "oh,that\ncat\n" :: ByteString
        filepath = "testfile.csv"
    ByteString.writeFile filepath bytestring
    Left e <- runExceptT @(Either String FileException) $ flow $ clId @@ csvClock filepath
    removeFile filepath
    e `shouldBe` Left "hi"

  it "Throws EndOfFile when given incorrect separator" $ do
    let bytestring = "oh,that\ncat" :: ByteString
        filepath = "testfile.csv"
    ByteString.writeFile filepath bytestring
    Left e <- runExceptT @(Either String FileException) $ flow $ clId @@ csvClock filepath
    removeFile filepath
    e `shouldBe` Left "hi"
