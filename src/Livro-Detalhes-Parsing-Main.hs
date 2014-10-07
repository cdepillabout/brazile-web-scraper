{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan, writeList2Chan)
import Control.Exception (catch, IOException)
import Control.Monad (forever, forM_, when)
import Data.Functor ((<$>))
import Data.String (fromString)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
--import Debug.Trace (traceIO)
import System.IO (withFile, IOMode(WriteMode))
import qualified Text.HTML.DOM as TextHTMLDOM
import Text.Printf (printf)
import Text.XML.Cursor (($/), ($//), (&|), (&/), (>=>), child, content, Cursor, element, fromDocument, node)


-------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------

start :: Int
start = 1

total :: Int
-- total = 20
total = 1

allRequestNumbers :: [Int]
allRequestNumbers = [start .. total]

printStatusEvery :: Int
printStatusEvery = 1

concurrentConnectionCount :: Int
concurrentConnectionCount = 1

-------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------

inFilePath :: Int -> FilePath
inFilePath = printf "output/detalhes/%06d.html"

outFilePath :: Int -> FilePath
outFilePath = printf "output/parsed/%06d.html"

errFilePath :: Int -> FilePath
errFilePath = printf "output/err/%06d.html"

threadPoolIO :: Int -> (a -> IO ()) -> IO (Chan a, Chan ())
threadPoolIO nr mutator = do
    inputChan <- newChan
    outputChan <- newChan
    forM_ [1..nr] $
        \_ -> forkIO (forever $ do
            i <- readChan inputChan
            o <- mutator i
            writeChan outputChan o)
    return (inputChan, outputChan)

-------------------------------------------------------------------
-- file downloading functions
-------------------------------------------------------------------

doParse :: Int -> IO ()
doParse reqNum = doParse' >> printStatus
  where
    printStatus :: IO ()
    printStatus = when (reqNum `mod` printStatusEvery == 0) $ print reqNum

    doParse' :: IO ()
    -- doParse' = withFile (outFilePath reqNum) WriteMode $ \outFileHandle -> do
    --               withFile (inFilePath reqNum) WriteMode $ \inFileHandle -> do
    --           let lazybody = responseBody response
    --           bytestrings <- brConsume lazybody
    --           forM_ bytestrings $ \bs -> ByteStringStrict.hPut fileHandle bs
    --           ByteStringStrict.hPut fileHandle "\n"
    doParse' = do
      document <- TextHTMLDOM.readFile $ fromString $ inFilePath reqNum
      let cursor = fromDocument document
      --let familyData = concat $ cursor $// findNodes &| content
      let familyData = cursor $// findNodes &| child &| content
      let fixedFamilyData = fixFamilyData familyData
      print $ length fixedFamilyData
      print fixedFamilyData
      -- TODO: Take out uneeded spaces (strip)
      TextIO.putStrLn $ Text.intercalate " ; " fixedFamilyData
      -- forM_ (concat familyData) $ \familyDatum -> do
      --   TextIO.putStrLn who
      --   putStrLn "-------------------------------------"
      return ()

    fixElement :: [[Text.Text]] -> Text.Text
    fixElement [] = ""
    fixElement a = Text.concat $ concat a

    fixFamilyData :: [[[Text.Text]]] -> [Text.Text]
    fixFamilyData = map fixElement

    findNodes :: Cursor -> [Cursor]
    --findNodes = element "tr" >=> child
    --findNodes = element "tr" >=> child &/ element "div" >=> child
    findNodes = element "tr" >=> child &/ element "div"


doParseWrapper :: Int -> IO ()
doParseWrapper reqNum = do
    catch (doParse reqNum)
          (\err -> writeFile (errFilePath reqNum) $ show (err::IOException) ++ "\n")

runThreadPool :: IO ()
runThreadPool = do
    (inputChan, outputChan) <- threadPoolIO concurrentConnectionCount doParseWrapper
    _ <- writeList2Chan inputChan allRequestNumbers
    mapM_ (\_ -> readChan outputChan) allRequestNumbers

-------------------------------------------------------------------
-- main
-------------------------------------------------------------------

main :: IO ()
main = runThreadPool
