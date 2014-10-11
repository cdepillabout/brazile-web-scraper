{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan, writeList2Chan)
import Control.Exception (catch, IOException)
import Control.Monad (forever, forM_, when)
import Data.String (fromString)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
--import Debug.Trace (traceIO)
import System.IO (hPutStrLn, stderr)
import qualified Text.HTML.DOM as TextHTMLDOM
import Text.Printf (printf)
import Text.XML.Cursor (($//), (&|), (&/), (&//), (>=>), attributeIs, child, content, Cursor, element, fromDocument)


-------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------

start :: Int
start = 1

total :: Int
total = 20

allRequestNumbers :: [Int]
allRequestNumbers = [start .. total]

concurrentConnectionCount :: Int
concurrentConnectionCount = 1

-------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------

inFilePath :: Int -> FilePath
inFilePath = printf "output/detalhes/%06d.html"

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

chunksOf :: Int -> [a] -> [[a]]
chunksOf num = go
  where
        go xs = case splitAt num xs of
                 (a,b) | null a    -> []
                       | otherwise -> a : go b

textShow :: Show a => a -> Text.Text
textShow = Text.pack . show

-------------------------------------------------------------------
-- file downloading functions
-------------------------------------------------------------------

doParse :: Int -> IO ()
doParse reqNum = do
    document <- TextHTMLDOM.readFile $ fromString $ inFilePath reqNum
    let cursor = fromDocument document
    let allContent = cursor $// findNodes &| child &| content
    let familyData = concat $ concatMap (\x -> if null x then [[" "]] else x) allContent
    process familyData
  where
    process :: [Text.Text] -> IO()
    process familyData = do
      checkNull familyData
      --print $ length familyData
      --print familyData
      let chunkedFamilyData = chunksOf 13 familyData
      forM_ chunkedFamilyData processChunk

    checkNull :: [Text.Text] -> IO ()
    checkNull familyData =
      when (null familyData) $ do
        let familyDataString = show familyData
            errorMessage = "error: length for familyData from " ++ show reqNum ++ " is 0: "
        hPutStrLn stderr $ errorMessage ++ familyDataString

    processChunk :: [Text.Text] -> IO ()
    processChunk chunk =
      if length chunk /= 13
        then
          let chunkString = show chunk
              errorMessage = "error: length of chunk from " ++ show reqNum ++ " is not 13: "
          in hPutStrLn stderr $ errorMessage ++ chunkString
        else
          TextIO.putStrLn $ Text.intercalate " ; " $ map Text.strip $ textShow reqNum : chunk

    findNodes :: Cursor -> [Cursor]
    --findNodes = element "tr" >=> child &/ element "td"
    findNodes =
        element "table" >=> attributeIs "width" "908" &//
            element "tr" &/
                element "td" >=> attributeIs "class" "texto" >=> attributeIs "align" "left"




doParseWrapper :: Int -> IO ()
doParseWrapper reqNum =
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
main = do
    putStrLn "FamilyID ; Livro ; Página ; Família ; Chegada ; Sobrenome ; Nome ; Idade ; Sexo ; Parentesco ; Nacionalidade ; Vapor ; Est.Civil ; Religião"
    runThreadPool
