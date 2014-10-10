{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan, writeList2Chan)
import Control.Exception (catch, IOException)
import Control.Monad (forever, forM_)
import Data.String (fromString)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
--import Debug.Trace (traceIO)
import qualified Text.HTML.DOM as TextHTMLDOM
import Text.Printf (printf)
import Text.XML.Cursor (($//), (&|), (&/), (>=>), child, content, Cursor, element, fromDocument)


-------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------

start :: Int
start = 1

total :: Int
--total = 20
total = 1

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

-------------------------------------------------------------------
-- file downloading functions
-------------------------------------------------------------------

doParse :: Int -> IO ()
doParse reqNum = doParse'
  where
    doParse' :: IO ()
    doParse' = do
      document <- TextHTMLDOM.readFile $ fromString $ inFilePath reqNum
      let cursor = fromDocument document
      --let familyData = cursor $// findNodes &| child &| content
      let familyData = cursor $// element "table" >=> attributeIs "width" "908" &// element "tr" &/ element "td" >=> attributeIs "class" "texto" >=> attributeIs "align" "left" &/ content
      -- this works, but the result is hard to work with
      -- let familyData = cursor $// element "table" &| attributeIs "width" "908" &// element "tr" &/ element "td" &| attributeIs "class" "texto" &| attributeIs "align" "left" &/ content
      print $ length familyData
      print familyData
      --let fixedFamilyData = (Text.pack . show $ reqNum) : fixFamilyData familyData
      --print $ length fixedFamilyData
      --print fixedFamilyData
      --TextIO.putStrLn $ Text.intercalate " ; " $ map Text.strip fixedFamilyData
      return ()

    fixElement :: [[Text.Text]] -> Text.Text
    fixElement [] = ""
    fixElement a = Text.concat $ concat a

    fixFamilyData :: [[[Text.Text]]] -> [Text.Text]
    fixFamilyData = map fixElement

    findNodes :: Cursor -> [Cursor]
    --findNodes = element "tr" >=> child
    --findNodes = element "tr" >=> child &/ element "div" >=> child
    findNodes = element "tr" >=> child &/ element "td"


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
