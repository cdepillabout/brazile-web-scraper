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
total = 20
--total = 3

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
      let familyData = cursor $// findNodes &| child &| content
      let fixedFamilyData = (Text.pack . show $ reqNum) : fixFamilyData familyData
      --print $ length fixedFamilyData
      --print fixedFamilyData
      TextIO.putStrLn $ Text.intercalate " ; " $ map Text.strip fixedFamilyData
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
    putStrLn "FamilyID ; Livro ; Página ; Família ; Número Ordem ; Chefe ; Sobrenome ; Nome ; Parentesco ; Nacionalidade ; Idade ; Estado Civil ; Procedência ; Destino ; Vapor ; Chegada ; Nacion. Trad ; ContaGov ; Sexo ; Religião ; Ler ; Profissão ; Fazendeiro ; Observação ; Notas ; Dest_Est ; Res_Local ; Res_Pais ; Res_Tempo ; DesemBra ; Não Bra ; Bra_Lugar ; Bra_Tempo ; PQEntraram ; Repatriado ; Porto Emb. ;  Ferrovia ; Data Nasc. ;  Data Part. ;  Filiação ; Introductor ; Condição ; Lugar Nasc."
    runThreadPool
