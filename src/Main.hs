{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Control.Exception (catch, handle, IOException)
import Network.HTTP.Client (BodyReader, defaultManagerSettings, HttpException, Manager, managerConnCount, managerResponseTimeout, parseUrl, Request, Response, responseBody, withManager, withResponse)
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.ByteString as ByteStringStrict
import System.IO (withFile, IOMode(WriteMode))
import Text.Printf (printf)


-------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------

siteUrl :: String
siteUrl = "http://museudaimigracao.org.br/acervodigital/livros.php?pesq=1&nome=&sobrenome=&chegada=&vapor=&nacionalidade=&pagina="

start :: Int
start = 1

total :: Int
total = 132028

printStatusEvery :: Int
printStatusEvery = 10

downloadLimitBytes :: Int
downloadLimitBytes = 50000

concurrentConnectionCount :: Int
concurrentConnectionCount = 40

responseTimeoutSeconds :: Int
responseTimeoutSeconds = 60

-------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------

outFilePath :: Int -> FilePath
outFilePath num = printf "output/site/%06d.html" num

errFilePath :: Int -> FilePath
errFilePath num = printf "output/err/%06d.html" num

request :: Int -> IO Request
request pageNum = parseUrl $ siteUrl ++ show pageNum

brReadSome :: BodyReader -> Int -> IO ByteStringLazy.ByteString
brReadSome brRead =
    loop id
  where
    loop front remainder
        | remainder <= 0 = return $ ByteStringLazy.fromChunks $ front []
        | otherwise = do
            bs <- brRead
            if ByteStringStrict.null bs
                then return $ ByteStringLazy.fromChunks $ front []
                else loop (front . (bs:)) (remainder - ByteStringStrict.length bs)

-------------------------------------------------------------------
-- file downloading functions
-------------------------------------------------------------------

doRequests :: Manager -> IO ()
doRequests manager = do
    let reqNum = 1
    req <- request reqNum
    catch (withResponse req manager $ doResponse reqNum)
          (\err -> writeFile (errFilePath reqNum) $ show (err::HttpException) ++ "\n")

doResponse :: Int -> Response BodyReader -> IO ()
doResponse reqNum response = do
    handle (\err -> writeFile (errFilePath reqNum) $ show (err::IOException) ++ "\n")
           (withFile (outFilePath reqNum) WriteMode $ \fileHandle -> do
              let lazybody = responseBody response
              body <- brReadSome lazybody downloadLimitBytes
              ByteStringLazy.hPut fileHandle body
              ByteStringLazy.hPut fileHandle "\n")

-------------------------------------------------------------------
-- main
-------------------------------------------------------------------

main :: IO ()
main = do
    let managerSettings = defaultManagerSettings
          { managerConnCount = concurrentConnectionCount
          , managerResponseTimeout = Just $ responseTimeoutSeconds * 1000000
          }
    withManager managerSettings doRequests

