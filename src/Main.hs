{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan, writeList2Chan)
import Control.Exception (catch, IOException)
import Control.Monad (forever, forM_, when)
--import Debug.Trace (traceIO)
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

allRequestNumbers :: [Int]
allRequestNumbers = [start .. total]

printStatusEvery :: Int
printStatusEvery = 500

downloadLimitBytes :: Int
downloadLimitBytes = 50000

concurrentConnectionCount :: Int
concurrentConnectionCount = 10

responseTimeoutSeconds :: Int
responseTimeoutSeconds = 60

-------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------

outFilePath :: Int -> FilePath
outFilePath = printf "output/site/%06d.html"

errFilePath :: Int -> FilePath
errFilePath = printf "output/err/%06d.html"

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

doResponse :: Int -> Response BodyReader -> IO ()
doResponse reqNum response =
    catch (doResponse' >> printStatus) errHandler
  where
    printStatus :: IO ()
    printStatus = when (reqNum `mod` printStatusEvery == 0) $ print reqNum

    doResponse' :: IO ()
    doResponse' = withFile (outFilePath reqNum) WriteMode $ \fileHandle -> do
              let lazybody = responseBody response
              body <- brReadSome lazybody downloadLimitBytes
              ByteStringLazy.hPut fileHandle body
              ByteStringLazy.hPut fileHandle "\n"

    errHandler :: IOException -> IO ()
    errHandler err = writeFile (errFilePath reqNum) $ show err ++ "\n"

doRequest :: (Manager, Int) -> IO ()
doRequest (manager, reqNum) = do
    req <- request reqNum
    catch (withResponse req manager $ doResponse reqNum)
          (\err -> writeFile (errFilePath reqNum) $ show (err::HttpException) ++ "\n")

doRequestsThreadPool :: Manager -> IO ()
doRequestsThreadPool manager = do
    (inputChan, outputChan) <- threadPoolIO concurrentConnectionCount doRequest
    _ <- writeList2Chan inputChan $ zip (repeat manager) allRequestNumbers
    mapM_ (\_ -> readChan outputChan) allRequestNumbers

-------------------------------------------------------------------
-- main
-------------------------------------------------------------------

main :: IO ()
main = do
    let managerSettings = defaultManagerSettings
          { managerConnCount = concurrentConnectionCount
          , managerResponseTimeout = Just $ responseTimeoutSeconds * 1000000
          }
    withManager managerSettings doRequestsThreadPool

