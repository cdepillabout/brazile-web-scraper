{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Control.Concurrent (forkIO, threadDelay)
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
--allRequestNumbers = [ 001708
--                    , 001709
--                    , 001710
--                    , 001711
--                    , 026100
--                    , 026101
--                    , 026102
--                    , 026103
--                    , 032079
--                    , 032080
--                    , 032081
--                    , 044379
--                    , 044380
--                    , 044381
--                    , 047975
--                    , 047976
--                    , 047977
--                    , 050686
--                    , 050687
--                    , 050688
--                    , 050689
--                    , 050690
--                    , 059745
--                    , 061557
--                    , 071152
--                    , 071153
--                    , 071154
--                    , 086170
--                    , 086172
--                    , 086177
--                    , 086178
--                    , 086181
--                    , 086182
--                    , 086183
--                    , 086184
--                    , 086185
--                    , 086186
--                    , 086188
--                    , 086190
--                    , 086191
--                    , 086193
--                    , 086194
--                    , 086195
--                    , 086196
--                    , 086197
--                    , 086198
--                    , 086199
--                    , 086200
--                    , 086202
--                    , 086203
--                    , 086205
--                    , 086216
--                    , 086217
--                    , 086219
--                    , 086224
--                    , 086226
--                    , 086227
--                    , 086229
--                    , 086232
--                    , 086236
--                    , 086237
--                    , 086238
--                    , 086239
--                    , 086240
--                    , 086241
--                    , 086242
--                    , 086243
--                    , 086247
--                    , 095215
--                    , 100764
--                    , 105081
--                    , 105082
--                    , 105083
--                    , 105084
--                    , 105273
--                    , 106132
--                    , 106133
--                    , 107832
--                    , 107833
--                    , 107834
--                    , 107835
--                    , 107836
--                    , 107837
--                    , 107838
--                    , 107842
--                    , 107843
--                    , 109297
--                    , 118011
--                    , 118012
--                    , 118013
--                    , 118014 ]

printStatusEvery :: Int
printStatusEvery = 500

downloadLimitBytes :: Int
downloadLimitBytes = 50000

concurrentConnectionCount :: Int
concurrentConnectionCount = 3

responseTimeoutSeconds :: Int
responseTimeoutSeconds = 120

threadSleepSeconds :: Int
threadSleepSeconds = 10

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
            threadDelay $ threadSleepSeconds * 1000000
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

