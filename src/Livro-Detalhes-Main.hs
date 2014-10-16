{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan, writeList2Chan)
import Control.Exception (catch, IOException)
import Control.Monad (forever, forM_, when)
import Data.Functor ((<$>))
--import Debug.Trace (traceIO)
import Network.HTTP.Client (BodyReader, brConsume, defaultManagerSettings, HttpException, Manager, managerConnCount, managerResponseTimeout, parseUrl, Request, Response, responseBody, withManager, withResponse)
import qualified Data.ByteString as ByteStringStrict
import System.IO (withFile, IOMode(WriteMode))
import Text.Printf (printf)


-------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------

allDetailsPath :: String
allDetailsPath = "all_livrodetalhes_sorted_uniq"

siteUrl :: String
siteUrl = "http://museudaimigracao.org.br/acervodigital/"

-- start :: Int
-- start = 1

-- total :: Int
-- total = 488562

allRequestNumbers :: [Int]
--allRequestNumbers = [start .. total]
allRequestNumbers = [
                      047746
                    , 081560
                    , 092379
                    , 120726
                    , 128670
                    , 150385
                    , 155625
                    , 194418
                    , 212431
                    , 219315
                    , 295769
                    , 442124
                    , 471895
                    ]

printStatusEvery :: Int
printStatusEvery = 10000

concurrentConnectionCount :: Int
concurrentConnectionCount = 6

responseTimeoutSeconds :: Int
responseTimeoutSeconds = 120

threadSleepSeconds :: Int
threadSleepSeconds = 1

-------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------

outFilePath :: Int -> FilePath
outFilePath = printf "output/detalhes/%06d.html"

errFilePath :: Int -> FilePath
errFilePath = printf "output/err/%06d.html"

request :: String -> IO Request
request url = parseUrl $ siteUrl ++ url

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
              bytestrings <- brConsume lazybody
              forM_ bytestrings $ \bs -> ByteStringStrict.hPut fileHandle bs
              ByteStringStrict.hPut fileHandle "\n"

    errHandler :: IOException -> IO ()
    errHandler err = writeFile (errFilePath reqNum) $ show err ++ "\n"

doRequest :: (Manager, Int, String) -> IO ()
doRequest (manager, reqNum, reqUrl) = do
    putStrLn $ "reqNum: " ++ show reqNum ++ ", url: " ++ reqUrl
    req <- request reqUrl
    catch (withResponse req manager $ doResponse reqNum)
          (\err -> writeFile (errFilePath reqNum) $ show (err::HttpException) ++ "\n")

doRequestsThreadPool :: [String] -> Manager -> IO ()
doRequestsThreadPool allDetailsList manager = do
    (inputChan, outputChan) <- threadPoolIO concurrentConnectionCount doRequest
    _ <- writeList2Chan inputChan $ zip3 (repeat manager) allRequestNumbers allDetailsList
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
    allDetailsList <- lines <$> readFile allDetailsPath
    let linesToFind = map (\x -> allDetailsList !! (x - 1)) allRequestNumbers
    withManager managerSettings $ doRequestsThreadPool linesToFind
