
import Network.HTTP.Client (BodyReader, defaultManagerSettings, Manager, managerConnCount, managerResponseTimeout, parseUrl, Request, Response, responseBody, withManager, withResponse)
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.ByteString as ByteStringStrict
import System.IO (withFile, IOMode(WriteMode))
import Text.Printf (printf)


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

outFilePath :: Int -> FilePath
outFilePath num = printf "output/site/%06d.html" num


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

main :: IO ()
main = do
    let managerSettings = defaultManagerSettings
          { managerConnCount = concurrentConnectionCount
          , managerResponseTimeout = Just $ responseTimeoutSeconds * 1000000
          }
    withManager managerSettings doRequests
  where
    request :: Int -> IO Request
    request pageNum = parseUrl $ siteUrl ++ show pageNum

    doRequests :: Manager -> IO ()
    doRequests manager = do
        req <- request 1
        withResponse req manager doResponse

    doResponse :: Response BodyReader -> IO ()
    doResponse response = do
        withFile (outFilePath 1) WriteMode $ \handle -> do
          let lazybody = responseBody response
          body <- brReadSome lazybody downloadLimitBytes
          ByteStringLazy.hPut handle body
          putStrLn ""

