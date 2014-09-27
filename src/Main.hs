
import Network.HTTP.Client (BodyReader, defaultManagerSettings, Manager, managerConnCount, managerResponseTimeout, parseUrl, Request, Response, responseBody, withManager, withResponse)
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.ByteString as ByteStringStrict


siteUrl :: String
siteUrl = "http://museudaimigracao.org.br/acervodigital/livros.php?pesq=1&nome=&sobrenome=&chegada=&vapor=&nacionalidade=&pagina="


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
    let managerSettings = defaultManagerSettings { managerConnCount = 10
                                                 , managerResponseTimeout = Just 60000000
                                                 }
    withManager managerSettings doRequests
  where
    request :: IO Request
    request = parseUrl $ siteUrl ++ show (1::Integer)

    doRequests :: Manager -> IO ()
    doRequests manager = do 
        req <- request
        withResponse req manager doResponse

    doResponse :: Response BodyReader -> IO ()
    doResponse response = do
        let lazybody = responseBody response
        body <- brReadSome lazybody 100
        ByteStringLazy.putStr body
        putStrLn ""

