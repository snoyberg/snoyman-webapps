{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.ByteString           (ByteString)
import qualified Data.HashMap.Strict       as HM
import           Data.Maybe                (fromMaybe)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import           Network.HTTP.Client       (defaultManagerSettings, newManager)
import           Network.HTTP.ReverseProxy (ProxyDest (..),
                                            WaiProxyResponse (..), defaultOnExc,
                                            waiProxyTo)
import           Network.HTTP.Types        (status404)
import           Network.Wai               (requestHeaderHost, responseLBS)
import           Network.Wai.Handler.Warp  (run)
import           System.Environment        (getArgs, getEnv)
import           Text.Read                 (readMaybe)

toPair :: String -> IO (ByteString, ProxyDest)
toPair s = maybe (error $ "Invalid argument: " ++ s) return $ do
    (vhost, '=':rest) <- Just $ break (== '=') s
    let (host, rest') =
            case break (== ':') rest of
                (x, ':':y) -> (toBS x, y)
                _ -> ("localhost", rest)
    port <- readMaybe rest'
    return (toBS vhost, ProxyDest host port)
  where
    toBS = T.encodeUtf8 . T.pack

main :: IO ()
main = do
    sport <- getEnv "PORT"
    port <-
        case readMaybe sport of
            Nothing -> error $ "Invalid port: " ++ sport
            Just port -> return port

    args <- getArgs
    pairs <- mapM toPair args
    manager <- newManager defaultManagerSettings
    let vhosts = HM.fromList pairs
        dispatch req = return $ fromMaybe defRes $ do
            vhost <- requestHeaderHost req
            fmap WPRProxyDest $ HM.lookup vhost vhosts
        defRes = WPRResponse $ responseLBS status404 [] "Host not found"
        app = waiProxyTo dispatch defaultOnExc manager

    run port app
