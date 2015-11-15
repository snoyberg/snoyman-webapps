{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Main (main) where

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
import           Text.Read                 (readMaybe)
import Data.Aeson (FromJSON (..), (.:), withObject)
import Data.Streaming.Network (bindRandomPortTCP)
import qualified Data.Yaml as Yaml
import Network.Socket (sClose)
import Control.Exception (throwIO)
import Control.Concurrent.Async (Concurrently (..))
import System.Environment (getEnvironment)
import System.Process (createProcess, proc, env, cwd, waitForProcess)

data App port = App
    { appVhost :: !String
    , appPort :: !port
    , appDir :: !FilePath
    , appExe :: !FilePath
    , appArgs :: ![String]
    }
    deriving (Show, Functor, Foldable, Traversable)
instance port ~ () => FromJSON (App port) where
    parseJSON = withObject "App" $ \o -> App
        <$> o .: "vhost"
        <*> pure ()
        <*> o .: "dir"
        <*> o .: "exe"
        <*> o .: "args"

getRandomPort :: IO Int
getRandomPort = do
    (port, socket) <- bindRandomPortTCP "*"
    sClose socket
    return port

main :: IO ()
main = do
    apps <- Yaml.decodeFileEither "config/webapps.yaml"
        >>= either throwIO return
        >>= mapM (mapM $ const getRandomPort)
    env0 <- getEnvironment
    let env1 = filter (\(k, _) -> k /= "PORT" && k /= "APPROOT") env0
    port <-
        case lookup "PORT" env0 of
            Nothing -> error "No PORT environment variable"
            Just sport ->
                case readMaybe sport of
                    Nothing -> error $ "Invalid port: " ++ sport
                    Just port -> return port
    runConcurrently $ foldr
        (\app c -> Concurrently (runWebApp env1 app) *> c)
        (Concurrently $ runProxy port apps)
        apps

runProxy :: Int -> [App Int] -> IO ()
runProxy proxyPort apps = do
    manager <- newManager defaultManagerSettings
    putStrLn $ "Listening on: " ++ show proxyPort
    run proxyPort (app manager)
  where
    vhosts = HM.fromList $ map toPair apps
    toPair App {..} = (T.encodeUtf8 $ T.pack appVhost, appPort)

    dispatch req = return $ fromMaybe defRes $ do
        vhost <- requestHeaderHost req
        fmap (WPRProxyDest . ProxyDest "localhost") $ HM.lookup vhost vhosts
    defRes = WPRResponse $ responseLBS status404 [] "Host not found"

    app = waiProxyTo dispatch defaultOnExc

runWebApp :: [(String, String)] -> App Int -> IO ()
runWebApp env0 App {..} = do
    (Nothing, Nothing, Nothing, ph) <- createProcess (proc appExe appArgs)
        { env = Just $ ("PORT", show appPort)
                     : ("APPROOT", "http://" ++ appVhost)
                     : env0
        , cwd = Just appDir
        }
    waitForProcess ph >>= throwIO
