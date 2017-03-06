{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import           Control.Monad             (when)
import           Control.Concurrent        (threadDelay)
import qualified Data.HashMap.Strict       as HM
import           Data.Maybe                (fromMaybe)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import           Development.GitRev        (gitHash, gitDirty)
import           Network.HTTP.Client       (defaultManagerSettings, newManager)
import           Network.HTTP.ReverseProxy (ProxyDest (..),
                                            WaiProxyResponse (..), defaultOnExc,
                                            waiProxyTo)
import           Network.HTTP.Types        (status200, status404, status307)
import           Network.Wai               (requestHeaderHost, responseLBS, rawPathInfo, rawQueryString, responseBuilder, isSecure)
import           Network.Wai.Handler.Warp  (setPort, defaultSettings)
import           Network.Wai.Middleware.Gzip (gzip, def)
import           Text.Read                 (readMaybe)
import Data.Aeson (FromJSON (..), (.:), withObject)
import Data.Streaming.Network (bindRandomPortTCP)
import qualified Data.Yaml as Yaml
import Network.Socket (close)
import Control.Exception (throwIO)
import Control.Concurrent.Async (Concurrently (..))
import System.Environment (getEnvironment)
import System.Process (createProcess, proc, env, cwd, waitForProcess)
import qualified Data.ByteString as S
import Lucid
import Lucid.Html5
import Data.Functor.Identity (runIdentity)
import Control.Monad (forM_)
import Warp.LetsEncrypt

data Config port = Config
    { configApps :: ![App port]
    , configRedirects :: ![Redirect]
    , configIndexVhost :: !String
    , configTitle :: !T.Text
    }
    deriving (Show, Functor, Foldable, Traversable)
instance port ~ () => FromJSON (Config port) where
    parseJSON = withObject "Config" $ \o -> Config
        <$> o .: "apps"
        <*> o .: "redirects"
        <*> o .: "index-vhost"
        <*> o .: "title"

configDomains :: Config port -> [T.Text]
configDomains cfg = map T.pack $ configIndexVhost cfg : map appVhost (configApps cfg)

data App port = App
    { appVhost :: !String
    , appPort :: !port
    , appDir :: !FilePath
    , appExe :: !FilePath
    , appArgs :: ![String]
    , appDesc :: !T.Text
    }
    deriving (Show, Functor, Foldable, Traversable)
instance port ~ () => FromJSON (App port) where
    parseJSON = withObject "App" $ \o -> App
        <$> o .: "vhost"
        <*> pure ()
        <*> o .: "dir"
        <*> o .: "exe"
        <*> o .: "args"
        <*> o .: "desc"

data Redirect = Redirect
    { redSrc :: !String
    , redDst :: !String
    }
    deriving Show
instance FromJSON Redirect where
    parseJSON = withObject "Redirect" $ \o -> Redirect
        <$> o .: "src"
        <*> o .: "dst"

getRandomPort :: IO Int
getRandomPort = do
    (port, socket) <- bindRandomPortTCP "*"
    close socket
    return port

main :: IO ()
main = do
    cfg  <- Yaml.decodeFileEither "config/webapps.yaml"
        >>= either throwIO return
        >>= mapM (const getRandomPort)
    env0 <- getEnvironment
    let env1 = filter (\(k, _) -> k /= "PORT" && k /= "APPROOT") env0
    port <-
        case lookup "PORT" env0 of
            Nothing -> error "No PORT environment variable"
            Just sport ->
                case readMaybe sport of
                    Nothing -> error $ "Invalid port: " ++ sport
                    Just port -> return port
    portssl <-
        case lookup "PORTSSL" env0 of
            Nothing -> error "No PORTSSL environment variable"
            Just sport ->
                case readMaybe sport of
                    Nothing -> error $ "Invalid port (SSL): " ++ sport
                    Just portssl -> return portssl
    runConcurrently $ foldr
        (\app c -> Concurrently (runWebApp env1 app) *> c)
        (Concurrently $ runProxy port portssl cfg)
        (configApps cfg)

data Dest = Index | Port !Int | Host !S.ByteString

runProxy :: Int -> Int -> Config Int -> IO ()
runProxy proxyPort proxyPortSSL cfg = do
    manager <- newManager defaultManagerSettings
    putStrLn $ "Listening on: " ++ show proxyPort
    runLetsEncrypt LetsEncryptSettings
      { lesInsecureSettings = setPort proxyPort defaultSettings
      , lesSecureSettings = setPort proxyPortSSL defaultSettings
      , lesEmailAddress = "michael@snoyman.com"
      , lesDomains = configDomains cfg
      , lesApp = middleware $ app manager
      , lesBeforeSecure = threadDelay $ 1000 * 1000 * 30 -- thirty seconds to make Kube happy with the new deployment
      }
  where
    vhosts = HM.insert (T.encodeUtf8 $ T.pack $ configIndexVhost cfg) Index
           $ HM.fromList $ map toPair (configApps cfg)
                        ++ map redToPair (configRedirects cfg)
    toPair App {..} = (T.encodeUtf8 $ T.pack appVhost, Port appPort)

    dispatch req = return $ fromMaybe defRes $ do
        vhost <- requestHeaderHost req
        res <- HM.lookup vhost vhosts
        return $ case res of
            Port port -> WPRProxyDest $ ProxyDest "localhost" port
            Host host -> WPRResponse $ responseLBS status307
                [ ("Location", getLocation host req)
                ]
                "Redirecting"
            Index -> WPRResponse $ responseBuilder
                status200
                [("Content-Type", "text/html; charset=utf-8")]
                (runIdentity $ execHtmlT $ indexHtml cfg)
    defRes = WPRResponse $ responseLBS status404 [] "Host not found"

    getLocation host req = S.concat
        [ if isSecure req then "https://" else "http://"
        , host
        , rawPathInfo req
        , rawQueryString req
        ]

    redToPair (Redirect src dst) =
        ( T.encodeUtf8 $ T.pack src
        , Host $ T.encodeUtf8 $ T.pack dst
        )

    app = waiProxyTo dispatch defaultOnExc

    middleware = gzip def

indexHtml :: Config a -> Html ()
indexHtml cfg =
    doctypehtml_ $ do
        head_ $ do
            title_ $ toHtml $ configTitle cfg
        body_ $ do
            h1_ $ toHtml $ configTitle cfg
            p_ $ do
                "Git SHA: "
                b_ $gitHash
                when $gitDirty $ do
                    " "
                    i_ "dirty"
            ul_ $ do
                forM_ (configApps cfg) $ \app -> do
                    let url = T.pack $ concat
                            [ "http://"
                            , appVhost app
                            , "/"
                            ]
                    li_ $ a_ [href_ url] $ toHtml $ appDesc app

runWebApp :: [(String, String)] -> App Int -> IO ()
runWebApp env0 App {..} = do
    (Nothing, Nothing, Nothing, ph) <- createProcess (proc appExe appArgs)
        { env = Just $ ("PORT", show appPort)
                     : env0
        , cwd = Just appDir
        }
    ec <- waitForProcess ph
    error $ concat
        [ "Application on "
        , appVhost
        , " exited with "
        , show ec
        ]
