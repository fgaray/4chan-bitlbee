{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Chan where


import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import qualified Data.Text as T
import Network.HTTP.Client.TLS (tlsManagerSettings)


data ThreadCatalog = ThreadCatalog
    { images        :: Int
    , last_modified :: Int
    , stiky         :: Maybe Int
    , semantic_url  :: String
    , sub           :: Maybe String     -- Subject
    , no            :: Int
    , replies       :: Int
    } deriving (Show, Generic)

noThreadCatalog = no :: ThreadCatalog -> Int

instance FromJSON ThreadCatalog


data Catalog = Catalog
    { page    :: Int
    , threads :: [ThreadCatalog]
    } deriving (Show, Generic)

instance FromJSON Catalog

data Boards = Boards
    { boards :: [Board]
    } deriving (Show, Generic)

instance FromJSON Boards

data Board = Board
    { title            :: String
    , meta_description :: String
    , board            :: String
    } deriving (Show, Generic)

instance FromJSON Board


newtype ThreadNumber = ThreadNumber Int
    deriving (Read, Show)

instance ToHttpApiData ThreadNumber where
    toUrlPiece (ThreadNumber x) = T.pack $ show x ++ ".json"

data Thread = Thread
    { posts :: [ThreadJSON]
    }
    deriving (Show, Generic)
instance FromJSON Thread

data ThreadJSON = ThreadJSON
    { name         :: String       -- Author
    , no           :: Int          -- Number of the post
    , com          :: Maybe String -- The comment. Includes the html
    , country_name :: Maybe String
    , time         :: Int          -- UNIX
    , resto        :: Maybe Int    -- Respond to
    , filename     :: Maybe String
    , tim          :: Maybe Int    -- Image rename file (https://i.4cdn.org/g/TIM.jpg)
    , ext          :: Maybe String -- Extension of the image file
    , id           :: Maybe String -- The ID in boards like pol
    }
    deriving (Show, Generic)
instance FromJSON ThreadJSON

noThread = no :: ThreadJSON -> Int


-- API

-- | http(s)://a.4cdn.org/board/thread/threadnumber.json
type Chan =
         Capture "board" String :> "catalog.json" :> Get '[JSON] [Catalog]
    :<|> "boards.json" :> Get '[JSON] Boards
    :<|> Capture "board" String :> "thread" :> Capture "number" ThreadNumber :> Get '[JSON] Thread


api :: Proxy Chan
api = Proxy


baseUrl :: BaseUrl
baseUrl = BaseUrl Https "a.4cdn.org" 443 ""

mergeCatalogs :: [Catalog] -> Catalog
mergeCatalogs = Catalog 0 . concat . map threads


-- | Api functions

getCatalog :: String -> ClientM [Catalog]
getBoards :: ClientM Boards
getThread :: String -> ThreadNumber -> ClientM Thread

-- | Pattern match for the API
getCatalog :<|> getBoards :<|> getThread = client api



run :: ClientM a -> IO (Either ServantError a)
run endpoint = do
    manager <- newManager tlsManagerSettings
    res <- runClientM endpoint  (ClientEnv manager baseUrl)
    case res of
      Left err -> error $ show err
      Right x -> return (Right x)
