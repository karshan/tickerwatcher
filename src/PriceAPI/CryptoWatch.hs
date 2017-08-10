{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE OverloadedStrings     #-}
module PriceAPI.CryptoWatch
    ( getPrices
    , Prices(..)
    ) where

import Control.Monad.Except (ExceptT)

import Data.Aeson
import Data.Proxy (Proxy(..))

import GHC.Generics (Generic)

import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Servant.API
import Servant.Client

type GdaxApi = "markets" :> "gdax" :>
    (    "btcusd" :> "price" :> Get '[JSON] Price
    :<|> "ltcusd" :> "price" :> Get '[JSON] Price
    :<|> "ethusd" :> "price" :> Get '[JSON] Price)

data Price =
    Price {
        result    :: PriceResult
      , allowance :: GdaxAllowance
    } deriving (Eq, Ord, Show, Generic, FromJSON)

data PriceResult =
    PriceResult {
        price :: Double
    } deriving (Eq, Ord, Show, Generic, FromJSON)

data GdaxAllowance =
    GdaxAllowance {
        cost      :: Int
      , remaining :: Int
    } deriving (Eq, Ord, Show, Generic, FromJSON)

btcC :: ClientM Price
ltcC :: ClientM Price
ethC :: ClientM Price
btcC :<|> ltcC :<|> ethC = client (Proxy :: Proxy GdaxApi)

data Prices =
    Prices {
        btcusd :: Maybe Double
      , ltcusd :: Maybe Double
      , ethusd :: Maybe Double
    } deriving (Eq, Ord, Show)

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right a) = Just a

getPrices :: IO Prices
getPrices = do
    mgr     <- newManager tlsManagerSettings
    results <- mapM (\x -> runClientM x (ClientEnv mgr (BaseUrl Https "api.cryptowat.ch" 443 ""))) [btcC, ltcC, ethC]
    return $
        (\x -> Prices (head x) (x !! 1) (x !! 2)) $
            map (eitherToMaybe . fmap (price . result)) results
