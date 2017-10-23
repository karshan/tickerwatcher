{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE DataKinds                     #-}
{-# LANGUAGE TypeOperators                 #-}
{-# LANGUAGE TypeFamilies                  #-}
{-# LANGUAGE DeriveGeneric                 #-}
{-# LANGUAGE DeriveAnyClass                #-}
{-# LANGUAGE OverloadedStrings             #-}
{-# LANGUAGE NoImplicitPrelude             #-}
module PriceAPI.CryptoWatch
    ( getPrices
    , Prices
    , Ticker (..)
    ) where

import Data.Aeson
import qualified Data.Map as Map
import Data.Proxy (Proxy(..))

import GHC.Generics (Generic)

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Servant.API
import Servant.Client

import Protolude

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

btcClient :: ClientM Price
ltcClient :: ClientM Price
ethClient :: ClientM Price
btcClient :<|> ltcClient :<|> ethClient = client (Proxy :: Proxy GdaxApi)

type Prices = Map Ticker Double

data Ticker = BTC | LTC | ETH deriving (Eq, Ord, Show, Read)

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right a) = Just a

getPrices :: MonadIO m => m Prices
getPrices = liftIO $ do
    mgr <- newManager tlsManagerSettings
    foldM
        (\acc (ticker, client_) -> do
            r <- runClientM client_ (ClientEnv mgr (BaseUrl Https "api.cryptowat.ch" 443 ""))
            return $ maybe acc
                (\p -> Map.insert ticker p acc)
                (eitherToMaybe . fmap (price . result) $ r))
        Map.empty
        [ (BTC, btcClient), (LTC, ltcClient), (ETH, ethClient) ]
