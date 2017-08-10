{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Monad

import Data.Monoid ((<>))
import Data.Time.Clock
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import System.Process

import PriceAPI.CryptoWatch
import Trigger

import Google.SendMail

minuteDelay :: Int -> IO ()
minuteDelay n = threadDelay (n * 60 * 1000000)

head' :: [a] -> Maybe a
head' []    = Nothing
head' (x:_) = Just x

readMaybe :: Read a => String -> Maybe a
readMaybe = fmap fst . head' . reads

data Config =
    Config {
        btcC :: TriggerCond
      , ltcC :: TriggerCond
      , ethC :: TriggerCond
    } deriving (Eq, Ord, Show, Read)

main :: IO ()
main = forever $ do
    print =<< getCurrentTime
    prices <- getPrices
    mConfig <- readMaybe <$> readFile "config"
    maybe (putStrLn "bad config")
        (evalConfig prices)
        mConfig
    minuteDelay 5

evalConfig :: Prices -> Config -> IO ()
evalConfig Prices{..} Config{..} = do
    void $ sequence $ zipWith3
        (\cond mPrice name ->
            maybe (return ())
                (\price ->
                    if evalCond cond price then do
                        let msg = name <> " = " <> T.pack (show price) <> (if lt cond then " < " else " > ") <> T.pack (show (val cond))
                        putStrLn $ T.unpack msg
                        void $ sendMail "svc-acc-key.json" "karshan@karshan.me" "karshan.sharma@gmail.com" msg (TL.fromStrict msg)
                    else
                        return ())
                mPrice)
        [btcC, ltcC, ethC]
        [btcusd, ltcusd, ethusd]
        ["btc", "ltc", "eth"]
