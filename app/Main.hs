{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE FlexibleContexts           #-}
module Main where

import Control.Concurrent
--import Control.Lens (zoom, _1)
import Control.Monad

import Data.Time.Clock
import qualified Data.Map as Map

import PriceAPI.CryptoWatch
import Trigger

import Google.SendMail

import Protolude

zoomMaybe :: (Monad m) => StateT s m a -> StateT (Maybe s) m (Maybe a)
zoomMaybe origAction = do
    mS <- get
    maybe
        (return Nothing)
        (\s -> do
            (out, newS) <- lift (runStateT origAction s)
            put (Just newS)
            return (Just out))
        mS

minuteDelay :: MonadIO m => Int -> m ()
minuteDelay n = liftIO $ threadDelay (n * 60 * 1000000)

type Config = Map Ticker [TriggerCond]
type Notification = Text

-- log and send mail
sm :: MonadIO m => Notification -> m ()
sm m = do
    putStrLn $ "Sending " <> m
    void $ liftIO $ sendMail "svc-acc-key.json" "karshan@karshan.me" "karshan.sharma@gmail.com" m (toS m)

pTriggers :: Double -> Double -> [TriggerCond]
pTriggers percent curPrice =
    [ TriggerCond True  ((1 - (percent/100)) * curPrice)
    , TriggerCond False ((1 + (percent/100)) * curPrice)
    ]

main :: IO ()
main = do
    initialPrices <- getPrices
    let mConfig :: Maybe Config = (do
            curBtc <- Map.lookup BTC initialPrices
            curLtc <- Map.lookup LTC initialPrices
            curEth <- Map.lookup ETH initialPrices
            return $ Map.fromList [
                      (BTC, pTriggers 5 curBtc)
                    , (LTC, pTriggers 5 curLtc)
                    , (ETH, pTriggers 5 curEth)
                    ])
    print mConfig
    maybe
        (putStrLn "getPrices failed initially, manually retry please")
        (\initConfig -> void $ flip runStateT initConfig $ forever $ do
            minuteDelay 1
            time <- lift getCurrentTime
            prices <- getPrices
            print =<< get
            putStrLn $ (show time) ++ ": " ++ show prices
            evalConfig prices >>= mapM_ sm)
        mConfig

evalConfig :: MonadState Config m => Prices -> m [Notification]
evalConfig prices = do
    config <- get
    fmap concat $ mapM
        (\(ticker, triggers) ->
            maybe
                (return $ [(show ticker) <> " = Nothing"])
                (\price -> do
                    let (didTrigger, notifications) = foldl
                            (\(accDidTrigger, accNotifications) trigger ->
                                if evalCond trigger price then
                                    let notification = (show ticker) <> " = " <> show price <>
                                            (if lt trigger then " < " else " > ") <> show (val trigger)
                                    in (True, notification:accNotifications)
                                else
                                    (accDidTrigger, accNotifications))
                            (False, [])
                            triggers
                    when didTrigger (modify $ Map.insert ticker (pTriggers 5 price))
                    return notifications)
                (Map.lookup ticker prices))
        (Map.toList config)
