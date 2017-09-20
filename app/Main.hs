{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE FlexibleContexts           #-}
module Main where

import Control.Concurrent
import Control.Lens (zoom, _1)
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

main :: IO ()
main = void $ flip runStateT (Nothing, "") $ forever $ do
    time <- lift getCurrentTime
    prices <- getPrices
    putStrLn $ (show time) ++ ": " ++ show prices
    curConfigStr <- lift (readFile "config")
    (_, lastConfigStr) <- get
    when (curConfigStr /= lastConfigStr)
        (do
            putStrLn "Loading new config"
            put (readMaybe . toS $ curConfigStr, curConfigStr))
    zoom _1 (zoomMaybe $ (evalConfig prices >>= mapM_ sm))
    minuteDelay 1

evalConfig :: MonadState Config m => Prices -> m [Notification]
evalConfig prices = do
    config :: Config <- get
    fmap concat $ mapM
        (\(ticker, triggers) ->
            maybe
                (return $ [(show ticker) <> " = Nothing"])
                (\price -> do
                    let (newTriggers, notifications) = foldl
                            (\(accTriggers, accNotifications) trigger ->
                                if evalCond trigger price then
                                    let notification = (show ticker) <> " = " <> show price <> (if lt trigger then " < " else " > ") <> show (val trigger)
                                        invertedTrigger = trigger { lt = not (lt trigger) }
                                    in (invertedTrigger:accTriggers, notification:accNotifications)
                                else
                                    (trigger:accTriggers, accNotifications))
                            ([], [])
                            triggers
                    modify (Map.insert ticker newTriggers)
                    return notifications)
                (Map.lookup ticker prices))
        (Map.toList config)
