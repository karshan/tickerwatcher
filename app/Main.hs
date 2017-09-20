{-# OPTIONS_GHC -fno-warn-type-defaults #-}
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

type Config = Map Ticker TriggerCond

-- log and send mail
sm :: MonadIO m => Text -> m ()
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

evalConfig :: MonadState Config m => Prices -> m [Text]
evalConfig prices = do
    config <- get
    fmap catMaybes $ mapM 
        (\(ticker, trigger) ->
            maybe
                (return $ Just $ (show ticker) <> " = Nothing")
                (\price -> do
                    if evalCond trigger price then do
                        modify (Map.insert ticker (trigger { lt = not (lt trigger) }))
                        return $ Just $ (show ticker) <> " = " <> show price <> (if lt trigger then " < " else " > ") <> show (val trigger)
                    else
                        return Nothing)
                (Map.lookup ticker prices))
        (Map.toList config)
