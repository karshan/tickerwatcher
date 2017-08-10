module Trigger
    ( TriggerCond(..)
    , evalCond
    ) where

data TriggerCond =
    TriggerCond {
        lt  :: Bool
      , val :: Double
    } deriving (Eq, Ord, Show, Read)

evalCond :: TriggerCond -> Double -> Bool
evalCond tc price = (lt tc && price < val tc) || (not (lt tc) && price > val tc)
