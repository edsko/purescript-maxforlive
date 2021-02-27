module MaxForLive.Message (
    Message(..)
  ) where

import Data.Function.Uncurried (Fn2, runFn2)

import MaxForLive.Conversions (MaxValue, class ToMax, toMax)

data Message a = Message {
      messageName    :: String
    , messagePayload :: a
    }

foreign import mkMaxMessage :: Fn2 String MaxValue MaxValue

instance toMaxMessage :: ToMax a => ToMax (Message a) where
  toMax (Message { messageName, messagePayload }) =
      runFn2 mkMaxMessage messageName (toMax messagePayload)
