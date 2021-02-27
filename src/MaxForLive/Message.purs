module MaxForLive.Message (
    Message(..)
  , Bang(..)
  ) where

import Data.Function.Uncurried (Fn2, runFn2)

import MaxForLive.Conversions (MaxValue, class ToMax, toMax)

{-------------------------------------------------------------------------------
  Message
-------------------------------------------------------------------------------}

data Message a = Message {
      messageName    :: String
    , messagePayload :: a
    }

foreign import mkMaxMessage :: Fn2 String MaxValue MaxValue

instance toMaxMessage :: ToMax a => ToMax (Message a) where
  toMax (Message { messageName, messagePayload }) =
      runFn2 mkMaxMessage messageName (toMax messagePayload)

{-------------------------------------------------------------------------------
  Bang
-------------------------------------------------------------------------------}

data Bang = Bang

foreign import bang :: MaxValue

instance toMaxBang :: ToMax Bang where
  toMax Bang = bang
