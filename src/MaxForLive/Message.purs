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
      name    :: String
    , payload :: a
    }

foreign import mkMaxMessage :: Fn2 String MaxValue MaxValue

instance toMaxMessage :: ToMax a => ToMax (Message a) where
  toMax (Message { name, payload }) = runFn2 mkMaxMessage name (toMax payload)

{-------------------------------------------------------------------------------
  Bang
-------------------------------------------------------------------------------}

data Bang = Bang

foreign import bang :: MaxValue

instance toMaxBang :: ToMax Bang where
  toMax Bang = bang
