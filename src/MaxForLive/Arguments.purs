module MaxForLive.Arguments (
    Arguments
  , getArg
  , getRemainingArgs
  ) where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)

import MaxForLive.Conversions (MaxValue, class FromMax, fromMax)

-- | The Max 'Arguments' object
-- |
-- | This is entirely opaque to the PureScript code: we only pass it between
-- | the foreign functions.
foreign import data Arguments :: Type

-- | Get the `i`th argument
-- |
-- | Throws an exception if out of range.
foreign import getArgImpl :: Fn2 Arguments Int MaxValue

-- | Get the `i`th argument
-- |
-- | Throws an exception if out of range.
getArg :: forall a. FromMax a => Arguments -> Int -> a
getArg xs = fromMax <<< runFn2 getArgImpl xs

-- | Get the remaining arguments, starting at `i`.
foreign import getRemainingArgsImpl :: Fn2 Arguments Int (Array MaxValue)

-- | Get the remaining arguments, starting at `i`.
getRemainingArgs :: forall a. FromMax a => Arguments -> Int -> (Array a)
getRemainingArgs xs = map fromMax <<< runFn2 getRemainingArgsImpl xs
