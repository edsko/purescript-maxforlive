module MaxForLive.Conversions (
    MaxValue
  , class FromMax
  , fromMax
  , class ToMax
  , toMax
  ) where

import Prelude
import Unsafe.Coerce (unsafeCoerce)

{-------------------------------------------------------------------------------
  Public API
-------------------------------------------------------------------------------}

-- | Uninterpreted value in the Max world
foreign import data MaxValue :: Type

-- | Translate from the Max world to the PureScript world
class FromMax a where
  fromMax :: MaxValue -> a

-- | Translate from the PureScript world to the Max world
class ToMax a where
  toMax :: a -> MaxValue

{-------------------------------------------------------------------------------
  `FromMax` instances
-------------------------------------------------------------------------------}

foreign import fromMaxIntImpl    :: MaxValue -> Int
foreign import fromMaxNumberImpl :: MaxValue -> Number
foreign import fromMaxStringImpl :: MaxValue -> String

instance fromMaxInt :: FromMax Int where
  fromMax = fromMaxIntImpl

instance fromMaxNumber :: FromMax Number where
  fromMax = fromMaxNumberImpl

instance fromMaxString :: FromMax String where
  fromMax = fromMaxStringImpl

{-------------------------------------------------------------------------------
  `ToMax` instances
-------------------------------------------------------------------------------}

instance toMaxMaxValue :: ToMax MaxValue where
  toMax = identity

instance toMaxInt :: ToMax Int where
  toMax = unsafeCoerce

instance toMaxString :: ToMax String where
  toMax = unsafeCoerce

instance toMaxArray :: ToMax a => ToMax (Array a) where
  toMax = unsafeCoerce <<< map toMax
