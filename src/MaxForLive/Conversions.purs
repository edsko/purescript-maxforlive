module MaxForLive.Conversions (
    MaxValue
    -- * Conversion from Max to PureScript
  , class FromMax
  , fromMax
    -- * Conversion from pureScript to Max
  , class ToMax
  , toMax
    -- * Enumerations
  , class SimpleEnum
  , toSimpleEnum
  , fromSimpleEnum
  , genericToSimpleEnum
  , genericFromSimpleEnum
  , maxFromEnum
  , maxToEnum
  ) where

import Prelude
import Data.Enum.Generic (
    class GenericBoundedEnum
  , genericToEnum
  , genericFromEnum
  )
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (Maybe(..))
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

instance fromMaxBoolean :: FromMax Boolean where
  fromMax = conv <<< fromMax
    where
      conv :: Int -> Boolean
      conv 0 = false
      conv _ = true

{-------------------------------------------------------------------------------
  `ToMax` instances
-------------------------------------------------------------------------------}

instance toMaxMaxValue :: ToMax MaxValue where
  toMax = identity

instance toMaxInt :: ToMax Int where
  toMax = unsafeCoerce

instance toMaxBoolean :: ToMax Boolean where
  toMax = toMax <<< conv
    where
      conv :: Boolean -> Int
      conv false = 0
      conv true  = 1

instance toMaxString :: ToMax String where
  toMax = unsafeCoerce

instance toMaxArray :: ToMax a => ToMax (Array a) where
  toMax = unsafeCoerce <<< map toMax

instance toMaxList :: ToMax a => ToMax (List a) where
  toMax = toMax <<< Array.fromFoldable

{-------------------------------------------------------------------------------
  Simple enumerations

  It is often useful to have simple enumerations available for conversion to
  and from Max.
-------------------------------------------------------------------------------}

class SimpleEnum a where
  fromSimpleEnum :: a -> Int
  toSimpleEnum   :: Int -> a

genericFromSimpleEnum :: forall a rep.
     Generic a rep
  => GenericBoundedEnum rep
  => a -> Int
genericFromSimpleEnum = genericFromEnum

genericToSimpleEnum :: forall a rep.
     Generic a rep
  => GenericBoundedEnum rep
  => Int -> a
genericToSimpleEnum = maxFromJust <<< genericToEnum

maxToEnum :: forall a. SimpleEnum a => MaxValue -> a
maxToEnum = toSimpleEnum <<< fromMax

maxFromEnum :: forall a. SimpleEnum a => a -> MaxValue
maxFromEnum = toMax <<< fromSimpleEnum

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

foreign import maxError :: forall a. String -> a

-- | Turn partiality into a Max error
maxFromJust :: forall a. Maybe a -> a
maxFromJust (Just x) = x
maxFromJust Nothing  = maxError "maxFromJust"
