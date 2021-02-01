module MaxForLive.LiveAPI (
    -- | Object types
    Application
  , Clip
  , Device
  , Song
  , Track
    -- | Views
  , View
  , class HasView
  , view
    -- | Parents
  , class HasParent
    -- | Paths
  , kind Root
  , Absolute
  , Relative
  , Path -- opaque
  , canonicalParent
  , liveSet
  , selectedTrack
  , thisDevice
  , thisTrack
    -- | LiveAPI object proper
  , LiveAPI
  , new
  , Id
  , id
  , compareId
  , sameId
  , unquotedPath
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Leibniz (Leibniz(..), type (~))
import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)

{-------------------------------------------------------------------------------
  Types of objects
-------------------------------------------------------------------------------}

-- | Application
-- |
-- | https://docs.cycling74.com/max8/vignettes/live_object_model#Application
foreign import data Application :: Type

-- | Device
-- |
-- | https://docs.cycling74.com/max8/vignettes/live_object_model#Device
foreign import data Device :: Type

-- | Track
-- |
-- | https://docs.cycling74.com/max8/vignettes/live_object_model#Track
foreign import data Track :: Type

-- | Song
-- |
-- | https://docs.cycling74.com/max8/vignettes/live_object_model#Song
foreign import data Song :: Type

-- | Clip
-- |
-- | https://docs.cycling74.com/max8/vignettes/live_object_model#Clip
foreign import data Clip :: Type

-- | View on an object
foreign import data View :: Type -> Type

{-------------------------------------------------------------------------------
  Parents
-------------------------------------------------------------------------------}

-- | Parents
-- |
-- | This essentially encodes a type family, mapping children to their parents.
class HasParent c p | c -> p

instance hasParentDevice :: HasParent Device Track

{-------------------------------------------------------------------------------
  Views
-------------------------------------------------------------------------------}

class HasView o

view :: forall r o. HasView o => Path r o -> Path Relative (View o)
view (Path p) = Path (p <> " view")

-- | https://docs.cycling74.com/max8/vignettes/live_object_model#Application.View
instance hasViewApplication :: HasView Application

-- | https://docs.cycling74.com/max8/vignettes/live_object_model#Song.View
instance hasViewSong :: HasView Song

-- | https://docs.cycling74.com/max8/vignettes/live_object_model#Track.View
instance hasViewTrack :: HasView Track

-- | https://docs.cycling74.com/max8/vignettes/live_object_model#Device.View
instance hasViewDevice :: HasView Device

-- | https://docs.cycling74.com/max8/vignettes/live_object_model#Clip.View
instance hasViewClip :: HasView Clip

{-------------------------------------------------------------------------------
  Paths
-------------------------------------------------------------------------------}

-- | Different kinds of paths
foreign import kind Root

-- | Relative path
-- |
-- | E.g. `live_set view selected_track`
foreign import data Relative :: Root

-- | Absolute path
-- |
-- | E.g. `live_set tracks 3`
foreign import data Absolute :: Root

-- | Paths
-- |
-- | The phantom parameter indicates what kind of device this corresponds to.
-- | The constructor is therefore not exported, paths must be constructed
-- | with the combinators provided in this module.
newtype Path (root :: Root) (a :: Type) = Path String

-- | Root path: Relative path to the current device
-- |
-- | https://docs.cycling74.com/max8/vignettes/live_object_model#this_device
thisDevice :: Path Relative Device
thisDevice = Path "this_device"

-- | Current Live set
-- |
-- | https://docs.cycling74.com/max8/vignettes/live_object_model#Song
liveSet :: Path Relative Song
liveSet = Path "live_set"

-- | Our own track object
-- |
-- | https://docs.cycling74.com/max8/vignettes/live_api_overview#Canonical_Parent
thisTrack :: Path Relative Track
thisTrack = canonicalParent thisDevice

-- | Canonical parent
-- |
-- | https://docs.cycling74.com/max8/vignettes/live_api_overview#Canonical_Parent
canonicalParent :: forall r c p. HasParent c p => Path r c -> Path Relative p
canonicalParent (Path p) = Path (p <> " canonical_parent")

-- | Selected track
-- |
-- | https://docs.cycling74.com/max8/vignettes/live_object_model#selected_track
selectedTrack :: forall r. Path r (View Song) -> Path Relative Track
selectedTrack (Path p) = Path (p <> " selected_track")

{-------------------------------------------------------------------------------
  LiveAPI proper
-------------------------------------------------------------------------------}

-- | A `LiveAPI` object
-- |
-- | The phantom type argument indicates the kind of object.
-- | See https://docs.cycling74.com/max8/vignettes/live_object_model
foreign import data LiveAPI :: Type -> Type

-- | Construct `LiveAPI` object
foreign import new :: forall r a. Path r a -> Effect (LiveAPI a)

-- | Object ID
-- |
-- | https://docs.cycling74.com/max8/vignettes/jsliveapi#id
foreign import data Id :: Type -> Type

-- | Object ID
-- |
-- | See `Id`.
foreign import id :: forall a. LiveAPI a -> Id a

-- | Compare two IDs
-- |
-- | See `compareId`
foreign import sameIdImpl :: forall a b. Fn2 (Id a) (Id b) Boolean

-- | Compare two IDs
compareId :: forall a b. Id a -> Id b -> Maybe (a ~ b)
compareId a b =
    if runFn2 sameIdImpl a b
      then Just (Leibniz unsafeCoerce)
      else Nothing

-- | Simplified form of 'compareId'
-- |
-- | See also `compareId`
sameId :: forall a b. Id a -> Id b -> Boolean
sameId = runFn2 sameIdImpl

-- | The path to the Live object referred to by the LiveAPI object
-- |
-- | These paths will be _stable_ ("absolute"), and may therefore be
-- | different from the path that was used to construct the object.
-- |
-- | https://docs.cycling74.com/max8/vignettes/jsliveapi#unquotedpath
foreign import unquotedPath :: forall a. LiveAPI a -> Path Absolute a
