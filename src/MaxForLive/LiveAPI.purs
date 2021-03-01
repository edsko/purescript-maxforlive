module MaxForLive.LiveAPI (
    -- | Object types
    kind LOM
  , Application
  , Clip
  , Device
  , Song
  , Track
  , ControlSurface
  , Element
    -- | Views
  , View
  , class HasView
  , view
    -- | Paths
  , kind Root
  , Absolute
  , Relative
  , Path -- opaque
  , liveApp
  , liveSet
  , selectedTrack
  , thisDevice
  , controlSurface
    -- | LiveAPI object proper
  , LiveAPI
  , withPath
  , withId
  , Id
  , id
  , objectType
  , sameId
  , unquotedPath
  , getCount
    -- | Parents
  , deviceTrack
    -- | Type specializations
  , countControlSurfaces
    -- | Controls
  , kind Control
  , ButtonMatrix
  , ControlName -- opaque
  , buttonMatrix
  , grabControl
  , releaseControl
  , getControlId
  , getControl
    -- | Specific controls
  , setButtonMatrixColor
  ) where

import Prelude
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Unsafe.Coerce (unsafeCoerce)

import MaxForLive.Conversions (MaxValue, class ToMax, class FromMax, toMax)
import MaxForLive.Message (Message(..))

{-------------------------------------------------------------------------------
  Types of objects
-------------------------------------------------------------------------------}

foreign import kind LOM

-- | Application
-- |
-- | https://docs.cycling74.com/max8/vignettes/live_object_model#Application
foreign import data Application :: LOM

-- | Device
-- |
-- | https://docs.cycling74.com/max8/vignettes/live_object_model#Device
foreign import data Device :: LOM

-- | Track
-- |
-- | https://docs.cycling74.com/max8/vignettes/live_object_model#Track
foreign import data Track :: LOM

-- | Song
-- |
-- | https://docs.cycling74.com/max8/vignettes/live_object_model#Song
foreign import data Song :: LOM

-- | Clip
-- |
-- | https://docs.cycling74.com/max8/vignettes/live_object_model#Clip
foreign import data Clip :: LOM

-- | Control surface
-- |
-- | https://docs.cycling74.com/max8/vignettes/live_object_model#ControlSurface
foreign import data ControlSurface :: LOM

-- | Control surface elements
foreign import data Element :: Control -> LOM

-- | View on an object
foreign import data View :: LOM -> LOM

{-------------------------------------------------------------------------------
  Views
-------------------------------------------------------------------------------}

class HasView (o :: LOM)

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
newtype Path (root :: Root) (a :: LOM) = Path String

instance showPath :: Show (Path r a) where
  show (Path p) = p

instance toMaxPath :: ToMax (Path r a) where
  toMax = unsafeCoerce

-- | Root path: Relative path to the Ableton application
-- |
-- | https://docs.cycling74.com/max8/vignettes/live_object_model#Application
liveApp :: Path Relative Application
liveApp = Path "live_app"

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

-- | Selected track
-- |
-- | https://docs.cycling74.com/max8/vignettes/live_object_model#selected_track
selectedTrack :: forall r. Path r (View Song) -> Path Relative Track
selectedTrack (Path p) = Path (p <> " selected_track")

-- | Specific control surface
-- |
-- | See 'countControlSurfaces'
-- |
-- | TODO: Confirm that this is an absolute path.
controlSurface :: Int -> Path Absolute ControlSurface
controlSurface n = Path ("control_surfaces " <> show n)

{-------------------------------------------------------------------------------
  LiveAPI proper
-------------------------------------------------------------------------------}

-- | A `LiveAPI` object
-- |
-- | The phantom type argument indicates the kind of object.
-- | See https://docs.cycling74.com/max8/vignettes/live_object_model
foreign import data LiveAPI :: LOM -> Type

-- | Construct `LiveAPI` object from a `Path`
foreign import withPath :: forall r a. Path r a -> Effect (LiveAPI a)

-- | Construct `LiveAPI` object from previously discovered `Id`
foreign import withId :: forall a. Id a -> Effect (LiveAPI a)

-- | Object ID
-- |
-- | https://docs.cycling74.com/max8/vignettes/jsliveapi#id
-- |
-- | Max for Live sometimes represents IDs simply as numbers (for example, that
-- | is what `id` returns on a `LiveAPI` object, and sometimes as a list
-- | `["id", id]` (for example, this is the format Max messages should be in).
-- | We represent it /always/ as a single number, doing conversions back and
-- | forth as required.
newtype Id (a :: LOM) = Id Int

-- | Object ID
-- |
-- | See `Id`.
foreign import id :: forall a. LiveAPI a -> Id a

-- | Object type
-- |
-- | https://docs.cycling74.com/max8/vignettes/jsliveapi#type
foreign import objectType :: forall a. LiveAPI a -> String

-- | Compare two IDs
-- |
-- | TODO: We may wish to define a more strongly typed version of this,
-- | returning evidence that `a` and `b` must be equal if the IDs match.
sameId :: forall a b. Id a -> Id b -> Boolean
sameId (Id i) (Id i') = i == i'

-- | More restricted form of 'sameId' that insists the object types must match
instance eqId :: Eq (Id a) where
  eq = sameId

instance showId :: Show (Id a) where
  show (Id i) = show i

-- | ID to Max value (to send to an outlet)
-- |
-- | Intended for use in the `ToMax` instance.
-- |
-- | This will turn the ID into a list `["id", id]`, which is the format
-- | expected in Max messages.
instance toMaxId :: ToMax (Id a) where
  toMax (Id i) = toMax $ Message { messageName: "id", messagePayload: i }

-- | Low-level function for use in the `FromMax` instance
foreign import idFromMax :: forall a. MaxValue -> Id a

-- | Max value to ID
-- |
-- | NOTE: We have no way of verifying the type of the identifier, so this is
-- | polymorphic in `a`. Use responsibly.
instance fromMaxId :: FromMax (Id a) where
  fromMax = idFromMax

-- | The path to the Live object referred to by the LiveAPI object
-- |
-- | These paths will be _stable_ ("absolute"), and may therefore be
-- | different from the path that was used to construct the object.
-- |
-- | https://docs.cycling74.com/max8/vignettes/jsliveapi#unquotedpath
foreign import unquotedPath :: forall a. LiveAPI a -> Path Absolute a

-- | Number of children at the given path
-- |
-- | Low level function.
-- |
-- | https://docs.cycling74.com/max8/vignettes/jsliveapi#getcount
foreign import getCount :: forall a. EffectFn2 String (LiveAPI a) Int

{-------------------------------------------------------------------------------
  Parents

  We can use `canonical_parent` to get the canonical parent of any object
  (https://docs.cycling74.com/max8/vignettes/live_api_overview#Canonical_Parent).
  Despite the name, however, this isn't necessary "canonical"; for instance,
  the parent of a device might be a track, but could also be a chain (if the
  device is grouped). We therefore provide specialized functions here that
  take this into account.
-------------------------------------------------------------------------------}

-- | Track this device sits on
foreign import deviceTrack :: forall r. Path r Device -> Effect (LiveAPI Track)

{-------------------------------------------------------------------------------
  Type specializations
-------------------------------------------------------------------------------}

countControlSurfaces :: LiveAPI Application -> Effect Int
countControlSurfaces = runEffectFn2 getCount "control_surfaces"

{-------------------------------------------------------------------------------
  Controls

  The various functions we can call on `ControlSurface` don't have HTML
  anchors associated with them; instead, refer to

  https://docs.cycling74.com/max8/vignettes/live_object_model#ControlSurface

  The control elements don't seem to be documented at all; some are mentioned
  at the beginning of

  https://docs.cycling74.com/max8/vignettes/live_object_model
-------------------------------------------------------------------------------}

foreign import kind Control
foreign import data ButtonMatrix :: Control

-- | Control names
--
-- We use the phantom parameter in the same way that we do for 'Path', but
-- controls (aka control elements) are not accessed through paths.
newtype ControlName (c :: Control) = ControlName String

buttonMatrix :: ControlName ButtonMatrix
buttonMatrix = ControlName "Button_Matrix"

-- | Grab control
foreign import grabControl :: forall c.
     LiveAPI ControlSurface
  -> ControlName c
  -> Effect Unit

-- | Release control
foreign import releaseControl :: forall c.
     LiveAPI ControlSurface
  -> ControlName c
  -> Effect Unit

-- | Get ID of the given control
foreign import getControlId :: forall e.
     LiveAPI ControlSurface
  -> ControlName e
  -> Effect (Id (Element e))

-- | Get control element
getControl :: forall e.
     LiveAPI ControlSurface
  -> ControlName e
  -> Effect (LiveAPI (Element e))
getControl cs control = withId =<< getControlId cs control

{-------------------------------------------------------------------------------
  Specific controls
-------------------------------------------------------------------------------}

foreign import setButtonMatrixColor ::
     LiveAPI (Element ButtonMatrix)
  -> { col :: Int, row :: Int }
  -> Int
  -> Effect Unit
