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
    -- | Parents
  , class HasParent
    -- | Paths
  , kind Root
  , Absolute
  , Relative
  , Path -- opaque
  , canonicalParent
  , liveApp
  , liveSet
  , selectedTrack
  , thisDevice
  , thisTrack
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

import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)

import MaxForLive.Conversions (MaxValue, class ToMax)

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
  Parents
-------------------------------------------------------------------------------}

-- | Parents
-- |
-- | This essentially encodes a type family, mapping children to their parents.
class HasParent (c :: LOM) (p :: LOM) | c -> p

instance hasParentDevice :: HasParent Device Track

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
foreign import data Id :: LOM -> Type

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
-- | See `compareId`
foreign import sameIdImpl :: forall a b. Fn2 (Id a) (Id b) Boolean

-- | Simplified form of `compareId`
-- |
-- | See also `compareId`
sameId :: forall a b. Id a -> Id b -> Boolean
sameId = runFn2 sameIdImpl

-- | Show ID
-- |
-- | Intended for use in the `Show` instance.
foreign import idToString :: forall a. Id a -> String

-- | ID to Max value (to send to an outlet)
-- |
-- | Intended for use in the `ToMax` instance.
-- |
-- | NOTE: We cannot simply call `unsafeCoerce`, because Max is not very
-- | consistent in how it represents IDs: sometimes it represents them as
-- | an array `["id", id]`, and sometimes simply as the raw ID `id`. When
-- | we send an ID to an outlet, however, they must have the former shape.
foreign import idToMax :: forall a. Id a -> MaxValue

instance showId :: Show (Id a) where
  show = idToString

instance toMaxId :: ToMax (Id a) where
  toMax = idToMax

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
