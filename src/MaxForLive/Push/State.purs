-- | Push internal state
module MaxForLive.Push.State (
    PushState(..)
  , init
    -- * Effectful getters
  , withPush
  , withButtonMatrix
    -- * Simple updates
  , setColor
  ) where

import Prelude
import Data.List ((..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import MaxForLive.Global (postLn)
import MaxForLive.LiveAPI (
    LiveAPI
  , ButtonMatrix
  , ControlSurface
  , Element
  )
import MaxForLive.LiveAPI as LiveAPI
import MaxForLive.Push.API (Button, Color)
import MaxForLive.Util (firstJustM)

type PushState = {
      push         :: Maybe (LiveAPI ControlSurface)
    , buttonMatrix :: Maybe (LiveAPI (Element ButtonMatrix))
    , colors       :: Map Button Color
    }

init :: PushState
init = {
      push: Nothing
    , buttonMatrix: Nothing
    , colors: Map.empty
    }

withPush ::
     Ref PushState
  -> (LiveAPI ControlSurface -> Effect Unit)
  -> Effect Unit
withPush ref k = do
    mPush <- (_.push) <$> Ref.read ref
    case mPush of
      Just push -> k push
      Nothing   -> findPush $ \push -> do
        Ref.modify_ (_ { push = Just push }) ref
        k push

withButtonMatrix ::
     Ref PushState
  -> (LiveAPI ControlSurface -> LiveAPI (Element ButtonMatrix) -> Effect Unit)
  -> Effect Unit
withButtonMatrix ref k = withPush ref $ \push -> do
    mMatrix <- (_.buttonMatrix) <$> Ref.read ref
    case mMatrix of
      Just matrix -> k push matrix
      Nothing     -> do
        matrix <- LiveAPI.getControl push LiveAPI.buttonMatrix
        Ref.modify_ (_ { buttonMatrix = Just matrix }) ref
        k push matrix

{-------------------------------------------------------------------------------
  Simple updates
-------------------------------------------------------------------------------}

setColor :: Button -> Color -> PushState -> PushState
setColor b c st = st { colors = Map.insert b c st.colors }

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

findPush :: (LiveAPI ControlSurface -> Effect Unit) -> Effect Unit
findPush k = do
    liveApp <- LiveAPI.withPath LiveAPI.liveApp
    numCS   <- LiveAPI.countControlSurfaces liveApp

    mPush <- firstJustM (0 .. (numCS - 1)) $ \i -> do
      controlSurface <- LiveAPI.withPath (LiveAPI.controlSurface i)
      if LiveAPI.objectType controlSurface == "Push2"
        then pure (Just controlSurface)
        else pure Nothing

    case mPush of
      Nothing   -> postLn "No push found"
      Just push -> k push
