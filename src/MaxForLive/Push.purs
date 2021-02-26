module MaxForLive.Push (
    Push(..)
  , Button
  , Color
  , new
  ) where

import Prelude
import Data.FoldableWithIndex (forWithIndex_)
import Data.List ((..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref as Ref

import MaxForLive.LiveAPI (LiveAPI, ControlSurface, Element, ButtonMatrix)
import MaxForLive.LiveAPI as LiveAPI
import MaxForLive.Util (firstJustM)

type Button = { col :: Int, row :: Int }
type Color  = Int

data Push = Push {
      grabButtonMatrix     :: Effect Unit
    , releaseButtonMatrix  :: Effect Unit
    , setButtonMatrixColor :: Button -> Color -> Effect Unit
    }

new :: Effect (Maybe Push)
new = do
    mPush <- findPush
    case mPush of
      Nothing ->
        pure Nothing
      Just push ->
        Just <$> mkPush push

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

mkPush :: LiveAPI ControlSurface -> Effect Push
mkPush push = do
    buttonMatrix <- LiveAPI.getControl push LiveAPI.buttonMatrix
    colors <- Ref.new Map.empty
    pure $ Push {
        grabButtonMatrix: do
          LiveAPI.grabControl push LiveAPI.buttonMatrix
          refreshColors buttonMatrix =<< Ref.read colors
      , releaseButtonMatrix:
          LiveAPI.releaseControl push LiveAPI.buttonMatrix
      , setButtonMatrixColor: \button color -> do
          Ref.modify_ (Map.insert button color) colors
          LiveAPI.setButtonMatrixColor buttonMatrix button color
      }

refreshColors ::
      LiveAPI (Element ButtonMatrix)
   -> Map Button Color
   -> Effect Unit
refreshColors buttonMatrix colors =
    forWithIndex_ colors $ LiveAPI.setButtonMatrixColor buttonMatrix

findPush :: Effect (Maybe (LiveAPI ControlSurface))
findPush = do
    liveApp <- LiveAPI.withPath LiveAPI.liveApp
    numCS   <- LiveAPI.countControlSurfaces liveApp

    firstJustM (0 .. (numCS - 1)) $ \i -> do
      controlSurface <- LiveAPI.withPath (LiveAPI.controlSurface i)
      if LiveAPI.objectType controlSurface == "Push2"
        then pure (Just controlSurface)
        else pure Nothing
