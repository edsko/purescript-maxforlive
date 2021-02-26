module MaxForLive.Push (
    Push(..)
  , Button
  , Colour
  , new
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.List ((..))
import Effect (Effect)

import MaxForLive.LiveAPI (LiveAPI, ControlSurface)
import MaxForLive.LiveAPI as LiveAPI
import MaxForLive.Util (firstJustM)

type Button = { col :: Int, row :: Int }
type Colour = Int

data Push = Push {
      grabButtonMatrix     :: Effect Unit
    , releaseButtonMatrix  :: Effect Unit
    , setButtonMatrixColor :: Button -> Colour -> Effect Unit
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
    pure $ Push {
        grabButtonMatrix:
          LiveAPI.grabControl push LiveAPI.buttonMatrix
      , releaseButtonMatrix:
          LiveAPI.releaseControl push LiveAPI.buttonMatrix
      , setButtonMatrixColor: \button color ->
          LiveAPI.setButtonMatrixColor buttonMatrix {
              col: button.col
            , row: button.row
            , color: color
            }
      }

findPush :: Effect (Maybe (LiveAPI ControlSurface))
findPush = do
    liveApp <- LiveAPI.withPath LiveAPI.liveApp
    numCS   <- LiveAPI.countControlSurfaces liveApp

    firstJustM (0 .. (numCS - 1)) $ \i -> do
      controlSurface <- LiveAPI.withPath (LiveAPI.controlSurface i)
      if LiveAPI.objectType controlSurface == "Push2"
        then pure (Just controlSurface)
        else pure Nothing
