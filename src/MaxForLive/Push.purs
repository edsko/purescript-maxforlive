module MaxForLive.Push (
    Push(..)
  , new
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.List ((..))
import Effect (Effect)

import MaxForLive.LiveAPI (LiveAPI, ControlSurface)
import MaxForLive.LiveAPI as LiveAPI
import MaxForLive.Util (firstJustM)

data Push = Push {
      grabButtonMatrix    :: Effect Unit
    , releaseButtonMatrix :: Effect Unit
    }

new :: Effect (Maybe Push)
new = map mkPush <$> findPush

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

mkPush :: LiveAPI ControlSurface -> Push
mkPush push = Push {
      grabButtonMatrix: LiveAPI.grabControl push "Button_Matrix"
    , releaseButtonMatrix: LiveAPI.releaseControl push "Button_Matrix"
    }

findPush :: Effect (Maybe (LiveAPI ControlSurface))
findPush = do
    liveApp <- LiveAPI.new LiveAPI.liveApp
    numCS   <- LiveAPI.countControlSurfaces liveApp

    firstJustM (0 .. (numCS - 1)) $ \i -> do
      controlSurface <- LiveAPI.new (LiveAPI.controlSurface i)
      if LiveAPI.objectType controlSurface == "Push2"
        then pure (Just controlSurface)
        else pure Nothing
