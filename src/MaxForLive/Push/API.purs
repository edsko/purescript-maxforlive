-- | User-facing API
module MaxForLive.Push.API (
    Push
  , Button
  , Color
  ) where

import Prelude
import Effect (Effect)

import MaxForLive.LiveAPI (Id, Element, ButtonMatrix)

type Button = { col :: Int, row :: Int }
type Color  = Int

type Push = {
      -- | Grab control over the button matrix
      --
      -- When Max has control over the button matrix, we can set colors and
      -- listen to button presses.
      grabButtonMatrix :: Effect Unit

      -- | Release control over the button matrix
    , releaseButtonMatrix  :: Effect Unit

      -- | Set color of one of the buttons
      --
      -- If the Push is not yet initialized, remember the color but don't
      -- actually change anything on the Push.
    , setButtonMatrixColor :: Button -> Color -> Effect Unit

      -- | Get the ID of the button matrix (if Push found)
    , withButtonMatrixId ::
           (Id (Element ButtonMatrix) -> Effect Unit)
        -> Effect Unit
    }
