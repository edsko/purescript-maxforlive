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
      grabButtonMatrix     :: Effect Unit
    , releaseButtonMatrix  :: Effect Unit
    , setButtonMatrixColor :: Button -> Color -> Effect Unit
    , withButtonMatrixId   :: (Id (Element ButtonMatrix) -> Effect Unit)
                           -> Effect Unit
    }
