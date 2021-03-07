module MaxForLive.Push (
    new
  , module MaxForLive.Push.API
  ) where

import Prelude
import Data.FoldableWithIndex (forWithIndex_)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import MaxForLive.LiveAPI (
    LiveAPI
  , ButtonMatrix
  , Element
  , Id
  )
import MaxForLive.LiveAPI as LiveAPI
import MaxForLive.Push.State (PushState)
import MaxForLive.Push.State as State
import MaxForLive.Push.API (Push, Button, Color)

new :: Effect Push
new = do
    ref <- Ref.new State.init
    pure {
        grabButtonMatrix:     grabButtonMatrix     ref
      , releaseButtonMatrix:  releaseButtonMatrix  ref
      , setButtonMatrixColor: setButtonMatrixColor ref
      , withButtonMatrixId:   withButtonMatrixId   ref
      }

{-------------------------------------------------------------------------------
  Implementation of the API
-------------------------------------------------------------------------------}

grabButtonMatrix :: Ref PushState -> Effect Unit
grabButtonMatrix ref = do
    State.withButtonMatrix ref $ \push matrix -> do
      LiveAPI.grabControl push LiveAPI.buttonMatrix
      refreshColors matrix =<< (_.colors) <$> Ref.read ref

releaseButtonMatrix :: Ref PushState -> Effect Unit
releaseButtonMatrix ref = do
    -- Don't use `withPush`: we don't want to _acquire_ the Push just to
    -- release it.
    mPush <- (_.push) <$> Ref.read ref
    case mPush of
      Nothing -> do
        -- If the ID of the Push is not known, we can't possibly have
        -- control over it. Ignore this call.
        pure unit
      Just push -> do
        LiveAPI.releaseControl push LiveAPI.buttonMatrix

        -- When we release, also forget the ID of the Push. This way, if the
        -- release was in response to the device being re-initialized, we'll
        -- correctly establish new IDs when we grab the Push again.
        Ref.modify_ (_ { push = Nothing, buttonMatrix = Nothing }) ref

-- | Set color of a button in the button matrix
-- |
-- | If the Push has not yet been initialized, we remember the color but do
-- | nothing else.
setButtonMatrixColor :: Ref PushState -> Button -> Color -> Effect Unit
setButtonMatrixColor ref b c = do
    Ref.modify_ (State.setColor b c) ref

    -- Only update the button matrix if its already initialized
    mMatrix <- (_.buttonMatrix) <$> Ref.read ref
    case mMatrix of
      Nothing     -> pure unit
      Just matrix -> LiveAPI.setButtonMatrixColor matrix b c

withButtonMatrixId ::
     Ref PushState
  -> (Id (Element ButtonMatrix) -> Effect Unit)
  -> Effect Unit
withButtonMatrixId ref k = State.withButtonMatrix ref $ \_push matrix ->
    k (LiveAPI.id matrix)

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

refreshColors ::
      LiveAPI (Element ButtonMatrix)
   -> Map Button Color
   -> Effect Unit
refreshColors buttonMatrix colors = do
    forWithIndex_ colors $ LiveAPI.setButtonMatrixColor buttonMatrix
