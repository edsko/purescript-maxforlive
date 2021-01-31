-- | Set up Max message handlers
-- |
-- | https://docs.cycling74.com/max8/vignettes/jsbasic
module MaxForLive.Handlers (
     -- | Max message handlers
     setHandler
     -- | FFI boundary
   , class InvokeHandler
   , invokeHandler
   ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (
    EffectFn1
  , EffectFn3
  , mkEffectFn1
  , runEffectFn3
  )

import MaxForLive.Arguments (Arguments, getArg)
import MaxForLive.Conversions (class FromMax)

foreign import setHandlerImpl ::
     EffectFn3 Int String (EffectFn1 Arguments Unit) Unit

{-------------------------------------------------------------------------------
  Max message handlers
-------------------------------------------------------------------------------}

-- | Register Max message handlers
-- |
-- | For example, to respond to a bang:
-- |
-- | ```purescript
-- | setHandler { inlet: 0, msg: "bang", handler: postLn "BANG!" }
-- | ```
-- |
-- | Use `"msg_int"` and co as `message` to handle values as messages, see
-- | https://docs.cycling74.com/max8/vignettes/jsbasic#Special_Function_Names
setHandler ::
     forall a.
     InvokeHandler a
  => { inlet   :: Int
     , msg     :: String
     , handler :: a
     }
  -> Effect Unit
setHandler {inlet, msg, handler} =
    runEffectFn3 setHandlerImpl inlet msg $
      mkEffectFn1 (invokeHandler 0 handler)

{-------------------------------------------------------------------------------
  FFI: Functions of arbitrary arguments
-------------------------------------------------------------------------------}

class InvokeHandler a where
  invokeHandler :: Int -> a -> Arguments -> Effect Unit

instance invokeNoArgs :: InvokeHandler (Effect Unit) where
  invokeHandler _i = const

instance invokeWithArg ::
       (FromMax a, InvokeHandler b)
    => InvokeHandler (a -> b) where
  invokeHandler i f xs = invokeHandler (i + 1) (f (getArg xs i)) xs
