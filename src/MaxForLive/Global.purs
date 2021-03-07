-- | Methods available in the global JavaScript context
-- |
-- | https://docs.cycling74.com/max8/vignettes/jsglobal
module MaxForLive.Global (
    -- | Universally Available Methods
    post
  , postLn
    -- | Properties of `jsthis`
  , setInlets
  , setOutlets
  , numJsArgs
  , jsArg
  , setInletAssist
  , setOutletAssist
  , setAutowatch
    -- | `jsthis` Methods
  , outlet
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)

import MaxForLive.Conversions (
    MaxValue
  , class FromMax
  , class ToMax
  , fromMax
  , toMax
  )

{-------------------------------------------------------------------------------
  Universally Available Methods
-------------------------------------------------------------------------------}

-- | Post to the Max console
foreign import post :: String -> Effect Unit

-- | Like 'post', but add a linebreak at the end
postLn :: String -> Effect Unit
postLn str = post (str <> "\n")

{-------------------------------------------------------------------------------
  Properties of `jsthis`
-------------------------------------------------------------------------------}

-- | Set number of patcher inlets
-- |
-- | https://docs.cycling74.com/max8/vignettes/jsglobal#inlets
foreign import setInlets :: Int -> Effect Unit

-- | Set number of patcher outlets
-- |
-- | https://docs.cycling74.com/max8/vignettes/jsglobal#outlets
foreign import setOutlets :: Int -> Effect Unit

-- | Get number of arguments to the `js` object
-- |
-- | https://docs.cycling74.com/max8/vignettes/jsglobal#jsarguments
foreign import numJsArgs :: Int

-- | Get the specified argument
-- |
-- | Low-level function. See 'jsArg' instead.
foreign import jsArgImpl :: Int -> MaxValue

-- | Get the @i@th argument
-- |
-- | This provides access to `jsarguments`. See
-- |
-- | * https://docs.cycling74.com/max8/vignettes/jsglobal#jsarguments
-- | * https://docs.cycling74.com/max8/tutorials/javascriptchapter03
-- |
-- | Throws an exception if the argument is of the wrong type.
jsArg :: forall a. FromMax a => Int -> a
jsArg = fromMax <<< jsArgImpl

-- | Set help text for the specified inlet
-- |
-- | https://docs.cycling74.com/max8/vignettes/jsglobal#setinletassist
foreign import setInletAssist :: Int -> String -> Effect Unit

-- | Set help text for the specified outlet
-- |
-- | https://docs.cycling74.com/max8/vignettes/jsglobal#setoutletassist
foreign import setOutletAssist :: Int -> String -> Effect Unit

-- | Set autowatch
-- |
-- | https://docs.cycling74.com/max8/vignettes/jsglobal#autowatch
foreign import setAutowatch :: Int -> Effect Unit

{-------------------------------------------------------------------------------
  `jsthis` Methods
-------------------------------------------------------------------------------}

foreign import outletImpl :: EffectFn2 Int MaxValue Unit

-- | Send value on the specified outlet
outlet :: forall a. ToMax a => Int -> a -> Effect Unit
outlet i x = runEffectFn2 outletImpl i (toMax x)
