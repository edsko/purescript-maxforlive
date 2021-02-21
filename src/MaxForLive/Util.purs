-- | Miscellaneous utility functions
module MaxForLive.Util (
    -- | Monadic utilities
    firstJustM
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.List (List(..), (:))

firstJustM ::
     forall m a b. Monad m
  => List a -> (a -> m (Maybe b)) -> m (Maybe b)
firstJustM as f = go as
  where
    go :: List a -> m (Maybe b)
    go Nil     = pure Nothing
    go (a:as') = do
       mb <- f a
       case mb of
         Nothing -> go as'
         Just b  -> pure (Just b)
