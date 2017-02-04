module Util where

import Text.ParserCombinators.ReadP

maybeP :: ReadP a -> ReadP (Maybe a)
maybeP p = option Nothing (fmap return p)

maybeM :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
maybeM f = maybe (return Nothing) (fmap Just . f)
