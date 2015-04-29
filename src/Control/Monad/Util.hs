module Control.Monad.Util where

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p x y = do
    z <- p
    if z then x else y
{-# INLINE ifM #-}

whenM :: Monad m => m Bool -> m () -> m ()
whenM p x = ifM p x (return ())
{-# INLINE whenM #-}
