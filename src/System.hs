{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
module System ( System (..)
              , Indexed(..)
              ) where

import Storage

class Monad m => System f m s where
    runSystem :: s -> f -> m ()

data Indexed i a = Indexed i a deriving (Functor)

instance (StorageGet m s gs, StorageSet m s ss, Index s ~ i) => System (Indexed i gs -> ss) m s where
    runSystem s f = iter s $ \i -> get s i >>= set s i . f . Indexed i
    {-# INLINE runSystem #-}

instance {-# OVERLAPPING #-} (StorageGet m s gs, Index s ~ i) => System (Indexed i gs -> m ()) m s where
    runSystem s f = iter s $ \i -> get s i >>= f . Indexed i
    {-# INLINE runSystem #-}
