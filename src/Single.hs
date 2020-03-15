{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Single ( Single
              , newSingle ) where

import Types (HList(..))
import Storage
import System
import World
import Data.STRef
import Control.Monad.ST
import Data.Type.Equality

newtype Single s a = Single (STRef s a)

type instance Index (Single s a) = ()
type instance WorldFilter (WorldRead r) (Single s a) = r == a
type instance WorldFilter (WorldWrite w) (Single s a) = w == a

newSingle :: a -> ST s (Single s a)
newSingle a = fmap Single $ newSTRef a

instance Storage (ST s) (Single s a) where
    iter _ f = f ()
    {-# INLINE iter #-}

instance StorageGet (ST s) (Single s a) (HList '[a]) where
    get (Single s) _ = fmap hlistSingleton $ readSTRef s
    {-# INLINE get #-}

instance StorageSet (ST s) (Single s a) (HList '[a]) where
    set (Single s) _ = writeSTRef s . hlistUnSingleton
    {-# INLINE set #-}

instance {-# OVERLAPPING #-} System (HList '[a] -> HList '[a]) (ST s) (Single s a) where
    runSystem (Single s) f = modifySTRef s (hlistUnSingleton . f . hlistSingleton)
    {-# INLINE runSystem #-}

instance {-# OVERLAPPING #-} System (HList '[a] -> ST s ()) (ST s) (Single s a) where
    runSystem (Single s) f = readSTRef s >>= f . hlistSingleton
    {-# INLINE runSystem #-}

hlistSingleton :: a -> HList '[a]
hlistSingleton x = HCons x HNil
{-# INLINE hlistSingleton #-}

hlistUnSingleton :: HList '[a] -> a
hlistUnSingleton (HCons x HNil) = x
{-# INLINE hlistUnSingleton #-}
