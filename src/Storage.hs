{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
module Storage( Index
              , Storage(..)
              , StorageGet(..)
              , StorageSet(..)
              , WrappedStorage(..)) where

import Types (All)

type family Index s :: *

class Monad m => Storage m s where
    iter :: s -> (Index s -> m ()) -> m ()

class Storage m s => StorageGet m s e where
    get :: s -> Index s -> m e

class Storage m s => StorageSet m s e where
    set :: s -> Index s -> e -> m ()

data WrappedStorage m gs ss i where
    WrappedStorage :: (All (StorageGet m s) gs, All (StorageSet m s) ss) => s -> WrappedStorage m gs ss (Index s)
