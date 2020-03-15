{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Storage( Index
              , Storage(..)
              , StorageGet(..)
              , StorageSet(..)) where

type family Index s :: *

class Monad m => Storage m s where
    iter :: s -> (Index s -> m ()) -> m ()

class Storage m s => StorageGet m s e where
    get :: s -> Index s -> m e

class Storage m s => StorageSet m s e where
    set :: s -> Index s -> e -> m ()
