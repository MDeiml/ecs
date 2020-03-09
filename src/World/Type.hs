{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module World.Type ( World(..)
                  , World'
                  , WorldIndex(..)
                  , WorldIndex'(..)
                  , GetTypes
                  , SetTypes
                  ) where

import Types
import Storage
import Data.Proxy (Proxy(..))

type family GetTypes s :: [*]
type family SetTypes s :: [*]

type instance Index (World m as gs ss) = WorldIndex as

data World m (as :: [([*], [*], *)]) (gs :: [*]) (ss :: [*]) where
    WNil :: World m '[] gs ss
    WCons :: (All (StorageGet m s) (GetTypes s), All (StorageGet m s) gs, All (StorageSet m s) (SetTypes s), All (StorageSet m s) ss, Storage m s) => s -> World m as gs ss -> World m ('(GetTypes s, SetTypes s, Index s) ': as) gs ss

type World' m ss = World m ss '[] '[]

data WorldIndex (as :: [([*], [*], *)]) where
    WorldIndex :: WorldIndex' n => Proxy n -> Thrd (ListIndex as n) -> WorldIndex as

class WorldIndex' (n :: Nat) where
    worldIndex :: Proxy n -> World m as gs ss -> WrappedStorage m gs ss (Thrd (ListIndex as n))

instance WorldIndex' Z where
    worldIndex _ (WCons s _) = WrappedStorage s

instance WorldIndex' n => WorldIndex' (S n) where
    worldIndex _ (WCons _ ss) = worldIndex (Proxy :: Proxy n) ss

instance Monad m => Storage m (World m as gs ss) where
    iter WNil _ = return ()
    iter (WCons s ss) f = iter s f1 >> iter ss f2
        where
            f1 i = f $ WorldIndex (Proxy :: Proxy Z) i
            f2 (WorldIndex p i) = f $ WorldIndex (incProxy p) i
                where
                    incProxy :: Proxy n -> Proxy (S n)
                    incProxy _ = Proxy
