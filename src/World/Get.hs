{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module World.Get ( demoteGet
                 , WorldHasGet(..)
                 , FilterGet
                 ) where

import Types (IsElem, All, AllIsElem(..), Dict(..), HList(..))
import World.Type
import Storage
import Data.Type.Bool (If)
import Data.Proxy (Proxy(..))

type family FilterGet (as :: [([*], [*], *)]) (c :: *) where
    FilterGet '[] c = '[]
    FilterGet ('(g, s, i) ': as) c = If (IsElem c g) ('(g, s, i) ': FilterGet as c) (FilterGet as c)

demoteGet :: World m as (g ': gs) ss -> World m as gs ss
demoteGet WNil = WNil
demoteGet (WCons s ss) = WCons s (demoteGet ss)

class WorldHasGet c as where
    filterWorldGet :: Proxy c -> World m as gs ss -> World m (FilterGet as c) (c ': gs) ss

instance WorldHasGet c '[] where
    filterWorldGet _ WNil = WNil

instance (IfHasGet a c (IsElem c a), WorldHasGet c ss) => WorldHasGet c ('(a, b, i) ': ss) where
    filterWorldGet _ (WCons s ss) = ifHasGet (Proxy :: Proxy a) s ss'
        where
            ss' = filterWorldGet (Proxy :: Proxy c) ss

class IfHasGet (a :: [*]) c (b :: Bool) where
    ifHasGet :: (All (StorageGet m s) cs, All (StorageGet m s) (GetTypes s), All (StorageSet m s) (SetTypes s), All (StorageSet m s) ss, b ~ IsElem c a, a ~ GetTypes s) => Proxy a -> s -> World m as (c ': cs) ss -> World m (If b ('(a, SetTypes s, Index s) ': as) as) (c ': cs) ss

instance IfHasGet '[] c 'False where
    ifHasGet _ _ w = w

instance AllIsElem c (a ': as) => IfHasGet (a ': as) c 'True where
    ifHasGet _ s w = case allIsElem (Proxy :: Proxy (a ': as)) (Proxy :: Proxy c) (mkProxy s w) of
                    Dict -> WCons s w
        where
            mkProxy :: s -> World m as1 (c ': cs) ss -> Proxy (StorageGet m s)
            mkProxy _ _ = Proxy

instance IfHasGet (a ': as) c 'False where
    ifHasGet _ s w = w

instance Monad m => StorageGet m (World m as '[] ss) (HList '[]) where
    get _ _ = return HNil

instance StorageGet m (World m as cs ss) (HList cs) => StorageGet m (World m as (c ': cs) ss) (HList (c ': cs)) where
    get w wi@(WorldIndex p i) = do
                v <- case worldIndex p w of
                       WrappedStorage s -> get s i
                vs <- get (demoteGet w) wi
                return (HCons v vs)
