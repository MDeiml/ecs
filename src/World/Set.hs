{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module World.Set ( demoteSet
                 , WorldHasSet(..)
                 , FilterSet
                 ) where

import Types (IsElem, All, AllIsElem(..), Dict(..), HList(..))
import World.Type
import Storage
import Data.Type.Bool (If)
import Data.Proxy (Proxy(..))


type family FilterSet (as :: [([*], [*], *)]) (c :: *) where
    FilterSet '[] c = '[]
    FilterSet ('(g, s, i) ': as) c = If (IsElem c s) ('(g, s, i) ': FilterSet as c) (FilterSet as c)

demoteSet :: World m as gs (s ': ss) -> World m as gs ss
demoteSet WNil = WNil
demoteSet (WCons s ss) = WCons s (demoteSet ss)



class WorldHasSet s as where
    filterWorldSet :: Proxy s -> World m as gs ss -> World m (FilterSet as s) gs (s ': ss)

instance WorldHasSet s '[] where
    filterWorldSet _ WNil = WNil

instance (IfHasSet b c (IsElem c b), WorldHasSet c ss) => WorldHasSet c ('(a, b, i) ': ss) where
    filterWorldSet _ (WCons s ss) = ifHasSet (Proxy :: Proxy b) s ss'
        where
            ss' = filterWorldSet (Proxy :: Proxy c) ss

class IfHasSet (d :: [*]) c (b :: Bool) where
    ifHasSet :: (All (StorageGet m s) cs, All (StorageGet m s) (GetTypes s), All (StorageSet m s) (SetTypes s), All (StorageSet m s) ss, b ~ IsElem c d, d ~ SetTypes s) => Proxy d -> s -> World m as cs (c ': ss) -> World m (If b ('((GetTypes s), d, Index s) ': as) as) cs (c ': ss)

instance IfHasSet '[] c 'False where
    ifHasSet _ _ w = w

instance AllIsElem c (d ': ds) => IfHasSet (d ': ds) c 'True where
    ifHasSet _ s w = case allIsElem (Proxy :: Proxy (d ': ds)) (Proxy :: Proxy c) (mkProxy s w) of
                    Dict -> WCons s w
        where
            mkProxy :: s -> World m as1 cs (c ': ss) -> Proxy (StorageSet m s)
            mkProxy _ _ = Proxy

instance IfHasSet (a ': as) c 'False where
    ifHasSet _ s w = w


instance Monad m => StorageSet m (World m as cs '[]) (HList '[]) where
    set _ _ _ = return ()

instance StorageSet m (World m as cs ss) (HList ss) => StorageSet m (World m as cs (s ': ss)) (HList (s ': ss)) where
    set w wi@(WorldIndex p i) (HCons v vs) = case worldIndex p w of
                                               WrappedStorage s -> do
                                                   set s i v
                                                   set (demoteSet w) wi vs
