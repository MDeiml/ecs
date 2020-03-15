{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
module World ( World(..)
             , FilterWorld(..)
             , WorldIndex(..)
             , FilteredWorld(..)
             , WorldFilter
             , Filter
             , WorldRead
             , WorldWrite
             ) where

import Data.Proxy (Proxy(..))
import Data.Type.Bool
import Storage
import Types
import System
import GHC.TypeLits
import GHC.Exts (Constraint)

newtype World ss = World { unWorld :: HList ss }

-- STORAGE INSTANCE

type instance Index (FilteredWorld ss) = WorldIndex ss
data WorldIndex (ss :: [*]) where
    WorldIndex :: SNat n -> Index (ListIndex ss n) -> WorldIndex ss

newtype FilteredWorld as = FilteredWorld { unFilteredWorld :: HList as }

instance Monad m => Storage m (FilteredWorld '[]) where
    iter (FilteredWorld HNil) _ = return ()
    {-# INLINE iter #-}

instance (Monad m, Storage m a, Storage m (FilteredWorld as)) => Storage m (FilteredWorld (a ': as)) where
    iter (FilteredWorld (HCons s ss)) f = iter s f1 >> iter (FilteredWorld ss) f2
        where
            f1 i = f $ WorldIndex SingZ i
            f2 (WorldIndex p i) = f $ WorldIndex (SingS p) i
    {-# INLINE iter #-}

instance Monad m => StorageGet m (FilteredWorld '[]) e where
    get = error "Invalid index"
    {-# INLINE get #-}

instance (StorageGet m a e, StorageGet m (FilteredWorld as) e) => StorageGet m (FilteredWorld (a ': as)) e where
    get (FilteredWorld (HCons s _)) (WorldIndex SingZ i) = get s i
    get (FilteredWorld (HCons _ ss)) (WorldIndex (SingS n) i) = get (FilteredWorld ss) (WorldIndex n i)
    {-# INLINE get #-}

instance Monad m => StorageSet m (FilteredWorld '[]) e where
    set = error "Invalid index"
    {-# INLINE set #-}

instance (StorageSet m a e, StorageSet m (FilteredWorld as) e) => StorageSet m (FilteredWorld (a ': as)) e where
    set (FilteredWorld (HCons s _)) (WorldIndex SingZ i) = set s i
    set (FilteredWorld (HCons _ ss)) (WorldIndex (SingS n) i) = set (FilteredWorld ss) (WorldIndex n i)
    {-# INLINE set #-}


-- FILTERING

data WorldRead a
data WorldWrite a

type family WorldFilter (f :: *) (s :: *) :: Bool

type family Filter (fs :: [*]) as where
    Filter '[] as = as
    Filter (f ': fs) as = Filter fs (FilterSingle f as)

type family FilterSingle f as where
    FilterSingle f '[] = '[]
    FilterSingle f (a ': as) = If (WorldFilter f a) (a ': FilterSingle f as) (FilterSingle f as)

type family CheckNotEmpty fs as :: Constraint where
    CheckNotEmpty fs as = If
        (Empty (Filter fs as))
        (TypeError (
            'Text "The filter " ':<>: 'ShowType fs ':$$:
            'Text "had no matches for `" ':<>: 'ShowType (World as) ':<>: 'Text "'"
        ))
        (() :: Constraint)

class FilterWorld fs as where
    filterWorld :: Proxy fs -> World as -> FilteredWorld (Filter fs as)

instance CheckNotEmpty '[] as => FilterWorld '[] as where
    filterWorld _ (World w) = (FilteredWorld w)
    {-# INLINE filterWorld #-}

instance (CheckNotEmpty (f ': fs) as, FilterWorldSingle f as, FilterWorld fs (FilterSingle f as)) => FilterWorld (f ': fs) as where
    filterWorld _ = filterWorld (Proxy :: Proxy fs) . filterWorldSingle (Proxy :: Proxy f) 
    {-# INLINE filterWorld #-}

class FilterWorldSingle f as where
    filterWorldSingle :: Proxy f -> World as -> World (FilterSingle f as)

instance FilterWorldSingle f '[] where
    filterWorldSingle _ = id
    {-# INLINE filterWorldSingle #-}

instance (FilterIf (WorldFilter f a), FilterWorldSingle f as) => FilterWorldSingle f (a ': as) where
    filterWorldSingle p (World (HCons a as)) = filterIf (Proxy :: Proxy (WorldFilter f a)) a (filterWorldSingle p $ World as)
    {-# INLINE filterWorldSingle #-}

class FilterIf (b :: Bool) where
    filterIf :: Proxy b -> s -> World ss -> World (If b (s ': ss) ss)

instance FilterIf 'False where
    filterIf _ _ w = w
    {-# INLINE filterIf #-}

instance FilterIf 'True where
    filterIf _ s (World ss) = World (HCons s ss)
    {-# INLINE filterIf #-}


-- SYSTEM

type family FilterReads rs where
    FilterReads '[] = '[]
    FilterReads (r ': rs) = WorldRead r ': FilterReads rs

type family FilterWrites ws where
    FilterWrites '[] = '[]
    FilterWrites (w ': ws) = WorldWrite w ': FilterWrites ws

type BuildFilter gs ss = Append (FilterReads gs) (FilterWrites ss)

type SystemConstraints' m as gs ss = (Monad m, System (Indexed (WorldIndex as) (HList gs) -> HList ss) m (FilteredWorld as))
type SystemConstraints m as gs ss = (SystemConstraints' m (Filter (BuildFilter gs ss) as) gs ss, FilterWorld (BuildFilter gs ss) as)
instance {-# OVERLAPPING #-} (SystemConstraints m as (UnsimplifiedHList gs) (UnsimplifiedHList ss), SimplifyHList gs, UnsimplifyHList ss) => System (gs -> ss) m (World as) where
    runSystem w f = runSystem (filterWorld (Proxy :: Proxy (BuildFilter (UnsimplifiedHList gs) (UnsimplifiedHList ss))) w) (unsimplifyHList . f . simplifyHList . unIndex)
        where
            unIndex :: Indexed (WorldIndex (Filter (BuildFilter (UnsimplifiedHList gs) (UnsimplifiedHList ss)) as)) x -> x
            unIndex (Indexed _ x) = x
    {-# INLINE runSystem #-}

type SystemConstraintsIndexed m as gs ss i = (i ~ WorldIndex (Filter (BuildFilter gs ss) as), SystemConstraints m as gs ss)

instance {-# OVERLAPPING #-} (SystemConstraintsIndexed m as (UnsimplifiedHList gs) (UnsimplifiedHList ss) i, SimplifyHList gs, UnsimplifyHList ss)
  => System (Indexed i gs -> ss) m (World as) where
    runSystem w f = runSystem (filterWorld (Proxy :: Proxy (BuildFilter (UnsimplifiedHList gs) (UnsimplifiedHList ss))) w) (unsimplifyHList . f . fmap simplifyHList)
    {-# INLINE runSystem #-}

type SystemConstraintsGet' m as gs = (Monad m, System (Indexed (WorldIndex as) (HList gs) -> m ()) m (FilteredWorld as))
type SystemConstraintsGet m as gs x = (SystemConstraintsGet' m (Filter (FilterReads gs) as) gs, FilterWorld (FilterReads gs) as, System x m (World as))
instance {-# OVERLAPPING #-} (SystemConstraintsGet m as (UnsimplifiedHList gs) (a -> b), SimplifyHList gs) => System (gs -> (a -> b)) m (World as) where
    runSystem w f = runSystem (filterWorld (Proxy :: Proxy (FilterReads (UnsimplifiedHList gs))) w) f'
        where
            f' :: Indexed (WorldIndex (Filter (FilterReads (UnsimplifiedHList gs)) as)) (HList (UnsimplifiedHList gs)) -> m ()
            f' (Indexed _ gs) = runSystem w (f $ simplifyHList gs)
    {-# INLINE runSystem #-}

type SystemConstraintsGetIndexed m as gs x i = (i ~ WorldIndex (Filter (FilterReads gs) as), SystemConstraintsGet m as gs x)
instance {-# OVERLAPPING #-} (SystemConstraintsGetIndexed m as (UnsimplifiedHList gs) (a -> b) i, SimplifyHList gs)
  => System (Indexed i gs -> (a -> b)) m (World as) where
    runSystem w f = runSystem (filterWorld (Proxy :: Proxy (FilterReads (UnsimplifiedHList gs))) w) f'
        where
            f' :: Indexed (WorldIndex (Filter (FilterReads (UnsimplifiedHList gs)) as)) (HList (UnsimplifiedHList gs)) -> m ()
            f' (Indexed i gs) = runSystem w (f $ Indexed i $ simplifyHList gs)
    {-# INLINE runSystem #-}

-- SYSTEM FILTERED WORLD

instance {-# OVERLAPPING #-} Monad m => System (gs -> ss) m (FilteredWorld '[]) where
    runSystem _ _ = return ()
    {-# INLINE runSystem #-}

instance {-# OVERLAPPING #-} (System (gs -> ss) m (FilteredWorld as), System (gs -> ss) m a) => System (gs -> ss) m (FilteredWorld (a ': as)) where
    runSystem (FilteredWorld (HCons s ss)) f = runSystem s f >> runSystem (FilteredWorld ss) f
    {-# INLINE runSystem #-}


instance {-# OVERLAPPING #-} Monad m => System (Indexed i gs -> ss) m (FilteredWorld '[]) where
    runSystem _ _ = return ()
    {-# INLINE runSystem #-}

instance {-# OVERLAPPING #-} (System (Indexed (WorldIndex as) gs -> ss) m (FilteredWorld as), System (Indexed i gs -> ss) m a, i ~ Index a) => System (Indexed (WorldIndex (a ': as)) gs -> ss) m (FilteredWorld (a ': as)) where
    runSystem (FilteredWorld (HCons s ss)) f = runSystem s f1 >> runSystem (FilteredWorld ss) f2
        where
            f1 (Indexed i x) = f $ Indexed (WorldIndex SingZ i) x
            f2 :: Indexed (WorldIndex as) gs -> ss
            f2 (Indexed (WorldIndex n i) x) = f $ Indexed (WorldIndex (SingS n) i) x
    {-# INLINE runSystem #-}



instance {-# OVERLAPPING #-} Monad m => System (Indexed i gs -> m ()) m (FilteredWorld '[]) where
    runSystem _ _ = return ()
    {-# INLINE runSystem #-}

instance {-# OVERLAPPING #-} (System (Indexed (WorldIndex as) gs -> m ()) m (FilteredWorld as), System (Indexed i gs -> m ()) m a, i ~ Index a) => System (Indexed (WorldIndex (a ': as)) gs -> m ()) m (FilteredWorld (a ': as)) where
    runSystem (FilteredWorld (HCons s ss)) f = runSystem s f1 >> runSystem (FilteredWorld ss) f2
        where
            f1 (Indexed i x) = f $ Indexed (WorldIndex SingZ i) x
            f2 :: Indexed (WorldIndex as) gs -> m ()
            f2 (Indexed (WorldIndex n i) x) = f $ Indexed (WorldIndex (SingS n) i) x
    {-# INLINE runSystem #-}
