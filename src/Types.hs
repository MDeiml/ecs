{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Types ( Nat(..)
             , HList(..)
             , (:>)(..)
             , Dict(..)
             , IsElem
             , Empty
             , All
             , ListIndex
             , Thrd
             , UnsimplifiedHList
             , UnsimplifyHList(..)
             , SimplifyHList(..)
             , AllIsElem(..) ) where
                 
import GHC.Exts (Constraint)
import Data.Proxy (Proxy(..))

data Nat = Z | S Nat

data HList (as :: [*]) where
    HNil :: HList '[]
    HCons :: a -> HList as -> HList (a ': as)

data (:>) a b = a :> b
infixr 5 :>

data Dict a where
    Dict :: a => Dict a

type family IsElem a as where
    IsElem a '[] = 'False
    IsElem a (a ': as) = 'True
    IsElem a (b ': as) = IsElem a as

type family Empty a where
    Empty '[] = 'True
    Empty a = 'False

type family All (c :: * -> Constraint) (as :: [*]) :: Constraint where
    All c '[] = ()
    All c (a ': as) = (c a, All c as)

type family ListIndex (ss :: [k]) (i :: Nat) where
    ListIndex (s ': ss) Z = s
    ListIndex (s ': ss) (S n) = ListIndex ss n

type family Thrd a where
    Thrd '(a, b, c) = c

type family UnsimplifiedHList as where
    UnsimplifiedHList (a :> as) = a ': UnsimplifiedHList as
    UnsimplifiedHList a = '[a]

class UnsimplifyHList as where
    unsimplifyHList :: as -> HList (UnsimplifiedHList as)

instance UnsimplifiedHList a ~ '[a] => UnsimplifyHList a where
    unsimplifyHList a = HCons a HNil

instance {-# OVERLAPPING #-} UnsimplifyHList as => UnsimplifyHList (a :> as) where
    unsimplifyHList (a :> as) = HCons a $ unsimplifyHList as

class SimplifyHList as where
    simplifyHList :: HList (UnsimplifiedHList as) -> as

instance UnsimplifiedHList a ~ '[a] => SimplifyHList a where
    simplifyHList (HCons a _) = a

instance {-# OVERLAPPING #-} SimplifyHList as => SimplifyHList (a :> as) where
    simplifyHList (HCons a as) = a :> simplifyHList as

class AllIsElem a as where
    allIsElem :: (All c as, IsElem a as ~ 'True) => Proxy as -> Proxy a -> Proxy c -> Dict (c a)

instance (IsElem a as ~ 'True, AllIsElem a as) => AllIsElem a (b ': as) where
    allIsElem as a c = allIsElem (Proxy :: Proxy as) a c

instance {-# OVERLAPPING #-} AllIsElem a (a ': as) where
    allIsElem _ _ _  = Dict
