{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Types ( Nat(..)
             , SNat(..)
             , HList(..)
             , (:>)(..)
             , IsElem
             , Append
             , Empty
             , All
             , ListIndex
             , UnsimplifiedHList
             , UnsimplifyHList(..)
             , SimplifyHList(..)) where
                 
import GHC.Exts (Constraint)

data Nat = Z | S Nat

data SNat n where
    SingZ :: SNat 'Z
    SingS :: SNat n -> SNat ('S n)

data HList (as :: [*]) where
    HNil :: HList '[]
    HCons :: a -> HList as -> HList (a ': as)

data (:>) a b = a :> b
infixr 5 :>

type family IsElem a as where
    IsElem a '[] = 'False
    IsElem a (a ': as) = 'True
    IsElem a (b ': as) = IsElem a as

type family Append as bs where
    Append '[] bs = bs
    Append (a ': as) bs = a ': Append as bs

type family Empty a where
    Empty '[] = 'True
    Empty a = 'False

type family All (c :: * -> Constraint) (as :: [*]) :: Constraint where
    All c '[] = ()
    All c (a ': as) = (c a, All c as)

type family ListIndex (ss :: [k]) (i :: Nat) where
    ListIndex (s ': ss) 'Z = s
    ListIndex (s ': ss) ('S n) = ListIndex ss n

type family UnsimplifiedHList as where
    UnsimplifiedHList (a :> as) = a ': UnsimplifiedHList as
    UnsimplifiedHList a = '[a]

class UnsimplifyHList as where
    unsimplifyHList :: as -> HList (UnsimplifiedHList as)

instance UnsimplifiedHList a ~ '[a] => UnsimplifyHList a where
    unsimplifyHList a = HCons a HNil
    {-# INLINE unsimplifyHList #-}

instance {-# OVERLAPPING #-} UnsimplifyHList as => UnsimplifyHList (a :> as) where
    unsimplifyHList (a :> as) = HCons a $ unsimplifyHList as
    {-# INLINE unsimplifyHList #-}

class SimplifyHList as where
    simplifyHList :: HList (UnsimplifiedHList as) -> as

instance UnsimplifiedHList a ~ '[a] => SimplifyHList a where
    simplifyHList (HCons a _) = a
    {-# INLINE simplifyHList #-}

instance {-# OVERLAPPING #-} SimplifyHList as => SimplifyHList (a :> as) where
    simplifyHList (HCons a as) = a :> simplifyHList as
    {-# INLINE simplifyHList #-}
