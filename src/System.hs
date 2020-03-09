{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
module System ( System (..)
              ) where

import Types (HList(..), SimplifyHList(..), UnsimplifyHList(..), UnsimplifiedHList)
import World
import Storage

class System m as f where
    runSystem :: World' m as -> f -> m ()

type SystemWorldConstraint m as gs ss = (WorldFilter as gs ss, StorageGet m (World m (Filter as gs ss) gs ss) (HList gs), StorageSet m (World m (Filter as gs ss) gs ss) (HList ss))

instance (SystemWorldConstraint m as (UnsimplifiedHList gs) (UnsimplifiedHList ss), SimplifyHList gs, UnsimplifyHList ss) => System m as (gs -> ss) where
    runSystem w f = let w' = ((filterWorld w) :: World m (Filter as (UnsimplifiedHList gs) (UnsimplifiedHList ss)) (UnsimplifiedHList gs) (UnsimplifiedHList ss))
                     in iter w' $ \i -> get w' i >>= set w' i . unsimplifyHList . f . simplifyHList

type SystemWorldConstraintGet m as gs = (WorldFilter as gs '[], StorageGet m (World m (Filter as gs '[]) gs '[]) (HList gs))

instance {-# OVERLAPPING #-} (System m as (a -> b), SystemWorldConstraintGet m as (UnsimplifiedHList gs), SimplifyHList gs) => System m as (gs -> (a -> b)) where
    runSystem w f = let w' = ((filterWorld w) :: World m (Filter as (UnsimplifiedHList gs) '[]) (UnsimplifiedHList gs) '[])
                     in iter w' $ \i -> do
                         v <- get w' i
                         runSystem w (f $ simplifyHList v)
