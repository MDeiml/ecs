{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module World ( World(..)
             , World'
             , WorldFilter(..)
             , Filter(..)
             , GetTypes
             , SetTypes
             ) where

import World.Type
import World.Get
import World.Set
import Storage
import Types
import Data.Type.Bool (If)
import Data.Proxy (Proxy(..))

type family Filter as gs ss where
    Filter as '[] '[] = as
    Filter as (g ': gs) '[] = FilterGet (Filter as gs '[]) g
    Filter as gs (s ': ss) = FilterSet (Filter as gs ss) s

class Empty (Filter as gs ss) ~ 'False => WorldFilter as gs ss where
    filterWorld :: World' m as -> World m (Filter as gs ss) gs ss

instance Empty as ~ 'False => WorldFilter as '[] '[] where
    filterWorld = id

instance (Empty (Filter as (g ': gs) '[]) ~ 'False, WorldFilter as gs '[], WorldHasGet g (Filter as gs '[])) => WorldFilter as (g ': gs) '[] where
    filterWorld = filterWorldGet (Proxy :: Proxy g) . filterWorld

instance (Empty (Filter as gs (s ':  ss)) ~ 'False, WorldFilter as gs ss, WorldHasSet s (Filter as gs ss)) => WorldFilter as gs (s ': ss) where
    filterWorld = filterWorldSet (Proxy :: Proxy s) . filterWorld

