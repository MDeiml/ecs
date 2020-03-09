{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Archetype where

import Types (HList(..))
import Storage
import World (GetTypes, SetTypes)
import Control.Monad.ST
import Control.Monad (forM_, when)
import qualified Data.Vector.Generic.Mutable as G
import qualified Data.Vector.Unboxed.Mutable as U
import qualified Data.Vector.Mutable as M

chunkSize :: Int
chunkSize = 2 ^ 10

type family StorageVector a :: * -> * -> *

type instance Index (Archetype s as) = Int

data Chunk s as where
    CNil :: Chunk s as
    CCons :: G.MVector (StorageVector a) a => (StorageVector a) s a -> Chunk s as -> Chunk s (a ': as)

type ChunkHeader s = U.MVector s Bool

data Archetype s as = Archetype {
    archetypeNextIndex :: Int,
    archetypeData :: M.MVector s (ChunkHeader s, Chunk s as)
                                }

archetypeNew :: forall as s. ST s (Archetype s as)
archetypeNew = fmap (Archetype 0) $ G.new 0

chunkSet :: Chunk s as -> Int -> HList as -> ST s ()
chunkSet CNil _ _ = return ()
chunkSet (CCons c cs) i (HCons v vs) = do
    G.write c i v
    chunkSet cs i vs

archetypeAdd :: ChunkNew as => Archetype s as -> HList as -> ST s (Int, Archetype s as)
archetypeAdd a v
  | archetypeNextIndex a `rem` chunkSize == 0 = do
      chunk <- chunkNew
      chunkSet chunk 0 v
      header <- U.replicate chunkSize False 
      U.write header 0 True
      d <- M.grow (archetypeData a) 1
      M.write d (M.length d - 1) (header, chunk)
      return (archetypeNextIndex a, Archetype (archetypeNextIndex a + 1) d)
  | otherwise = do
    (header, chunk) <- M.read (archetypeData a) (M.length (archetypeData a) - 1)
    let i = archetypeNextIndex a `rem` chunkSize
    chunkSet chunk i v
    U.write header i True
    return (archetypeNextIndex a, a { archetypeNextIndex = archetypeNextIndex a + 1 })

type instance GetTypes (Archetype s as) = as
type instance SetTypes (Archetype s as) = as

instance Storage (ST s) (Archetype s as) where
    iter a f = forM_ [0 .. M.length (archetypeData a) - 1] $ \i -> do
        (header, _chunk) <- M.read (archetypeData a) i
        forM_ [0 .. chunkSize - 1] $ \j -> do
            flag <- U.read header j
            when flag $ f (i * chunkSize + j)

class ChunkNew as where
    chunkNew :: ST s (Chunk s as)

instance ChunkNew '[] where
    chunkNew = return CNil

instance (G.MVector (StorageVector a) a, ChunkNew as) => ChunkNew (a ': as) where
    chunkNew = do
        c <- G.new chunkSize
        cs <- chunkNew
        return $ CCons c cs

class ChunkHas as a where
    findChunk :: Chunk s as -> (forall v. G.MVector v a => v s a -> x) -> x

instance ChunkHas as a => ChunkHas (b ': as) a where
    findChunk (CCons _ as) = findChunk as

instance {-# OVERLAPPING #-} ChunkHas (a ': as) a where
    findChunk (CCons a _) f = f a

instance ChunkHas as a => StorageGet (ST s) (Archetype s as) a where
    get a index = do
        let (i, j) = index `quotRem` chunkSize
        (header, chunk) <- M.read (archetypeData a) i
        findChunk chunk $ \ca -> G.read ca j

instance ChunkHas as a => StorageSet (ST s) (Archetype s as) a where
    set a index x = do
        let (i, j) = index `quotRem` chunkSize
        (header, chunk) <- M.read (archetypeData a) i
        findChunk chunk $ \ca -> G.write ca j x
