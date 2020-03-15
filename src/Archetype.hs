{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Archetype where

import Types (HList(..), IsElem)
import Storage
import System
import World (WorldFilter, WorldRead, WorldWrite)
import Data.Proxy (Proxy(..))
import Control.Monad.ST
import Control.Monad (when)
import qualified Data.Vector.Generic.Mutable as G
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Unboxed.Mutable as U
import qualified Data.Vector as V
import Data.Bits

chunkSize :: Int
chunkSize = 1024

bitsize :: Int
bitsize = finiteBitSize (0 :: Word)

headerSize :: Int
headerSize = chunkSize `quot` bitsize

class G.MVector (StorageVector c) c => Component c where
    type StorageVector c :: * -> * -> *

type family MapStorageVector s as where
    MapStorageVector s '[] = '[]
    MapStorageVector s (a ': as) = StorageVector a s a ': MapStorageVector s as

type instance Index (Archetype s as) = (Int, Int)

data Chunk s as where
    CNil :: Chunk s '[]
    CCons :: G.MVector (StorageVector a) a => (StorageVector a) s a -> Chunk s as -> Chunk s (a ': as)

type ChunkHeader s = U.MVector s Word

data Archetype s as = Archetype {
    archetypeNextIndex :: !Int,
    archetypeData :: !(V.Vector (ChunkHeader s, Chunk s as))
                                }

archetypeNew :: forall as s. Archetype s as
archetypeNew = Archetype 0 V.empty

chunkWrite :: Chunk s as -> Int -> HList as -> ST s ()
chunkWrite CNil _ _ = return ()
chunkWrite (CCons c cs) i (HCons v vs) = do
    G.write c i v
    chunkWrite cs i vs

archetypeAdd :: (GV.Vector v (HList as), ChunkNew as) => Archetype s as -> v (HList as) -> ST s (Archetype s as)
archetypeAdd a v = do
    let numA = min (V.length (archetypeData a) * chunkSize - archetypeNextIndex a) (GV.length v)
        (numB, numC) = (GV.length v - numA) `quotRem` chunkSize
        (header, chunk) = V.last (archetypeData a)
        ai1 = archetypeNextIndex a `rem` chunkSize
    forLoop 0 numA $ \i -> do
        chunkWrite chunk (ai1 + i) $ v GV.! i
        let (j, k) = (ai1 + i) `quotRem` bitsize
        U.modify header (.|. bit k) j
    data' <- V.generateM numB $ \c -> do
        header' <- U.replicate headerSize (complement 0)
        chunk' <- chunkNew
        forLoop 0 chunkSize $ \i -> do
            chunkWrite chunk' i $ v GV.! (i + c * chunkSize + numA)
        return (header', chunk')
    header' <- U.replicate headerSize 0
    chunk' <- chunkNew
    forLoop 0 numC $ \i -> do
        let i' = i + numA + numB * chunkSize
        chunkWrite chunk' i $ v GV.! i'
        let (j, k) = i' `quotRem` bitsize
        U.modify header' (.|. bit k) j
    return $ Archetype (archetypeNextIndex a + GV.length v) ((archetypeData a V.++ data') `V.snoc` (header', chunk'))

type instance WorldFilter (WorldRead r) (Archetype s as) = IsElem r as
type instance WorldFilter (WorldWrite r) (Archetype s as) = IsElem r as

class ChunkNew as where
    chunkNew :: ST s (Chunk s as)

instance ChunkNew '[] where
    chunkNew = return CNil

instance (Component a, ChunkNew as) => ChunkNew (a ': as) where
    chunkNew = do
        c <- G.new chunkSize
        cs <- chunkNew
        return $ CCons c cs

class ChunkHas as a where
    chunkFind :: Chunk s as -> StorageVector a s a

instance ChunkHas as a => ChunkHas (b ': as) a where
    chunkFind (CCons _ as) = chunkFind as
    {-# INLINE chunkFind #-}

instance {-# OVERLAPPING #-} ChunkHas (a ': as) a where
    chunkFind (CCons a _) = a
    {-# INLINE chunkFind #-}

class ChunkHas' as bs where
    chunkFind' :: Proxy bs -> Chunk s as -> HList (MapStorageVector s bs)

instance ChunkHas' as '[] where
    chunkFind' _ = return HNil
    {-# INLINE chunkFind' #-}

instance (ChunkHas as b, ChunkHas' as bs) => ChunkHas' as (b ': bs) where
    chunkFind' _ c = HCons (chunkFind c) (chunkFind' (Proxy :: Proxy bs) c)
    {-# INLINE chunkFind' #-}

class IsStorageVector as where
    vectorGet :: HList (MapStorageVector s as) -> Int -> ST s (HList as)
    vectorSet :: HList (MapStorageVector s as) -> Int -> HList as -> ST s ()

instance IsStorageVector '[] where
    vectorGet _ _ = return HNil
    {-# INLINE vectorGet #-}
    vectorSet _ _ _ = return ()
    {-# INLINE vectorSet #-}

instance (G.MVector (StorageVector a) a, IsStorageVector as) => IsStorageVector (a ': as) where
    vectorGet (HCons a as) i = do
        v <- G.read a i
        vs <- vectorGet as i
        return $ HCons v vs
    {-# INLINE vectorGet #-}
    vectorSet (HCons a as) i (HCons v vs) = do
        G.write a i v
        vectorSet as i vs
    {-# INLINE vectorSet #-}

instance Storage (ST s) (Archetype s as) where
    iter a f = flip V.imapM_ (archetypeData a) $ \i (header, _chunk) -> do
        forLoop 0 headerSize $ \j -> do
            flag <- U.read header j
            let j' = j * bitsize
            forLoop 0 bitsize $ \k -> do
                when (testBit flag k) $ f (i, j' + k)
    {-# INLINE iter #-}

instance (IsStorageVector bs, ChunkHas' as bs) => StorageGet (ST s) (Archetype s as) (HList bs) where
    get a (i, j) = do
        let (_header, chunk) = archetypeData a V.! i
            l = chunkFind' (Proxy :: Proxy bs) chunk
        vectorGet l j
    {-# INLINE get #-}

instance (IsStorageVector bs, ChunkHas' as bs) => StorageSet (ST s) (Archetype s as) (HList bs) where
    set a (i, j) x = do
        let (_header, chunk) = archetypeData a V.! i
            l = chunkFind' (Proxy :: Proxy bs) chunk
        vectorSet l j x
    {-# INLINE set #-}

instance {-# OVERLAPPING #-} (ChunkHas' as gs, ChunkHas' as ss, IsStorageVector gs, IsStorageVector ss) => System (Indexed (Int, Int) (HList gs) -> HList ss) (ST s) (Archetype s as) where
    runSystem a f = flip V.imapM_ (archetypeData a) $ \i (header, chunk) -> do
        let g = chunkFind' (Proxy :: Proxy gs) chunk
            s = chunkFind' (Proxy :: Proxy ss) chunk
        forLoop 0 headerSize $ \j -> do
            flag <- U.read header j
            let j' = j * bitsize
            forLoop 0 bitsize $ \k -> do
                when (testBit flag k) $ vectorGet g (j' + k) >>= vectorSet s (j' + k) . f . Indexed (i, j' + k)
        where
    {-# INLINE runSystem #-}

instance {-# OVERLAPPING #-} (ChunkHas' as gs, IsStorageVector gs) => System (Indexed (Int, Int) (HList gs) -> ST s ()) (ST s) (Archetype s as) where
    runSystem a f = flip V.imapM_ (archetypeData a) $ \i (header, chunk) -> do
        let g = chunkFind' (Proxy :: Proxy gs) chunk
        forLoop 0 headerSize $ \j -> do
            flag <- U.read header j
            let j' = j * bitsize
            forLoop 0 bitsize $ \k -> do
                when (testBit flag k) $ vectorGet g (j' + k) >>= f . Indexed (i, j' + k)
        where
    {-# INLINE runSystem #-}

forLoop :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
forLoop i n f = if i >= n then return () else go i
    where
        go !j
          | j == n = return ()
          | otherwise = f j >> go (j + 1)
{-# INLINE forLoop #-}
