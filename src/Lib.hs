{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Lib
     where

import System
import Storage
import World
import Types
import Archetype
import Data.Proxy (Proxy(..))
import Control.Monad.ST
import Control.Monad (replicateM, foldM)
import qualified Data.Vector.Mutable as M

import Linear
-- import Criterion
-- import qualified Criterion.Main as C
-- import Criterion.Types

newtype ECSPos = ECSPos (V2 Float)

type instance StorageVector ECSPos = M.MVector

newtype ECSVel = ECSVel (V2 Float)

type instance StorageVector ECSVel = M.MVector

testSystem ((ECSVel v) :> (ECSPos p)) = ECSPos (p + v)

type W s = World' (ST s) '[ '( '[ECSPos, ECSVel], '[ECSPos, ECSVel], Int ), '( '[ECSPos], '[ECSPos], Int ) ]

initWorld :: ST s (W s)
initWorld= do
    a <- archetypeNew @'[ECSPos, ECSVel]
    a1 <- foldM (\s _ -> fmap snd $ archetypeAdd s (HCons (ECSPos 0) (HCons (ECSVel 1) HNil))) a [1..1000]
    b <- archetypeNew @'[ECSPos]
    b1 <- foldM (\s _ -> fmap snd $ archetypeAdd s (HCons (ECSPos 0) HNil)) b [1..9000]
    return $ WCons a1 $ WCons b1 WNil

step :: W s -> ST s ()
step w = runSystem w $ \(ECSVel v :> ECSPos p) -> ECSPos (p + v)
        
-- someFunc :: IO ()
-- someFunc = C.defaultMainWith (C.defaultConfig {timeLimit = 10})
--     [ bgroup "pos_vel"
--         [ bench "init" $ whnfIO $ stToIO $ initWorld
--         , bench "step" $ whnfIO $ stToIO $ initWorld >>= step
--         ]
--     ]

someFunc :: IO ()
someFunc = stToIO $ do
    w <- initWorld
    replicateM 1000 (step w)
    return ()
