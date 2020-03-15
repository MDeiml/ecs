{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Lib
     where

import System
import Single
import World
import Types
import Archetype
import Control.Monad.ST
import Control.Monad (replicateM)
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector as V
import Data.Vector.Unboxed.Deriving

import Linear
import Criterion
import qualified Criterion.Main as C
import Criterion.Types

newtype ECSPos = ECSPos { unECSPos :: V2 Float } deriving (Show)
derivingUnbox "ECSPos" [t| ECSPos -> V2 Float |] [| unECSPos |] [| ECSPos |]

instance Component ECSPos where
    type StorageVector ECSPos = M.MVector

newtype ECSVel = ECSVel { unECSVel :: V2 Float }

derivingUnbox "ECSVel" [t| ECSVel -> V2 Float |] [| unECSVel |] [| ECSVel |]
instance Component ECSVel where
    type StorageVector ECSVel = M.MVector

newtype Delta = Delta Float

type APosVel s = Archetype s '[ECSPos, ECSVel]
type APos s = Archetype s '[ECSPos]
type World' s = World '[APosVel s, APos s, Single s Delta]

initWorld :: ST s (World' s)
initWorld = do
    let a = archetypeNew @'[ECSPos, ECSVel]
    let b = archetypeNew @'[ECSPos]
    a1 <- archetypeAdd a $ V.replicate 1000 (HCons (ECSPos 0) (HCons (ECSVel 1) HNil))
    b1 <- archetypeAdd b $ V.replicate 1000 (HCons (ECSPos 0) HNil)
    d <- newSingle (Delta 0.5)
    return $ World (HCons a1 $ HCons b1 $ HCons d HNil)

step :: World' s -> ST s ()
step w = do
    _ <- replicateM 10 $ runSystem w $ \(Indexed _ (ECSVel v :> ECSPos p)) -> ECSPos (p + v)
    return ()
        
someFunc :: IO ()
someFunc = C.defaultMainWith (C.defaultConfig {timeLimit = 10})
    [ bgroup "pos_vel"
        [ bench "init" $ whnfIO $ stToIO $ initWorld
        , bench "step" $ whnfIO $ stToIO $ initWorld >>= step
        ]
    ]

-- someFunc :: IO ()
-- someFunc = do
--     _ <- replicateM 1000 $ do
--         _ <- stToIO $ do
--             _ <- initWorld
--             -- step w
--             -- _ <- replicateM 10000 (step w)
--             -- let (World (HCons a _)) = w
--             -- get a (0, 0)
--             return ()
--         -- let HCons res' _ = (res :: HList '[ECSPos])
--         -- print res'
--         return ()
--     return ()
