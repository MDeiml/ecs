Stack has not been tested with GHC versions above 8.6, and using 8.8.2, this may fail
Stack has not been tested with Cabal versions above 2.4, but version 3.0.1.0 was found, this may fail
ecs-0.1.0.0: unregistering (local file changes: ChangeLog.md README.md app/Main.hs ecs.cabal package.yaml src/Archetype.hs src/Lib.hs src/Single....)
Building all executables for `ecs' once. After a successful build of all of them, only specified executables will be rebuilt.
ecs> configure (lib + exe)
Configuring ecs-0.1.0.0...
ecs> build (lib + exe)
Preprocessing library for ecs-0.1.0.0..
Building library for ecs-0.1.0.0..
[1 of 8] Compiling Paths_ecs

==================== Tidy Core ====================
Result size of Tidy Core
  = {terms: 444, types: 938, coercions: 95, joins: 0/0}

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
version5 = I# 0#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
version6 = I# 1#

-- RHS size: {terms: 3, types: 2, coercions: 0, joins: 0/0}
version4 = : version5 []

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
version3 = : version5 version4

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
version2 = : version6 version3

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
version1 = : version5 version2

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
version = Version version1 []

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$trModule4 = "ecs-0.1.0.0-6T7cQlWNmHv6W00SuOHZxa"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$trModule3 = TrNameS $trModule4

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$trModule2 = "Paths_ecs"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$trModule1 = TrNameS $trModule2

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$trModule = Module $trModule3 $trModule1

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
getBinDir7 = "ecs_bindir"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
getBinDir6 = unpackCString# getBinDir7

-- RHS size: {terms: 23, types: 40, coercions: 8, joins: 0/0}
getBinDir5
  = \ s_a27o ->
      case ((noinline getForeignEncoding) `cast` <Co:2>) s_a27o of
      { (# ipv_a2fw, ipv1_a2fx #) ->
      case charIsRepresentable3
             ipv1_a2fx getBinDir6 (getEnv3 `cast` <Co:6>) ipv_a2fw
      of
      { (# ipv2_a2hQ, ipv3_a2hR #) ->
      case ipv3_a2hR of {
        Nothing -> getEnv2 getBinDir6 ipv2_a2hQ;
        Just x_a2hX -> (# ipv2_a2hQ, x_a2hX #)
      }
      }
      }

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
getBinDir4
  = "/home/matthias/projects/haskell/ecs/.stack-work/install/x86_64-linux/e1eb528d6f8833bd973f1c3cf17f694ca918906ed63718d4a96c0cee13d0c40a/8.8.2/bin"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
getBinDir3 = unpackCString# getBinDir4

-- RHS size: {terms: 21, types: 63, coercions: 4, joins: 0/0}
getBinDir2
  = \ e1_a1Eo eta_B1 ->
      case e1_a1Eo of wild_a220
      { SomeException @ e2_a26v $dException1_a26w e3_a26x ->
      case eqTypeRep
             (($p1Exception $dException1_a26w) `cast` <Co:4>)
             $fExceptionIOException4
      of {
        Nothing -> raiseIO# wild_a220 eta_B1;
        Just ds1_a26Z ->
          case ds1_a26Z of { HRefl co_a274 co1_a275 ->
          (# eta_B1, getBinDir3 #)
          }
      }
      }

-- RHS size: {terms: 5, types: 4, coercions: 0, joins: 0/0}
getBinDir1 = \ eta_X1EM -> catch# getBinDir5 getBinDir2 eta_X1EM

-- RHS size: {terms: 1, types: 0, coercions: 3, joins: 0/0}
getBinDir = getBinDir1 `cast` <Co:3>

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
getLibDir7 = "ecs_libdir"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
getLibDir6 = unpackCString# getLibDir7

-- RHS size: {terms: 23, types: 40, coercions: 8, joins: 0/0}
getLibDir5
  = \ s_a27o ->
      case ((noinline getForeignEncoding) `cast` <Co:2>) s_a27o of
      { (# ipv_a2fw, ipv1_a2fx #) ->
      case charIsRepresentable3
             ipv1_a2fx getLibDir6 (getEnv3 `cast` <Co:6>) ipv_a2fw
      of
      { (# ipv2_a2hQ, ipv3_a2hR #) ->
      case ipv3_a2hR of {
        Nothing -> getEnv2 getLibDir6 ipv2_a2hQ;
        Just x_a2hX -> (# ipv2_a2hQ, x_a2hX #)
      }
      }
      }

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
getLibDir4
  = "/home/matthias/projects/haskell/ecs/.stack-work/install/x86_64-linux/e1eb528d6f8833bd973f1c3cf17f694ca918906ed63718d4a96c0cee13d0c40a/8.8.2/lib/x86_64-linux-ghc-8.8.2/ecs-0.1.0.0-6T7cQlWNmHv6W00SuOHZxa"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
getLibDir3 = unpackCString# getLibDir4

-- RHS size: {terms: 21, types: 63, coercions: 4, joins: 0/0}
getLibDir2
  = \ e1_a1Eo eta_B1 ->
      case e1_a1Eo of wild_a220
      { SomeException @ e2_a26v $dException1_a26w e3_a26x ->
      case eqTypeRep
             (($p1Exception $dException1_a26w) `cast` <Co:4>)
             $fExceptionIOException4
      of {
        Nothing -> raiseIO# wild_a220 eta_B1;
        Just ds1_a26Z ->
          case ds1_a26Z of { HRefl co_a274 co1_a275 ->
          (# eta_B1, getLibDir3 #)
          }
      }
      }

-- RHS size: {terms: 5, types: 4, coercions: 0, joins: 0/0}
getLibDir1 = \ eta_X1Fl -> catch# getLibDir5 getLibDir2 eta_X1Fl

-- RHS size: {terms: 1, types: 0, coercions: 3, joins: 0/0}
getLibDir = getLibDir1 `cast` <Co:3>

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
getDynLibDir7 = "ecs_dynlibdir"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
getDynLibDir6 = unpackCString# getDynLibDir7

-- RHS size: {terms: 23, types: 40, coercions: 8, joins: 0/0}
getDynLibDir5
  = \ s_a27o ->
      case ((noinline getForeignEncoding) `cast` <Co:2>) s_a27o of
      { (# ipv_a2fw, ipv1_a2fx #) ->
      case charIsRepresentable3
             ipv1_a2fx getDynLibDir6 (getEnv3 `cast` <Co:6>) ipv_a2fw
      of
      { (# ipv2_a2hQ, ipv3_a2hR #) ->
      case ipv3_a2hR of {
        Nothing -> getEnv2 getDynLibDir6 ipv2_a2hQ;
        Just x_a2hX -> (# ipv2_a2hQ, x_a2hX #)
      }
      }
      }

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
getDynLibDir4
  = "/home/matthias/projects/haskell/ecs/.stack-work/install/x86_64-linux/e1eb528d6f8833bd973f1c3cf17f694ca918906ed63718d4a96c0cee13d0c40a/8.8.2/lib/x86_64-linux-ghc-8.8.2"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
getDynLibDir3 = unpackCString# getDynLibDir4

-- RHS size: {terms: 21, types: 63, coercions: 4, joins: 0/0}
getDynLibDir2
  = \ e1_a1Eo eta_B1 ->
      case e1_a1Eo of wild_a220
      { SomeException @ e2_a26v $dException1_a26w e3_a26x ->
      case eqTypeRep
             (($p1Exception $dException1_a26w) `cast` <Co:4>)
             $fExceptionIOException4
      of {
        Nothing -> raiseIO# wild_a220 eta_B1;
        Just ds1_a26Z ->
          case ds1_a26Z of { HRefl co_a274 co1_a275 ->
          (# eta_B1, getDynLibDir3 #)
          }
      }
      }

-- RHS size: {terms: 5, types: 4, coercions: 0, joins: 0/0}
getDynLibDir1
  = \ eta_X1Fo -> catch# getDynLibDir5 getDynLibDir2 eta_X1Fo

-- RHS size: {terms: 1, types: 0, coercions: 3, joins: 0/0}
getDynLibDir = getDynLibDir1 `cast` <Co:3>

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
getDataDir7 = "ecs_datadir"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
getDataDir6 = unpackCString# getDataDir7

-- RHS size: {terms: 23, types: 40, coercions: 8, joins: 0/0}
getDataDir5
  = \ s_a27o ->
      case ((noinline getForeignEncoding) `cast` <Co:2>) s_a27o of
      { (# ipv_a2fw, ipv1_a2fx #) ->
      case charIsRepresentable3
             ipv1_a2fx getDataDir6 (getEnv3 `cast` <Co:6>) ipv_a2fw
      of
      { (# ipv2_a2hQ, ipv3_a2hR #) ->
      case ipv3_a2hR of {
        Nothing -> getEnv2 getDataDir6 ipv2_a2hQ;
        Just x_a2hX -> (# ipv2_a2hQ, x_a2hX #)
      }
      }
      }

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
getDataDir4
  = "/home/matthias/projects/haskell/ecs/.stack-work/install/x86_64-linux/e1eb528d6f8833bd973f1c3cf17f694ca918906ed63718d4a96c0cee13d0c40a/8.8.2/share/x86_64-linux-ghc-8.8.2/ecs-0.1.0.0"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
getDataDir3 = unpackCString# getDataDir4

-- RHS size: {terms: 21, types: 63, coercions: 4, joins: 0/0}
getDataDir2
  = \ e1_a1Eo eta_B1 ->
      case e1_a1Eo of wild_a220
      { SomeException @ e2_a26v $dException1_a26w e3_a26x ->
      case eqTypeRep
             (($p1Exception $dException1_a26w) `cast` <Co:4>)
             $fExceptionIOException4
      of {
        Nothing -> raiseIO# wild_a220 eta_B1;
        Just ds1_a26Z ->
          case ds1_a26Z of { HRefl co_a274 co1_a275 ->
          (# eta_B1, getDataDir3 #)
          }
      }
      }

-- RHS size: {terms: 5, types: 4, coercions: 0, joins: 0/0}
getDataDir1 = \ eta_X1Fr -> catch# getDataDir5 getDataDir2 eta_X1Fr

-- RHS size: {terms: 1, types: 0, coercions: 3, joins: 0/0}
getDataDir = getDataDir1 `cast` <Co:3>

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
getDataFileName2 = "/"#

-- RHS size: {terms: 15, types: 25, coercions: 0, joins: 0/0}
getDataFileName1
  = \ name_a1lS s_a2is ->
      case catch# getDataDir5 getDataDir2 s_a2is of
      { (# ipv_a2iv, ipv1_a2iw #) ->
      (# ipv_a2iv,
         ++ ipv1_a2iw (unpackAppendCString# getDataFileName2 name_a1lS) #)
      }

-- RHS size: {terms: 1, types: 0, coercions: 5, joins: 0/0}
getDataFileName = getDataFileName1 `cast` <Co:5>

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
getLibexecDir7 = "ecs_libexecdir"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
getLibexecDir6 = unpackCString# getLibexecDir7

-- RHS size: {terms: 23, types: 40, coercions: 8, joins: 0/0}
getLibexecDir5
  = \ s_a27o ->
      case ((noinline getForeignEncoding) `cast` <Co:2>) s_a27o of
      { (# ipv_a2fw, ipv1_a2fx #) ->
      case charIsRepresentable3
             ipv1_a2fx getLibexecDir6 (getEnv3 `cast` <Co:6>) ipv_a2fw
      of
      { (# ipv2_a2hQ, ipv3_a2hR #) ->
      case ipv3_a2hR of {
        Nothing -> getEnv2 getLibexecDir6 ipv2_a2hQ;
        Just x_a2hX -> (# ipv2_a2hQ, x_a2hX #)
      }
      }
      }

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
getLibexecDir4
  = "/home/matthias/projects/haskell/ecs/.stack-work/install/x86_64-linux/e1eb528d6f8833bd973f1c3cf17f694ca918906ed63718d4a96c0cee13d0c40a/8.8.2/libexec/x86_64-linux-ghc-8.8.2/ecs-0.1.0.0"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
getLibexecDir3 = unpackCString# getLibexecDir4

-- RHS size: {terms: 21, types: 63, coercions: 4, joins: 0/0}
getLibexecDir2
  = \ e1_a1Eo eta_B1 ->
      case e1_a1Eo of wild_a220
      { SomeException @ e2_a26v $dException1_a26w e3_a26x ->
      case eqTypeRep
             (($p1Exception $dException1_a26w) `cast` <Co:4>)
             $fExceptionIOException4
      of {
        Nothing -> raiseIO# wild_a220 eta_B1;
        Just ds1_a26Z ->
          case ds1_a26Z of { HRefl co_a274 co1_a275 ->
          (# eta_B1, getLibexecDir3 #)
          }
      }
      }

-- RHS size: {terms: 5, types: 4, coercions: 0, joins: 0/0}
getLibexecDir1
  = \ eta_X1Fx -> catch# getLibexecDir5 getLibexecDir2 eta_X1Fx

-- RHS size: {terms: 1, types: 0, coercions: 3, joins: 0/0}
getLibexecDir = getLibexecDir1 `cast` <Co:3>

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
getSysconfDir7 = "ecs_sysconfdir"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
getSysconfDir6 = unpackCString# getSysconfDir7

-- RHS size: {terms: 23, types: 40, coercions: 8, joins: 0/0}
getSysconfDir5
  = \ s_a27o ->
      case ((noinline getForeignEncoding) `cast` <Co:2>) s_a27o of
      { (# ipv_a2fw, ipv1_a2fx #) ->
      case charIsRepresentable3
             ipv1_a2fx getSysconfDir6 (getEnv3 `cast` <Co:6>) ipv_a2fw
      of
      { (# ipv2_a2hQ, ipv3_a2hR #) ->
      case ipv3_a2hR of {
        Nothing -> getEnv2 getSysconfDir6 ipv2_a2hQ;
        Just x_a2hX -> (# ipv2_a2hQ, x_a2hX #)
      }
      }
      }

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
getSysconfDir4
  = "/home/matthias/projects/haskell/ecs/.stack-work/install/x86_64-linux/e1eb528d6f8833bd973f1c3cf17f694ca918906ed63718d4a96c0cee13d0c40a/8.8.2/etc"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
getSysconfDir3 = unpackCString# getSysconfDir4

-- RHS size: {terms: 21, types: 63, coercions: 4, joins: 0/0}
getSysconfDir2
  = \ e1_a1Eo eta_B1 ->
      case e1_a1Eo of wild_a220
      { SomeException @ e2_a26v $dException1_a26w e3_a26x ->
      case eqTypeRep
             (($p1Exception $dException1_a26w) `cast` <Co:4>)
             $fExceptionIOException4
      of {
        Nothing -> raiseIO# wild_a220 eta_B1;
        Just ds1_a26Z ->
          case ds1_a26Z of { HRefl co_a274 co1_a275 ->
          (# eta_B1, getSysconfDir3 #)
          }
      }
      }

-- RHS size: {terms: 5, types: 4, coercions: 0, joins: 0/0}
getSysconfDir1
  = \ eta_X1FA -> catch# getSysconfDir5 getSysconfDir2 eta_X1FA

-- RHS size: {terms: 1, types: 0, coercions: 3, joins: 0/0}
getSysconfDir = getSysconfDir1 `cast` <Co:3>



[2 of 8] Compiling Storage

==================== Tidy Core ====================
Result size of Tidy Core
  = {terms: 120, types: 250, coercions: 0, joins: 0/0}

-- RHS size: {terms: 8, types: 25, coercions: 0, joins: 0/0}
$p1StorageSet
  = \ @ m_a3MU[sk:1] @ s_a3MV[sk:1] @ e_a3MW[sk:1] v_B1 ->
      case v_B1 of v_B1 { C:StorageSet v_B2 v_B3 -> v_B2 }

-- RHS size: {terms: 8, types: 25, coercions: 0, joins: 0/0}
set
  = \ @ m_a3MU[sk:1] @ s_a3MV[sk:1] @ e_a3MW[sk:1] v_B1 ->
      case v_B1 of v_B1 { C:StorageSet v_B2 v_B3 -> v_B3 }

-- RHS size: {terms: 8, types: 24, coercions: 0, joins: 0/0}
$p1StorageGet
  = \ @ m_a3MX[sk:1] @ s_a3MY[sk:1] @ e_a3MZ[sk:1] v_B1 ->
      case v_B1 of v_B1 { C:StorageGet v_B2 v_B3 -> v_B2 }

-- RHS size: {terms: 8, types: 24, coercions: 0, joins: 0/0}
get
  = \ @ m_a3MX[sk:1] @ s_a3MY[sk:1] @ e_a3MZ[sk:1] v_B1 ->
      case v_B1 of v_B1 { C:StorageGet v_B2 v_B3 -> v_B3 }

-- RHS size: {terms: 7, types: 21, coercions: 0, joins: 0/0}
$p1Storage
  = \ @ m_a3N0[sk:1] @ s_a3N1[sk:1] v_B1 ->
      case v_B1 of v_B1 { C:Storage v_B2 v_B3 -> v_B2 }

-- RHS size: {terms: 7, types: 21, coercions: 0, joins: 0/0}
iter
  = \ @ m_a3N0[sk:1] @ s_a3N1[sk:1] v_B1 ->
      case v_B1 of v_B1 { C:Storage v_B2 v_B3 -> v_B3 }

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$trModule2 = "ecs-0.1.0.0-6T7cQlWNmHv6W00SuOHZxa"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$trModule1 = TrNameS $trModule2

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tcStorage3 = "Storage"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tcStorage2 = TrNameS $tcStorage3

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$trModule = Module $trModule1 $tcStorage2

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
$krep_r44y = KindRepTyConApp $tcConstraint []

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep1_r44z = KindRepFun krep$* $krep_r44y

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep2_r44A = KindRepFun krep$* $krep1_r44z

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tcStorage1 = KindRepFun krep$*Arr* $krep1_r44z

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tcStorageGet1 = KindRepFun krep$*Arr* $krep2_r44A

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tcStorage
  = TyCon
      5189453513905196517##
      8338336852055007055##
      $trModule
      $tcStorage2
      0#
      $tcStorage1

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tcStorageGet3 = "StorageGet"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tcStorageGet2 = TrNameS $tcStorageGet3

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tcStorageGet
  = TyCon
      8075925299506377297##
      12701237503727818770##
      $trModule
      $tcStorageGet2
      0#
      $tcStorageGet1

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tcStorageSet2 = "StorageSet"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tcStorageSet1 = TrNameS $tcStorageSet2

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tcStorageSet
  = TyCon
      307797211646198511##
      14141429703145738251##
      $trModule
      $tcStorageSet1
      0#
      $tcStorageGet1



[3 of 8] Compiling Types

==================== Tidy Core ====================
Result size of Tidy Core
  = {terms: 414, types: 545, coercions: 132, joins: 0/0}

-- RHS size: {terms: 3, types: 4, coercions: 2, joins: 0/0}
simplifyHList = \ @ as_a4er[sk:1] v_B1 -> v_B1 `cast` <Co:2>

-- RHS size: {terms: 3, types: 4, coercions: 2, joins: 0/0}
unsimplifyHList = \ @ as_a4es[sk:1] v_B1 -> v_B1 `cast` <Co:2>

-- RHS size: {terms: 1, types: 3, coercions: 3, joins: 0/0}
$WHNil = HNil @~ <Co:3>

-- RHS size: {terms: 7, types: 15, coercions: 5, joins: 0/0}
$WHCons
  = \ @ a_X4f5 @ as_X4f7 dt_a4gS dt_a4gT ->
      HCons @~ <Co:5> dt_a4gS dt_a4gT

-- RHS size: {terms: 1, types: 1, coercions: 1, joins: 0/0}
$WSingZ = SingZ @~ <Co:1>

-- RHS size: {terms: 4, types: 6, coercions: 2, joins: 0/0}
$WSingS = \ @ n_X4f8 dt_a4gf -> SingS @~ <Co:2> dt_a4gf

-- RHS size: {terms: 10, types: 55, coercions: 10, joins: 0/0}
$fUnsimplifyHLista_$cunsimplifyHList
  = \ @ a_a5n5 $d~_a5n6 eta_B1 ->
      case eq_sel $d~_a5n6 of co_a5nI { __DEFAULT ->
      (HCons @~ <Co:7> eta_B1 $WHNil) `cast` <Co:3>
      }

-- RHS size: {terms: 1, types: 0, coercions: 20, joins: 0/0}
$fUnsimplifyHLista
  = $fUnsimplifyHLista_$cunsimplifyHList `cast` <Co:20>

-- RHS size: {terms: 11, types: 23, coercions: 13, joins: 0/0}
$fUnsimplifyHList:>_$cunsimplifyHList
  = \ @ as_a5mK @ a_a5mL $dUnsimplifyHList_a5mM eta_B1 ->
      case eta_B1 of { :> a1_a4fg as1_a4fh ->
      (HCons
         @~ <Co:6>
         a1_a4fg
         (($dUnsimplifyHList_a5mM `cast` <Co:2>) as1_a4fh))
      `cast` <Co:5>
      }

-- RHS size: {terms: 1, types: 0, coercions: 14, joins: 0/0}
$fUnsimplifyHList:>
  = $fUnsimplifyHList:>_$cunsimplifyHList `cast` <Co:14>

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
lvl_r5td = "src/Types.hs:75:5-33|function simplifyHList"#

-- RHS size: {terms: 3, types: 4, coercions: 0, joins: 0/0}
lvl1_r5te = \ @ a_a5mx -> patError lvl_r5td

-- RHS size: {terms: 13, types: 84, coercions: 6, joins: 0/0}
$fSimplifyHLista_$csimplifyHList
  = \ @ a_a5mx $d~_a5my eta_B1 ->
      case eq_sel $d~_a5my of co_a5nq { __DEFAULT ->
      case eta_B1 of {
        HNil ipv_s5pR -> lvl1_r5te;
        HCons @ a1_a5mD @ as_a5mE co1_a5mF a2_a4fd ds_d5pp ->
          a2_a4fd `cast` <Co:6>
      }
      }

-- RHS size: {terms: 1, types: 0, coercions: 20, joins: 0/0}
$fSimplifyHLista = $fSimplifyHLista_$csimplifyHList `cast` <Co:20>

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
lvl2_r5tf = "src/Types.hs:79:5-54|function simplifyHList"#

-- RHS size: {terms: 4, types: 8, coercions: 0, joins: 0/0}
lvl3_r5tg = \ @ as_a5me @ a_a5mf -> patError lvl2_r5tf

-- RHS size: {terms: 13, types: 58, coercions: 20, joins: 0/0}
$fSimplifyHList:>_$csimplifyHList
  = \ @ as_a5me @ a_a5mf $dSimplifyHList_a5mg ds_d5on ->
      case ds_d5on of {
        HNil ipv_s5pY -> lvl3_r5tg;
        HCons @ a1_a5ml @ as1_a5mm co_a5mn a2_a4fa as2_a4fb ->
          (:>
             a2_a4fa
             (($dSimplifyHList_a5mg `cast` <Co:2>) (as2_a4fb `cast` <Co:8>)))
          `cast` <Co:10>
      }

-- RHS size: {terms: 1, types: 0, coercions: 14, joins: 0/0}
$fSimplifyHList:>
  = $fSimplifyHList:>_$csimplifyHList `cast` <Co:14>

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$trModule4 = "ecs-0.1.0.0-6T7cQlWNmHv6W00SuOHZxa"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$trModule3 = TrNameS $trModule4

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$trModule2 = "Types"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$trModule1 = TrNameS $trModule2

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$trModule = Module $trModule3 $trModule1

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$krep_r5th = KindRepVar 0#

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
$krep1_r5ti = KindRepTyConApp $tcConstraint []

-- RHS size: {terms: 3, types: 2, coercions: 0, joins: 0/0}
$krep2_r5tj = : krep$* []

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep3_r5tk = KindRepTyConApp $tc[] $krep2_r5tj

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tcHList1 = KindRepFun $krep3_r5tk krep$*

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tcSimplifyHList1 = KindRepFun krep$* $krep1_r5ti

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$krep4_r5tl = KindRepVar 1#

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep5_r5tm = KindRepTyConApp $tc'[] $krep2_r5tj

-- RHS size: {terms: 3, types: 2, coercions: 0, joins: 0/0}
$krep6_r5tn = : $krep4_r5tl []

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
$krep7_r5to = : $krep_r5th $krep6_r5tn

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
$krep8_r5tp = : krep$* $krep7_r5to

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep9_r5tq = KindRepTyConApp $tc': $krep8_r5tp

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tcNat2 = "Nat"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tcNat1 = TrNameS $tcNat2

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tcNat
  = TyCon
      9787096750819552233##
      14562795403823518016##
      $trModule
      $tcNat1
      0#
      krep$*

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
$tc'Z1 = KindRepTyConApp $tcNat []

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tc'Z3 = "'Z"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tc'Z2 = TrNameS $tc'Z3

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tc'Z
  = TyCon
      1453311367276839733##
      7111613143057673720##
      $trModule
      $tc'Z2
      0#
      $tc'Z1

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
$krep10_r5tr = KindRepTyConApp $tc'Z []

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tcSNat1 = KindRepFun $tc'Z1 krep$*

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tc'S1 = KindRepFun $tc'Z1 $tc'Z1

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tc'S3 = "'S"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tc'S2 = TrNameS $tc'S3

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tc'S
  = TyCon
      3147137578730442212##
      5037998957080752719##
      $trModule
      $tc'S2
      0#
      $tc'S1

-- RHS size: {terms: 3, types: 2, coercions: 0, joins: 0/0}
$krep11_r5ts = : $krep_r5th []

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep12_r5tt = KindRepTyConApp $tc'S $krep11_r5ts

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tcSNat3 = "SNat"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tcSNat2 = TrNameS $tcSNat3

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tcSNat
  = TyCon
      15727921468944175253##
      6573240200436968834##
      $trModule
      $tcSNat2
      0#
      $tcSNat1

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep13_r5tu = KindRepTyConApp $tcSNat $krep11_r5ts

-- RHS size: {terms: 3, types: 2, coercions: 0, joins: 0/0}
$krep14_r5tv = : $krep12_r5tt []

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep15_r5tw = KindRepTyConApp $tcSNat $krep14_r5tv

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tc'SingS1 = KindRepFun $krep13_r5tu $krep15_r5tw

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tc'SingS3 = "'SingS"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tc'SingS2 = TrNameS $tc'SingS3

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tc'SingS
  = TyCon
      655760552433940595##
      6445228129480776894##
      $trModule
      $tc'SingS2
      1#
      $tc'SingS1

-- RHS size: {terms: 3, types: 2, coercions: 0, joins: 0/0}
$krep16_r5tx = : $krep10_r5tr []

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tc'SingZ1 = KindRepTyConApp $tcSNat $krep16_r5tx

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tc'SingZ3 = "'SingZ"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tc'SingZ2 = TrNameS $tc'SingZ3

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tc'SingZ
  = TyCon
      285004052306958530##
      15657081553532898953##
      $trModule
      $tc'SingZ2
      0#
      $tc'SingZ1

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tcHList3 = "HList"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tcHList2 = TrNameS $tcHList3

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tcHList
  = TyCon
      8957249829500015820##
      8502105249894919873##
      $trModule
      $tcHList2
      0#
      $tcHList1

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep17_r5ty = KindRepTyConApp $tcHList $krep6_r5tn

-- RHS size: {terms: 3, types: 2, coercions: 0, joins: 0/0}
$krep18_r5tz = : $krep9_r5tq []

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep19_r5tA = KindRepTyConApp $tcHList $krep18_r5tz

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep20_r5tB = KindRepFun $krep17_r5ty $krep19_r5tA

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tc'HCons1 = KindRepFun $krep_r5th $krep20_r5tB

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tc'HCons3 = "'HCons"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tc'HCons2 = TrNameS $tc'HCons3

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tc'HCons
  = TyCon
      12930761376598298613##
      12054478677434131681##
      $trModule
      $tc'HCons2
      2#
      $tc'HCons1

-- RHS size: {terms: 3, types: 2, coercions: 0, joins: 0/0}
$krep21_r5tC = : $krep5_r5tm []

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tc'HNil1 = KindRepTyConApp $tcHList $krep21_r5tC

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tc'HNil3 = "'HNil"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tc'HNil2 = TrNameS $tc'HNil3

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tc'HNil
  = TyCon
      12042389135663178914##
      16512656995326866555##
      $trModule
      $tc'HNil2
      0#
      $tc'HNil1

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tc:>2 = ":>"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tc:>1 = TrNameS $tc:>2

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tc:>
  = TyCon
      12740124230616469275##
      1492150465282601836##
      $trModule
      $tc:>1
      0#
      krep$*->*->*

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep22_r5tD = KindRepTyConApp $tc:> $krep7_r5to

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep23_r5tE = KindRepFun $krep4_r5tl $krep22_r5tD

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tc':>1 = KindRepFun $krep_r5th $krep23_r5tE

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tc':>3 = "':>"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tc':>2 = TrNameS $tc':>3

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tc':>
  = TyCon
      11420886492468546436##
      18428826291743840505##
      $trModule
      $tc':>2
      2#
      $tc':>1

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tcUnsimplifyHList2 = "UnsimplifyHList"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tcUnsimplifyHList1 = TrNameS $tcUnsimplifyHList2

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tcUnsimplifyHList
  = TyCon
      15669068492814156776##
      4466926639957741098##
      $trModule
      $tcUnsimplifyHList1
      0#
      $tcSimplifyHList1

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tcSimplifyHList3 = "SimplifyHList"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tcSimplifyHList2 = TrNameS $tcSimplifyHList3

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tcSimplifyHList
  = TyCon
      14322478174405957905##
      8608020719636508953##
      $trModule
      $tcSimplifyHList2
      0#
      $tcSimplifyHList1



[4 of 8] Compiling World

==================== Tidy Core ====================
Result size of Tidy Core
  = {terms: 620, types: 1,960, coercions: 471, joins: 0/4}

-- RHS size: {terms: 3, types: 3, coercions: 2, joins: 0/0}
filterIf = \ @ b_a5M1[sk:1] v_B1 -> v_B1 `cast` <Co:2>

-- RHS size: {terms: 4, types: 8, coercions: 3, joins: 0/0}
filterWorldSingle
  = \ @ f_a5M4[sk:1] @ as_a5M5[sk:1] v_B1 -> v_B1 `cast` <Co:3>

-- RHS size: {terms: 7, types: 32, coercions: 0, joins: 0/0}
$p1FilterWorld
  = \ @ fs_a5M6[sk:1] @ as_a5M7[sk:1] v_B1 ->
      case v_B1 of v_B1 { C:FilterWorld v_B2 v_B3 -> v_B2 }

-- RHS size: {terms: 7, types: 32, coercions: 0, joins: 0/0}
filterWorld
  = \ @ fs_a5M6[sk:1] @ as_a5M7[sk:1] v_B1 ->
      case v_B1 of v_B1 { C:FilterWorld v_B2 v_B3 -> v_B3 }

-- RHS size: {terms: 7, types: 14, coercions: 0, joins: 0/0}
$WWorldIndex
  = \ @ n_X5Mq @ ss_a5Mq dt_a5PD dt_a5PE ->
      WorldIndex dt_a5PD dt_a5PE

-- RHS size: {terms: 10, types: 36, coercions: 4, joins: 0/0}
$fStoragemWorld_$citer
  = \ @ m_a643 $dMonad_a644 ds_d6fy _ ->
      case ds_d6fy `cast` <Co:4> of { HNil co_a64d ->
      return $dMonad_a644 ()
      }

-- RHS size: {terms: 6, types: 12, coercions: 0, joins: 0/0}
$fStoragemWorld
  = \ @ m_a643 $dMonad_a644 ->
      C:Storage $dMonad_a644 ($fStoragemWorld_$citer $dMonad_a644)

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
lvl_r6qD = "error"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl1_r6qE = unpackCString# lvl_r6qD

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$trModule4 = "ecs-0.1.0.0-6T7cQlWNmHv6W00SuOHZxa"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl2_r6qF = unpackCString# $trModule4

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$trModule2 = "World"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl3_r6qG = unpackCString# $trModule2

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
lvl4_r6qH = "src/World.hs"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl5_r6qI = unpackCString# lvl4_r6qH

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl6_r6qJ = I# 43#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl7_r6qK = I# 11#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl8_r6qL = I# 32#

-- RHS size: {terms: 8, types: 0, coercions: 0, joins: 0/0}
lvl9_r6qM
  = SrcLoc
      lvl2_r6qF
      lvl3_r6qG
      lvl5_r6qI
      lvl6_r6qJ
      lvl7_r6qK
      lvl6_r6qJ
      lvl8_r6qL

-- RHS size: {terms: 4, types: 0, coercions: 0, joins: 0/0}
lvl10_r6qN = PushCallStack lvl1_r6qE lvl9_r6qM EmptyCallStack

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
lvl11_r6qO = "Invalid index"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl12_r6qP = unpackCString# lvl11_r6qO

-- RHS size: {terms: 6, types: 18, coercions: 4, joins: 0/0}
$w$cget_r6qQ
  = \ @ m_s6mk @ e_s6ml _ ->
      error (lvl10_r6qN `cast` <Co:4>) lvl12_r6qP

-- RHS size: {terms: 5, types: 10, coercions: 0, joins: 0/0}
$cget_r6qR = \ @ m_a63M @ e_a63N _ -> $w$cget_r6qQ void#

-- RHS size: {terms: 1, types: 0, coercions: 25, joins: 0/0}
$fStorageGetmWorlde_$cget = $cget_r6qR `cast` <Co:25>

-- RHS size: {terms: 8, types: 17, coercions: 0, joins: 0/0}
$fStorageGetmWorlde
  = \ @ m_X64P @ e_X64R $dMonad_X64T ->
      C:StorageGet
        ($fStoragemWorld $dMonad_X64T)
        ($fStorageGetmWorlde_$cget $dMonad_X64T)

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl13_r6qS = I# 52#

-- RHS size: {terms: 8, types: 0, coercions: 0, joins: 0/0}
lvl14_r6qT
  = SrcLoc
      lvl2_r6qF
      lvl3_r6qG
      lvl5_r6qI
      lvl13_r6qS
      lvl7_r6qK
      lvl13_r6qS
      lvl8_r6qL

-- RHS size: {terms: 4, types: 0, coercions: 0, joins: 0/0}
lvl15_r6qU = PushCallStack lvl1_r6qE lvl14_r6qT EmptyCallStack

-- RHS size: {terms: 6, types: 19, coercions: 4, joins: 0/0}
$w$cset_r6qV
  = \ @ m_s6mo @ e_s6mp _ ->
      error (lvl15_r6qU `cast` <Co:4>) lvl12_r6qP

-- RHS size: {terms: 5, types: 10, coercions: 0, joins: 0/0}
$cset_r6qW = \ @ m_a61U @ e_a61V _ -> $w$cset_r6qV void#

-- RHS size: {terms: 1, types: 0, coercions: 26, joins: 0/0}
$fStorageSetmWorlde_$cset = $cset_r6qW `cast` <Co:26>

-- RHS size: {terms: 8, types: 17, coercions: 0, joins: 0/0}
$fStorageSetmWorlde
  = \ @ m_X634 @ e_X636 $dMonad_X638 ->
      C:StorageSet
        ($fStoragemWorld $dMonad_X638)
        ($fStorageSetmWorlde_$cset $dMonad_X638)

-- RHS size: {terms: 33, types: 101, coercions: 67, joins: 0/0}
$fStorageGetmWorlde0_$citer
  = \ @ m_a60P
      @ a_a60Q
      @ as_a60R
      $dMonad_a60S
      $dStorage_a60T
      $dStorage1_a60U
      ds_d6eg
      f_a5Nm ->
      case ds_d6eg `cast` <Co:6> of
      { HCons @ a1_a613 @ as1_a614 co_a615 s_a5Nk ss_a5Nl ->
      >>
        $dMonad_a60S
        (iter
           ($dStorage_a60T `cast` <Co:4>)
           s_a5Nk
           ((\ i_a5Np ->
               f_a5Nm ((WorldIndex $WSingZ (i_a5Np `cast` <Co:7>)) `cast` <Co:8>))
            `cast` <Co:6>))
        (iter
           ($dStorage1_a60U `cast` <Co:5>)
           (ss_a5Nl `cast` <Co:3>)
           ((\ ds1_d6fb ->
               case ds1_d6fb of { WorldIndex @ n_a61c p_a5Nq i_a5Nr ->
               f_a5Nm
                 ((WorldIndex ($WSingS p_a5Nq) (i_a5Nr `cast` <Co:8>))
                  `cast` <Co:8>)
               })
            `cast` <Co:12>))
      }

-- RHS size: {terms: 12, types: 28, coercions: 0, joins: 0/0}
$fStoragemWorld0
  = \ @ m_a60P
      @ a_a60Q
      @ as_a60R
      $dMonad_a60S
      $dStorage_a60T
      $dStorage1_a60U ->
      C:Storage
        $dMonad_a60S
        ($fStorageGetmWorlde0_$citer
           $dMonad_a60S $dStorage_a60T $dStorage1_a60U)

-- RHS size: {terms: 28, types: 108, coercions: 72, joins: 0/0}
$fStorageGetmWorlde0_$cget
  = \ @ m_a600
      @ a_a601
      @ e_a602
      @ as_a603
      $dStorageGet_a604
      $dStorageGet1_a605
      eta_B2
      eta1_X2f ->
      case eta_B2 `cast` <Co:6> of
      { HCons @ a1_a60e @ as1_a60f co_a60g s_a5Na ds_d6e7 ->
      case eta1_X2f `cast` <Co:7> of
      { WorldIndex @ n_a60j ds1_d6e9 i_a5Nb ->
      case ds1_d6e9 of {
        SingZ co1_a60k ->
          get
            ($dStorageGet_a604 `cast` <Co:5>) s_a5Na (i_a5Nb `cast` <Co:19>);
        SingS @ n1_a60z co1_a60A n2_a5Nd ->
          get
            ($dStorageGet1_a605 `cast` <Co:6>)
            (ds_d6e7 `cast` <Co:3>)
            ((WorldIndex n2_a5Nd (i_a5Nb `cast` <Co:17>)) `cast` <Co:9>)
      }
      }
      }

-- RHS size: {terms: 18, types: 44, coercions: 0, joins: 0/1}
$fStorageGetmWorlde0_$cp1StorageGet
  = \ @ m_s6ms @ a_s6mt @ e_s6mu @ as_s6mv w_s6mw w1_s6mx ->
      let { $dStorage_s6iC = $p1StorageGet w_s6mw } in
      C:Storage
        ($p1Storage $dStorage_s6iC)
        ($fStorageGetmWorlde0_$citer
           ($p1Storage $dStorage_s6iC) $dStorage_s6iC ($p1StorageGet w1_s6mx))

-- RHS size: {terms: 13, types: 36, coercions: 0, joins: 0/0}
$fStorageGetmWorlde0
  = \ @ m_a600
      @ a_a601
      @ e_a602
      @ as_a603
      $dStorageGet_a604
      $dStorageGet1_a605 ->
      C:StorageGet
        ($fStorageGetmWorlde0_$cp1StorageGet
           $dStorageGet_a604 $dStorageGet1_a605)
        ($fStorageGetmWorlde0_$cget $dStorageGet_a604 $dStorageGet1_a605)

-- RHS size: {terms: 28, types: 108, coercions: 72, joins: 0/0}
$fStorageSetmWorlde0_$cset
  = \ @ m_a5Zb
      @ a_a5Zc
      @ e_a5Zd
      @ as_a5Ze
      $dStorageSet_a5Zf
      $dStorageSet1_a5Zg
      eta_B2
      eta1_X2i ->
      case eta_B2 `cast` <Co:6> of
      { HCons @ a1_a5Zp @ as1_a5Zq co_a5Zr s_a5MK ds_d6bo ->
      case eta1_X2i `cast` <Co:7> of
      { WorldIndex @ n_a5Zu ds1_d6bq i_a5ML ->
      case ds1_d6bq of {
        SingZ co1_a5Zv ->
          set
            ($dStorageSet_a5Zf `cast` <Co:5>) s_a5MK (i_a5ML `cast` <Co:19>);
        SingS @ n1_a5ZK co1_a5ZL n2_a5MN ->
          set
            ($dStorageSet1_a5Zg `cast` <Co:6>)
            (ds_d6bo `cast` <Co:3>)
            ((WorldIndex n2_a5MN (i_a5ML `cast` <Co:17>)) `cast` <Co:9>)
      }
      }
      }

-- RHS size: {terms: 18, types: 44, coercions: 0, joins: 0/1}
$fStorageSetmWorlde0_$cp1StorageSet
  = \ @ m_s6mC @ a_s6mD @ e_s6mE @ as_s6mF w_s6mG w1_s6mH ->
      let { $dStorage_s6iA = $p1StorageSet w_s6mG } in
      C:Storage
        ($p1Storage $dStorage_s6iA)
        ($fStorageGetmWorlde0_$citer
           ($p1Storage $dStorage_s6iA) $dStorage_s6iA ($p1StorageSet w1_s6mH))

-- RHS size: {terms: 13, types: 36, coercions: 0, joins: 0/0}
$fStorageSetmWorlde0
  = \ @ m_a5Zb
      @ a_a5Zc
      @ e_a5Zd
      @ as_a5Ze
      $dStorageSet_a5Zf
      $dStorageSet1_a5Zg ->
      C:StorageSet
        ($fStorageSetmWorlde0_$cp1StorageSet
           $dStorageSet_a5Zf $dStorageSet1_a5Zg)
        ($fStorageSetmWorlde0_$cset $dStorageSet_a5Zf $dStorageSet1_a5Zg)

-- RHS size: {terms: 9, types: 33, coercions: 4, joins: 0/0}
$fFilterWorld[]as_$cfilterWorld
  = \ @ as_a5YX $d~_a5YY _ eta1_X1B ->
      case eq_sel $d~_a5YY of co_a65b { __DEFAULT ->
      eta1_X1B `cast` <Co:4>
      }

-- RHS size: {terms: 7, types: 24, coercions: 9, joins: 0/0}
$fFilterWorld[]as_$cp1FilterWorld
  = \ @ as_a5YX $d~_a5YY ->
      case eq_sel $d~_a5YY of co_a65b { __DEFAULT ->
      $d~_a5YY `cast` <Co:9>
      }

-- RHS size: {terms: 7, types: 16, coercions: 0, joins: 0/0}
$fFilterWorld[]as
  = \ @ as_a5YX $d~_a5YY ->
      C:FilterWorld
        ($fFilterWorld[]as_$cp1FilterWorld $d~_a5YY)
        ($fFilterWorld[]as_$cfilterWorld $d~_a5YY)

-- RHS size: {terms: 22, types: 93, coercions: 12, joins: 0/2}
$fFilterWorld:as_$cfilterWorld
  = \ @ f_a5Yd
      @ fs_a5Ye
      @ as_a5Yf
      $d~_a5Yg
      $dFilterWorldSingle_a5Yh
      $dFilterWorld_a5Yi
      _ ->
      case eq_sel $d~_a5Yg of co_a64O { __DEFAULT ->
      let { f1_s6iy = filterWorld $dFilterWorld_a5Yi Proxy } in
      let { g_s6ix = ($dFilterWorldSingle_a5Yh `cast` <Co:3>) Proxy } in
      (\ x_a68o -> f1_s6iy (g_s6ix x_a68o)) `cast` <Co:9>
      }

-- RHS size: {terms: 16, types: 64, coercions: 0, joins: 0/0}
$fFilterWorld:as
  = \ @ f_a5Yd
      @ fs_a5Ye
      @ as_a5Yf
      $d~_a5Yg
      $dFilterWorldSingle_a5Yh
      $dFilterWorld_a5Yi ->
      C:FilterWorld
        (case eq_sel $d~_a5Yg of co_a64O { __DEFAULT -> $d~_a5Yg })
        ($fFilterWorld:as_$cfilterWorld
           $d~_a5Yg $dFilterWorldSingle_a5Yh $dFilterWorld_a5Yi)

-- RHS size: {terms: 3, types: 10, coercions: 0, joins: 0/0}
$cfilterWorldSingle_r6qX = \ @ f_a5Y2 _ -> id

-- RHS size: {terms: 1, types: 0, coercions: 17, joins: 0/0}
$fFilterWorldSinglef[]_$cfilterWorldSingle
  = $cfilterWorldSingle_r6qX `cast` <Co:17>

-- RHS size: {terms: 1, types: 0, coercions: 9, joins: 0/0}
$fFilterWorldSinglef[]
  = $fFilterWorldSinglef[]_$cfilterWorldSingle `cast` <Co:9>

-- RHS size: {terms: 16, types: 63, coercions: 49, joins: 0/0}
$fFilterWorldSinglef:_$cfilterWorldSingle
  = \ @ f_a5Xr
      @ a_a5Xs
      @ as_a5Xt
      $dFilterIf_a5Xu
      $dFilterWorldSingle_a5Xv
      eta_B2
      eta1_X2s ->
      case eta1_X2s `cast` <Co:6> of
      { HCons @ a1_a5XA @ as1_a5XB co_a5XC a2_a5Mz as2_a5MA ->
      (($dFilterIf_a5Xu `cast` <Co:4>)
         Proxy
         a2_a5Mz
         ((($dFilterWorldSingle_a5Xv `cast` <Co:4>)
             eta_B2 (as2_a5MA `cast` <Co:3>))
          `cast` <Co:6>))
      `cast` <Co:26>
      }

-- RHS size: {terms: 1, types: 0, coercions: 27, joins: 0/0}
$fFilterWorldSinglef:
  = $fFilterWorldSinglef:_$cfilterWorldSingle `cast` <Co:27>

-- RHS size: {terms: 6, types: 11, coercions: 0, joins: 0/0}
$cfilterIf_r6qY = \ @ s_a5Xk @ ss_a5Xl _ _ w_a5Mu -> w_a5Mu

-- RHS size: {terms: 1, types: 0, coercions: 28, joins: 0/0}
$fFilterIfFalse_$cfilterIf = $cfilterIf_r6qY `cast` <Co:28>

-- RHS size: {terms: 1, types: 0, coercions: 3, joins: 0/0}
$fFilterIfFalse = $fFilterIfFalse_$cfilterIf `cast` <Co:3>

-- RHS size: {terms: 8, types: 13, coercions: 2, joins: 0/0}
$cfilterIf1_r6qZ
  = \ @ s_a5X7 @ ss_a5X8 _ s1_a5Ms ds1_d67y ->
      $WHCons s1_a5Ms (ds1_d67y `cast` <Co:2>)

-- RHS size: {terms: 1, types: 0, coercions: 29, joins: 0/0}
$fFilterIfTrue_$cfilterIf = $cfilterIf1_r6qZ `cast` <Co:29>

-- RHS size: {terms: 1, types: 0, coercions: 3, joins: 0/0}
$fFilterIfTrue = $fFilterIfTrue_$cfilterIf `cast` <Co:3>

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$trModule3 = TrNameS $trModule4

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$trModule1 = TrNameS $trModule2

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$trModule = Module $trModule3 $trModule1

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
$krep_r6r0 = KindRepTyConApp $tcConstraint []

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
$krep1_r6r1 = KindRepTyConApp $tcBool []

-- RHS size: {terms: 3, types: 2, coercions: 0, joins: 0/0}
$krep2_r6r2 = : krep$* []

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep3_r6r3 = KindRepTyConApp $tc[] $krep2_r6r2

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tcFilterIf1 = KindRepFun $krep1_r6r1 $krep_r6r0

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep4_r6r4 = KindRepFun $krep3_r6r3 $krep_r6r0

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tcWorld1 = KindRepFun $krep3_r6r3 krep$*

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tcFilterWorld1 = KindRepFun $krep3_r6r3 $krep4_r6r4

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tcFilterWorldSingle1 = KindRepFun krep$* $krep4_r6r4

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$krep5_r6r5 = KindRepVar 0#

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tcWorldRead1 = KindRepFun $krep5_r6r5 krep$*

-- RHS size: {terms: 3, types: 2, coercions: 0, joins: 0/0}
$krep6_r6r6 = : $krep5_r6r5 []

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep7_r6r7 = KindRepTyConApp $tcHList $krep6_r6r6

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tcWorld
  = TyCon
      1238515735882882834##
      24109011526723917##
      $trModule
      $trModule1
      0#
      $tcWorld1

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep8_r6r8 = KindRepTyConApp $tcWorld $krep6_r6r6

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tc'World1 = KindRepFun $krep7_r6r7 $krep8_r6r8

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tc'World3 = "'World"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tc'World2 = TrNameS $tc'World3

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tc'World
  = TyCon
      8744432166345674939##
      5191061379710228053##
      $trModule
      $tc'World2
      1#
      $tc'World1

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tcWorldIndex2 = "WorldIndex"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tcWorldIndex1 = TrNameS $tcWorldIndex2

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tcWorldIndex
  = TyCon
      5908128961257034075##
      9369087035609409882##
      $trModule
      $tcWorldIndex1
      0#
      $tcWorld1

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tcWorldRead3 = "WorldRead"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tcWorldRead2 = TrNameS $tcWorldRead3

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tcWorldRead
  = TyCon
      5003270797055291235##
      204008234550824823##
      $trModule
      $tcWorldRead2
      1#
      $tcWorldRead1

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tcWorldWrite2 = "WorldWrite"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tcWorldWrite1 = TrNameS $tcWorldWrite2

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tcWorldWrite
  = TyCon
      4026630623970689696##
      8802764867890491314##
      $trModule
      $tcWorldWrite1
      1#
      $tcWorldRead1

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tcFilterWorld3 = "FilterWorld"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tcFilterWorld2 = TrNameS $tcFilterWorld3

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tcFilterWorld
  = TyCon
      4206345182269720998##
      13458977015486760349##
      $trModule
      $tcFilterWorld2
      0#
      $tcFilterWorld1

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tcFilterWorldSingle3 = "FilterWorldSingle"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tcFilterWorldSingle2 = TrNameS $tcFilterWorldSingle3

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tcFilterWorldSingle
  = TyCon
      17581359974083111163##
      1278625793680963546##
      $trModule
      $tcFilterWorldSingle2
      0#
      $tcFilterWorldSingle1

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tcFilterIf3 = "FilterIf"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tcFilterIf2 = TrNameS $tcFilterIf3

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tcFilterIf
  = TyCon
      18185415031483632726##
      5220191722032320967##
      $trModule
      $tcFilterIf2
      0#
      $tcFilterIf1



[5 of 8] Compiling System

==================== Tidy Core ====================
Result size of Tidy Core
  = {terms: 464, types: 1,450, coercions: 82, joins: 0/2}

-- RHS size: {terms: 8, types: 22, coercions: 0, joins: 0/0}
$p1System
  = \ @ f_a76L[sk:1] @ m_a76M[sk:1] @ s_a76N[sk:1] v_B1 ->
      case v_B1 of v_B1 { C:System v_B2 v_B3 -> v_B2 }

-- RHS size: {terms: 8, types: 22, coercions: 0, joins: 0/0}
runSystem
  = \ @ f_a76L[sk:1] @ m_a76M[sk:1] @ s_a76N[sk:1] v_B1 ->
      case v_B1 of v_B1 { C:System v_B2 v_B3 -> v_B3 }

-- RHS size: {terms: 30, types: 51, coercions: 0, joins: 0/1}
$fSystem->ms0_$crunSystem
  = \ @ m_a7jG
      @ s_a7jH
      @ gs_a7jI
      @ ss_a7jJ
      $dStorageGet_a7jK
      $dStorageSet_a7jL
      eta_B2
      eta1_B1 ->
      case $p1StorageGet $dStorageGet_a7jK of
      { C:Storage ww1_s7ri ww2_s7rj ->
      ww2_s7rj
        eta_B2
        (\ i_a77y ->
           >>=
             ww1_s7ri
             (get $dStorageGet_a7jK eta_B2 i_a77y)
             (let { f_s7qD = set $dStorageSet_a7jL eta_B2 i_a77y } in
              \ x_a68o -> f_s7qD (eta1_B1 x_a68o)))
      }

-- RHS size: {terms: 9, types: 23, coercions: 0, joins: 0/0}
$fSystem->ms0_$cp1System
  = \ @ m_a7jG @ s_a7jH @ gs_a7jI @ ss_a7jJ $dStorageGet_a7jK _ ->
      $p1Storage ($p1StorageGet $dStorageGet_a7jK)

-- RHS size: {terms: 13, types: 30, coercions: 0, joins: 0/0}
$fSystem->ms0
  = \ @ m_a7jG
      @ s_a7jH
      @ gs_a7jI
      @ ss_a7jJ
      $dStorageGet_a7jK
      $dStorageSet_a7jL ->
      C:System
        ($fSystem->ms0_$cp1System $dStorageGet_a7jK $dStorageSet_a7jL)
        ($fSystem->ms0_$crunSystem $dStorageGet_a7jK $dStorageSet_a7jL)

-- RHS size: {terms: 20, types: 39, coercions: 0, joins: 0/0}
$fSystem->ms_$crunSystem
  = \ @ m_a7je @ s_a7jf @ gs_a7jg $dStorageGet_a7jh eta_B2 eta1_B1 ->
      case $p1StorageGet $dStorageGet_a7jh of
      { C:Storage ww1_s7rn ww2_s7ro ->
      ww2_s7ro
        eta_B2
        (\ i_a77a ->
           >>= ww1_s7rn (get $dStorageGet_a7jh eta_B2 i_a77a) eta1_B1)
      }

-- RHS size: {terms: 7, types: 17, coercions: 0, joins: 0/0}
$fSystem->ms_$cp1System
  = \ @ m_a7je @ s_a7jf @ gs_a7jg $dStorageGet_a7jh ->
      $p1Storage ($p1StorageGet $dStorageGet_a7jh)

-- RHS size: {terms: 9, types: 23, coercions: 0, joins: 0/0}
$fSystem->ms
  = \ @ m_a7je @ s_a7jf @ gs_a7jg $dStorageGet_a7jh ->
      C:System
        ($fSystem->ms_$cp1System $dStorageGet_a7jh)
        ($fSystem->ms_$crunSystem $dStorageGet_a7jh)

-- RHS size: {terms: 9, types: 18, coercions: 0, joins: 0/0}
$fSystem->mFilteredWorld2_$crunSystem
  = \ @ m_a7iX @ gs_a7iY @ ss_a7iZ $dMonad_a7j0 _ _ ->
      return $dMonad_a7j0 ()

-- RHS size: {terms: 8, types: 20, coercions: 0, joins: 0/0}
$fSystem->mFilteredWorld2
  = \ @ m_a7iX @ gs_a7iY @ ss_a7iZ $dMonad_a7j0 ->
      C:System
        $dMonad_a7j0 ($fSystem->mFilteredWorld2_$crunSystem $dMonad_a7j0)

-- RHS size: {terms: 23, types: 79, coercions: 27, joins: 0/0}
$fSystem->mFilteredWorld1_$crunSystem
  = \ @ gs_a7io
      @ ss_a7ip
      @ m_a7iq
      @ as_a7ir
      @ a_a7is
      $dSystem_a7it
      $dSystem1_a7iu
      eta_B2
      eta1_B1 ->
      case eta_B2 `cast` <Co:8> of
      { HCons @ a1_a7iD @ as1_a7iE co_a7iF s_a77m ss1_a77n ->
      >>
        ($p1System $dSystem1_a7iu)
        (runSystem ($dSystem1_a7iu `cast` <Co:6>) s_a77m eta1_B1)
        (runSystem
           ($dSystem_a7it `cast` <Co:7>) (ss1_a77n `cast` <Co:6>) eta1_B1)
      }

-- RHS size: {terms: 13, types: 42, coercions: 0, joins: 0/0}
$fSystem->mFilteredWorld1
  = \ @ gs_a7io
      @ ss_a7ip
      @ m_a7iq
      @ as_a7ir
      @ a_a7is
      $dSystem_a7it
      $dSystem1_a7iu ->
      C:System
        ($p1System $dSystem1_a7iu)
        ($fSystem->mFilteredWorld1_$crunSystem
           $dSystem_a7it $dSystem1_a7iu)

-- RHS size: {terms: 8, types: 17, coercions: 0, joins: 0/0}
$fSystem->mFilteredWorld0_$crunSystem
  = \ @ m_a7i8 @ gs_a7i9 $dMonad_a7ia _ _ -> return $dMonad_a7ia ()

-- RHS size: {terms: 7, types: 18, coercions: 0, joins: 0/0}
$fSystem->mFilteredWorld0
  = \ @ m_a7i8 @ gs_a7i9 $dMonad_a7ia ->
      C:System
        $dMonad_a7ia ($fSystem->mFilteredWorld0_$crunSystem $dMonad_a7ia)

-- RHS size: {terms: 22, types: 83, coercions: 29, joins: 0/0}
$fSystem->mFilteredWorld_$crunSystem
  = \ @ gs_a7hA
      @ m_a7hB
      @ as_a7hC
      @ a_a7hD
      $dSystem_a7hE
      $dSystem1_a7hF
      eta_B2
      eta1_B1 ->
      case eta_B2 `cast` <Co:8> of
      { HCons @ a1_a7hO @ as1_a7hP co_a7hQ s_a770 ss_a771 ->
      >>
        ($p1System $dSystem1_a7hF)
        (runSystem ($dSystem1_a7hF `cast` <Co:7>) s_a770 eta1_B1)
        (runSystem
           ($dSystem_a7hE `cast` <Co:8>) (ss_a771 `cast` <Co:6>) eta1_B1)
      }

-- RHS size: {terms: 12, types: 43, coercions: 0, joins: 0/0}
$fSystem->mFilteredWorld
  = \ @ gs_a7hA
      @ m_a7hB
      @ as_a7hC
      @ a_a7hD
      $dSystem_a7hE
      $dSystem1_a7hF ->
      C:System
        ($p1System $dSystem1_a7hF)
        ($fSystem->mFilteredWorld_$crunSystem $dSystem_a7hE $dSystem1_a7hF)

-- RHS size: {terms: 22, types: 120, coercions: 17, joins: 0/0}
$fSystem->mWorld_$crunSystem
  = \ @ m_a7gN
      @ as_a7gO
      @ gs_a7gP
      @ ss_a7gQ
      $d(%,,%)_a7gR
      $dSimplifyHList_a7gS
      $dUnsimplifyHList_a7gT
      eta_B2
      eta1_B1 ->
      runSystem
        ($p2(%,,%) $d(%,,%)_a7gR)
        ((filterWorld ($p3(%,,%) $d(%,,%)_a7gR) Proxy eta_B2)
         `cast` <Co:13>)
        (\ x_a68o ->
           ($dUnsimplifyHList_a7gT `cast` <Co:2>)
             (eta1_B1 (($dSimplifyHList_a7gS `cast` <Co:2>) x_a68o)))

-- RHS size: {terms: 14, types: 56, coercions: 0, joins: 0/0}
$fSystem->mWorld
  = \ @ m_a7gN
      @ as_a7gO
      @ gs_a7gP
      @ ss_a7gQ
      $d(%,,%)_a7gR
      $dSimplifyHList_a7gS
      $dUnsimplifyHList_a7gT ->
      C:System
        ($p1(%,,%) $d(%,,%)_a7gR)
        ($fSystem->mWorld_$crunSystem
           $d(%,,%)_a7gR $dSimplifyHList_a7gS $dUnsimplifyHList_a7gT)

-- RHS size: {terms: 27, types: 141, coercions: 9, joins: 0/1}
$fSystem->mWorld0_$crunSystem
  = \ @ m_a7fU
      @ as_a7fV
      @ gs_a7fW
      @ a_a7fX
      @ b_a7fY
      $d(%,,,%)_a7fZ
      $dSimplifyHList_a7g0
      eta_B2
      eta1_B1 ->
      let { $dSystem_s7qv = $p3(%,,,%) $d(%,,,%)_a7fZ } in
      runSystem
        ($p2(%,,,%) $d(%,,,%)_a7fZ)
        ((filterWorld ($p4(%,,,%) $d(%,,,%)_a7fZ) Proxy eta_B2)
         `cast` <Co:7>)
        (\ gs1_a76V ->
           runSystem
             $dSystem_s7qv
             eta_B2
             (eta1_B1 (($dSimplifyHList_a7g0 `cast` <Co:2>) gs1_a76V)))

-- RHS size: {terms: 13, types: 59, coercions: 0, joins: 0/0}
$fSystem->mWorld0
  = \ @ m_a7fU
      @ as_a7fV
      @ gs_a7fW
      @ a_a7fX
      @ b_a7fY
      $d(%,,,%)_a7fZ
      $dSimplifyHList_a7g0 ->
      C:System
        ($p1(%,,,%) $d(%,,,%)_a7fZ)
        ($fSystem->mWorld0_$crunSystem $d(%,,,%)_a7fZ $dSimplifyHList_a7g0)

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$trModule4 = "ecs-0.1.0.0-6T7cQlWNmHv6W00SuOHZxa"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$trModule3 = TrNameS $trModule4

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$trModule2 = "System"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$trModule1 = TrNameS $trModule2

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$trModule = Module $trModule3 $trModule1

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
$krep_r7sb = KindRepTyConApp $tc() []

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
$krep1_r7sc = KindRepTyConApp $tcConstraint []

-- RHS size: {terms: 3, types: 2, coercions: 0, joins: 0/0}
$krep2_r7sd = : krep$* []

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep3_r7se = KindRepTyConApp $tc[] $krep2_r7sd

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep4_r7sf = KindRepFun krep$* $krep1_r7sc

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tcFilteredWorld1 = KindRepFun $krep3_r7se krep$*

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep5_r7sg = KindRepFun krep$*Arr* $krep4_r7sf

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tcSystem1 = KindRepFun krep$* $krep5_r7sg

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$krep6_r7sh = KindRepVar 2#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$krep7_r7si = KindRepVar 0#

-- RHS size: {terms: 3, types: 2, coercions: 0, joins: 0/0}
$krep8_r7sj = : $krep7_r7si []

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep9_r7sk = KindRepTyConApp $tcWorld $krep8_r7sj

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$krep10_r7sl = KindRepVar 1#

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep11_r7sm = KindRepApp $krep10_r7sl $krep_r7sb

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep12_r7sn = KindRepFun $krep7_r7si $krep11_r7sm

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep13_r7so = KindRepFun $krep6_r7sh $krep12_r7sn

-- RHS size: {terms: 3, types: 2, coercions: 0, joins: 0/0}
$krep14_r7sp = : $krep10_r7sl []

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep15_r7sq = KindRepTyConApp $tcMonad $krep14_r7sp

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tcSystem
  = TyCon
      5615715031488363517##
      2307553463639565657##
      $trModule
      $trModule1
      0#
      $tcSystem1

-- RHS size: {terms: 3, types: 2, coercions: 0, joins: 0/0}
$krep16_r7sr = : $krep6_r7sh []

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
$krep17_r7ss = : $krep10_r7sl $krep16_r7sr

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
$krep18_r7st = : $krep7_r7si $krep17_r7ss

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep19_r7su = KindRepTyConApp $tcSystem $krep18_r7st

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep20_r7sv = KindRepFun $krep13_r7so $krep19_r7su

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tc'C:System1 = KindRepFun $krep15_r7sq $krep20_r7sv

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tc'C:System3 = "'C:System"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tc'C:System2 = TrNameS $tc'C:System3

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tc'C:System
  = TyCon
      13211050808566504358##
      12321944591975998873##
      $trModule
      $tc'C:System2
      3#
      $tc'C:System1

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tcFilteredWorld3 = "FilteredWorld"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tcFilteredWorld2 = TrNameS $tcFilteredWorld3

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tcFilteredWorld
  = TyCon
      14208159656349873537##
      18320601550895007885##
      $trModule
      $tcFilteredWorld2
      0#
      $tcFilteredWorld1

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep21_r7sw = KindRepTyConApp $tcFilteredWorld $krep8_r7sj

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tc'FilteredWorld1 = KindRepFun $krep9_r7sk $krep21_r7sw

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tc'FilteredWorld3 = "'FilteredWorld"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tc'FilteredWorld2 = TrNameS $tc'FilteredWorld3

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tc'FilteredWorld
  = TyCon
      11654297051642264938##
      2163240292834851538##
      $trModule
      $tc'FilteredWorld2
      1#
      $tc'FilteredWorld1



[6 of 8] Compiling Single

==================== Tidy Core ====================
Result size of Tidy Core
  = {terms: 235, types: 896, coercions: 164, joins: 0/0}

-- RHS size: {terms: 6, types: 14, coercions: 5, joins: 0/0}
$fStorageSTSingle_$citer
  = \ @ s_a84V @ a_a84W _ f_a808 -> f_a808 (() `cast` <Co:5>)

-- RHS size: {terms: 5, types: 12, coercions: 0, joins: 0/0}
$fStorageSTSingle
  = \ @ s_a84V @ a_a84W ->
      C:Storage $fMonadST $fStorageSTSingle_$citer

-- RHS size: {terms: 13, types: 35, coercions: 4, joins: 0/0}
newSingle1
  = \ @ a_a838 @ s_a839 a1_a80l s1_a87C ->
      case newMutVar# a1_a80l s1_a87C of { (# ipv_a87O, ipv1_a87P #) ->
      (# ipv_a87O, (STRef ipv1_a87P) `cast` <Co:4> #)
      }

-- RHS size: {terms: 1, types: 0, coercions: 14, joins: 0/0}
newSingle = newSingle1 `cast` <Co:14>

-- RHS size: {terms: 5, types: 14, coercions: 7, joins: 0/0}
hlistSingleton = \ @ a_a834 x_a80m -> HCons @~ <Co:7> x_a80m $WHNil

-- RHS size: {terms: 17, types: 40, coercions: 6, joins: 0/0}
$crunSystem_r8eF
  = \ @ a_a83r @ s_a83s eta_B2 eta1_B1 s1_a882 ->
      case eta_B2 `cast` <Co:3> of { STRef var#_a88h ->
      case readMutVar# var#_a88h s1_a882 of
      { (# ipv_a885, ipv1_a886 #) ->
      ((eta1_B1 (hlistSingleton ipv1_a886)) `cast` <Co:3>) ipv_a885
      }
      }

-- RHS size: {terms: 1, types: 0, coercions: 26, joins: 0/0}
$fSystem->STSingle_$crunSystem = $crunSystem_r8eF `cast` <Co:26>

-- RHS size: {terms: 5, types: 23, coercions: 0, joins: 0/0}
$fSystem->STSingle
  = \ @ a_X841 @ s_X843 ->
      C:System $fMonadST $fSystem->STSingle_$crunSystem

-- RHS size: {terms: 17, types: 47, coercions: 3, joins: 0/0}
$cget_r8eG
  = \ @ s_a84A @ a_a84B eta_XF _ s1_a87C ->
      case eta_XF `cast` <Co:3> of { STRef var#_a88h ->
      case readMutVar# var#_a88h s1_a87C of
      { (# ipv_a87F, ipv1_a87G #) ->
      (# ipv_a87F, hlistSingleton ipv1_a87G #)
      }
      }

-- RHS size: {terms: 1, types: 0, coercions: 26, joins: 0/0}
$fStorageGetSTSingleHList_$cget = $cget_r8eG `cast` <Co:26>

-- RHS size: {terms: 5, types: 21, coercions: 0, joins: 0/0}
$fStorageGetSTSingleHList
  = \ @ s_X85h @ a_X85j ->
      C:StorageGet $fStorageSTSingle $fStorageGetSTSingleHList_$cget

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
lvl_r8eH = "src/Single.hs:51:1-35|function hlistUnSingleton"#

-- RHS size: {terms: 3, types: 4, coercions: 0, joins: 0/0}
lvl1_r8eI = \ @ a_a82U -> patError lvl_r8eH

-- RHS size: {terms: 11, types: 80, coercions: 4, joins: 0/0}
hlistUnSingleton
  = \ @ a_a82U ds_d85y ->
      case ds_d85y of
      { HCons @ a1_a82W @ as_a82X co_a82Y x_a80n ds1_d86d ->
      case ds1_d86d of {
        HNil co1_a82Z -> x_a80n `cast` <Co:4>;
        HCons @ ipv_s88v @ ipv1_s88w ipv2_s88x ipv3_s88y ipv4_s88z ->
          lvl1_r8eI
      }
      }

-- RHS size: {terms: 32, types: 126, coercions: 7, joins: 0/0}
$crunSystem1_r8eJ
  = \ @ a_a83Q @ s_a83R ds_d86y f_a7ZZ s1_a88N ->
      case ds_d86y `cast` <Co:3> of { STRef var#_a88Q ->
      case readMutVar# var#_a88Q s1_a88N of
      { (# ipv_a88U, ipv1_a88V #) ->
      case writeMutVar#
             var#_a88Q
             (case f_a7ZZ (hlistSingleton ipv1_a88V) of
              { HCons @ a1_a82W @ as_a82X co_a82Y x_a80n ds2_d86d ->
              case ds2_d86d of {
                HNil co1_a82Z -> x_a80n `cast` <Co:4>;
                HCons @ ipv2_s88D @ ipv3_s88E ipv4_s88F ipv5_s88G ipv6_s88H ->
                  lvl1_r8eI
              }
              })
             ipv_a88U
      of s2#_a88X
      { __DEFAULT ->
      (# s2#_a88X, () #)
      }
      }
      }

-- RHS size: {terms: 1, types: 0, coercions: 31, joins: 0/0}
$fSystem->STSingle0_$crunSystem = $crunSystem1_r8eJ `cast` <Co:31>

-- RHS size: {terms: 5, types: 28, coercions: 0, joins: 0/0}
$fSystem->STSingle0
  = \ @ a_X84F @ s_X84H ->
      C:System $fMonadST $fSystem->STSingle0_$crunSystem

-- RHS size: {terms: 19, types: 39, coercions: 3, joins: 0/0}
$cset_r8eK
  = \ @ s_a84f @ a_a84g ds_d86H _ eta_B2 eta1_B1 ->
      case ds_d86H `cast` <Co:3> of { STRef var#_a89g ->
      case writeMutVar# var#_a89g (hlistUnSingleton eta_B2) eta1_B1
      of s2#_a89i
      { __DEFAULT ->
      (# s2#_a89i, () #)
      }
      }

-- RHS size: {terms: 1, types: 0, coercions: 28, joins: 0/0}
$fStorageSetSTSingleHList_$cset = $cset_r8eK `cast` <Co:28>

-- RHS size: {terms: 5, types: 21, coercions: 0, joins: 0/0}
$fStorageSetSTSingleHList
  = \ @ s_X85d @ a_X85f ->
      C:StorageSet $fStorageSTSingle $fStorageSetSTSingleHList_$cset

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$trModule4 = "ecs-0.1.0.0-6T7cQlWNmHv6W00SuOHZxa"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$trModule3 = TrNameS $trModule4

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$trModule2 = "Single"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$trModule1 = TrNameS $trModule2

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$trModule = Module $trModule3 $trModule1

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$krep_r8eL = KindRepVar 1#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$krep1_r8eM = KindRepVar 0#

-- RHS size: {terms: 3, types: 2, coercions: 0, joins: 0/0}
$krep2_r8eN = : $krep_r8eL []

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
$krep3_r8eO = : $krep1_r8eM $krep2_r8eN

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep4_r8eP = KindRepTyConApp $tcSTRef $krep3_r8eO

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tcSingle
  = TyCon
      5029187421481057928##
      11333052789353071885##
      $trModule
      $trModule1
      0#
      krep$*->*->*

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep5_r8eQ = KindRepTyConApp $tcSingle $krep3_r8eO

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tc'Single1 = KindRepFun $krep4_r8eP $krep5_r8eQ

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tc'Single3 = "'Single"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tc'Single2 = TrNameS $tc'Single3

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tc'Single
  = TyCon
      14450069322167118801##
      6282282169919698191##
      $trModule
      $tc'Single2
      2#
      $tc'Single1



[7 of 8] Compiling Archetype

==================== Tidy Core ====================
Result size of Tidy Core
  = {terms: 2,469, types: 5,886, coercions: 1,384, joins: 31/53}

-- RHS size: {terms: 6, types: 30, coercions: 0, joins: 0/0}
vectorGet
  = \ @ as_abFR[sk:1] v_B1 ->
      case v_B1 of v_B1 { C:IsStorageVector v_B2 v_B3 -> v_B2 }

-- RHS size: {terms: 6, types: 30, coercions: 0, joins: 0/0}
vectorSet
  = \ @ as_abFR[sk:1] v_B1 ->
      case v_B1 of v_B1 { C:IsStorageVector v_B2 v_B3 -> v_B3 }

-- RHS size: {terms: 4, types: 9, coercions: 3, joins: 0/0}
chunkFind'
  = \ @ as_abFU[sk:1] @ bs_abFV[sk:1] v_B1 -> v_B1 `cast` <Co:3>

-- RHS size: {terms: 4, types: 8, coercions: 3, joins: 0/0}
chunkFind
  = \ @ as_abFX[sk:1] @ a_abFY[sk:1] v_B1 -> v_B1 `cast` <Co:3>

-- RHS size: {terms: 3, types: 5, coercions: 2, joins: 0/0}
chunkNew = \ @ as_abG0[sk:1] v_B1 -> v_B1 `cast` <Co:2>

-- RHS size: {terms: 13, types: 24, coercions: 0, joins: 0/0}
$WArchetype
  = \ @ s_abG2[sk:1] @ as_abG3[sk:1] dt_abWF dt_abWG ->
      case dt_abWF of { I# dt_abWH ->
      case dt_abWG of dt_XbWN { __DEFAULT -> Archetype dt_abWH dt_XbWN }
      }

-- RHS size: {terms: 2, types: 6, coercions: 3, joins: 0/0}
$WCNil = \ @ s_abG7 -> CNil @~ <Co:3>

-- RHS size: {terms: 10, types: 26, coercions: 5, joins: 0/0}
$WCCons
  = \ @ a_XbG9 @ s_abG9 @ as_XbGc dt_abW7 dt_abW8 dt_abW9 ->
      CCons @~ <Co:5> dt_abW7 dt_abW8 dt_abW9

-- RHS size: {terms: 5, types: 16, coercions: 0, joins: 0/0}
$fChunkNew[]1 = \ @ s_acys s1_XevX -> (# s1_XevX, $WCNil #)

-- RHS size: {terms: 1, types: 0, coercions: 17, joins: 0/0}
$fChunkNew[] = $fChunkNew[]1 `cast` <Co:17>

-- RHS size: {terms: 11, types: 60, coercions: 4, joins: 0/0}
$fChunkHas:a0_$cchunkFind
  = \ @ as_acxi @ a_acxj @ b_acxk $dChunkHas_acxl @ s_acxp ds_desE ->
      case ds_desE of
      { CCons @ a1_acxr @ as1_acxs co_acxt $dMVector_acxu ds1_det0
              as2_abJp ->
      ($dChunkHas_acxl `cast` <Co:4>) as2_abJp
      }

-- RHS size: {terms: 1, types: 0, coercions: 22, joins: 0/0}
$fChunkHas:a0 = $fChunkHas:a0_$cchunkFind `cast` <Co:22>

-- RHS size: {terms: 8, types: 54, coercions: 8, joins: 0/0}
$fChunkHas:a_$cchunkFind
  = \ @ a_acx2 @ as_acx3 @ s_acx7 ds_desf ->
      case ds_desf of
      { CCons @ a1_acx9 @ as1_acxa co_acxb $dMVector_acxc a2_abJl
              ds1_desA ->
      a2_abJl `cast` <Co:8>
      }

-- RHS size: {terms: 1, types: 0, coercions: 15, joins: 0/0}
$fChunkHas:a = $fChunkHas:a_$cchunkFind `cast` <Co:15>

-- RHS size: {terms: 5, types: 15, coercions: 0, joins: 0/0}
$cchunkFind'_ri80 = \ @ as_acwP @ s_acwT _ _ -> $WHNil

-- RHS size: {terms: 1, types: 0, coercions: 23, joins: 0/0}
$fChunkHas'as[]_$cchunkFind' = $cchunkFind'_ri80 `cast` <Co:23>

-- RHS size: {terms: 1, types: 0, coercions: 10, joins: 0/0}
$fChunkHas'as[] = $fChunkHas'as[]_$cchunkFind' `cast` <Co:10>

-- RHS size: {terms: 14, types: 51, coercions: 16, joins: 0/0}
$cchunkFind'1_ri81
  = \ @ as_acwi
      @ b_acwj
      @ bs_acwk
      $dChunkHas_acwl
      $dChunkHas'_acwm
      @ s_acwq
      _
      eta1_X1v ->
      HCons
        @~ <Co:10>
        (($dChunkHas_acwl `cast` <Co:3>) eta1_X1v)
        (($dChunkHas'_acwm `cast` <Co:3>) Proxy eta1_X1v)

-- RHS size: {terms: 1, types: 0, coercions: 42, joins: 0/0}
$fChunkHas'as:_$cchunkFind' = $cchunkFind'1_ri81 `cast` <Co:42>

-- RHS size: {terms: 1, types: 0, coercions: 27, joins: 0/0}
$fChunkHas'as: = $fChunkHas'as:_$cchunkFind' `cast` <Co:27>

-- RHS size: {terms: 8, types: 22, coercions: 0, joins: 0/0}
$cvectorSet_ri82 = \ @ s_acwa _ _ _ s1_aeuK -> (# s1_aeuK, () #)

-- RHS size: {terms: 1, types: 0, coercions: 21, joins: 0/0}
$fIsStorageVector[]_$cvectorSet = $cvectorSet_ri82 `cast` <Co:21>

-- RHS size: {terms: 7, types: 21, coercions: 0, joins: 0/0}
$cvectorGet_ri83 = \ @ s_acw1 _ _ s1_aeuK -> (# s1_aeuK, $WHNil #)

-- RHS size: {terms: 1, types: 0, coercions: 19, joins: 0/0}
$fIsStorageVector[]_$cvectorGet = $cvectorGet_ri83 `cast` <Co:19>

-- RHS size: {terms: 3, types: 3, coercions: 0, joins: 0/0}
$fIsStorageVector[]
  = C:IsStorageVector
      $fIsStorageVector[]_$cvectorGet $fIsStorageVector[]_$cvectorSet

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
lvl6_ri84 = "src/Archetype.hs:(141,5)-(143,25)|function vectorSet"#

-- RHS size: {terms: 3, types: 6, coercions: 0, joins: 0/0}
lvl7_ri85 = \ @ s_acvv -> patError lvl6_ri84

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
file_ri86 = "./Data/Vector/Generic/Mutable.hs"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
file1_ri87 = unpackCString# file_ri86

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
lvl8_ri88 = "write"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl9_ri89 = unpackCString# lvl8_ri88

-- RHS size: {terms: 11, types: 7, coercions: 0, joins: 0/0}
lvl10_ri8a
  = \ @ s_acvv i#_aebg n#_aecB ->
      $wcheckError
        file1_ri87 703# Bounds lvl9_ri89 (checkIndex_msg# i#_aebg n#_aecB)

-- RHS size: {terms: 63, types: 182, coercions: 71, joins: 0/1}
$w$cvectorSet_ri8b
  = \ @ a_shxZ
      @ as_shy0
      ww_shya
      ww1_shyi
      ww2_shyw
      @ s_shy3
      w_shy4
      ww3_shyA
      w1_shy6
      w2_shy7 ->
      case w_shy4 of {
        HNil ipv_sev4 -> case lvl7_ri85 of wild1_00 { };
        HCons @ a1_acvx @ as1_acvy co_acvz a2_abJ9 as2_abJa ->
          case w1_shy6 of
          { HCons @ a3_acvA @ as3_acvB co1_acvC v_abJc vs_abJd ->
          case >=# ww3_shyA 0# of {
            __DEFAULT ->
              case ww_shya (a2_abJ9 `cast` <Co:16>) of { I# n#_aecB ->
              case lvl10_ri8a ww3_shyA n#_aecB of wild3_00 { }
              };
            1# ->
              case ww_shya (a2_abJ9 `cast` <Co:16>) of { I# y_aevT ->
              case <# ww3_shyA y_aevT of {
                __DEFAULT -> case lvl10_ri8a ww3_shyA y_aevT of wild3_00 { };
                1# ->
                  let { wild3_aevp = I# ww3_shyA } in
                  case ((ww1_shyi
                           $fPrimMonadST
                           (a2_abJ9 `cast` <Co:16>)
                           wild3_aevp
                           (v_abJc `cast` <Co:4>))
                        `cast` <Co:3>)
                         w2_shy7
                  of
                  { (# ipv_aevf, ipv1_aevg #) ->
                  ((ww2_shyw
                      (as2_abJa `cast` <Co:9>) wild3_aevp (vs_abJd `cast` <Co:4>))
                   `cast` <Co:3>)
                    ipv_aevf
                  }
              }
              }
          }
          }
      }

-- RHS size: {terms: 26, types: 244, coercions: 0, joins: 0/0}
$cvectorSet1_ri8c
  = \ @ a_shxZ
      @ as_shy0
      w_shy1
      w1_shy2
      @ s_shy3
      w2_shy4
      w3_shy5
      w4_shy6
      w5_shy7 ->
      case w_shy1 of
      { C:MVector ww1_shya ww2_shyb ww3_shyc ww4_shye ww5_shyf ww6_shyg
                  ww7_shyh ww8_shyi ww9_shyj ww10_shyl ww11_shyn ww12_shyp
                  ww13_shyr ->
      case w1_shy2 of { C:IsStorageVector ww15_shyv ww16_shyw ->
      case w3_shy5 of { I# ww18_shyA ->
      $w$cvectorSet_ri8b
        ww1_shya ww8_shyi ww16_shyw w2_shy4 ww18_shyA w4_shy6 w5_shy7
      }
      }
      }

-- RHS size: {terms: 1, types: 0, coercions: 40, joins: 0/0}
$fIsStorageVector:_$cvectorSet = $cvectorSet1_ri8c `cast` <Co:40>

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
lvl11_ri8d
  = "src/Archetype.hs:(136,5)-(139,27)|function vectorGet"#

-- RHS size: {terms: 5, types: 16, coercions: 0, joins: 0/0}
lvl12_ri8e = \ @ as_XcwR @ a_XcwP @ s_acuO -> patError lvl11_ri8d

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
lvl13_ri8f = "read"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl14_ri8g = unpackCString# lvl13_ri8f

-- RHS size: {terms: 12, types: 9, coercions: 0, joins: 0/0}
lvl15_ri8h
  = \ @ a_XcwP @ s_acuO i#_aepE n#_aepI ->
      $wcheckError
        file1_ri87 697# Bounds lvl14_ri8g (checkIndex_msg# i#_aepE n#_aepI)

-- RHS size: {terms: 64, types: 182, coercions: 64, joins: 0/1}
$w$cvectorGet_ri8i
  = \ @ a_shyL
      @ as_shyM
      ww_shyV
      ww1_shz1
      ww2_shzb
      @ s_shyP
      w_shyQ
      ww3_shzg
      w1_shyS ->
      case w_shyQ of {
        HNil ipv_sewb -> case lvl12_ri8e of wild1_00 { };
        HCons @ a1_acuQ @ as1_acuR co_acuS a2_abJ4 as2_abJ5 ->
          case >=# ww3_shzg 0# of {
            __DEFAULT ->
              case ww_shyV (a2_abJ4 `cast` <Co:16>) of { I# n#_aepI ->
              case lvl15_ri8h ww3_shzg n#_aepI of wild1_00 { }
              };
            1# ->
              case ww_shyV (a2_abJ4 `cast` <Co:16>) of { I# y_aevT ->
              case <# ww3_shzg y_aevT of {
                __DEFAULT -> case lvl15_ri8h ww3_shzg y_aevT of wild2_00 { };
                1# ->
                  let { wild2_aevp = I# ww3_shzg } in
                  case ((ww1_shz1 $fPrimMonadST (a2_abJ4 `cast` <Co:16>) wild2_aevp)
                        `cast` <Co:3>)
                         w1_shyS
                  of
                  { (# ipv_a885, ipv1_a886 #) ->
                  case ((ww2_shzb (as2_abJ5 `cast` <Co:9>) wild2_aevp) `cast` <Co:4>)
                         ipv_a885
                  of
                  { (# ipv2_X8am, ipv3_X8ao #) ->
                  (# ipv2_X8am, $WHCons ipv1_a886 ipv3_X8ao #)
                  }
                  }
              }
              }
          }
      }

-- RHS size: {terms: 24, types: 238, coercions: 0, joins: 0/0}
$cvectorGet1_ri8j
  = \ @ a_shyL
      @ as_shyM
      w_shyN
      w1_shyO
      @ s_shyP
      w2_shyQ
      w3_shyR
      w4_shyS ->
      case w_shyN of
      { C:MVector ww1_shyV ww2_shyW ww3_shyX ww4_shyY ww5_shyZ ww6_shz0
                  ww7_shz1 ww8_shz2 ww9_shz3 ww10_shz4 ww11_shz5 ww12_shz6
                  ww13_shz7 ->
      case w1_shyO of { C:IsStorageVector ww15_shzb ww16_shzc ->
      case w3_shyR of { I# ww18_shzg ->
      $w$cvectorGet_ri8i
        ww1_shyV ww7_shz1 ww15_shzb w2_shyQ ww18_shzg w4_shyS
      }
      }
      }

-- RHS size: {terms: 1, types: 0, coercions: 38, joins: 0/0}
$fIsStorageVector:_$cvectorGet = $cvectorGet1_ri8j `cast` <Co:38>

-- RHS size: {terms: 11, types: 20, coercions: 0, joins: 0/0}
$fIsStorageVector:
  = \ @ a_XcwY @ as_Xcx0 $dMVector_Xcx2 $dIsStorageVector_Xcx4 ->
      C:IsStorageVector
        ($fIsStorageVector:_$cvectorGet
           $dMVector_Xcx2 $dIsStorageVector_Xcx4)
        ($fIsStorageVector:_$cvectorSet
           $dMVector_Xcx2 $dIsStorageVector_Xcx4)

-- RHS size: {terms: 7, types: 19, coercions: 0, joins: 0/0}
archetypeData
  = \ @ s_abWX @ as_abWY ds_denN ->
      case ds_denN of { Archetype dt_detX ds1_denP -> ds1_denP }

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
file2_ri8k = "./Data/Vector/Generic.hs"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl16_ri8l = unpackCString# file2_ri8k

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
lvl17_ri8m = "(!)"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl18_ri8n = unpackCString# lvl17_ri8m

-- RHS size: {terms: 12, types: 13, coercions: 0, joins: 0/0}
lvl19_ri8o
  = \ @ as_acta @ s_actb x_aekx dt1_shV9 ->
      $wcheckError
        lvl16_ri8l 248# Bounds lvl18_ri8n (checkIndex_msg# x_aekx dt1_shV9)

-- RHS size: {terms: 57, types: 112, coercions: 7, joins: 0/0}
$fStorageSetSTArchetypeHList_$cset
  = \ @ bs_act9
      @ as_acta
      @ s_actb
      $dIsStorageVector_actc
      $dChunkHas'_actd
      eta_X1Y
      eta1_X3U
      eta2_X3V ->
      case eta1_X3U `cast` <Co:4> of { (i_abIC, j_abID) ->
      vectorSet
        $dIsStorageVector_actc
        (($dChunkHas'_actd `cast` <Co:3>)
           Proxy
           (case eta_X1Y of { Archetype dt_detX ds_denP ->
            case ds_denP of { Vector dt1_shV8 dt2_shV9 dt3_shVa ->
            case i_abIC of { I# x_aekx ->
            case >=# x_aekx 0# of {
              __DEFAULT -> case lvl19_ri8o x_aekx dt2_shV9 of wild4_00 { };
              1# ->
                case <# x_aekx dt2_shV9 of {
                  __DEFAULT -> case lvl19_ri8o x_aekx dt2_shV9 of wild4_00 { };
                  1# ->
                    case indexArray# dt3_shVa (+# dt1_shV8 x_aekx) of
                    { (# ipv_ag9e #) ->
                    case ipv_ag9e of { (_header_XbIT, chunk_XbIV) -> chunk_XbIV }
                    }
                }
            }
            }
            }
            }))
        j_abID
        eta2_X3V
      }

-- RHS size: {terms: 55, types: 110, coercions: 7, joins: 0/0}
$fStorageGetSTArchetypeHList_$cget
  = \ @ bs_actW
      @ as_actX
      @ s_actY
      $dIsStorageVector_actZ
      $dChunkHas'_acu0
      eta_X1Y
      eta1_X3V ->
      case eta1_X3V `cast` <Co:4> of { (i_abIM, j_abIN) ->
      vectorGet
        $dIsStorageVector_actZ
        (($dChunkHas'_acu0 `cast` <Co:3>)
           Proxy
           (case eta_X1Y of { Archetype dt_detX ds_denP ->
            case ds_denP of { Vector dt1_shVs dt2_shVt dt3_shVu ->
            case i_abIM of { I# x_aekx ->
            case >=# x_aekx 0# of {
              __DEFAULT -> case lvl19_ri8o x_aekx dt2_shVt of wild4_00 { };
              1# ->
                case <# x_aekx dt2_shVt of {
                  __DEFAULT -> case lvl19_ri8o x_aekx dt2_shVt of wild4_00 { };
                  1# ->
                    case indexArray# dt3_shVu (+# dt1_shVs x_aekx) of
                    { (# ipv_ag9e #) ->
                    case ipv_ag9e of { (_header_XbJ1, chunk_XbJ3) -> chunk_XbJ3 }
                    }
                }
            }
            }
            }
            }))
        j_abIN
      }

-- RHS size: {terms: 8, types: 19, coercions: 0, joins: 0/0}
archetypeNextIndex
  = \ @ s_abWU @ as_abWV ds_denJ ->
      case ds_denJ of { Archetype dt_detW ds1_denL -> I# dt_detW }

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
chunkSize = I# 1024#

-- RHS size: {terms: 30, types: 91, coercions: 35, joins: 0/0}
$fChunkNew:1
  = \ @ a_acxE
      @ as_acxF
      $dMVector_acxG
      $dChunkNew_acxH
      @ s_acxL
      s1_a882 ->
      case ((basicUnsafeNew $dMVector_acxG $fPrimMonadST chunkSize)
            `cast` <Co:8>)
             s1_a882
      of
      { (# ipv_a885, ipv1_a886 #) ->
      case ((basicInitialize $dMVector_acxG $fPrimMonadST ipv1_a886)
            `cast` <Co:3>)
             ipv_a885
      of
      { (# ipv2_aevf, ipv3_aevg #) ->
      case (($dChunkNew_acxH `cast` <Co:2>) `cast` <Co:5>) ipv2_aevf of
      { (# ipv4_X8ah, ipv5_X8aj #) ->
      (# ipv4_X8ah,
         $WCCons $dMVector_acxG (ipv1_a886 `cast` <Co:17>) ipv5_X8aj #)
      }
      }
      }

-- RHS size: {terms: 1, types: 0, coercions: 36, joins: 0/0}
$fChunkNew: = $fChunkNew:1 `cast` <Co:36>

-- RHS size: {terms: 20, types: 110, coercions: 28, joins: 0/0}
archetypeNew1
  = \ @ as_ackh @ s_acki s1_XeCI ->
      case newArray# 0# uninitialised (s1_XeCI `cast` <Co:25>) of
      { (# ipv_agbM, ipv1_agbN #) ->
      case unsafeFreezeArray# ipv1_agbN ipv_agbM of
      { (# ipv2_agcY, ipv3_agcZ #) ->
      (# ipv2_agcY `cast` <Co:3>, Vector 0# 0# ipv3_agcZ #)
      }
      }

-- RHS size: {terms: 12, types: 73, coercions: 0, joins: 0/0}
archetypeNew
  = \ @ as_ackh @ s_acki ->
      case runRW# archetypeNew1 of { (# ipv1_aey6, ipv2_aey7 #) ->
      case ipv2_aey7 of dt_XbWN { Vector ipv_sfN3 ipv4_sfN4 ipv5_sfN5 ->
      Archetype 0# dt_XbWN
      }
      }

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
lvl20_ri8p = "src/Archetype.hs:(58,1)-(61,22)|function chunkWrite"#

-- RHS size: {terms: 3, types: 6, coercions: 0, joins: 0/0}
lvl21_ri8q = \ @ s_acjz -> patError lvl20_ri8p

Rec {
-- RHS size: {terms: 66, types: 149, coercions: 63, joins: 0/0}
chunkWrite1
  = \ @ s_acjz @ as_acjA ds_deaW ds1_deaX ds2_deaY eta_B1 ->
      case ds_deaW of {
        CNil co_acjC -> (# eta_B1, () #);
        CCons @ a_acjH @ as1_acjI co_acjJ $dMVector_acjK c_abTQ cs_abTR ->
          case ds2_deaY of {
            HNil ipv_seKg -> case lvl21_ri8q of wild2_00 { };
            HCons @ a1_acjL @ as2_acjM co1_acjN v_abTT vs_abTU ->
              case ds1_deaX of wild2_aevp { I# x_aevr ->
              case >=# x_aevr 0# of {
                __DEFAULT ->
                  case basicLength
                         ($dMVector_acjK `cast` <Co:8>) (c_abTQ `cast` <Co:10>)
                  of
                  { I# n#_aecB ->
                  case lvl10_ri8a x_aevr n#_aecB of wild4_00 { }
                  };
                1# ->
                  case basicLength
                         ($dMVector_acjK `cast` <Co:8>) (c_abTQ `cast` <Co:10>)
                  of
                  { I# y_aevT ->
                  case <# x_aevr y_aevT of {
                    __DEFAULT -> case lvl10_ri8a x_aevr y_aevT of wild4_00 { };
                    1# ->
                      case ((basicUnsafeWrite
                               ($dMVector_acjK `cast` <Co:8>)
                               $fPrimMonadST
                               (c_abTQ `cast` <Co:10>)
                               wild2_aevp
                               v_abTT)
                            `cast` <Co:3>)
                             eta_B1
                      of
                      { (# ipv_aevf, ipv1_aevg #) ->
                      chunkWrite1 cs_abTR wild2_aevp (vs_abTU `cast` <Co:6>) ipv_aevf
                      }
                  }
                  }
              }
              }
          }
      }
end Rec }

-- RHS size: {terms: 1, types: 0, coercions: 20, joins: 0/0}
chunkWrite = chunkWrite1 `cast` <Co:20>

-- RHS size: {terms: 44, types: 30, coercions: 0, joins: 0/2}
forLoop
  = \ @ m_aciy $dMonad_aciA eta_X29 eta1_X4g eta2_X4h ->
      case eta_X29 of { I# x_aevr ->
      case eta1_X4g of { I# y_aevv ->
      case >=# x_aevr y_aevv of {
        __DEFAULT ->
          let { lvl38_sfHP = return $dMonad_aciA () } in
          letrec {
            $wgo_shzW
              = \ ww_shzU ->
                  case ==# ww_shzU y_aevv of {
                    __DEFAULT ->
                      >>
                        $dMonad_aciA (eta2_X4h (I# ww_shzU)) ($wgo_shzW (+# ww_shzU 1#));
                    1# -> lvl38_sfHP
                  }; } in
          $wgo_shzW x_aevr;
        1# -> return $dMonad_aciA ()
      }
      }
      }

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$trModule4 = "ecs-0.1.0.0-6T7cQlWNmHv6W00SuOHZxa"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$trModule3 = TrNameS $trModule4

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$trModule2 = "Archetype"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$trModule1 = TrNameS $trModule2

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$trModule = Module $trModule3 $trModule1

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
$krep_ri8r = KindRepTyConApp $tcWord []

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
$krep1_ri8s = KindRepTyConApp $tcInt []

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
$krep2_ri8t = KindRepTyConApp $tcConstraint []

-- RHS size: {terms: 3, types: 2, coercions: 0, joins: 0/0}
$krep3_ri8u = : krep$* []

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep4_ri8v = KindRepTyConApp $tc[] $krep3_ri8u

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep5_ri8w = KindRepFun krep$* $krep2_ri8t

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tcChunkNew1 = KindRepFun $krep4_ri8v $krep2_ri8t

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep6_ri8x = KindRepFun $krep4_ri8v krep$*

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tcArchetype1 = KindRepFun krep$* $krep6_ri8x

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tcChunkHas1 = KindRepFun $krep4_ri8v $krep5_ri8w

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tcChunkHas'1 = KindRepFun $krep4_ri8v $tcChunkNew1

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$krep7_ri8y = KindRepVar 0#

-- RHS size: {terms: 3, types: 2, coercions: 0, joins: 0/0}
$krep8_ri8z = : $krep_ri8r []

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
$krep9_ri8A = : $krep7_ri8y $krep8_ri8z

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep10_ri8B = KindRepTyConApp $tcMVector $krep9_ri8A

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep11_ri8C = KindRepTyConApp $tc'[] $krep3_ri8u

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$krep12_ri8D = KindRepVar 1#

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tcChunk2 = "Chunk"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tcChunk1 = TrNameS $tcChunk2

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tcChunk
  = TyCon
      9798049056765942083##
      12023821601745913449##
      $trModule
      $tcChunk1
      0#
      $tcArchetype1

-- RHS size: {terms: 3, types: 2, coercions: 0, joins: 0/0}
$krep13_ri8E = : $krep12_ri8D []

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
$krep14_ri8F = : $krep7_ri8y $krep13_ri8E

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep15_ri8G = KindRepTyConApp $tcChunk $krep14_ri8F

-- RHS size: {terms: 3, types: 2, coercions: 0, joins: 0/0}
$krep16_ri8H = : $krep15_ri8G []

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
$krep17_ri8I = : $krep10_ri8B $krep16_ri8H

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep18_ri8J = KindRepTyConApp $tc(,) $krep17_ri8I

-- RHS size: {terms: 3, types: 2, coercions: 0, joins: 0/0}
$krep19_ri8K = : $krep18_ri8J []

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep20_ri8L = KindRepTyConApp $tcVector $krep19_ri8K

-- RHS size: {terms: 3, types: 2, coercions: 0, joins: 0/0}
$krep21_ri8M = : $krep11_ri8C []

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
$krep22_ri8N = : $krep7_ri8y $krep21_ri8M

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tc'CNil1 = KindRepTyConApp $tcChunk $krep22_ri8N

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tc'CNil3 = "'CNil"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tc'CNil2 = TrNameS $tc'CNil3

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tc'CNil
  = TyCon
      15065993215097952239##
      1541541454628918920##
      $trModule
      $tc'CNil2
      1#
      $tc'CNil1

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tcArchetype
  = TyCon
      17754343376927562947##
      15147790878248492261##
      $trModule
      $trModule1
      0#
      $tcArchetype1

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep23_ri8O = KindRepTyConApp $tcArchetype $krep14_ri8F

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep24_ri8P = KindRepFun $krep20_ri8L $krep23_ri8O

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tc'Archetype1 = KindRepFun $krep1_ri8s $krep24_ri8P

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tc'Archetype3 = "'Archetype"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tc'Archetype2 = TrNameS $tc'Archetype3

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tc'Archetype
  = TyCon
      5342527493610936406##
      7564524906236202187##
      $trModule
      $tc'Archetype2
      2#
      $tc'Archetype1

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tcChunkNew3 = "ChunkNew"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tcChunkNew2 = TrNameS $tcChunkNew3

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tcChunkNew
  = TyCon
      18412823350559233361##
      9333462470527698045##
      $trModule
      $tcChunkNew2
      0#
      $tcChunkNew1

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tcChunkHas3 = "ChunkHas"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tcChunkHas2 = TrNameS $tcChunkHas3

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tcChunkHas
  = TyCon
      13883697037623489961##
      12039544648560559088##
      $trModule
      $tcChunkHas2
      0#
      $tcChunkHas1

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tcChunkHas'3 = "ChunkHas'"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tcChunkHas'2 = TrNameS $tcChunkHas'3

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tcChunkHas'
  = TyCon
      17148160707431613466##
      3550429968990063103##
      $trModule
      $tcChunkHas'2
      0#
      $tcChunkHas'1

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tcIsStorageVector2 = "IsStorageVector"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tcIsStorageVector1 = TrNameS $tcIsStorageVector2

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tcIsStorageVector
  = TyCon
      2672974044700321258##
      4002969178944529863##
      $trModule
      $tcIsStorageVector1
      0#
      $tcChunkNew1

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
bitsize = $fBitsInt1

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
headerSize = I# 16#

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
lvl22_ri8Q = "modify"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl23_ri8R = unpackCString# lvl22_ri8Q

-- RHS size: {terms: 11, types: 7, coercions: 0, joins: 0/0}
lvl24_ri8S
  = \ @ s_ackV i#_aeZI n#_aeZM ->
      $wcheckError
        file1_ri87 709# Bounds lvl23_ri8R (checkIndex_msg# i#_aeZI n#_aeZM)

-- RHS size: {terms: 2, types: 2, coercions: 0, joins: 0/0}
lvl25_ri8T = Right True

-- RHS size: {terms: 2, types: 4, coercions: 0, joins: 0/0}
lvl26_ri8U = Left lvl25_ri8T

-- RHS size: {terms: 2, types: 2, coercions: 0, joins: 0/0}
lvl27_ri8V = Left False

-- RHS size: {terms: 2, types: 4, coercions: 0, joins: 0/0}
lvl28_ri8W = Left lvl27_ri8V

-- RHS size: {terms: 2, types: 4, coercions: 0, joins: 0/0}
lvl29_ri8X = Right True

-- RHS size: {terms: 2, types: 2, coercions: 0, joins: 0/0}
lvl30_ri8Y = Right False

-- RHS size: {terms: 2, types: 4, coercions: 0, joins: 0/0}
lvl31_ri8Z = Left lvl30_ri8Y

-- RHS size: {terms: 2, types: 4, coercions: 0, joins: 0/0}
lvl32_ri90 = Right False

-- RHS size: {terms: 2, types: 2, coercions: 0, joins: 0/0}
lvl33_ri91 = Left True

-- RHS size: {terms: 2, types: 4, coercions: 0, joins: 0/0}
lvl34_ri92 = Left lvl33_ri91

-- RHS size: {terms: 5, types: 4, coercions: 0, joins: 0/0}
lvl35_ri93 = \ @ s_shBj ipv_Xf7K -> lvl24_ri8S ipv_Xf7K 16#

-- RHS size: {terms: 11, types: 7, coercions: 0, joins: 0/0}
lvl36_ri94
  = \ @ as_shBi i'_sgyF n#_aekD ->
      $wcheckError
        lvl16_ri8l 248# Bounds lvl18_ri8n (checkIndex_msg# i'_sgyF n#_aekD)

-- RHS size: {terms: 709,
              types: 1,010,
              coercions: 426,
              joins: 13/24}
$warchetypeAdd
  = \ @ v_shBh
      @ as_shBi
      @ s_shBj
      ww_shBv
      ww1_shBx
      w_shBl
      ww2_shBD
      ww3_shBH
      ww4_shBI
      ww5_shBJ
      w1_shBn
      w2_shBo ->
      case w1_shBn of v2_Xf6i { __DEFAULT ->
      case ww_shBv v2_Xf6i of { I# ipv_Xf6q ->
      let { x1_aeWR = -# (*# ww4_shBI 1024#) ww2_shBD } in
      join {
        $w$j_shBg w3_shBe
          = join {
              $w$j1_shB6 w4_shB4
                = case quotRemInt# (-# ipv_Xf6q w3_shBe) 1024# of
                  { (# ipv1_aeZe, ipv2_aeZf #) ->
                  join {
                    $j_sgWj n#_agbG
                      = case newArray# n#_agbG uninitialised (w4_shB4 `cast` <Co:4>) of
                        { (# ipv3_agbM, ipv4_agbN #) ->
                        join {
                          $w$j2_shAF w5_shAz ww6_shAD
                            = case unsafeFreezeArray# ipv4_agbN (w5_shAz `cast` <Co:4>) of
                              { (# ipv5_agcY, ipv6_agcZ #) ->
                              case newByteArray# 128# ipv5_agcY of
                              { (# ipv7_afbs, ipv8_afbt #) ->
                              case {__pkg_ccall primitive-0.7.0.0 forall s.
                               MutableByteArray# s
                               -> Int#
                               -> Word#
                               -> Word#
                               -> State# RealWorld
                               -> (# State# RealWorld #)}_afc5
                                     ipv8_afbt 0# 16## 0## (ipv7_afbs `cast` <Co:7>)
                              of
                              { (# ds9_afca #) ->
                              case ((w_shBl `cast` <Co:2>) `cast` <Co:5>)
                                     (ds9_afca `cast` <Co:41>)
                              of
                              { (# ipv9_X8cc, ipv10_X8ce #) ->
                              join {
                                $w$j3_shAr w6_shAp
                                  = (# w6_shAp,
                                       case runRW#
                                              (\ s1_aexV ->
                                                 let { x2_ago0 = +# ww4_shBI ww6_shAD } in
                                                 case <# x2_ago0 ww4_shBI of {
                                                   __DEFAULT ->
                                                     case <# x2_ago0 ww6_shAD of {
                                                       __DEFAULT ->
                                                         let { x3_Xgxc = +# x2_ago0 1# } in
                                                         case <# x3_Xgxc x2_ago0 of {
                                                           __DEFAULT ->
                                                             case <# x3_Xgxc 1# of {
                                                               __DEFAULT ->
                                                                 case newArray#
                                                                        x3_Xgxc
                                                                        uninitialised
                                                                        (s1_aexV `cast` <Co:25>)
                                                                 of
                                                                 { (# ipv11_XgkW, ipv12_XgkY #) ->
                                                                 let {
                                                                   x_sgoX
                                                                     = MVector 0# 16# ipv8_afbt } in
                                                                 let {
                                                                   x4_afgy
                                                                     = (x_sgoX `cast` <Co:28>,
                                                                        ipv10_X8ce) } in
                                                                 join {
                                                                   exit_X4a ww7_shAm w7_shAj
                                                                     = case unsafeFreezeArray#
                                                                              ipv12_XgkY
                                                                              (w7_shAj
                                                                               `cast` <Co:4>)
                                                                       of
                                                                       { (# ipv13_Xgnh,
                                                                            ipv14_Xgnj #) ->
                                                                       (# ipv13_Xgnh `cast` <Co:3>,
                                                                          Vector
                                                                            0#
                                                                            ww7_shAm
                                                                            ipv14_Xgnj #)
                                                                       } } in
                                                                 joinrec {
                                                                   $wfoldlM'_loop_shAo w7_shAg
                                                                                       ww7_shAm
                                                                                       w8_shAi
                                                                                       w9_shAj
                                                                     = case w7_shAg of
                                                                       { __DEFAULT ->
                                                                       case w8_shAi of {
                                                                         Left sa_agmB ->
                                                                           case sa_agmB of {
                                                                             Left sa1_XgvD ->
                                                                               case sa1_XgvD of {
                                                                                 False ->
                                                                                   jump $wfoldlM'_loop_shAo
                                                                                     SPEC
                                                                                     ww7_shAm
                                                                                     lvl26_ri8U
                                                                                     w9_shAj;
                                                                                 True ->
                                                                                   case copyArray#
                                                                                          ww5_shBJ
                                                                                          ww3_shBH
                                                                                          ipv12_XgkY
                                                                                          ww7_shAm
                                                                                          ww4_shBI
                                                                                          (w9_shAj
                                                                                           `cast` <Co:4>)
                                                                                   of s'#_ageT
                                                                                   { __DEFAULT ->
                                                                                   jump $wfoldlM'_loop_shAo
                                                                                     SPEC
                                                                                     (+#
                                                                                        ww7_shAm
                                                                                        ww4_shBI)
                                                                                     lvl28_ri8W
                                                                                     (s'#_ageT
                                                                                      `cast` <Co:3>)
                                                                                   }
                                                                               };
                                                                             Right sb_agmP ->
                                                                               case sb_agmP of {
                                                                                 False ->
                                                                                   jump $wfoldlM'_loop_shAo
                                                                                     SPEC
                                                                                     ww7_shAm
                                                                                     lvl29_ri8X
                                                                                     w9_shAj;
                                                                                 True ->
                                                                                   case copyArray#
                                                                                          ipv6_agcZ
                                                                                          0#
                                                                                          ipv12_XgkY
                                                                                          ww7_shAm
                                                                                          ww6_shAD
                                                                                          (w9_shAj
                                                                                           `cast` <Co:4>)
                                                                                   of s'#_ageT
                                                                                   { __DEFAULT ->
                                                                                   jump $wfoldlM'_loop_shAo
                                                                                     SPEC
                                                                                     (+#
                                                                                        ww7_shAm
                                                                                        ww6_shAD)
                                                                                     lvl31_ri8Z
                                                                                     (s'#_ageT
                                                                                      `cast` <Co:3>)
                                                                                   }
                                                                               }
                                                                           };
                                                                         Right sb_agmP ->
                                                                           case sb_agmP of {
                                                                             False ->
                                                                               jump exit_X4a
                                                                                 ww7_shAm w9_shAj;
                                                                             True ->
                                                                               case writeArray#
                                                                                      ipv12_XgkY
                                                                                      ww7_shAm
                                                                                      x4_afgy
                                                                                      (w9_shAj
                                                                                       `cast` <Co:4>)
                                                                               of s'#_agqZ
                                                                               { __DEFAULT ->
                                                                               jump $wfoldlM'_loop_shAo
                                                                                 SPEC
                                                                                 (+# ww7_shAm 1#)
                                                                                 lvl32_ri90
                                                                                 (s'#_agqZ
                                                                                  `cast` <Co:3>)
                                                                               }
                                                                           }
                                                                       }
                                                                       }; } in
                                                                 jump $wfoldlM'_loop_shAo
                                                                   SPEC
                                                                   0#
                                                                   lvl34_ri92
                                                                   (ipv11_XgkW `cast` <Co:27>)
                                                                 };
                                                               1# ->
                                                                 case $wlvl1 x3_Xgxc of wild_00 { }
                                                             };
                                                           1# -> case $wlvl1 x3_Xgxc of wild_00 { }
                                                         };
                                                       1# -> case $wlvl1 x2_ago0 of wild_00 { }
                                                     };
                                                   1# -> case $wlvl1 x2_ago0 of wild_00 { }
                                                 })
                                       of
                                       { (# ipv11_aey6, ipv12_aey7 #) ->
                                       case ipv12_aey7 of dt_XbWN
                                       { Vector ipv13_sfPv ipv14_sfPw ipv15_sfPx ->
                                       Archetype (+# ww2_shBD ipv_Xf6q) dt_XbWN
                                       }
                                       } #) } in
                              case >=# 0# ipv2_aeZf of {
                                __DEFAULT ->
                                  joinrec {
                                    $wgo_shAy ww7_shAw w6_shAt
                                      = case ==# ww7_shAw ipv2_aeZf of {
                                          __DEFAULT ->
                                            let {
                                              i'_sgyF
                                                = +# (+# ww7_shAw w3_shBe) (*# ipv1_aeZe 1024#) } in
                                            case chunkWrite1
                                                   ipv10_X8ce
                                                   (I# ww7_shAw)
                                                   (case >=# i'_sgyF 0# of {
                                                      __DEFAULT -> lvl36_ri94 i'_sgyF ipv_Xf6q;
                                                      1# ->
                                                        case <# i'_sgyF ipv_Xf6q of {
                                                          __DEFAULT -> lvl36_ri94 i'_sgyF ipv_Xf6q;
                                                          1# ->
                                                            (ww1_shBx
                                                               $fMonadId v2_Xf6i (I# i'_sgyF))
                                                            `cast` <Co:3>
                                                        }
                                                    })
                                                   w6_shAt
                                            of
                                            { (# ipv11_XezB, ipv12_XezD #) ->
                                            case quotRemInt# i'_sgyF 64# of
                                            { (# ipv13_Xf7K, ipv14_Xf7M #) ->
                                            case >=# ipv13_Xf7K 0# of {
                                              __DEFAULT ->
                                                case lvl35_ri93 ipv13_Xf7K of wild_00 { };
                                              1# ->
                                                case <# ipv13_Xf7K 16# of {
                                                  __DEFAULT ->
                                                    case lvl35_ri93 ipv13_Xf7K of wild_00 { };
                                                  1# ->
                                                    case readWordArray#
                                                           ipv8_afbt
                                                           ipv13_Xf7K
                                                           (ipv11_XezB `cast` <Co:16>)
                                                    of
                                                    { (# ipv15_af2z, ipv16_af2A #) ->
                                                    case >=# ipv14_Xf7M 0# of {
                                                      __DEFAULT ->
                                                        case overflowError of wild_00 { };
                                                      1# ->
                                                        case >=# ipv14_Xf7M 64# of {
                                                          __DEFAULT ->
                                                            case writeWordArray#
                                                                   ipv8_afbt
                                                                   ipv13_Xf7K
                                                                   (or#
                                                                      ipv16_af2A
                                                                      (uncheckedShiftL#
                                                                         1## ipv14_Xf7M))
                                                                   ipv15_af2z
                                                            of s'#_af2Z
                                                            { __DEFAULT ->
                                                            jump $wgo_shAy
                                                              (+# ww7_shAw 1#)
                                                              (s'#_af2Z `cast` <Co:15>)
                                                            };
                                                          1# ->
                                                            case writeWordArray#
                                                                   ipv8_afbt
                                                                   ipv13_Xf7K
                                                                   ipv16_af2A
                                                                   ipv15_af2z
                                                            of s'#_af2Z
                                                            { __DEFAULT ->
                                                            jump $wgo_shAy
                                                              (+# ww7_shAw 1#)
                                                              (s'#_af2Z `cast` <Co:15>)
                                                            }
                                                        }
                                                    }
                                                    }
                                                }
                                            }
                                            }
                                            };
                                          1# -> jump $w$j3_shAr w6_shAt
                                        }; } in
                                  jump $wgo_shAy 0# ipv9_X8cc;
                                1# -> jump $w$j3_shAr ipv9_X8cc
                              }
                              }
                              }
                              }
                              } } in
                        joinrec {
                          $wfoldlM'_loop_shB2 w5_shAQ ww6_shAW ww7_shB0 w6_shAT
                            = case w5_shAQ of { __DEFAULT ->
                              case <# ww7_shB0 ipv1_aeZe of {
                                __DEFAULT -> jump $w$j2_shAF w6_shAT ww6_shAW;
                                1# ->
                                  case newByteArray# 128# (w6_shAT `cast` <Co:19>) of
                                  { (# ipv5_afbs, ipv6_afbt #) ->
                                  case {__pkg_ccall primitive-0.7.0.0 forall s.
                               MutableByteArray# s
                               -> Int#
                               -> Word#
                               -> Word#
                               -> State# RealWorld
                               -> (# State# RealWorld #)}_afc5
                                         ipv6_afbt
                                         0#
                                         16##
                                         18446744073709551615##
                                         (ipv5_afbs `cast` <Co:7>)
                                  of
                                  { (# ds9_afca #) ->
                                  case ((w_shBl `cast` <Co:2>) `cast` <Co:5>)
                                         (ds9_afca `cast` <Co:41>)
                                  of
                                  { (# ipv7_X8c8, ipv8_X8ca #) ->
                                  join {
                                    $w$j3_shAI w7_shAG
                                      = case writeArray#
                                               ipv4_agbN
                                               ww6_shAW
                                               ((MVector 0# 16# ipv6_afbt) `cast` <Co:28>,
                                                ipv8_X8ca)
                                               (w7_shAG `cast` <Co:4>)
                                        of s'#_agqZ
                                        { __DEFAULT ->
                                        jump $wfoldlM'_loop_shB2
                                          SPEC
                                          (+# ww6_shAW 1#)
                                          (+# ww7_shB0 1#)
                                          (s'#_agqZ `cast` <Co:3>)
                                        } } in
                                  joinrec {
                                    $wgo_shAP ww8_shAN w7_shAK
                                      = case ww8_shAN of wild_X7O {
                                          __DEFAULT ->
                                            case chunkWrite1
                                                   ipv8_X8ca
                                                   (I# wild_X7O)
                                                   (let {
                                                      x_aekx
                                                        = +#
                                                            (+# wild_X7O (*# ww7_shB0 1024#))
                                                            w3_shBe } in
                                                    case >=# x_aekx 0# of {
                                                      __DEFAULT -> lvl36_ri94 x_aekx ipv_Xf6q;
                                                      1# ->
                                                        case <# x_aekx ipv_Xf6q of {
                                                          __DEFAULT -> lvl36_ri94 x_aekx ipv_Xf6q;
                                                          1# ->
                                                            (ww1_shBx $fMonadId v2_Xf6i (I# x_aekx))
                                                            `cast` <Co:3>
                                                        }
                                                    })
                                                   w7_shAK
                                            of
                                            { (# ipv9_XezG, ipv10_XezI #) ->
                                            jump $wgo_shAP (+# wild_X7O 1#) ipv9_XezG
                                            };
                                          1024# -> jump $w$j3_shAI w7_shAK
                                        }; } in
                                  jump $wgo_shAP 0# ipv7_X8c8
                                  }
                                  }
                                  }
                              }
                              }; } in
                        jump $wfoldlM'_loop_shB2 SPEC 0# 0# (ipv3_agbM `cast` <Co:27>)
                        } } in
                  case <=# ipv1_aeZe 0# of {
                    __DEFAULT -> jump $j_sgWj ipv1_aeZe;
                    1# -> jump $j_sgWj 0#
                  }
                  } } in
            case >=# 0# w3_shBe of {
              __DEFAULT ->
                let {
                  ds_sfF1
                    = let { x_aeWo = -# ww4_shBI 1# } in
                      case >=# x_aeWo 0# of {
                        __DEFAULT -> lvl19_ri8o x_aeWo ww4_shBI;
                        1# ->
                          case <# x_aeWo ww4_shBI of {
                            __DEFAULT -> lvl19_ri8o x_aeWo ww4_shBI;
                            1# ->
                              case indexArray# ww5_shBJ (+# ww3_shBH x_aeWo) of
                              { (# ipv1_ag9e #) ->
                              ipv1_ag9e
                              }
                          }
                      } } in
                let { ai1_shjt = remInt# ww2_shBD 1024# } in
                let {
                  chunk_sfEZ
                    = case ds_sfF1 of { (header_XbVa, chunk1_XbXq) ->
                      chunk1_XbXq
                      } } in
                join {
                  exit_X3F ipv1_aeZe
                    = case ds_sfF1 of { (header_XbXm, chunk1_abU1) ->
                      case header_XbXm `cast` <Co:9> of
                      { MVector dt_af1X dt1_af1Y dt2_af1Z ->
                      case lvl24_ri8S ipv1_aeZe dt1_af1Y of wild2_00 { }
                      }
                      } } in
                joinrec {
                  $wgo_shBd ww6_shBb w4_shB8
                    = case ==# ww6_shBb w3_shBe of {
                        __DEFAULT ->
                          case chunkWrite1
                                 chunk_sfEZ
                                 (I# (+# ai1_shjt ww6_shBb))
                                 (case >=# ww6_shBb 0# of {
                                    __DEFAULT -> lvl36_ri94 ww6_shBb ipv_Xf6q;
                                    1# ->
                                      case <# ww6_shBb ipv_Xf6q of {
                                        __DEFAULT -> lvl36_ri94 ww6_shBb ipv_Xf6q;
                                        1# ->
                                          (ww1_shBx $fMonadId v2_Xf6i (I# ww6_shBb)) `cast` <Co:3>
                                      }
                                  })
                                 w4_shB8
                          of
                          { (# ipv1_aevf, ipv2_aevg #) ->
                          case quotRemInt# (+# ai1_shjt ww6_shBb) 64# of
                          { (# ipv3_aeZe, ipv4_aeZf #) ->
                          case >=# ipv3_aeZe 0# of {
                            __DEFAULT -> jump exit_X3F ipv3_aeZe;
                            1# ->
                              case ds_sfF1 of { (header_XbXm, chunk1_abU1) ->
                              case header_XbXm `cast` <Co:9> of
                              { MVector dt_af1X dt1_af1Y dt2_af1Z ->
                              case <# ipv3_aeZe dt1_af1Y of {
                                __DEFAULT -> case lvl24_ri8S ipv3_aeZe dt1_af1Y of wild2_00 { };
                                1# ->
                                  case readWordArray#
                                         dt2_af1Z (+# dt_af1X ipv3_aeZe) (ipv1_aevf `cast` <Co:16>)
                                  of
                                  { (# ipv5_af2z, ipv6_af2A #) ->
                                  case >=# ipv4_aeZf 0# of {
                                    __DEFAULT -> case overflowError of wild2_00 { };
                                    1# ->
                                      case >=# ipv4_aeZf 64# of {
                                        __DEFAULT ->
                                          case writeWordArray#
                                                 dt2_af1Z
                                                 (+# dt_af1X ipv3_aeZe)
                                                 (or# ipv6_af2A (uncheckedShiftL# 1## ipv4_aeZf))
                                                 ipv5_af2z
                                          of s'#_af2Z
                                          { __DEFAULT ->
                                          jump $wgo_shBd (+# ww6_shBb 1#) (s'#_af2Z `cast` <Co:15>)
                                          };
                                        1# ->
                                          case writeWordArray#
                                                 dt2_af1Z (+# dt_af1X ipv3_aeZe) ipv6_af2A ipv5_af2z
                                          of s'#_af2Z
                                          { __DEFAULT ->
                                          jump $wgo_shBd (+# ww6_shBb 1#) (s'#_af2Z `cast` <Co:15>)
                                          }
                                      }
                                  }
                                  }
                              }
                              }
                              }
                          }
                          }
                          };
                        1# -> jump $w$j1_shB6 w4_shB8
                      }; } in
                jump $wgo_shBd 0# w2_shBo;
              1# -> jump $w$j1_shB6 w2_shBo
            } } in
      case <=# x1_aeWR ipv_Xf6q of {
        __DEFAULT -> jump $w$j_shBg ipv_Xf6q;
        1# -> jump $w$j_shBg x1_aeWR
      }
      }
      }

-- RHS size: {terms: 27, types: 145, coercions: 0, joins: 0/0}
archetypeAdd1
  = \ @ v_shBh
      @ as_shBi
      @ s_shBj
      w_shBk
      w1_shBl
      w2_shBm
      w3_shBn
      w4_shBo ->
      case w_shBk of
      { C:Vector ww1_shBr ww2_shBs ww3_shBt ww4_shBv ww5_shBw ww6_shBx
                 ww7_shBy ww8_shBz ->
      case w2_shBm of { Archetype ww10_shBD ww11_shBE ->
      case ww11_shBE of { Vector ww13_shWO ww14_shWP ww15_shWQ ->
      $warchetypeAdd
        ww4_shBv
        ww6_shBx
        w1_shBl
        ww10_shBD
        ww13_shWO
        ww14_shWP
        ww15_shWQ
        w3_shBn
        w4_shBo
      }
      }
      }

-- RHS size: {terms: 1, types: 0, coercions: 34, joins: 0/0}
archetypeAdd = archetypeAdd1 `cast` <Co:34>

-- RHS size: {terms: 11, types: 7, coercions: 0, joins: 0/0}
lvl37_ri95
  = \ @ s_acq3 ipv_seL3 n#_aepI ->
      $wcheckError
        file1_ri87
        697#
        Bounds
        lvl14_ri8g
        (checkIndex_msg# ipv_seL3 n#_aepI)

-- RHS size: {terms: 177, types: 241, coercions: 59, joins: 6/8}
$w$crunSystem_ri96
  = \ @ as_shCi
      @ gs_shCj
      @ s_shCk
      w_shCl
      w1_shCm
      ww_shCw
      ww1_shCx
      ww2_shCy
      w2_shCo
      w3_shCp ->
      joinrec {
        $wconsume_loop_shCh w4_shCa ww3_shCf w5_shCc
          = case w4_shCa of { __DEFAULT ->
            case >=# ww3_shCf ww1_shCx of {
              __DEFAULT ->
                case indexArray# ww2_shCy (+# ww_shCw ww3_shCf) of
                { (# ipv_ag9e #) ->
                case ipv_ag9e of { (header_abGT, chunk_abGU) ->
                let { g_sfEy = (w_shCl `cast` <Co:3>) Proxy chunk_abGU } in
                join {
                  exit_X3z wild1_X7r
                    = case header_abGT `cast` <Co:9> of
                      { MVector dt_af1X dt1_af1Y dt2_af1Z ->
                      case lvl37_ri95 wild1_X7r dt1_af1Y of wild3_00 { }
                      } } in
                join {
                  exit1_X3A w6_shC4
                    = jump $wconsume_loop_shCh SPEC (+# ww3_shCf 1#) w6_shC4 } in
                joinrec {
                  $wgo_shC9 ww4_shC7 w6_shC4
                    = case ww4_shC7 of wild1_X7r {
                        __DEFAULT ->
                          case >=# wild1_X7r 0# of {
                            __DEFAULT -> jump exit_X3z wild1_X7r;
                            1# ->
                              case header_abGT `cast` <Co:9> of
                              { MVector dt_af1X dt1_af1Y dt2_af1Z ->
                              case <# wild1_X7r dt1_af1Y of {
                                __DEFAULT -> case lvl37_ri95 wild1_X7r dt1_af1Y of wild3_00 { };
                                1# ->
                                  case readWordArray#
                                         dt2_af1Z (+# dt_af1X wild1_X7r) (w6_shC4 `cast` <Co:16>)
                                  of
                                  { (# ipv1_af2z, ipv2_af2A #) ->
                                  let { j'_sfEG = *# wild1_X7r 64# } in
                                  join {
                                    exit2_X3M w7_shBX
                                      = jump $wgo_shC9 (+# wild1_X7r 1#) w7_shBX } in
                                  joinrec {
                                    $wgo1_shC2 ww5_shC0 w7_shBX
                                      = case ww5_shC0 of wild3_X83 {
                                          __DEFAULT ->
                                            case >=# wild3_X83 0# of {
                                              __DEFAULT -> case overflowError of wild4_00 { };
                                              1# ->
                                                case >=# wild3_X83 64# of {
                                                  __DEFAULT ->
                                                    case and#
                                                           ipv2_af2A
                                                           (uncheckedShiftL# 1## wild3_X83)
                                                    of {
                                                      __DEFAULT ->
                                                        case ((vectorGet
                                                                 w1_shCm
                                                                 g_sfEy
                                                                 (I# (+# j'_sfEG wild3_X83)))
                                                              `cast` <Co:4>)
                                                               w7_shBX
                                                        of
                                                        { (# ipv3_X8c3, ipv4_X8c5 #) ->
                                                        case ((w2_shCo ipv4_X8c5) `cast` <Co:3>)
                                                               ipv3_X8c3
                                                        of
                                                        { (# ipv5_aevf, ipv6_aevg #) ->
                                                        jump $wgo1_shC2 (+# wild3_X83 1#) ipv5_aevf
                                                        }
                                                        };
                                                      0## ->
                                                        jump $wgo1_shC2 (+# wild3_X83 1#) w7_shBX
                                                    };
                                                  1# -> jump $wgo1_shC2 (+# wild3_X83 1#) w7_shBX
                                                }
                                            };
                                          64# -> jump exit2_X3M w7_shBX
                                        }; } in
                                  jump $wgo1_shC2 0# (ipv1_af2z `cast` <Co:15>)
                                  }
                              }
                              }
                          };
                        16# -> jump exit1_X3A w6_shC4
                      }; } in
                jump $wgo_shC9 0# w5_shCc
                }
                };
              1# -> (# w5_shCc, () #)
            }
            }; } in
      jump $wconsume_loop_shCh SPEC 0# w3_shCp

-- RHS size: {terms: 22, types: 53, coercions: 0, joins: 0/0}
$crunSystem_ri97
  = \ @ as_shCi
      @ gs_shCj
      @ s_shCk
      w_shCl
      w1_shCm
      w2_shCn
      w3_shCo
      w4_shCp ->
      case w2_shCn of { Archetype ww1_shCs ww2_shCt ->
      case ww2_shCt of { Vector ww4_shWV ww5_shWW ww6_shWX ->
      $w$crunSystem_ri96
        w_shCl w1_shCm ww4_shWV ww5_shWW ww6_shWX w3_shCo w4_shCp
      }
      }

-- RHS size: {terms: 1, types: 0, coercions: 32, joins: 0/0}
$fSystem->STArchetype_$crunSystem = $crunSystem_ri97 `cast` <Co:32>

-- RHS size: {terms: 10, types: 27, coercions: 0, joins: 0/0}
$fSystem->STArchetype
  = \ @ as_XcwL
      @ gs_XcwN
      @ s_XcwP
      $dChunkHas'_XcwR
      $dIsStorageVector_XcwT ->
      C:System
        $fMonadST
        ($fSystem->STArchetype_$crunSystem
           $dChunkHas'_XcwR $dIsStorageVector_XcwT)

-- RHS size: {terms: 191, types: 259, coercions: 62, joins: 6/9}
$w$crunSystem1_ri98
  = \ @ as_shD7
      @ gs_shD8
      @ ss_shD9
      @ s_shDa
      w_shDb
      w1_shDc
      w2_shDd
      w3_shDe
      ww_shDo
      ww1_shDp
      ww2_shDq
      w4_shDg
      w5_shDh ->
      joinrec {
        $wconsume_loop_shD6 w6_shCZ ww3_shD4 w7_shD1
          = case w6_shCZ of { __DEFAULT ->
            case >=# ww3_shD4 ww1_shDp of {
              __DEFAULT ->
                case indexArray# ww2_shDq (+# ww_shDo ww3_shD4) of
                { (# ipv_ag9e #) ->
                case ipv_ag9e of { (header_abIq, chunk_abIr) ->
                let { s1_sfEi = (w1_shDc `cast` <Co:3>) Proxy chunk_abIr } in
                let { g_sfEh = (w_shDb `cast` <Co:3>) Proxy chunk_abIr } in
                join {
                  exit_X3D wild1_X7P
                    = case header_abIq `cast` <Co:9> of
                      { MVector dt_af1X dt1_af1Y dt2_af1Z ->
                      case lvl37_ri95 wild1_X7P dt1_af1Y of wild3_00 { }
                      } } in
                join {
                  exit1_X3E w8_shCT
                    = jump $wconsume_loop_shD6 SPEC (+# ww3_shD4 1#) w8_shCT } in
                joinrec {
                  $wgo_shCY ww4_shCW w8_shCT
                    = case ww4_shCW of wild1_X7P {
                        __DEFAULT ->
                          case >=# wild1_X7P 0# of {
                            __DEFAULT -> jump exit_X3D wild1_X7P;
                            1# ->
                              case header_abIq `cast` <Co:9> of
                              { MVector dt_af1X dt1_af1Y dt2_af1Z ->
                              case <# wild1_X7P dt1_af1Y of {
                                __DEFAULT -> case lvl37_ri95 wild1_X7P dt1_af1Y of wild3_00 { };
                                1# ->
                                  case readWordArray#
                                         dt2_af1Z (+# dt_af1X wild1_X7P) (w8_shCT `cast` <Co:16>)
                                  of
                                  { (# ipv1_af2z, ipv2_af2A #) ->
                                  let { j'_sfEq = *# wild1_X7P 64# } in
                                  join {
                                    exit2_X3Q w9_shCM
                                      = jump $wgo_shCY (+# wild1_X7P 1#) w9_shCM } in
                                  joinrec {
                                    $wgo1_shCR ww5_shCP w9_shCM
                                      = case ww5_shCP of wild3_X8r {
                                          __DEFAULT ->
                                            case >=# wild3_X8r 0# of {
                                              __DEFAULT -> case overflowError of wild4_00 { };
                                              1# ->
                                                case >=# wild3_X8r 64# of {
                                                  __DEFAULT ->
                                                    case and#
                                                           ipv2_af2A
                                                           (uncheckedShiftL# 1## wild3_X8r)
                                                    of {
                                                      __DEFAULT ->
                                                        case ((vectorGet
                                                                 w2_shDd
                                                                 g_sfEh
                                                                 (I# (+# j'_sfEq wild3_X8r)))
                                                              `cast` <Co:4>)
                                                               w9_shCM
                                                        of
                                                        { (# ipv3_X8ca, ipv4_X8cc #) ->
                                                        case ((vectorSet
                                                                 w3_shDe
                                                                 s1_sfEi
                                                                 (I# (+# j'_sfEq wild3_X8r))
                                                                 (w4_shDg ipv4_X8cc))
                                                              `cast` <Co:3>)
                                                               ipv3_X8ca
                                                        of
                                                        { (# ipv5_aevf, ipv6_aevg #) ->
                                                        jump $wgo1_shCR (+# wild3_X8r 1#) ipv5_aevf
                                                        }
                                                        };
                                                      0## ->
                                                        jump $wgo1_shCR (+# wild3_X8r 1#) w9_shCM
                                                    };
                                                  1# -> jump $wgo1_shCR (+# wild3_X8r 1#) w9_shCM
                                                }
                                            };
                                          64# -> jump exit2_X3Q w9_shCM
                                        }; } in
                                  jump $wgo1_shCR 0# (ipv1_af2z `cast` <Co:15>)
                                  }
                              }
                              }
                          };
                        16# -> jump exit1_X3E w8_shCT
                      }; } in
                jump $wgo_shCY 0# w7_shD1
                }
                };
              1# -> (# w7_shD1, () #)
            }
            }; } in
      jump $wconsume_loop_shD6 SPEC 0# w5_shDh

-- RHS size: {terms: 27, types: 61, coercions: 0, joins: 0/0}
$crunSystem1_ri99
  = \ @ as_shD7
      @ gs_shD8
      @ ss_shD9
      @ s_shDa
      w_shDb
      w1_shDc
      w2_shDd
      w3_shDe
      w4_shDf
      w5_shDg
      w6_shDh ->
      case w4_shDf of { Archetype ww1_shDk ww2_shDl ->
      case ww2_shDl of { Vector ww4_shX2 ww5_shX3 ww6_shX4 ->
      $w$crunSystem1_ri98
        w_shDb
        w1_shDc
        w2_shDd
        w3_shDe
        ww4_shX2
        ww5_shX3
        ww6_shX4
        w5_shDg
        w6_shDh
      }
      }

-- RHS size: {terms: 1, types: 0, coercions: 42, joins: 0/0}
$fSystem->STArchetype0_$crunSystem
  = $crunSystem1_ri99 `cast` <Co:42>

-- RHS size: {terms: 15, types: 35, coercions: 0, joins: 0/0}
$fSystem->STArchetype0
  = \ @ as_XcyA
      @ gs_XcyC
      @ ss_XcyE
      @ s_XcyG
      $dChunkHas'_XcyI
      $dChunkHas'1_XcyK
      $dIsStorageVector_XcyM
      $dIsStorageVector1_XcyO ->
      C:System
        $fMonadST
        ($fSystem->STArchetype0_$crunSystem
           $dChunkHas'_XcyI
           $dChunkHas'1_XcyK
           $dIsStorageVector_XcyM
           $dIsStorageVector1_XcyO)

-- RHS size: {terms: 173, types: 216, coercions: 57, joins: 6/8}
$w$citer_ri9a
  = \ @ s_shE8 @ as_shE9 ww_shEj ww1_shEk ww2_shEl w_shEb w1_shEc ->
      joinrec {
        $wconsume_loop_shE7 w2_shDR ww3_shDZ ww4_shE4 w3_shDT
          = case w2_shDR of { __DEFAULT ->
            case >=# ww3_shDZ ww1_shEk of {
              __DEFAULT ->
                case indexArray# ww2_shEl (+# ww_shEj ww3_shDZ) of
                { (# ipv_ag9e #) ->
                case ipv_ag9e of { (header_abIW, _chunk_abIX) ->
                let { i1_afGd = I# ww4_shE4 } in
                join {
                  exit_X3x wild1_X86
                    = case header_abIW `cast` <Co:9> of
                      { MVector dt_af1X dt1_af1Y dt2_af1Z ->
                      case lvl37_ri95 wild1_X86 dt1_af1Y of wild3_00 { }
                      } } in
                join {
                  exit1_X3y w4_shDL
                    = jump $wconsume_loop_shE7
                        SPEC (+# ww3_shDZ 1#) (+# ww4_shE4 1#) w4_shDL } in
                joinrec {
                  $wgo_shDQ ww5_shDO w4_shDL
                    = case ww5_shDO of wild1_X86 {
                        __DEFAULT ->
                          case >=# wild1_X86 0# of {
                            __DEFAULT -> jump exit_X3x wild1_X86;
                            1# ->
                              case header_abIW `cast` <Co:9> of
                              { MVector dt_af1X dt1_af1Y dt2_af1Z ->
                              case <# wild1_X86 dt1_af1Y of {
                                __DEFAULT -> case lvl37_ri95 wild1_X86 dt1_af1Y of wild3_00 { };
                                1# ->
                                  case readWordArray#
                                         dt2_af1Z (+# dt_af1X wild1_X86) (w4_shDL `cast` <Co:16>)
                                  of
                                  { (# ipv1_af2z, ipv2_af2A #) ->
                                  let { j'_sfE8 = *# wild1_X86 64# } in
                                  join {
                                    exit2_X3K w5_shDE
                                      = jump $wgo_shDQ (+# wild1_X86 1#) w5_shDE } in
                                  joinrec {
                                    $wgo1_shDJ ww6_shDH w5_shDE
                                      = case ww6_shDH of wild3_X8I {
                                          __DEFAULT ->
                                            case >=# wild3_X8I 0# of {
                                              __DEFAULT -> case overflowError of wild4_00 { };
                                              1# ->
                                                case >=# wild3_X8I 64# of {
                                                  __DEFAULT ->
                                                    case and#
                                                           ipv2_af2A
                                                           (uncheckedShiftL# 1## wild3_X8I)
                                                    of {
                                                      __DEFAULT ->
                                                        case ((w_shEb
                                                                 ((i1_afGd,
                                                                   I# (+# j'_sfE8 wild3_X8I))
                                                                  `cast` <Co:5>))
                                                              `cast` <Co:3>)
                                                               w5_shDE
                                                        of
                                                        { (# ipv3_aevf, ipv4_aevg #) ->
                                                        jump $wgo1_shDJ (+# wild3_X8I 1#) ipv3_aevf
                                                        };
                                                      0## ->
                                                        jump $wgo1_shDJ (+# wild3_X8I 1#) w5_shDE
                                                    };
                                                  1# -> jump $wgo1_shDJ (+# wild3_X8I 1#) w5_shDE
                                                }
                                            };
                                          64# -> jump exit2_X3K w5_shDE
                                        }; } in
                                  jump $wgo1_shDJ 0# (ipv1_af2z `cast` <Co:15>)
                                  }
                              }
                              }
                          };
                        16# -> jump exit1_X3y w4_shDL
                      }; } in
                jump $wgo_shDQ 0# w3_shDT
                }
                };
              1# -> (# w3_shDT, () #)
            }
            }; } in
      jump $wconsume_loop_shE7 SPEC 0# 0# w1_shEc

-- RHS size: {terms: 17, types: 46, coercions: 0, joins: 0/0}
$citer_ri9b
  = \ @ s_shE8 @ as_shE9 w_shEa w1_shEb w2_shEc ->
      case w_shEa of { Archetype ww1_shEf ww2_shEg ->
      case ww2_shEg of { Vector ww4_shX9 ww5_shXa ww6_shXb ->
      $w$citer_ri9a ww4_shX9 ww5_shXa ww6_shXb w1_shEb w2_shEc
      }
      }

-- RHS size: {terms: 1, types: 0, coercions: 23, joins: 0/0}
$fStorageSTArchetype_$citer = $citer_ri9b `cast` <Co:23>

-- RHS size: {terms: 5, types: 13, coercions: 0, joins: 0/0}
$fStorageSTArchetype
  = \ @ s_XcG3 @ as_XcG5 ->
      C:Storage $fMonadST $fStorageSTArchetype_$citer

-- RHS size: {terms: 10, types: 25, coercions: 0, joins: 0/0}
$fStorageSetSTArchetypeHList
  = \ @ bs_act9
      @ as_acta
      @ s_actb
      $dIsStorageVector_actc
      $dChunkHas'_actd ->
      C:StorageSet
        $fStorageSTArchetype
        ($fStorageSetSTArchetypeHList_$cset
           $dIsStorageVector_actc $dChunkHas'_actd)

-- RHS size: {terms: 10, types: 25, coercions: 0, joins: 0/0}
$fStorageGetSTArchetypeHList
  = \ @ bs_actW
      @ as_actX
      @ s_actY
      $dIsStorageVector_actZ
      $dChunkHas'_acu0 ->
      C:StorageGet
        $fStorageSTArchetype
        ($fStorageGetSTArchetypeHList_$cget
           $dIsStorageVector_actZ $dChunkHas'_acu0)



[8 of 8] Compiling Lib

==================== Simplified expression ====================
derivingUnbox
  (unpackCString# "ECSPos"#)
  (appT
     (appT
        arrowT
        (conT
           (mkNameG_tc
              (unpackCString# "ecs-0.1.0.0-6T7cQlWNmHv6W00SuOHZxa"#)
              (unpackCString# "Lib"#)
              (unpackCString# "ECSPos"#))))
     (appT
        (conT
           (mkNameG_tc
              (unpackCString# "linear-1.20.9-BShWWKI0Do72wQDcPXivRD"#)
              (unpackCString# "Linear.V2"#)
              (unpackCString# "V2"#)))
        (conT
           (mkNameG_tc
              (unpackCString# "ghc-prim"#)
              (unpackCString# "GHC.Types"#)
              (unpackCString# "Float"#)))))
  (varE
     (mkNameG_v
        (unpackCString# "ecs-0.1.0.0-6T7cQlWNmHv6W00SuOHZxa"#)
        (unpackCString# "Lib"#)
        (unpackCString# "unECSPos"#)))
  (conE
     (mkNameG_d
        (unpackCString# "ecs-0.1.0.0-6T7cQlWNmHv6W00SuOHZxa"#)
        (unpackCString# "Lib"#)
        (unpackCString# "ECSPos"#)))



==================== Simplified expression ====================
derivingUnbox
  (unpackCString# "ECSVel"#)
  (appT
     (appT
        arrowT
        (conT
           (mkNameG_tc
              (unpackCString# "ecs-0.1.0.0-6T7cQlWNmHv6W00SuOHZxa"#)
              (unpackCString# "Lib"#)
              (unpackCString# "ECSVel"#))))
     (appT
        (conT
           (mkNameG_tc
              (unpackCString# "linear-1.20.9-BShWWKI0Do72wQDcPXivRD"#)
              (unpackCString# "Linear.V2"#)
              (unpackCString# "V2"#)))
        (conT
           (mkNameG_tc
              (unpackCString# "ghc-prim"#)
              (unpackCString# "GHC.Types"#)
              (unpackCString# "Float"#)))))
  (varE
     (mkNameG_v
        (unpackCString# "ecs-0.1.0.0-6T7cQlWNmHv6W00SuOHZxa"#)
        (unpackCString# "Lib"#)
        (unpackCString# "unECSVel"#)))
  (conE
     (mkNameG_d
        (unpackCString# "ecs-0.1.0.0-6T7cQlWNmHv6W00SuOHZxa"#)
        (unpackCString# "Lib"#)
        (unpackCString# "ECSVel"#)))



==================== Tidy Core ====================
Result size of Tidy Core
  = {terms: 5,658, types: 8,851, coercions: 2,304, joins: 19/159}

-- RHS size: {terms: 2, types: 3, coercions: 4, joins: 0/0}
$WV_ECSVel = \ dt_aE8Q -> (dt_aE8Q `cast` <Co:2>) `cast` <Co:2>

-- RHS size: {terms: 3, types: 6, coercions: 6, joins: 0/0}
$WMV_ECSVel
  = \ @ s_aE8h dt_aE9e -> (dt_aE9e `cast` <Co:3>) `cast` <Co:3>

-- RHS size: {terms: 2, types: 3, coercions: 4, joins: 0/0}
$WV_ECSPos = \ dt_aDOR -> (dt_aDOR `cast` <Co:2>) `cast` <Co:2>

-- RHS size: {terms: 3, types: 6, coercions: 6, joins: 0/0}
$WMV_ECSPos
  = \ @ s_aDOs dt_aDPf -> (dt_aDPf `cast` <Co:3>) `cast` <Co:3>

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl1_rQLL = I# 78#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl2_rQLM = I# 106#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl3_rQLN = I# 15#

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
lvl4_rQLO = "./Data/Vector/Primitive/Mutable.hs"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl5_rQLP = unpackCString# lvl4_rQLO

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
lvl6_rQLQ = "Data.Vector.Primitive.Mutable"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl7_rQLR = unpackCString# lvl6_rQLQ

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
lvl8_rQLS = "vector-0.12.1.2-9VEK1w6XuCFF3hLj52eD2N"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl9_rQLT = unpackCString# lvl8_rQLS

-- RHS size: {terms: 8, types: 0, coercions: 0, joins: 0/0}
lvl10_rQLU
  = SrcLoc
      lvl9_rQLT
      lvl7_rQLR
      lvl5_rQLP
      lvl2_rQLM
      lvl3_rQLN
      lvl2_rQLM
      lvl1_rQLL

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
lvl11_rQLV = "error"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl12_rQLW = unpackCString# lvl11_rQLV

-- RHS size: {terms: 4, types: 0, coercions: 0, joins: 0/0}
lvl13_rQLX = PushCallStack lvl12_rQLW lvl10_rQLU EmptyCallStack

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
lvl14_rQLY = "Primitive.basicUnsafeNew: negative length: "#

-- RHS size: {terms: 15, types: 22, coercions: 4, joins: 0/0}
$wlvl_rQLZ
  = \ @ m_sOxg ww_sOxk ->
      error
        (lvl13_rQLX `cast` <Co:4>)
        (unpackAppendCString#
           lvl14_rQLY
           (case $wshowSignedInt 0# ww_sOxk [] of
            { (# ww5_afde, ww6_afdf #) ->
            : ww5_afde ww6_afdf
            }))

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl15_rQM0 = I# 79#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl16_rQM1 = I# 107#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl17_rQM2 = I# 16#

-- RHS size: {terms: 8, types: 0, coercions: 0, joins: 0/0}
lvl18_rQM3
  = SrcLoc
      lvl9_rQLT
      lvl7_rQLR
      lvl5_rQLP
      lvl16_rQM1
      lvl17_rQM2
      lvl16_rQM1
      lvl15_rQM0

-- RHS size: {terms: 4, types: 0, coercions: 0, joins: 0/0}
lvl19_rQM4 = PushCallStack lvl12_rQLW lvl18_rQM3 EmptyCallStack

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
lvl20_rQM5 = "Primitive.basicUnsafeNew: length to large: "#

-- RHS size: {terms: 15, types: 22, coercions: 4, joins: 0/0}
$wlvl1_rQM6
  = \ @ m_sOxn ww_sOxr ->
      error
        (lvl19_rQM4 `cast` <Co:4>)
        (unpackAppendCString#
           lvl20_rQM5
           (case $wshowSignedInt 0# ww_sOxr [] of
            { (# ww5_afde, ww6_afdf #) ->
            : ww5_afde ww6_afdf
            }))

-- RHS size: {terms: 204, types: 339, coercions: 81, joins: 0/7}
$fMVectorMVectorECSVel_$cbasicUnsafeGrow
  = \ @ m_aEi9 $dPrimMonad_aEib eta_B1 eta1_X2 ->
      case $p1PrimMonad $dPrimMonad_aEib of
      { C:Monad ww1_sOxw ww2_sOxx ww3_sOxy ww4_sOxz ->
      ww2_sOxx
        (let { lvl23_sIdz = ww4_sOxz () } in
         ww2_sOxx
           (let {
              n1_sHQI
                = case eta_B1 `cast` <Co:12> of { MV_V2 dt_aEWg ds_aEWh ->
                  case eta1_X2 of { I# y_aEWl -> I# (+# dt_aEWg y_aEWl) }
                  } } in
            ww2_sOxx
              (ww2_sOxx
                 (case n1_sHQI of { I# y_aEWp ->
                  let { x_aevP = *# 2# y_aEWp } in
                  case <# x_aevP 0# of {
                    __DEFAULT ->
                      case ># x_aevP 2305843009213693951# of {
                        __DEFAULT ->
                          ww2_sOxx
                            (primitive
                               $dPrimMonad_aEib
                               (\ s#_aEYm ->
                                  case newByteArray# (*# x_aevP 4#) s#_aEYm of
                                  { (# ipv_aEYp, ipv1_aEYq #) ->
                                  (# ipv_aEYp, MutableByteArray ipv1_aEYq #)
                                  }))
                            (\ x1_aEYs ->
                               ww4_sOxz
                                 (case x1_aEYs of { MutableByteArray dt1_af1V ->
                                  MVector 0# x_aevP dt1_af1V
                                  }));
                        1# -> $wlvl1_rQM6 x_aevP
                      };
                    1# -> $wlvl_rQLZ x_aevP
                  }
                  })
                 (\ x1_aEYL -> ww4_sOxz (x1_aEYL `cast` <Co:9>)))
              (\ x1_aEWr ->
                 ww4_sOxz
                   (case n1_sHQI of { I# dt1_aEW7 ->
                    case x1_aEWr `cast` <Co:7> of nt_sItS
                    { MVector ipv_sItU ipv1_sItV ipv2_sItW ->
                    (MV_V2 dt1_aEW7 (nt_sItS `cast` <Co:9>)) `cast` <Co:5>
                    }
                    })))
           (\ v'_aEWs ->
              ww3_sOxy
                (case v'_aEWs `cast` <Co:4> of { MV_V2 dt_aEWw v_aEWx ->
                 case v_aEWx `cast` <Co:7> of
                 { MVector dt1_sPUA dt2_sPUB dt3_sPUC ->
                 case eta_B1 `cast` <Co:12> of { MV_V2 dt4_aEW9 ds_aEWa ->
                 case ds_aEWa `cast` <Co:7> of
                 { MVector dt5_sPUF dt6_sPUG dt7_sPUH ->
                 letrec {
                   $wdo_copy_sOy8
                     = \ ww5_sOy6 ->
                         case <# ww5_sOy6 dt4_aEW9 of {
                           __DEFAULT -> lvl23_sIdz;
                           1# ->
                             let { o_sNtE = *# 2# ww5_sOy6 } in
                             let { lvl24_sIe6 = $wdo_copy_sOy8 (+# ww5_sOy6 1#) } in
                             ww2_sOxx
                               (let {
                                  lvl25_sIdM
                                    = primitive
                                        $dPrimMonad_aEib
                                        ($fPrimCFloat_$creadByteArray#
                                           dt7_sPUH (+# 1# (+# dt5_sPUF o_sNtE))) } in
                                ww2_sOxx
                                  (primitive
                                     $dPrimMonad_aEib
                                     ($fPrimCFloat_$creadByteArray# dt7_sPUH (+# dt5_sPUF o_sNtE)))
                                  (\ x_aEX3 ->
                                     ww2_sOxx
                                       lvl25_sIdM
                                       (\ y_aEX8 ->
                                          ww4_sOxz
                                            (case x_aEX3 of dt8_XEXe { F# ipv_sIub ->
                                             case y_aEX8 of dt9_XEXg { F# ipv1_sIue ->
                                             V2 dt8_XEXe dt9_XEXg
                                             }
                                             }))))
                               (\ x_aEXc ->
                                  ww3_sOxy
                                    (case x_aEXc of { V2 x1_aEXh y_aEXi ->
                                     case x1_aEXh of { F# x#_sPUK ->
                                     case y_aEXi of { F# x#1_sPUN ->
                                     ww3_sOxy
                                       (primitive
                                          $dPrimMonad_aEib
                                          (\ s#_aF0u ->
                                             case writeFloatArray#
                                                    dt3_sPUC (+# dt1_sPUA o_sNtE) x#_sPUK s#_aF0u
                                             of s'#_aF0z
                                             { __DEFAULT ->
                                             (# s'#_aF0z, () #)
                                             }))
                                       (primitive
                                          $dPrimMonad_aEib
                                          (\ s#_aF0u ->
                                             case writeFloatArray#
                                                    dt3_sPUC
                                                    (+# 1# (+# dt1_sPUA o_sNtE))
                                                    x#1_sPUN
                                                    s#_aF0u
                                             of s'#_aF0z
                                             { __DEFAULT ->
                                             (# s'#_aF0z, () #)
                                             }))
                                     }
                                     }
                                     })
                                    lvl24_sIe6)
                         }; } in
                 $wdo_copy_sOy8 0#
                 }
                 }
                 }
                 })
                (ww4_sOxz v'_aEWs)))
        (\ x1_aETa -> ww4_sOxz (x1_aETa `cast` <Co:9>))
      }

-- RHS size: {terms: 460, types: 529, coercions: 90, joins: 2/18}
$fMVectorMVectorECSVel_$cbasicUnsafeMove
  = \ @ m_aEhR $dPrimMonad_aEhT eta_B1 ds_dEUv ->
      case eta_B1 `cast` <Co:12> of { MV_V2 ipv_aF1r ipv1_aF1s ->
      case ipv1_aF1s `cast` <Co:7> of
      { MVector dt_sPUS dt1_sPUT dt2_sPUU ->
      case ds_dEUv `cast` <Co:12> of { MV_V2 ipv2_aF1w ipv3_aF1x ->
      case ipv3_aF1x `cast` <Co:7> of
      { MVector dt4_sPUX dt5_sPUY dt6_sPUZ ->
      join {
        $j_sIw9
          = let { $dMonad1_sHPx = $p1PrimMonad $dPrimMonad_aEhT } in
            let { lvl23_sIab = return $dMonad1_sHPx () } in
            letrec {
              $wdo_copy_sOys
                = \ ww_sOyq ->
                    case <# ww_sOyq ipv2_aF1w of {
                      __DEFAULT -> lvl23_sIab;
                      1# ->
                        let { o_sNur = *# 2# ww_sOyq } in
                        let { lvl24_sIaI = $wdo_copy_sOys (+# ww_sOyq 1#) } in
                        >>=
                          $dMonad1_sHPx
                          (let {
                             lvl25_sIao
                               = primitive
                                   $dPrimMonad_aEhT
                                   ($fPrimCFloat_$creadByteArray#
                                      dt6_sPUZ (+# 1# (+# dt4_sPUX o_sNur))) } in
                           >>=
                             $dMonad1_sHPx
                             (primitive
                                $dPrimMonad_aEhT
                                ($fPrimCFloat_$creadByteArray# dt6_sPUZ (+# dt4_sPUX o_sNur)))
                             (\ x_aF1S ->
                                >>=
                                  $dMonad1_sHPx
                                  lvl25_sIao
                                  (\ y_aF1X ->
                                     return
                                       $dMonad1_sHPx
                                       (case x_aF1S of dt7_XEXe { F# ipv4_sIv7 ->
                                        case y_aF1X of dt8_XEXg { F# ipv5_sIva ->
                                        V2 dt7_XEXe dt8_XEXg
                                        }
                                        }))))
                          (\ x_aF1Y ->
                             >>
                               $dMonad1_sHPx
                               (case x_aF1Y of { V2 x1_aF23 y_aF24 ->
                                case x1_aF23 of { F# x#_sPV2 ->
                                case y_aF24 of { F# x#1_sPV5 ->
                                >>
                                  $dMonad1_sHPx
                                  (primitive
                                     $dPrimMonad_aEhT
                                     (\ s#_aF0u ->
                                        case writeFloatArray#
                                               dt2_sPUU (+# dt_sPUS o_sNur) x#_sPV2 s#_aF0u
                                        of s'#_aF0z
                                        { __DEFAULT ->
                                        (# s'#_aF0z, () #)
                                        }))
                                  (primitive
                                     $dPrimMonad_aEhT
                                     (\ s#_aF0u ->
                                        case writeFloatArray#
                                               dt2_sPUU (+# 1# (+# dt_sPUS o_sNur)) x#1_sPV5 s#_aF0u
                                        of s'#_aF0z
                                        { __DEFAULT ->
                                        (# s'#_aF0z, () #)
                                        }))
                                }
                                }
                                })
                               lvl24_sIaI)
                    }; } in
            $wdo_copy_sOys 0# } in
      case sameMutableByteArray# dt2_sPUU dt6_sPUZ of {
        __DEFAULT -> jump $j_sIw9;
        1# ->
          join {
            $j1_sIwa
              = case $p1PrimMonad $dPrimMonad_aEhT of
                { C:Monad ww1_sOyv ww2_sOyw ww3_sOyx ww4_sOyy ->
                let { lvl23_sIc9 = ww4_sOyy () } in
                ww2_sOyw
                  (ww2_sOyw
                     (ww2_sOyw
                        (let { n_sHQc = *# 2# ipv2_aF1w } in
                         case <# n_sHQc 0# of {
                           __DEFAULT ->
                             case ># n_sHQc 2305843009213693951# of {
                               __DEFAULT ->
                                 ww2_sOyw
                                   (primitive
                                      $dPrimMonad_aEhT
                                      (\ s#_aEYm ->
                                         case newByteArray# (*# n_sHQc 4#) s#_aEYm of
                                         { (# ipv4_aEYp, ipv5_aEYq #) ->
                                         (# ipv4_aEYp, MutableByteArray ipv5_aEYq #)
                                         }))
                                   (\ x1_aEYs ->
                                      ww4_sOyy
                                        (case x1_aEYs of { MutableByteArray dt8_af1V ->
                                         MVector 0# n_sHQc dt8_af1V
                                         }));
                               1# -> $wlvl1_rQM6 n_sHQc
                             };
                           1# -> $wlvl_rQLZ n_sHQc
                         })
                        (\ x1_aEYL -> ww4_sOyy (x1_aEYL `cast` <Co:9>)))
                     (\ x1_aF2m ->
                        ww4_sOyy
                          (case x1_aF2m `cast` <Co:7> of nt2_sIvn
                           { MVector ipv4_sIvp ipv5_sIvq ipv6_sIvr ->
                           (MV_V2 ipv2_aF1w (nt2_sIvn `cast` <Co:9>)) `cast` <Co:5>
                           })))
                  (\ srcCopy_aF2n ->
                     ww3_sOyx
                       (case srcCopy_aF2n `cast` <Co:4> of { MV_V2 ipv4_aF2s ipv5_aF2t ->
                        case ipv5_aF2t `cast` <Co:7> of
                        { MVector dt7_sPV8 dt8_sPV9 dt9_sPVa ->
                        letrec {
                          $wdo_copy_sOz0
                            = \ ww5_sOyY ->
                                case <# ww5_sOyY ipv2_aF1w of {
                                  __DEFAULT -> lvl23_sIc9;
                                  1# ->
                                    let { o_sNuJ = *# 2# ww5_sOyY } in
                                    let { lvl24_sIc5 = $wdo_copy_sOz0 (+# ww5_sOyY 1#) } in
                                    ww2_sOyw
                                      (let {
                                         lvl25_sIbL
                                           = primitive
                                               $dPrimMonad_aEhT
                                               ($fPrimCFloat_$creadByteArray#
                                                  dt6_sPUZ (+# 1# (+# dt4_sPUX o_sNuJ))) } in
                                       ww2_sOyw
                                         (primitive
                                            $dPrimMonad_aEhT
                                            ($fPrimCFloat_$creadByteArray#
                                               dt6_sPUZ (+# dt4_sPUX o_sNuJ)))
                                         (\ x_aF2J ->
                                            ww2_sOyw
                                              lvl25_sIbL
                                              (\ y_aF2O ->
                                                 ww4_sOyy
                                                   (case x_aF2J of dt10_XEXe { F# ipv7_sIvC ->
                                                    case y_aF2O of dt11_XEXg { F# ipv8_sIvF ->
                                                    V2 dt10_XEXe dt11_XEXg
                                                    }
                                                    }))))
                                      (\ x_aF2P ->
                                         ww3_sOyx
                                           (case x_aF2P of { V2 x1_aF2U y_aF2V ->
                                            case x1_aF2U of { F# x#_sPVd ->
                                            case y_aF2V of { F# x#1_sPVg ->
                                            ww3_sOyx
                                              (primitive
                                                 $dPrimMonad_aEhT
                                                 (\ s#_aF0u ->
                                                    case writeFloatArray#
                                                           dt9_sPVa
                                                           (+# dt7_sPV8 o_sNuJ)
                                                           x#_sPVd
                                                           s#_aF0u
                                                    of s'#_aF0z
                                                    { __DEFAULT ->
                                                    (# s'#_aF0z, () #)
                                                    }))
                                              (primitive
                                                 $dPrimMonad_aEhT
                                                 (\ s#_aF0u ->
                                                    case writeFloatArray#
                                                           dt9_sPVa
                                                           (+# 1# (+# dt7_sPV8 o_sNuJ))
                                                           x#1_sPVg
                                                           s#_aF0u
                                                    of s'#_aF0z
                                                    { __DEFAULT ->
                                                    (# s'#_aF0z, () #)
                                                    }))
                                            }
                                            }
                                            })
                                           lvl24_sIc5)
                                }; } in
                        $wdo_copy_sOz0 0#
                        }
                        })
                       (case srcCopy_aF2n `cast` <Co:4> of { MV_V2 ipv4_aF3d ipv5_aF3e ->
                        case ipv5_aF3e `cast` <Co:7> of
                        { MVector dt7_sPVj dt8_sPVk dt9_sPVl ->
                        letrec {
                          $wdo_copy_sOz6
                            = \ ww5_sOz4 ->
                                case <# ww5_sOz4 ipv4_aF3d of {
                                  __DEFAULT -> lvl23_sIc9;
                                  1# ->
                                    let { o_sNuV = *# 2# ww5_sOz4 } in
                                    let { lvl24_sIcG = $wdo_copy_sOz6 (+# ww5_sOz4 1#) } in
                                    ww2_sOyw
                                      (let {
                                         lvl25_sIcm
                                           = primitive
                                               $dPrimMonad_aEhT
                                               ($fPrimCFloat_$creadByteArray#
                                                  dt9_sPVl (+# 1# (+# dt7_sPVj o_sNuV))) } in
                                       ww2_sOyw
                                         (primitive
                                            $dPrimMonad_aEhT
                                            ($fPrimCFloat_$creadByteArray#
                                               dt9_sPVl (+# dt7_sPVj o_sNuV)))
                                         (\ x_aF3u ->
                                            ww2_sOyw
                                              lvl25_sIcm
                                              (\ y_aF3z ->
                                                 ww4_sOyy
                                                   (case x_aF3u of dt10_XEXe { F# ipv7_sIvX ->
                                                    case y_aF3z of dt11_XEXg { F# ipv8_sIw0 ->
                                                    V2 dt10_XEXe dt11_XEXg
                                                    }
                                                    }))))
                                      (\ x_aF3A ->
                                         ww3_sOyx
                                           (case x_aF3A of { V2 x1_aF3F y_aF3G ->
                                            case x1_aF3F of { F# x#_sPVo ->
                                            case y_aF3G of { F# x#1_sPVr ->
                                            ww3_sOyx
                                              (primitive
                                                 $dPrimMonad_aEhT
                                                 (\ s#_aF0u ->
                                                    case writeFloatArray#
                                                           dt2_sPUU
                                                           (+# dt_sPUS o_sNuV)
                                                           x#_sPVo
                                                           s#_aF0u
                                                    of s'#_aF0z
                                                    { __DEFAULT ->
                                                    (# s'#_aF0z, () #)
                                                    }))
                                              (primitive
                                                 $dPrimMonad_aEhT
                                                 (\ s#_aF0u ->
                                                    case writeFloatArray#
                                                           dt2_sPUU
                                                           (+# 1# (+# dt_sPUS o_sNuV))
                                                           x#1_sPVr
                                                           s#_aF0u
                                                    of s'#_aF0z
                                                    { __DEFAULT ->
                                                    (# s'#_aF0z, () #)
                                                    }))
                                            }
                                            }
                                            })
                                           lvl24_sIcG)
                                }; } in
                        $wdo_copy_sOz6 0#
                        }
                        }))
                } } in
          case >=# dt_sPUS dt4_sPUX of {
            __DEFAULT ->
              case >=# dt4_sPUX dt_sPUS of {
                __DEFAULT -> jump $j_sIw9;
                1# ->
                  case <# dt4_sPUX (+# dt_sPUS dt1_sPUT) of {
                    __DEFAULT -> jump $j_sIw9;
                    1# -> jump $j1_sIwa
                  }
              };
            1# ->
              case <# dt_sPUS (+# dt4_sPUX dt5_sPUY) of {
                __DEFAULT ->
                  case >=# dt4_sPUX dt_sPUS of {
                    __DEFAULT -> jump $j_sIw9;
                    1# ->
                      case <# dt4_sPUX (+# dt_sPUS dt1_sPUT) of {
                        __DEFAULT -> jump $j_sIw9;
                        1# -> jump $j1_sIwa
                      }
                  };
                1# -> jump $j1_sIwa
              }
          }
      }
      }
      }
      }
      }

-- RHS size: {terms: 127, types: 149, coercions: 38, joins: 0/6}
$fMVectorMVectorECSVel_$cbasicUnsafeCopy
  = \ @ m_aEhz $dPrimMonad_aEhB eta_B1 ds_dEUr ->
      case eta_B1 `cast` <Co:12> of { MV_V2 ipv_aF70 ipv1_aF71 ->
      case ipv1_aF71 `cast` <Co:7> of
      { MVector dt_sPVu dt1_sPVv dt2_sPVw ->
      case ds_dEUr `cast` <Co:12> of { MV_V2 ipv2_aF76 ipv3_aF77 ->
      case ipv3_aF77 `cast` <Co:7> of
      { MVector dt4_sPVz dt5_sPVA dt6_sPVB ->
      let { $dMonad_sHPc = $p1PrimMonad $dPrimMonad_aEhB } in
      let { lvl23_sI9s = return $dMonad_sHPc () } in
      letrec {
        $wdo_copy_sOzc
          = \ ww_sOza ->
              case <# ww_sOza ipv2_aF76 of {
                __DEFAULT -> lvl23_sI9s;
                1# ->
                  let { o_sNwb = *# 2# ww_sOza } in
                  let { lvl24_sI9Z = $wdo_copy_sOzc (+# ww_sOza 1#) } in
                  >>=
                    $dMonad_sHPc
                    (let {
                       lvl25_sI9F
                         = primitive
                             $dPrimMonad_aEhB
                             ($fPrimCFloat_$creadByteArray#
                                dt6_sPVB (+# 1# (+# dt4_sPVz o_sNwb))) } in
                     >>=
                       $dMonad_sHPc
                       (primitive
                          $dPrimMonad_aEhB
                          ($fPrimCFloat_$creadByteArray# dt6_sPVB (+# dt4_sPVz o_sNwb)))
                       (\ x_aF7n ->
                          >>=
                            $dMonad_sHPc
                            lvl25_sI9F
                            (\ y_aF7s ->
                               return
                                 $dMonad_sHPc
                                 (case x_aF7n of dt7_XEXe { F# ipv4_sIxp ->
                                  case y_aF7s of dt8_XEXg { F# ipv5_sIxs -> V2 dt7_XEXe dt8_XEXg }
                                  }))))
                    (\ x_aF7t ->
                       >>
                         $dMonad_sHPc
                         (case x_aF7t of { V2 x1_aF7y y_aF7z ->
                          case x1_aF7y of { F# x#_sPVE ->
                          case y_aF7z of { F# x#1_sPVH ->
                          >>
                            $dMonad_sHPc
                            (primitive
                               $dPrimMonad_aEhB
                               (\ s#_aF0u ->
                                  case writeFloatArray# dt2_sPVw (+# dt_sPVu o_sNwb) x#_sPVE s#_aF0u
                                  of s'#_aF0z
                                  { __DEFAULT ->
                                  (# s'#_aF0z, () #)
                                  }))
                            (primitive
                               $dPrimMonad_aEhB
                               (\ s#_aF0u ->
                                  case writeFloatArray#
                                         dt2_sPVw (+# 1# (+# dt_sPVu o_sNwb)) x#1_sPVH s#_aF0u
                                  of s'#_aF0z
                                  { __DEFAULT ->
                                  (# s'#_aF0z, () #)
                                  }))
                          }
                          }
                          })
                         lvl24_sI9Z)
              }; } in
      $wdo_copy_sOzc 0#
      }
      }
      }
      }

-- RHS size: {terms: 7, types: 13, coercions: 0, joins: 0/0}
$fMVectorMVectorECSVel_$cbasicClear
  = \ @ m_aEh5 $dPrimMonad_aEh7 _ ->
      return ($p1PrimMonad $dPrimMonad_aEh7) ()

-- RHS size: {terms: 64, types: 98, coercions: 21, joins: 0/2}
$fMVectorMVectorECSVel_$cbasicUnsafeRead
  = \ @ m_aEgx $dPrimMonad_aEgz eta_B1 eta1_X2 ->
      case $p1PrimMonad $dPrimMonad_aEgz of
      { C:Monad ww1_sOzf ww2_sOzg ww3_sOzh ww4_sOzi ->
      ww2_sOzg
        (case eta_B1 `cast` <Co:12> of { MV_V2 dt_aF8L v_aF8M ->
         case v_aF8M `cast` <Co:7> of
         { MVector dt1_sPVK dt2_sPVL dt3_sPVM ->
         let {
           o_sHP5 = case eta1_X2 of { I# y_aF8Q -> I# (*# 2# y_aF8Q) } } in
         let {
           lvl23_sI9r
             = case o_sHP5 of { I# x1_aF8V ->
               primitive
                 $dPrimMonad_aEgz
                 ($fPrimCFloat_$creadByteArray#
                    dt3_sPVM (+# 1# (+# dt1_sPVK x1_aF8V)))
               } } in
         ww2_sOzg
           (case o_sHP5 of { I# y_aEZV ->
            primitive
              $dPrimMonad_aEgz
              ($fPrimCFloat_$creadByteArray# dt3_sPVM (+# dt1_sPVK y_aEZV))
            })
           (\ x_aF8S ->
              ww2_sOzg
                lvl23_sI9r
                (\ y_aF8X ->
                   ww4_sOzi
                     (case x_aF8S of dt4_XEXe { F# ipv_sIy0 ->
                      case y_aF8X of dt5_XEXg { F# ipv1_sIy3 -> V2 dt4_XEXe dt5_XEXg }
                      })))
         }
         })
        (\ x1_aETa -> ww4_sOzi (x1_aETa `cast` <Co:2>))
      }

-- RHS size: {terms: 28, types: 51, coercions: 31, joins: 0/0}
$fMVectorMVectorECSVel_$cbasicInitialize
  = \ @ m_aEfZ $dPrimMonad_aEg1 ds_dEUg ->
      case ds_dEUg `cast` <Co:12> of { MV_V2 dt_aF9l v_aF9m ->
      case v_aF9m `cast` <Co:7> of
      { MVector dt1_sPVP dt2_sPVQ dt3_sPVR ->
      primitive
        $dPrimMonad_aEg1
        (\ s#_aF9C ->
           case {__pkg_ccall primitive-0.7.0.0 forall s.
                               MutableByteArray# s
                               -> Int#
                               -> Word#
                               -> Word#
                               -> State# RealWorld
                               -> (# State# RealWorld #)}_aF9G
                  dt3_sPVR
                  (*# dt1_sPVP 4#)
                  (int2Word# (*# dt2_sPVQ 4#))
                  0##
                  (s#_aF9C `cast` <Co:6>)
           of
           { (# ds9_aF9L #) ->
           (# ds9_aF9L `cast` <Co:6>, () #)
           })
      }
      }

-- RHS size: {terms: 74, types: 172, coercions: 39, joins: 0/1}
$fMVectorMVectorECSVel_$cbasicUnsafeNew
  = \ @ m_aEfG $dPrimMonad_aEfI eta_B1 ->
      case $p1PrimMonad $dPrimMonad_aEfI of
      { C:Monad ww1_sOzH ww2_sOzI ww3_sOzJ ww4_sOzK ->
      ww2_sOzI
        (ww2_sOzI
           (ww2_sOzI
              (case eta_B1 of { I# y_aFa5 ->
               let { x_aevP = *# 2# y_aFa5 } in
               case <# x_aevP 0# of {
                 __DEFAULT ->
                   case ># x_aevP 2305843009213693951# of {
                     __DEFAULT ->
                       ww2_sOzI
                         (primitive
                            $dPrimMonad_aEfI
                            (\ s#_aEYm ->
                               case newByteArray# (*# x_aevP 4#) s#_aEYm of
                               { (# ipv_aEYp, ipv1_aEYq #) ->
                               (# ipv_aEYp, MutableByteArray ipv1_aEYq #)
                               }))
                         (\ x1_aEYs ->
                            ww4_sOzK
                              (case x1_aEYs of { MutableByteArray dt1_af1V ->
                               MVector 0# x_aevP dt1_af1V
                               }));
                     1# -> $wlvl1_rQM6 x_aevP
                   };
                 1# -> $wlvl_rQLZ x_aevP
               }
               })
              (\ x1_aEYL -> ww4_sOzK (x1_aEYL `cast` <Co:9>)))
           (\ x1_aFa7 ->
              ww4_sOzK
                (case eta_B1 of { I# dt1_aEW7 ->
                 case x1_aFa7 `cast` <Co:7> of nt_sIyv
                 { MVector ipv_sIyx ipv1_sIyy ipv2_sIyz ->
                 (MV_V2 dt1_aEW7 (nt_sIyv `cast` <Co:9>)) `cast` <Co:5>
                 }
                 })))
        (\ x1_aETa -> ww4_sOzK (x1_aETa `cast` <Co:9>))
      }

-- RHS size: {terms: 63, types: 44, coercions: 28, joins: 0/0}
$fMVectorMVectorECSVel_$cbasicOverlaps
  = \ @ s_aEfu eta_B2 eta1_B1 ->
      case eta_B2 `cast` <Co:9> of { MV_V2 dt_aFas v_aFat ->
      case v_aFat `cast` <Co:5> of
      { MVector dt1_sPVW dt2_sPVX dt3_sPVY ->
      case eta1_B1 `cast` <Co:9> of { MV_V2 dt4_aFax u_aFay ->
      case u_aFay `cast` <Co:5> of
      { MVector dt5_sPW1 dt6_sPW2 dt7_sPW3 ->
      case sameMutableByteArray# dt3_sPVY dt7_sPW3 of {
        __DEFAULT -> False;
        1# ->
          case >=# dt1_sPVW dt5_sPW1 of {
            __DEFAULT ->
              case >=# dt5_sPW1 dt1_sPVW of {
                __DEFAULT -> False;
                1# -> tagToEnum# (<# dt5_sPW1 (+# dt1_sPVW dt2_sPVX))
              };
            1# ->
              case <# dt1_sPVW (+# dt5_sPW1 dt6_sPW2) of {
                __DEFAULT ->
                  case >=# dt5_sPW1 dt1_sPVW of {
                    __DEFAULT -> False;
                    1# -> tagToEnum# (<# dt5_sPW1 (+# dt1_sPVW dt2_sPVX))
                  };
                1# -> True
              }
          }
      }
      }
      }
      }
      }

-- RHS size: {terms: 28, types: 29, coercions: 33, joins: 0/0}
$fMVectorMVectorECSVel_$cbasicUnsafeSlice
  = \ @ s_aEfi eta_B3 eta1_B2 eta2_B1 ->
      case eta2_B1 `cast` <Co:9> of { MV_V2 dt_aFaZ v_aFb0 ->
      case v_aFb0 `cast` <Co:5> of
      { MVector dt1_sPW6 dt2_sPW7 dt3_sPW8 ->
      case eta1_B2 of { I# dt5_aEW7 ->
      case eta_B3 of { I# y_aFb4 ->
      (MV_V2
         dt5_aEW7
         ((MVector (+# dt1_sPW6 (*# 2# y_aFb4)) (*# 2# dt5_aEW7) dt3_sPW8)
          `cast` <Co:7>))
      `cast` <Co:12>
      }
      }
      }
      }

-- RHS size: {terms: 7, types: 12, coercions: 9, joins: 0/0}
$fMVectorMVectorECSVel_$cbasicLength
  = \ @ s_aEf7 eta_B1 ->
      case eta_B1 `cast` <Co:9> of { MV_V2 dt_aFbs ds1_aFbt ->
      I# dt_aFbs
      }

-- RHS size: {terms: 204, types: 339, coercions: 81, joins: 0/7}
$fMVectorMVectorECSPos_$cbasicUnsafeGrow
  = \ @ m_aE6X $dPrimMonad_aE6Z eta_B1 eta1_X2 ->
      case $p1PrimMonad $dPrimMonad_aE6Z of
      { C:Monad ww1_sOAn ww2_sOAo ww3_sOAp ww4_sOAq ->
      ww2_sOAo
        (let { lvl23_sI7E = ww4_sOAq () } in
         ww2_sOAo
           (let {
              n1_sHO4
                = case eta_B1 `cast` <Co:12> of { MV_V2 dt_aEWg ds_aEWh ->
                  case eta1_X2 of { I# y_aEWl -> I# (+# dt_aEWg y_aEWl) }
                  } } in
            ww2_sOAo
              (ww2_sOAo
                 (case n1_sHO4 of { I# y_aEWp ->
                  let { x_aevP = *# 2# y_aEWp } in
                  case <# x_aevP 0# of {
                    __DEFAULT ->
                      case ># x_aevP 2305843009213693951# of {
                        __DEFAULT ->
                          ww2_sOAo
                            (primitive
                               $dPrimMonad_aE6Z
                               (\ s#_aEYm ->
                                  case newByteArray# (*# x_aevP 4#) s#_aEYm of
                                  { (# ipv_aEYp, ipv1_aEYq #) ->
                                  (# ipv_aEYp, MutableByteArray ipv1_aEYq #)
                                  }))
                            (\ x1_aEYs ->
                               ww4_sOAq
                                 (case x1_aEYs of { MutableByteArray dt1_af1V ->
                                  MVector 0# x_aevP dt1_af1V
                                  }));
                        1# -> $wlvl1_rQM6 x_aevP
                      };
                    1# -> $wlvl_rQLZ x_aevP
                  }
                  })
                 (\ x1_aEYL -> ww4_sOAq (x1_aEYL `cast` <Co:9>)))
              (\ x1_aEWr ->
                 ww4_sOAq
                   (case n1_sHO4 of { I# dt1_aEW7 ->
                    case x1_aEWr `cast` <Co:7> of nt_sIzp
                    { MVector ipv_sIzr ipv1_sIzs ipv2_sIzt ->
                    (MV_V2 dt1_aEW7 (nt_sIzp `cast` <Co:9>)) `cast` <Co:5>
                    }
                    })))
           (\ v'_aEWs ->
              ww3_sOAp
                (case v'_aEWs `cast` <Co:4> of { MV_V2 dt_aEWw v_aEWx ->
                 case v_aEWx `cast` <Co:7> of
                 { MVector dt1_sPWd dt2_sPWe dt3_sPWf ->
                 case eta_B1 `cast` <Co:12> of { MV_V2 dt4_aEW9 ds_aEWa ->
                 case ds_aEWa `cast` <Co:7> of
                 { MVector dt5_sPWi dt6_sPWj dt7_sPWk ->
                 letrec {
                   $wdo_copy_sOAZ
                     = \ ww5_sOAX ->
                         case <# ww5_sOAX dt4_aEW9 of {
                           __DEFAULT -> lvl23_sI7E;
                           1# ->
                             let { o_sNxY = *# 2# ww5_sOAX } in
                             let { lvl24_sI8b = $wdo_copy_sOAZ (+# ww5_sOAX 1#) } in
                             ww2_sOAo
                               (let {
                                  lvl25_sI7R
                                    = primitive
                                        $dPrimMonad_aE6Z
                                        ($fPrimCFloat_$creadByteArray#
                                           dt7_sPWk (+# 1# (+# dt5_sPWi o_sNxY))) } in
                                ww2_sOAo
                                  (primitive
                                     $dPrimMonad_aE6Z
                                     ($fPrimCFloat_$creadByteArray# dt7_sPWk (+# dt5_sPWi o_sNxY)))
                                  (\ x_aEX3 ->
                                     ww2_sOAo
                                       lvl25_sI7R
                                       (\ y_aEX8 ->
                                          ww4_sOAq
                                            (case x_aEX3 of dt8_XEXe { F# ipv_sIzI ->
                                             case y_aEX8 of dt9_XEXg { F# ipv1_sIzL ->
                                             V2 dt8_XEXe dt9_XEXg
                                             }
                                             }))))
                               (\ x_aEXc ->
                                  ww3_sOAp
                                    (case x_aEXc of { V2 x1_aEXh y_aEXi ->
                                     case x1_aEXh of { F# x#_sPWn ->
                                     case y_aEXi of { F# x#1_sPWq ->
                                     ww3_sOAp
                                       (primitive
                                          $dPrimMonad_aE6Z
                                          (\ s#_aF0u ->
                                             case writeFloatArray#
                                                    dt3_sPWf (+# dt1_sPWd o_sNxY) x#_sPWn s#_aF0u
                                             of s'#_aF0z
                                             { __DEFAULT ->
                                             (# s'#_aF0z, () #)
                                             }))
                                       (primitive
                                          $dPrimMonad_aE6Z
                                          (\ s#_aF0u ->
                                             case writeFloatArray#
                                                    dt3_sPWf
                                                    (+# 1# (+# dt1_sPWd o_sNxY))
                                                    x#1_sPWq
                                                    s#_aF0u
                                             of s'#_aF0z
                                             { __DEFAULT ->
                                             (# s'#_aF0z, () #)
                                             }))
                                     }
                                     }
                                     })
                                    lvl24_sI8b)
                         }; } in
                 $wdo_copy_sOAZ 0#
                 }
                 }
                 }
                 })
                (ww4_sOAq v'_aEWs)))
        (\ x1_aETa -> ww4_sOAq (x1_aETa `cast` <Co:9>))
      }

-- RHS size: {terms: 460, types: 529, coercions: 90, joins: 2/18}
$fMVectorMVectorECSPos_$cbasicUnsafeMove
  = \ @ m_aE6F $dPrimMonad_aE6H eta_B1 ds_dETN ->
      case eta_B1 `cast` <Co:12> of { MV_V2 ipv_aF1r ipv1_aF1s ->
      case ipv1_aF1s `cast` <Co:7> of
      { MVector dt_sPWv dt1_sPWw dt2_sPWx ->
      case ds_dETN `cast` <Co:12> of { MV_V2 ipv2_aF1w ipv3_aF1x ->
      case ipv3_aF1x `cast` <Co:7> of
      { MVector dt4_sPWA dt5_sPWB dt6_sPWC ->
      join {
        $j_sIBG
          = let { $dMonad1_sHMT = $p1PrimMonad $dPrimMonad_aE6H } in
            let { lvl23_sI4g = return $dMonad1_sHMT () } in
            letrec {
              $wdo_copy_sOBj
                = \ ww_sOBh ->
                    case <# ww_sOBh ipv2_aF1w of {
                      __DEFAULT -> lvl23_sI4g;
                      1# ->
                        let { o_sNyL = *# 2# ww_sOBh } in
                        let { lvl24_sI4N = $wdo_copy_sOBj (+# ww_sOBh 1#) } in
                        >>=
                          $dMonad1_sHMT
                          (let {
                             lvl25_sI4t
                               = primitive
                                   $dPrimMonad_aE6H
                                   ($fPrimCFloat_$creadByteArray#
                                      dt6_sPWC (+# 1# (+# dt4_sPWA o_sNyL))) } in
                           >>=
                             $dMonad1_sHMT
                             (primitive
                                $dPrimMonad_aE6H
                                ($fPrimCFloat_$creadByteArray# dt6_sPWC (+# dt4_sPWA o_sNyL)))
                             (\ x_aF1S ->
                                >>=
                                  $dMonad1_sHMT
                                  lvl25_sI4t
                                  (\ y_aF1X ->
                                     return
                                       $dMonad1_sHMT
                                       (case x_aF1S of dt7_XEXe { F# ipv4_sIAE ->
                                        case y_aF1X of dt8_XEXg { F# ipv5_sIAH ->
                                        V2 dt7_XEXe dt8_XEXg
                                        }
                                        }))))
                          (\ x_aF1Y ->
                             >>
                               $dMonad1_sHMT
                               (case x_aF1Y of { V2 x1_aF23 y_aF24 ->
                                case x1_aF23 of { F# x#_sPWF ->
                                case y_aF24 of { F# x#1_sPWI ->
                                >>
                                  $dMonad1_sHMT
                                  (primitive
                                     $dPrimMonad_aE6H
                                     (\ s#_aF0u ->
                                        case writeFloatArray#
                                               dt2_sPWx (+# dt_sPWv o_sNyL) x#_sPWF s#_aF0u
                                        of s'#_aF0z
                                        { __DEFAULT ->
                                        (# s'#_aF0z, () #)
                                        }))
                                  (primitive
                                     $dPrimMonad_aE6H
                                     (\ s#_aF0u ->
                                        case writeFloatArray#
                                               dt2_sPWx (+# 1# (+# dt_sPWv o_sNyL)) x#1_sPWI s#_aF0u
                                        of s'#_aF0z
                                        { __DEFAULT ->
                                        (# s'#_aF0z, () #)
                                        }))
                                }
                                }
                                })
                               lvl24_sI4N)
                    }; } in
            $wdo_copy_sOBj 0# } in
      case sameMutableByteArray# dt2_sPWx dt6_sPWC of {
        __DEFAULT -> jump $j_sIBG;
        1# ->
          join {
            $j1_sIBH
              = case $p1PrimMonad $dPrimMonad_aE6H of
                { C:Monad ww1_sOBm ww2_sOBn ww3_sOBo ww4_sOBp ->
                let { lvl23_sI6e = ww4_sOBp () } in
                ww2_sOBn
                  (ww2_sOBn
                     (ww2_sOBn
                        (let { n_sHNy = *# 2# ipv2_aF1w } in
                         case <# n_sHNy 0# of {
                           __DEFAULT ->
                             case ># n_sHNy 2305843009213693951# of {
                               __DEFAULT ->
                                 ww2_sOBn
                                   (primitive
                                      $dPrimMonad_aE6H
                                      (\ s#_aEYm ->
                                         case newByteArray# (*# n_sHNy 4#) s#_aEYm of
                                         { (# ipv4_aEYp, ipv5_aEYq #) ->
                                         (# ipv4_aEYp, MutableByteArray ipv5_aEYq #)
                                         }))
                                   (\ x1_aEYs ->
                                      ww4_sOBp
                                        (case x1_aEYs of { MutableByteArray dt8_af1V ->
                                         MVector 0# n_sHNy dt8_af1V
                                         }));
                               1# -> $wlvl1_rQM6 n_sHNy
                             };
                           1# -> $wlvl_rQLZ n_sHNy
                         })
                        (\ x1_aEYL -> ww4_sOBp (x1_aEYL `cast` <Co:9>)))
                     (\ x1_aF2m ->
                        ww4_sOBp
                          (case x1_aF2m `cast` <Co:7> of nt2_sIAU
                           { MVector ipv4_sIAW ipv5_sIAX ipv6_sIAY ->
                           (MV_V2 ipv2_aF1w (nt2_sIAU `cast` <Co:9>)) `cast` <Co:5>
                           })))
                  (\ srcCopy_aF2n ->
                     ww3_sOBo
                       (case srcCopy_aF2n `cast` <Co:4> of { MV_V2 ipv4_aF2s ipv5_aF2t ->
                        case ipv5_aF2t `cast` <Co:7> of
                        { MVector dt7_sPWL dt8_sPWM dt9_sPWN ->
                        letrec {
                          $wdo_copy_sOBR
                            = \ ww5_sOBP ->
                                case <# ww5_sOBP ipv2_aF1w of {
                                  __DEFAULT -> lvl23_sI6e;
                                  1# ->
                                    let { o_sNz3 = *# 2# ww5_sOBP } in
                                    let { lvl24_sI6a = $wdo_copy_sOBR (+# ww5_sOBP 1#) } in
                                    ww2_sOBn
                                      (let {
                                         lvl25_sI5Q
                                           = primitive
                                               $dPrimMonad_aE6H
                                               ($fPrimCFloat_$creadByteArray#
                                                  dt6_sPWC (+# 1# (+# dt4_sPWA o_sNz3))) } in
                                       ww2_sOBn
                                         (primitive
                                            $dPrimMonad_aE6H
                                            ($fPrimCFloat_$creadByteArray#
                                               dt6_sPWC (+# dt4_sPWA o_sNz3)))
                                         (\ x_aF2J ->
                                            ww2_sOBn
                                              lvl25_sI5Q
                                              (\ y_aF2O ->
                                                 ww4_sOBp
                                                   (case x_aF2J of dt10_XEXe { F# ipv7_sIB9 ->
                                                    case y_aF2O of dt11_XEXg { F# ipv8_sIBc ->
                                                    V2 dt10_XEXe dt11_XEXg
                                                    }
                                                    }))))
                                      (\ x_aF2P ->
                                         ww3_sOBo
                                           (case x_aF2P of { V2 x1_aF2U y_aF2V ->
                                            case x1_aF2U of { F# x#_sPWQ ->
                                            case y_aF2V of { F# x#1_sPWT ->
                                            ww3_sOBo
                                              (primitive
                                                 $dPrimMonad_aE6H
                                                 (\ s#_aF0u ->
                                                    case writeFloatArray#
                                                           dt9_sPWN
                                                           (+# dt7_sPWL o_sNz3)
                                                           x#_sPWQ
                                                           s#_aF0u
                                                    of s'#_aF0z
                                                    { __DEFAULT ->
                                                    (# s'#_aF0z, () #)
                                                    }))
                                              (primitive
                                                 $dPrimMonad_aE6H
                                                 (\ s#_aF0u ->
                                                    case writeFloatArray#
                                                           dt9_sPWN
                                                           (+# 1# (+# dt7_sPWL o_sNz3))
                                                           x#1_sPWT
                                                           s#_aF0u
                                                    of s'#_aF0z
                                                    { __DEFAULT ->
                                                    (# s'#_aF0z, () #)
                                                    }))
                                            }
                                            }
                                            })
                                           lvl24_sI6a)
                                }; } in
                        $wdo_copy_sOBR 0#
                        }
                        })
                       (case srcCopy_aF2n `cast` <Co:4> of { MV_V2 ipv4_aF3d ipv5_aF3e ->
                        case ipv5_aF3e `cast` <Co:7> of
                        { MVector dt7_sPWW dt8_sPWX dt9_sPWY ->
                        letrec {
                          $wdo_copy_sOBX
                            = \ ww5_sOBV ->
                                case <# ww5_sOBV ipv4_aF3d of {
                                  __DEFAULT -> lvl23_sI6e;
                                  1# ->
                                    let { o_sNzf = *# 2# ww5_sOBV } in
                                    let { lvl24_sI6L = $wdo_copy_sOBX (+# ww5_sOBV 1#) } in
                                    ww2_sOBn
                                      (let {
                                         lvl25_sI6r
                                           = primitive
                                               $dPrimMonad_aE6H
                                               ($fPrimCFloat_$creadByteArray#
                                                  dt9_sPWY (+# 1# (+# dt7_sPWW o_sNzf))) } in
                                       ww2_sOBn
                                         (primitive
                                            $dPrimMonad_aE6H
                                            ($fPrimCFloat_$creadByteArray#
                                               dt9_sPWY (+# dt7_sPWW o_sNzf)))
                                         (\ x_aF3u ->
                                            ww2_sOBn
                                              lvl25_sI6r
                                              (\ y_aF3z ->
                                                 ww4_sOBp
                                                   (case x_aF3u of dt10_XEXe { F# ipv7_sIBu ->
                                                    case y_aF3z of dt11_XEXg { F# ipv8_sIBx ->
                                                    V2 dt10_XEXe dt11_XEXg
                                                    }
                                                    }))))
                                      (\ x_aF3A ->
                                         ww3_sOBo
                                           (case x_aF3A of { V2 x1_aF3F y_aF3G ->
                                            case x1_aF3F of { F# x#_sPX1 ->
                                            case y_aF3G of { F# x#1_sPX4 ->
                                            ww3_sOBo
                                              (primitive
                                                 $dPrimMonad_aE6H
                                                 (\ s#_aF0u ->
                                                    case writeFloatArray#
                                                           dt2_sPWx
                                                           (+# dt_sPWv o_sNzf)
                                                           x#_sPX1
                                                           s#_aF0u
                                                    of s'#_aF0z
                                                    { __DEFAULT ->
                                                    (# s'#_aF0z, () #)
                                                    }))
                                              (primitive
                                                 $dPrimMonad_aE6H
                                                 (\ s#_aF0u ->
                                                    case writeFloatArray#
                                                           dt2_sPWx
                                                           (+# 1# (+# dt_sPWv o_sNzf))
                                                           x#1_sPX4
                                                           s#_aF0u
                                                    of s'#_aF0z
                                                    { __DEFAULT ->
                                                    (# s'#_aF0z, () #)
                                                    }))
                                            }
                                            }
                                            })
                                           lvl24_sI6L)
                                }; } in
                        $wdo_copy_sOBX 0#
                        }
                        }))
                } } in
          case >=# dt_sPWv dt4_sPWA of {
            __DEFAULT ->
              case >=# dt4_sPWA dt_sPWv of {
                __DEFAULT -> jump $j_sIBG;
                1# ->
                  case <# dt4_sPWA (+# dt_sPWv dt1_sPWw) of {
                    __DEFAULT -> jump $j_sIBG;
                    1# -> jump $j1_sIBH
                  }
              };
            1# ->
              case <# dt_sPWv (+# dt4_sPWA dt5_sPWB) of {
                __DEFAULT ->
                  case >=# dt4_sPWA dt_sPWv of {
                    __DEFAULT -> jump $j_sIBG;
                    1# ->
                      case <# dt4_sPWA (+# dt_sPWv dt1_sPWw) of {
                        __DEFAULT -> jump $j_sIBG;
                        1# -> jump $j1_sIBH
                      }
                  };
                1# -> jump $j1_sIBH
              }
          }
      }
      }
      }
      }
      }

-- RHS size: {terms: 127, types: 149, coercions: 38, joins: 0/6}
$fMVectorMVectorECSPos_$cbasicUnsafeCopy
  = \ @ m_aE6n $dPrimMonad_aE6p eta_B1 ds_dETJ ->
      case eta_B1 `cast` <Co:12> of { MV_V2 ipv_aF70 ipv1_aF71 ->
      case ipv1_aF71 `cast` <Co:7> of
      { MVector dt_sPX7 dt1_sPX8 dt2_sPX9 ->
      case ds_dETJ `cast` <Co:12> of { MV_V2 ipv2_aF76 ipv3_aF77 ->
      case ipv3_aF77 `cast` <Co:7> of
      { MVector dt4_sPXc dt5_sPXd dt6_sPXe ->
      let { $dMonad_sHMy = $p1PrimMonad $dPrimMonad_aE6p } in
      let { lvl23_sI3x = return $dMonad_sHMy () } in
      letrec {
        $wdo_copy_sOC3
          = \ ww_sOC1 ->
              case <# ww_sOC1 ipv2_aF76 of {
                __DEFAULT -> lvl23_sI3x;
                1# ->
                  let { o_sNAv = *# 2# ww_sOC1 } in
                  let { lvl24_sI44 = $wdo_copy_sOC3 (+# ww_sOC1 1#) } in
                  >>=
                    $dMonad_sHMy
                    (let {
                       lvl25_sI3K
                         = primitive
                             $dPrimMonad_aE6p
                             ($fPrimCFloat_$creadByteArray#
                                dt6_sPXe (+# 1# (+# dt4_sPXc o_sNAv))) } in
                     >>=
                       $dMonad_sHMy
                       (primitive
                          $dPrimMonad_aE6p
                          ($fPrimCFloat_$creadByteArray# dt6_sPXe (+# dt4_sPXc o_sNAv)))
                       (\ x_aF7n ->
                          >>=
                            $dMonad_sHMy
                            lvl25_sI3K
                            (\ y_aF7s ->
                               return
                                 $dMonad_sHMy
                                 (case x_aF7n of dt7_XEXe { F# ipv4_sICW ->
                                  case y_aF7s of dt8_XEXg { F# ipv5_sICZ -> V2 dt7_XEXe dt8_XEXg }
                                  }))))
                    (\ x_aF7t ->
                       >>
                         $dMonad_sHMy
                         (case x_aF7t of { V2 x1_aF7y y_aF7z ->
                          case x1_aF7y of { F# x#_sPXh ->
                          case y_aF7z of { F# x#1_sPXk ->
                          >>
                            $dMonad_sHMy
                            (primitive
                               $dPrimMonad_aE6p
                               (\ s#_aF0u ->
                                  case writeFloatArray# dt2_sPX9 (+# dt_sPX7 o_sNAv) x#_sPXh s#_aF0u
                                  of s'#_aF0z
                                  { __DEFAULT ->
                                  (# s'#_aF0z, () #)
                                  }))
                            (primitive
                               $dPrimMonad_aE6p
                               (\ s#_aF0u ->
                                  case writeFloatArray#
                                         dt2_sPX9 (+# 1# (+# dt_sPX7 o_sNAv)) x#1_sPXk s#_aF0u
                                  of s'#_aF0z
                                  { __DEFAULT ->
                                  (# s'#_aF0z, () #)
                                  }))
                          }
                          }
                          })
                         lvl24_sI44)
              }; } in
      $wdo_copy_sOC3 0#
      }
      }
      }
      }

-- RHS size: {terms: 7, types: 13, coercions: 0, joins: 0/0}
$fMVectorMVectorECSPos_$cbasicClear
  = \ @ m_aE5T $dPrimMonad_aE5V _ ->
      return ($p1PrimMonad $dPrimMonad_aE5V) ()

-- RHS size: {terms: 64, types: 98, coercions: 21, joins: 0/2}
$fMVectorMVectorECSPos_$cbasicUnsafeRead
  = \ @ m_aE5l $dPrimMonad_aE5n eta_B1 eta1_X2 ->
      case $p1PrimMonad $dPrimMonad_aE5n of
      { C:Monad ww1_sOC6 ww2_sOC7 ww3_sOC8 ww4_sOC9 ->
      ww2_sOC7
        (case eta_B1 `cast` <Co:12> of { MV_V2 dt_aF8L v_aF8M ->
         case v_aF8M `cast` <Co:7> of
         { MVector dt1_sPXn dt2_sPXo dt3_sPXp ->
         let {
           o_sHMr = case eta1_X2 of { I# y_aF8Q -> I# (*# 2# y_aF8Q) } } in
         let {
           lvl23_sI3w
             = case o_sHMr of { I# x1_aF8V ->
               primitive
                 $dPrimMonad_aE5n
                 ($fPrimCFloat_$creadByteArray#
                    dt3_sPXp (+# 1# (+# dt1_sPXn x1_aF8V)))
               } } in
         ww2_sOC7
           (case o_sHMr of { I# y_aEZV ->
            primitive
              $dPrimMonad_aE5n
              ($fPrimCFloat_$creadByteArray# dt3_sPXp (+# dt1_sPXn y_aEZV))
            })
           (\ x_aF8S ->
              ww2_sOC7
                lvl23_sI3w
                (\ y_aF8X ->
                   ww4_sOC9
                     (case x_aF8S of dt4_XEXe { F# ipv_sIDx ->
                      case y_aF8X of dt5_XEXg { F# ipv1_sIDA -> V2 dt4_XEXe dt5_XEXg }
                      })))
         }
         })
        (\ x1_aETa -> ww4_sOC9 (x1_aETa `cast` <Co:2>))
      }

-- RHS size: {terms: 28, types: 51, coercions: 31, joins: 0/0}
$fMVectorMVectorECSPos_$cbasicInitialize
  = \ @ m_aE4N $dPrimMonad_aE4P ds_dETy ->
      case ds_dETy `cast` <Co:12> of { MV_V2 dt_aF9l v_aF9m ->
      case v_aF9m `cast` <Co:7> of
      { MVector dt1_sPXs dt2_sPXt dt3_sPXu ->
      primitive
        $dPrimMonad_aE4P
        (\ s#_aF9C ->
           case {__pkg_ccall primitive-0.7.0.0 forall s.
                               MutableByteArray# s
                               -> Int#
                               -> Word#
                               -> Word#
                               -> State# RealWorld
                               -> (# State# RealWorld #)}_aF9G
                  dt3_sPXu
                  (*# dt1_sPXs 4#)
                  (int2Word# (*# dt2_sPXt 4#))
                  0##
                  (s#_aF9C `cast` <Co:6>)
           of
           { (# ds9_aF9L #) ->
           (# ds9_aF9L `cast` <Co:6>, () #)
           })
      }
      }

-- RHS size: {terms: 74, types: 172, coercions: 39, joins: 0/1}
$fMVectorMVectorECSPos_$cbasicUnsafeNew
  = \ @ m_aE4u $dPrimMonad_aE4w eta_B1 ->
      case $p1PrimMonad $dPrimMonad_aE4w of
      { C:Monad ww1_sOCy ww2_sOCz ww3_sOCA ww4_sOCB ->
      ww2_sOCz
        (ww2_sOCz
           (ww2_sOCz
              (case eta_B1 of { I# y_aFa5 ->
               let { x_aevP = *# 2# y_aFa5 } in
               case <# x_aevP 0# of {
                 __DEFAULT ->
                   case ># x_aevP 2305843009213693951# of {
                     __DEFAULT ->
                       ww2_sOCz
                         (primitive
                            $dPrimMonad_aE4w
                            (\ s#_aEYm ->
                               case newByteArray# (*# x_aevP 4#) s#_aEYm of
                               { (# ipv_aEYp, ipv1_aEYq #) ->
                               (# ipv_aEYp, MutableByteArray ipv1_aEYq #)
                               }))
                         (\ x1_aEYs ->
                            ww4_sOCB
                              (case x1_aEYs of { MutableByteArray dt1_af1V ->
                               MVector 0# x_aevP dt1_af1V
                               }));
                     1# -> $wlvl1_rQM6 x_aevP
                   };
                 1# -> $wlvl_rQLZ x_aevP
               }
               })
              (\ x1_aEYL -> ww4_sOCB (x1_aEYL `cast` <Co:9>)))
           (\ x1_aFa7 ->
              ww4_sOCB
                (case eta_B1 of { I# dt1_aEW7 ->
                 case x1_aFa7 `cast` <Co:7> of nt_sIE2
                 { MVector ipv_sIE4 ipv1_sIE5 ipv2_sIE6 ->
                 (MV_V2 dt1_aEW7 (nt_sIE2 `cast` <Co:9>)) `cast` <Co:5>
                 }
                 })))
        (\ x1_aETa -> ww4_sOCB (x1_aETa `cast` <Co:9>))
      }

-- RHS size: {terms: 63, types: 44, coercions: 28, joins: 0/0}
$fMVectorMVectorECSPos_$cbasicOverlaps
  = \ @ s_aE4i eta_B2 eta1_B1 ->
      case eta_B2 `cast` <Co:9> of { MV_V2 dt_aFas v_aFat ->
      case v_aFat `cast` <Co:5> of
      { MVector dt1_sPXz dt2_sPXA dt3_sPXB ->
      case eta1_B1 `cast` <Co:9> of { MV_V2 dt4_aFax u_aFay ->
      case u_aFay `cast` <Co:5> of
      { MVector dt5_sPXE dt6_sPXF dt7_sPXG ->
      case sameMutableByteArray# dt3_sPXB dt7_sPXG of {
        __DEFAULT -> False;
        1# ->
          case >=# dt1_sPXz dt5_sPXE of {
            __DEFAULT ->
              case >=# dt5_sPXE dt1_sPXz of {
                __DEFAULT -> False;
                1# -> tagToEnum# (<# dt5_sPXE (+# dt1_sPXz dt2_sPXA))
              };
            1# ->
              case <# dt1_sPXz (+# dt5_sPXE dt6_sPXF) of {
                __DEFAULT ->
                  case >=# dt5_sPXE dt1_sPXz of {
                    __DEFAULT -> False;
                    1# -> tagToEnum# (<# dt5_sPXE (+# dt1_sPXz dt2_sPXA))
                  };
                1# -> True
              }
          }
      }
      }
      }
      }
      }

-- RHS size: {terms: 28, types: 29, coercions: 33, joins: 0/0}
$fMVectorMVectorECSPos_$cbasicUnsafeSlice
  = \ @ s_aE46 eta_B3 eta1_B2 eta2_B1 ->
      case eta2_B1 `cast` <Co:9> of { MV_V2 dt_aFaZ v_aFb0 ->
      case v_aFb0 `cast` <Co:5> of
      { MVector dt1_sPXJ dt2_sPXK dt3_sPXL ->
      case eta1_B2 of { I# dt5_aEW7 ->
      case eta_B3 of { I# y_aFb4 ->
      (MV_V2
         dt5_aEW7
         ((MVector (+# dt1_sPXJ (*# 2# y_aFb4)) (*# 2# dt5_aEW7) dt3_sPXL)
          `cast` <Co:7>))
      `cast` <Co:12>
      }
      }
      }
      }

-- RHS size: {terms: 7, types: 12, coercions: 9, joins: 0/0}
$fMVectorMVectorECSPos_$cbasicLength
  = \ @ s_aE3V eta_B1 ->
      case eta_B1 `cast` <Co:9> of { MV_V2 dt_aFbs ds1_aFbt ->
      I# dt_aFbs
      }

-- RHS size: {terms: 362, types: 451, coercions: 51, joins: 0/14}
$fMVectorMVectorECSPos_$cbasicUnsafeReplicate
  = \ @ m_aE52 $dPrimMonad_aE54 eta_B1 eta1_X2 ->
      case $p1PrimMonad $dPrimMonad_aE54 of
      { C:Monad ww1_sODe ww2_sODf ww3_sODg ww4_sODh ->
      ww2_sODf
        (let { lvl23_sI2h = ww4_sODh () } in
         ww2_sODf
           (ww2_sODf
              (ww2_sODf
                 (case eta_B1 of { I# y_aFom ->
                  let { x_aevP = *# 2# y_aFom } in
                  case <# x_aevP 0# of {
                    __DEFAULT ->
                      case ># x_aevP 2305843009213693951# of {
                        __DEFAULT ->
                          ww2_sODf
                            (primitive
                               $dPrimMonad_aE54
                               (\ s#_aEYm ->
                                  case newByteArray# (*# x_aevP 4#) s#_aEYm of
                                  { (# ipv_aEYp, ipv1_aEYq #) ->
                                  (# ipv_aEYp, MutableByteArray ipv1_aEYq #)
                                  }))
                            (\ x1_aEYs ->
                               ww4_sODh
                                 (case x1_aEYs of { MutableByteArray dt1_af1V ->
                                  MVector 0# x_aevP dt1_af1V
                                  }));
                        1# -> $wlvl1_rQM6 x_aevP
                      };
                    1# -> $wlvl_rQLZ x_aevP
                  }
                  })
                 (\ x1_aEYL -> ww4_sODh (x1_aEYL `cast` <Co:9>)))
              (\ x1_aFoo ->
                 ww4_sODh
                   (case eta_B1 of { I# dt1_aEW7 ->
                    case x1_aFoo `cast` <Co:7> of nt_sIET
                    { MVector ipv_sIEV ipv1_sIEW ipv2_sIEX ->
                    (MV_V2 dt1_aEW7 (nt_sIET `cast` <Co:9>)) `cast` <Co:5>
                    }
                    })))
           (\ v1_aFop ->
              ww3_sODg
                (case v1_aFop `cast` <Co:4> of { MV_V2 ipv_aFou ipv1_aFov ->
                 case ipv1_aFov `cast` <Co:7> of
                 { MVector dt_sPXQ dt1_sPXR dt2_sPXS ->
                 case ipv_aFou of wild1_XcS {
                   __DEFAULT ->
                     ww3_sODg
                       (case eta1_X2 `cast` <Co:1> of { V2 x1_aFqw y_aFqx ->
                        case x1_aFqw of { F# x#_sPXV ->
                        case y_aFqx of { F# x#1_sPXY ->
                        ww3_sODg
                          (primitive
                             $dPrimMonad_aE54
                             (\ s#_aF0u ->
                                case writeFloatArray# dt2_sPXS dt_sPXQ x#_sPXV s#_aF0u of s'#_aF0z
                                { __DEFAULT ->
                                (# s'#_aF0z, () #)
                                }))
                          (primitive
                             $dPrimMonad_aE54
                             (\ s#_aF0u ->
                                case writeFloatArray# dt2_sPXS (+# dt_sPXQ 1#) x#1_sPXY s#_aF0u
                                of s'#_aF0z
                                { __DEFAULT ->
                                (# s'#_aF0z, () #)
                                }))
                        }
                        }
                        })
                       (letrec {
                          $wdo_set_sOE2
                            = \ ww5_sOE0 ->
                                case <# (*# 2# ww5_sOE0) wild1_XcS of {
                                  __DEFAULT ->
                                    let { dt4_sIF3 = +# dt_sPXQ (*# 2# ww5_sOE0) } in
                                    let { n3_sHKM = -# wild1_XcS ww5_sOE0 } in
                                    letrec {
                                      $wdo_copy_sODQ
                                        = \ ww6_sODO ->
                                            case <# ww6_sODO n3_sHKM of {
                                              __DEFAULT -> lvl23_sI2h;
                                              1# ->
                                                let { o_sNCm = *# 2# ww6_sODO } in
                                                let {
                                                  lvl24_sI1b = $wdo_copy_sODQ (+# ww6_sODO 1#) } in
                                                ww2_sODf
                                                  (let {
                                                     lvl25_sI0R
                                                       = primitive
                                                           $dPrimMonad_aE54
                                                           ($fPrimCFloat_$creadByteArray#
                                                              dt2_sPXS
                                                              (+# 1# (+# dt_sPXQ o_sNCm))) } in
                                                   ww2_sODf
                                                     (primitive
                                                        $dPrimMonad_aE54
                                                        ($fPrimCFloat_$creadByteArray#
                                                           dt2_sPXS (+# dt_sPXQ o_sNCm)))
                                                     (\ x1_aFp8 ->
                                                        ww2_sODf
                                                          lvl25_sI0R
                                                          (\ y1_aFpd ->
                                                             ww4_sODh
                                                               (case x1_aFp8 of dt5_XEXe
                                                                { F# ipv2_sIFf ->
                                                                case y1_aFpd of dt6_XEXg
                                                                { F# ipv3_sIFi ->
                                                                V2 dt5_XEXe dt6_XEXg
                                                                }
                                                                }))))
                                                  (\ x1_aFpe ->
                                                     ww3_sODg
                                                       (case x1_aFpe of { V2 x2_aFpj y1_aFpk ->
                                                        case x2_aFpj of { F# x#_sPY1 ->
                                                        case y1_aFpk of { F# x#1_sPY4 ->
                                                        ww3_sODg
                                                          (primitive
                                                             $dPrimMonad_aE54
                                                             (\ s#_aF0u ->
                                                                case writeFloatArray#
                                                                       dt2_sPXS
                                                                       (+# dt4_sIF3 o_sNCm)
                                                                       x#_sPY1
                                                                       s#_aF0u
                                                                of s'#_aF0z
                                                                { __DEFAULT ->
                                                                (# s'#_aF0z, () #)
                                                                }))
                                                          (primitive
                                                             $dPrimMonad_aE54
                                                             (\ s#_aF0u ->
                                                                case writeFloatArray#
                                                                       dt2_sPXS
                                                                       (+# 1# (+# dt4_sIF3 o_sNCm))
                                                                       x#1_sPY4
                                                                       s#_aF0u
                                                                of s'#_aF0z
                                                                { __DEFAULT ->
                                                                (# s'#_aF0z, () #)
                                                                }))
                                                        }
                                                        }
                                                        })
                                                       lvl24_sI1b)
                                            }; } in
                                    $wdo_copy_sODQ 0#;
                                  1# ->
                                    ww3_sODg
                                      (let { dt4_sIFr = +# dt_sPXQ (*# 2# ww5_sOE0) } in
                                       letrec {
                                         $wdo_copy_sODW
                                           = \ ww6_sODU ->
                                               case <# ww6_sODU ww5_sOE0 of {
                                                 __DEFAULT -> lvl23_sI2h;
                                                 1# ->
                                                   let { o_sNCw = *# 2# ww6_sODU } in
                                                   let {
                                                     lvl24_sI1W
                                                       = $wdo_copy_sODW (+# ww6_sODU 1#) } in
                                                   ww2_sODf
                                                     (let {
                                                        lvl25_sI1C
                                                          = primitive
                                                              $dPrimMonad_aE54
                                                              ($fPrimCFloat_$creadByteArray#
                                                                 dt2_sPXS
                                                                 (+# 1# (+# dt_sPXQ o_sNCw))) } in
                                                      ww2_sODf
                                                        (primitive
                                                           $dPrimMonad_aE54
                                                           ($fPrimCFloat_$creadByteArray#
                                                              dt2_sPXS (+# dt_sPXQ o_sNCw)))
                                                        (\ x1_aFq0 ->
                                                           ww2_sODf
                                                             lvl25_sI1C
                                                             (\ y1_aFq5 ->
                                                                ww4_sODh
                                                                  (case x1_aFq0 of dt5_XEXe
                                                                   { F# ipv2_sIFD ->
                                                                   case y1_aFq5 of dt6_XEXg
                                                                   { F# ipv3_sIFG ->
                                                                   V2 dt5_XEXe dt6_XEXg
                                                                   }
                                                                   }))))
                                                     (\ x1_aFq6 ->
                                                        ww3_sODg
                                                          (case x1_aFq6 of { V2 x2_aFqb y1_aFqc ->
                                                           case x2_aFqb of { F# x#_sPY7 ->
                                                           case y1_aFqc of { F# x#1_sPYa ->
                                                           ww3_sODg
                                                             (primitive
                                                                $dPrimMonad_aE54
                                                                (\ s#_aF0u ->
                                                                   case writeFloatArray#
                                                                          dt2_sPXS
                                                                          (+# dt4_sIFr o_sNCw)
                                                                          x#_sPY7
                                                                          s#_aF0u
                                                                   of s'#_aF0z
                                                                   { __DEFAULT ->
                                                                   (# s'#_aF0z, () #)
                                                                   }))
                                                             (primitive
                                                                $dPrimMonad_aE54
                                                                (\ s#_aF0u ->
                                                                   case writeFloatArray#
                                                                          dt2_sPXS
                                                                          (+#
                                                                             1#
                                                                             (+# dt4_sIFr o_sNCw))
                                                                          x#1_sPYa
                                                                          s#_aF0u
                                                                   of s'#_aF0z
                                                                   { __DEFAULT ->
                                                                   (# s'#_aF0z, () #)
                                                                   }))
                                                           }
                                                           }
                                                           })
                                                          lvl24_sI1W)
                                               }; } in
                                       $wdo_copy_sODW 0#)
                                      ($wdo_set_sOE2 (*# 2# ww5_sOE0))
                                }; } in
                        $wdo_set_sOE2 1#);
                   0# -> lvl23_sI2h
                 }
                 }
                 })
                (ww4_sODh v1_aFop)))
        (\ x1_aETa -> ww4_sODh (x1_aETa `cast` <Co:9>))
      }

-- RHS size: {terms: 69, types: 85, coercions: 20, joins: 0/1}
$fMVectorMVectorECSPos_$cbasicUnsafeWrite
  = \ @ m_aE5E $dPrimMonad_aE5G eta_B2 eta1_B1 val_aDOm ->
      case eta_B2 `cast` <Co:12> of { MV_V2 dt_aFsQ v_aFsR ->
      case v_aFsR `cast` <Co:7> of
      { MVector dt1_sPYd dt2_sPYe dt3_sPYf ->
      case val_aDOm `cast` <Co:1> of { V2 x_aFsW y_aFsX ->
      case x_aFsW of { F# x#_sPYi ->
      case y_aFsX of { F# x#1_sPYl ->
      let {
        o_sHKv = case eta1_B1 of { I# y1_aFt1 -> I# (*# 2# y1_aFt1) } } in
      >>
        ($p1PrimMonad $dPrimMonad_aE5G)
        (case o_sHKv of { I# y1_aF0s ->
         primitive
           $dPrimMonad_aE5G
           (\ s#_aF0u ->
              case writeFloatArray#
                     dt3_sPYf (+# dt1_sPYd y1_aF0s) x#_sPYi s#_aF0u
              of s'#_aF0z
              { __DEFAULT ->
              (# s'#_aF0z, () #)
              })
         })
        (case o_sHKv of { I# x1_aFt5 ->
         primitive
           $dPrimMonad_aE5G
           (\ s#_aF0u ->
              case writeFloatArray#
                     dt3_sPYf (+# 1# (+# dt1_sPYd x1_aFt5)) x#1_sPYl s#_aF0u
              of s'#_aF0z
              { __DEFAULT ->
              (# s'#_aF0z, () #)
              })
         })
      }
      }
      }
      }
      }

-- RHS size: {terms: 291, types: 295, coercions: 20, joins: 0/13}
$fMVectorMVectorECSPos_$cbasicSet
  = \ @ m_aE68 $dPrimMonad_aE6a eta_B1 val_aDOm ->
      case eta_B1 `cast` <Co:12> of { MV_V2 ipv_aFtF ipv1_aFtG ->
      case ipv1_aFtG `cast` <Co:7> of
      { MVector dt_sPYo dt1_sPYp dt2_sPYq ->
      case $p1PrimMonad $dPrimMonad_aE6a of
      { C:Monad ww1_sOE5 ww2_sOE6 ww3_sOE7 ww4_sOE8 ->
      case ipv_aFtF of wild1_XcP {
        __DEFAULT ->
          ww3_sOE7
            (case val_aDOm `cast` <Co:1> of { V2 x1_aFvH y_aFvI ->
             case x1_aFvH of { F# x#_sPYt ->
             case y_aFvI of { F# x#1_sPYw ->
             ww3_sOE7
               (primitive
                  $dPrimMonad_aE6a
                  (\ s#_aF0u ->
                     case writeFloatArray# dt2_sPYq dt_sPYo x#_sPYt s#_aF0u of s'#_aF0z
                     { __DEFAULT ->
                     (# s'#_aF0z, () #)
                     }))
               (primitive
                  $dPrimMonad_aE6a
                  (\ s#_aF0u ->
                     case writeFloatArray# dt2_sPYq (+# dt_sPYo 1#) x#1_sPYw s#_aF0u
                     of s'#_aF0z
                     { __DEFAULT ->
                     (# s'#_aF0z, () #)
                     }))
             }
             }
             })
            (let { lvl23_sHYz = ww4_sOE8 () } in
             letrec {
               $wdo_set_sOEr
                 = \ ww5_sOEp ->
                     case <# (*# 2# ww5_sOEp) wild1_XcP of {
                       __DEFAULT ->
                         let { dt4_sIHh = +# dt_sPYo (*# 2# ww5_sOEp) } in
                         let { n2_sHK8 = -# wild1_XcP ww5_sOEp } in
                         letrec {
                           $wdo_copy_sOEf
                             = \ ww6_sOEd ->
                                 case <# ww6_sOEd n2_sHK8 of {
                                   __DEFAULT -> lvl23_sHYz;
                                   1# ->
                                     let { o_sNE8 = *# 2# ww6_sOEd } in
                                     let { lvl24_sHYl = $wdo_copy_sOEf (+# ww6_sOEd 1#) } in
                                     ww2_sOE6
                                       (let {
                                          lvl25_sHY1
                                            = primitive
                                                $dPrimMonad_aE6a
                                                ($fPrimCFloat_$creadByteArray#
                                                   dt2_sPYq (+# 1# (+# dt_sPYo o_sNE8))) } in
                                        ww2_sOE6
                                          (primitive
                                             $dPrimMonad_aE6a
                                             ($fPrimCFloat_$creadByteArray#
                                                dt2_sPYq (+# dt_sPYo o_sNE8)))
                                          (\ x1_aFuj ->
                                             ww2_sOE6
                                               lvl25_sHY1
                                               (\ y1_aFuo ->
                                                  ww4_sOE8
                                                    (case x1_aFuj of dt5_XEXe { F# ipv2_sIHt ->
                                                     case y1_aFuo of dt6_XEXg { F# ipv3_sIHw ->
                                                     V2 dt5_XEXe dt6_XEXg
                                                     }
                                                     }))))
                                       (\ x1_aFup ->
                                          ww3_sOE7
                                            (case x1_aFup of { V2 x2_aFuu y1_aFuv ->
                                             case x2_aFuu of { F# x#_sPYz ->
                                             case y1_aFuv of { F# x#1_sPYC ->
                                             ww3_sOE7
                                               (primitive
                                                  $dPrimMonad_aE6a
                                                  (\ s#_aF0u ->
                                                     case writeFloatArray#
                                                            dt2_sPYq
                                                            (+# dt4_sIHh o_sNE8)
                                                            x#_sPYz
                                                            s#_aF0u
                                                     of s'#_aF0z
                                                     { __DEFAULT ->
                                                     (# s'#_aF0z, () #)
                                                     }))
                                               (primitive
                                                  $dPrimMonad_aE6a
                                                  (\ s#_aF0u ->
                                                     case writeFloatArray#
                                                            dt2_sPYq
                                                            (+# 1# (+# dt4_sIHh o_sNE8))
                                                            x#1_sPYC
                                                            s#_aF0u
                                                     of s'#_aF0z
                                                     { __DEFAULT ->
                                                     (# s'#_aF0z, () #)
                                                     }))
                                             }
                                             }
                                             })
                                            lvl24_sHYl)
                                 }; } in
                         $wdo_copy_sOEf 0#;
                       1# ->
                         ww3_sOE7
                           (let { dt4_sIHF = +# dt_sPYo (*# 2# ww5_sOEp) } in
                            letrec {
                              $wdo_copy_sOEl
                                = \ ww6_sOEj ->
                                    case <# ww6_sOEj ww5_sOEp of {
                                      __DEFAULT -> lvl23_sHYz;
                                      1# ->
                                        let { o_sNEi = *# 2# ww6_sOEj } in
                                        let { lvl24_sHZ6 = $wdo_copy_sOEl (+# ww6_sOEj 1#) } in
                                        ww2_sOE6
                                          (let {
                                             lvl25_sHYM
                                               = primitive
                                                   $dPrimMonad_aE6a
                                                   ($fPrimCFloat_$creadByteArray#
                                                      dt2_sPYq (+# 1# (+# dt_sPYo o_sNEi))) } in
                                           ww2_sOE6
                                             (primitive
                                                $dPrimMonad_aE6a
                                                ($fPrimCFloat_$creadByteArray#
                                                   dt2_sPYq (+# dt_sPYo o_sNEi)))
                                             (\ x1_aFvb ->
                                                ww2_sOE6
                                                  lvl25_sHYM
                                                  (\ y1_aFvg ->
                                                     ww4_sOE8
                                                       (case x1_aFvb of dt5_XEXe { F# ipv2_sIHR ->
                                                        case y1_aFvg of dt6_XEXg { F# ipv3_sIHU ->
                                                        V2 dt5_XEXe dt6_XEXg
                                                        }
                                                        }))))
                                          (\ x1_aFvh ->
                                             ww3_sOE7
                                               (case x1_aFvh of { V2 x2_aFvm y1_aFvn ->
                                                case x2_aFvm of { F# x#_sPYF ->
                                                case y1_aFvn of { F# x#1_sPYI ->
                                                ww3_sOE7
                                                  (primitive
                                                     $dPrimMonad_aE6a
                                                     (\ s#_aF0u ->
                                                        case writeFloatArray#
                                                               dt2_sPYq
                                                               (+# dt4_sIHF o_sNEi)
                                                               x#_sPYF
                                                               s#_aF0u
                                                        of s'#_aF0z
                                                        { __DEFAULT ->
                                                        (# s'#_aF0z, () #)
                                                        }))
                                                  (primitive
                                                     $dPrimMonad_aE6a
                                                     (\ s#_aF0u ->
                                                        case writeFloatArray#
                                                               dt2_sPYq
                                                               (+# 1# (+# dt4_sIHF o_sNEi))
                                                               x#1_sPYI
                                                               s#_aF0u
                                                        of s'#_aF0z
                                                        { __DEFAULT ->
                                                        (# s'#_aF0z, () #)
                                                        }))
                                                }
                                                }
                                                })
                                               lvl24_sHZ6)
                                    }; } in
                            $wdo_copy_sOEl 0#)
                           ($wdo_set_sOEr (*# 2# ww5_sOEp))
                     }; } in
             $wdo_set_sOEr 1#);
        0# -> ww4_sOE8 ()
      }
      }
      }
      }

-- RHS size: {terms: 14, types: 2, coercions: 0, joins: 0/0}
$fMVectorMVectorECSPos
  = C:MVector
      $fMVectorMVectorECSPos_$cbasicLength
      $fMVectorMVectorECSPos_$cbasicUnsafeSlice
      $fMVectorMVectorECSPos_$cbasicOverlaps
      $fMVectorMVectorECSPos_$cbasicUnsafeNew
      $fMVectorMVectorECSPos_$cbasicInitialize
      $fMVectorMVectorECSPos_$cbasicUnsafeReplicate
      $fMVectorMVectorECSPos_$cbasicUnsafeRead
      $fMVectorMVectorECSPos_$cbasicUnsafeWrite
      $fMVectorMVectorECSPos_$cbasicClear
      $fMVectorMVectorECSPos_$cbasicSet
      $fMVectorMVectorECSPos_$cbasicUnsafeCopy
      $fMVectorMVectorECSPos_$cbasicUnsafeMove
      $fMVectorMVectorECSPos_$cbasicUnsafeGrow

-- RHS size: {terms: 26, types: 75, coercions: 116, joins: 0/0}
$sarchetypeAdd2
  = \ @ s_XcAh s1_X8az ->
      case newByteArray# 8192# (s1_X8az `cast` <Co:19>) of
      { (# ipv_aEYp, ipv1_aEYq #) ->
      case {__pkg_ccall primitive-0.7.0.0 forall s.
                               MutableByteArray# s
                               -> Int#
                               -> Word#
                               -> Word#
                               -> State# RealWorld
                               -> (# State# RealWorld #)}_aF9G
             ipv1_aEYq 0# 8192## 0## (ipv_aEYp `cast` <Co:7>)
      of
      { (# ds9_aF9L #) ->
      (# ds9_aF9L `cast` <Co:24>,
         CCons
           @~ <Co:7>
           ($fMVectorMVectorECSPos `cast` <Co:4>)
           ((MV_V2 1024# ((MVector 0# 2048# ipv1_aEYq) `cast` <Co:11>))
            `cast` <Co:44>)
           $WCNil #)
      }
      }

-- RHS size: {terms: 362, types: 451, coercions: 51, joins: 0/14}
$fMVectorMVectorECSVel_$cbasicUnsafeReplicate
  = \ @ m_aEge $dPrimMonad_aEgg eta_B1 eta1_X2 ->
      case $p1PrimMonad $dPrimMonad_aEgg of
      { C:Monad ww1_sOEK ww2_sOEL ww3_sOEM ww4_sOEN ->
      ww2_sOEL
        (let { lvl23_sHXB = ww4_sOEN () } in
         ww2_sOEL
           (ww2_sOEL
              (ww2_sOEL
                 (case eta_B1 of { I# y_aFom ->
                  let { x_aevP = *# 2# y_aFom } in
                  case <# x_aevP 0# of {
                    __DEFAULT ->
                      case ># x_aevP 2305843009213693951# of {
                        __DEFAULT ->
                          ww2_sOEL
                            (primitive
                               $dPrimMonad_aEgg
                               (\ s#_aEYm ->
                                  case newByteArray# (*# x_aevP 4#) s#_aEYm of
                                  { (# ipv_aEYp, ipv1_aEYq #) ->
                                  (# ipv_aEYp, MutableByteArray ipv1_aEYq #)
                                  }))
                            (\ x1_aEYs ->
                               ww4_sOEN
                                 (case x1_aEYs of { MutableByteArray dt1_af1V ->
                                  MVector 0# x_aevP dt1_af1V
                                  }));
                        1# -> $wlvl1_rQM6 x_aevP
                      };
                    1# -> $wlvl_rQLZ x_aevP
                  }
                  })
                 (\ x1_aEYL -> ww4_sOEN (x1_aEYL `cast` <Co:9>)))
              (\ x1_aFoo ->
                 ww4_sOEN
                   (case eta_B1 of { I# dt1_aEW7 ->
                    case x1_aFoo `cast` <Co:7> of nt_sIJp
                    { MVector ipv_sIJr ipv1_sIJs ipv2_sIJt ->
                    (MV_V2 dt1_aEW7 (nt_sIJp `cast` <Co:9>)) `cast` <Co:5>
                    }
                    })))
           (\ v1_aFop ->
              ww3_sOEM
                (case v1_aFop `cast` <Co:4> of { MV_V2 ipv_aFou ipv1_aFov ->
                 case ipv1_aFov `cast` <Co:7> of
                 { MVector dt_sPYS dt1_sPYT dt2_sPYU ->
                 case ipv_aFou of wild1_XdF {
                   __DEFAULT ->
                     ww3_sOEM
                       (case eta1_X2 `cast` <Co:1> of { V2 x1_aFqw y_aFqx ->
                        case x1_aFqw of { F# x#_sPYX ->
                        case y_aFqx of { F# x#1_sPZ0 ->
                        ww3_sOEM
                          (primitive
                             $dPrimMonad_aEgg
                             (\ s#_aF0u ->
                                case writeFloatArray# dt2_sPYU dt_sPYS x#_sPYX s#_aF0u of s'#_aF0z
                                { __DEFAULT ->
                                (# s'#_aF0z, () #)
                                }))
                          (primitive
                             $dPrimMonad_aEgg
                             (\ s#_aF0u ->
                                case writeFloatArray# dt2_sPYU (+# dt_sPYS 1#) x#1_sPZ0 s#_aF0u
                                of s'#_aF0z
                                { __DEFAULT ->
                                (# s'#_aF0z, () #)
                                }))
                        }
                        }
                        })
                       (letrec {
                          $wdo_set_sOFy
                            = \ ww5_sOFw ->
                                case <# (*# 2# ww5_sOFw) wild1_XdF of {
                                  __DEFAULT ->
                                    let { dt4_sIJz = +# dt_sPYS (*# 2# ww5_sOFw) } in
                                    let { n3_sHJq = -# wild1_XdF ww5_sOFw } in
                                    letrec {
                                      $wdo_copy_sOFm
                                        = \ ww6_sOFk ->
                                            case <# ww6_sOFk n3_sHJq of {
                                              __DEFAULT -> lvl23_sHXB;
                                              1# ->
                                                let { o_sNFK = *# 2# ww6_sOFk } in
                                                let {
                                                  lvl24_sHWv = $wdo_copy_sOFm (+# ww6_sOFk 1#) } in
                                                ww2_sOEL
                                                  (let {
                                                     lvl25_sHWb
                                                       = primitive
                                                           $dPrimMonad_aEgg
                                                           ($fPrimCFloat_$creadByteArray#
                                                              dt2_sPYU
                                                              (+# 1# (+# dt_sPYS o_sNFK))) } in
                                                   ww2_sOEL
                                                     (primitive
                                                        $dPrimMonad_aEgg
                                                        ($fPrimCFloat_$creadByteArray#
                                                           dt2_sPYU (+# dt_sPYS o_sNFK)))
                                                     (\ x1_aFp8 ->
                                                        ww2_sOEL
                                                          lvl25_sHWb
                                                          (\ y1_aFpd ->
                                                             ww4_sOEN
                                                               (case x1_aFp8 of dt5_XEXe
                                                                { F# ipv2_sIJL ->
                                                                case y1_aFpd of dt6_XEXg
                                                                { F# ipv3_sIJO ->
                                                                V2 dt5_XEXe dt6_XEXg
                                                                }
                                                                }))))
                                                  (\ x1_aFpe ->
                                                     ww3_sOEM
                                                       (case x1_aFpe of { V2 x2_aFpj y1_aFpk ->
                                                        case x2_aFpj of { F# x#_sPZ3 ->
                                                        case y1_aFpk of { F# x#1_sPZ6 ->
                                                        ww3_sOEM
                                                          (primitive
                                                             $dPrimMonad_aEgg
                                                             (\ s#_aF0u ->
                                                                case writeFloatArray#
                                                                       dt2_sPYU
                                                                       (+# dt4_sIJz o_sNFK)
                                                                       x#_sPZ3
                                                                       s#_aF0u
                                                                of s'#_aF0z
                                                                { __DEFAULT ->
                                                                (# s'#_aF0z, () #)
                                                                }))
                                                          (primitive
                                                             $dPrimMonad_aEgg
                                                             (\ s#_aF0u ->
                                                                case writeFloatArray#
                                                                       dt2_sPYU
                                                                       (+# 1# (+# dt4_sIJz o_sNFK))
                                                                       x#1_sPZ6
                                                                       s#_aF0u
                                                                of s'#_aF0z
                                                                { __DEFAULT ->
                                                                (# s'#_aF0z, () #)
                                                                }))
                                                        }
                                                        }
                                                        })
                                                       lvl24_sHWv)
                                            }; } in
                                    $wdo_copy_sOFm 0#;
                                  1# ->
                                    ww3_sOEM
                                      (let { dt4_sIJX = +# dt_sPYS (*# 2# ww5_sOFw) } in
                                       letrec {
                                         $wdo_copy_sOFs
                                           = \ ww6_sOFq ->
                                               case <# ww6_sOFq ww5_sOFw of {
                                                 __DEFAULT -> lvl23_sHXB;
                                                 1# ->
                                                   let { o_sNFU = *# 2# ww6_sOFq } in
                                                   let {
                                                     lvl24_sHXg
                                                       = $wdo_copy_sOFs (+# ww6_sOFq 1#) } in
                                                   ww2_sOEL
                                                     (let {
                                                        lvl25_sHWW
                                                          = primitive
                                                              $dPrimMonad_aEgg
                                                              ($fPrimCFloat_$creadByteArray#
                                                                 dt2_sPYU
                                                                 (+# 1# (+# dt_sPYS o_sNFU))) } in
                                                      ww2_sOEL
                                                        (primitive
                                                           $dPrimMonad_aEgg
                                                           ($fPrimCFloat_$creadByteArray#
                                                              dt2_sPYU (+# dt_sPYS o_sNFU)))
                                                        (\ x1_aFq0 ->
                                                           ww2_sOEL
                                                             lvl25_sHWW
                                                             (\ y1_aFq5 ->
                                                                ww4_sOEN
                                                                  (case x1_aFq0 of dt5_XEXe
                                                                   { F# ipv2_sIK9 ->
                                                                   case y1_aFq5 of dt6_XEXg
                                                                   { F# ipv3_sIKc ->
                                                                   V2 dt5_XEXe dt6_XEXg
                                                                   }
                                                                   }))))
                                                     (\ x1_aFq6 ->
                                                        ww3_sOEM
                                                          (case x1_aFq6 of { V2 x2_aFqb y1_aFqc ->
                                                           case x2_aFqb of { F# x#_sPZ9 ->
                                                           case y1_aFqc of { F# x#1_sPZc ->
                                                           ww3_sOEM
                                                             (primitive
                                                                $dPrimMonad_aEgg
                                                                (\ s#_aF0u ->
                                                                   case writeFloatArray#
                                                                          dt2_sPYU
                                                                          (+# dt4_sIJX o_sNFU)
                                                                          x#_sPZ9
                                                                          s#_aF0u
                                                                   of s'#_aF0z
                                                                   { __DEFAULT ->
                                                                   (# s'#_aF0z, () #)
                                                                   }))
                                                             (primitive
                                                                $dPrimMonad_aEgg
                                                                (\ s#_aF0u ->
                                                                   case writeFloatArray#
                                                                          dt2_sPYU
                                                                          (+#
                                                                             1#
                                                                             (+# dt4_sIJX o_sNFU))
                                                                          x#1_sPZc
                                                                          s#_aF0u
                                                                   of s'#_aF0z
                                                                   { __DEFAULT ->
                                                                   (# s'#_aF0z, () #)
                                                                   }))
                                                           }
                                                           }
                                                           })
                                                          lvl24_sHXg)
                                               }; } in
                                       $wdo_copy_sOFs 0#)
                                      ($wdo_set_sOFy (*# 2# ww5_sOFw))
                                }; } in
                        $wdo_set_sOFy 1#);
                   0# -> lvl23_sHXB
                 }
                 }
                 })
                (ww4_sOEN v1_aFop)))
        (\ x1_aETa -> ww4_sOEN (x1_aETa `cast` <Co:9>))
      }

-- RHS size: {terms: 69, types: 85, coercions: 20, joins: 0/1}
$fMVectorMVectorECSVel_$cbasicUnsafeWrite
  = \ @ m_aEgQ $dPrimMonad_aEgS eta_B2 eta1_B1 val_aE83 ->
      case eta_B2 `cast` <Co:12> of { MV_V2 dt_aFsQ v_aFsR ->
      case v_aFsR `cast` <Co:7> of
      { MVector dt1_sPZf dt2_sPZg dt3_sPZh ->
      case val_aE83 `cast` <Co:1> of { V2 x_aFsW y_aFsX ->
      case x_aFsW of { F# x#_sPZk ->
      case y_aFsX of { F# x#1_sPZn ->
      let {
        o_sHJ9 = case eta1_B1 of { I# y1_aFt1 -> I# (*# 2# y1_aFt1) } } in
      >>
        ($p1PrimMonad $dPrimMonad_aEgS)
        (case o_sHJ9 of { I# y1_aF0s ->
         primitive
           $dPrimMonad_aEgS
           (\ s#_aF0u ->
              case writeFloatArray#
                     dt3_sPZh (+# dt1_sPZf y1_aF0s) x#_sPZk s#_aF0u
              of s'#_aF0z
              { __DEFAULT ->
              (# s'#_aF0z, () #)
              })
         })
        (case o_sHJ9 of { I# x1_aFt5 ->
         primitive
           $dPrimMonad_aEgS
           (\ s#_aF0u ->
              case writeFloatArray#
                     dt3_sPZh (+# 1# (+# dt1_sPZf x1_aFt5)) x#1_sPZn s#_aF0u
              of s'#_aF0z
              { __DEFAULT ->
              (# s'#_aF0z, () #)
              })
         })
      }
      }
      }
      }
      }

-- RHS size: {terms: 291, types: 295, coercions: 20, joins: 0/13}
$fMVectorMVectorECSVel_$cbasicSet
  = \ @ m_aEhk $dPrimMonad_aEhm eta_B1 val_aE83 ->
      case eta_B1 `cast` <Co:12> of { MV_V2 ipv_aFtF ipv1_aFtG ->
      case ipv1_aFtG `cast` <Co:7> of
      { MVector dt_sPZq dt1_sPZr dt2_sPZs ->
      case $p1PrimMonad $dPrimMonad_aEhm of
      { C:Monad ww1_sOFB ww2_sOFC ww3_sOFD ww4_sOFE ->
      case ipv_aFtF of wild1_XdC {
        __DEFAULT ->
          ww3_sOFD
            (case val_aE83 `cast` <Co:1> of { V2 x1_aFvH y_aFvI ->
             case x1_aFvH of { F# x#_sPZv ->
             case y_aFvI of { F# x#1_sPZy ->
             ww3_sOFD
               (primitive
                  $dPrimMonad_aEhm
                  (\ s#_aF0u ->
                     case writeFloatArray# dt2_sPZs dt_sPZq x#_sPZv s#_aF0u of s'#_aF0z
                     { __DEFAULT ->
                     (# s'#_aF0z, () #)
                     }))
               (primitive
                  $dPrimMonad_aEhm
                  (\ s#_aF0u ->
                     case writeFloatArray# dt2_sPZs (+# dt_sPZq 1#) x#1_sPZy s#_aF0u
                     of s'#_aF0z
                     { __DEFAULT ->
                     (# s'#_aF0z, () #)
                     }))
             }
             }
             })
            (let { lvl23_sHTT = ww4_sOFE () } in
             letrec {
               $wdo_set_sOFX
                 = \ ww5_sOFV ->
                     case <# (*# 2# ww5_sOFV) wild1_XdC of {
                       __DEFAULT ->
                         let { dt4_sILN = +# dt_sPZq (*# 2# ww5_sOFV) } in
                         let { n2_sHIM = -# wild1_XdC ww5_sOFV } in
                         letrec {
                           $wdo_copy_sOFL
                             = \ ww6_sOFJ ->
                                 case <# ww6_sOFJ n2_sHIM of {
                                   __DEFAULT -> lvl23_sHTT;
                                   1# ->
                                     let { o_sNHw = *# 2# ww6_sOFJ } in
                                     let { lvl24_sHTF = $wdo_copy_sOFL (+# ww6_sOFJ 1#) } in
                                     ww2_sOFC
                                       (let {
                                          lvl25_sHTl
                                            = primitive
                                                $dPrimMonad_aEhm
                                                ($fPrimCFloat_$creadByteArray#
                                                   dt2_sPZs (+# 1# (+# dt_sPZq o_sNHw))) } in
                                        ww2_sOFC
                                          (primitive
                                             $dPrimMonad_aEhm
                                             ($fPrimCFloat_$creadByteArray#
                                                dt2_sPZs (+# dt_sPZq o_sNHw)))
                                          (\ x1_aFuj ->
                                             ww2_sOFC
                                               lvl25_sHTl
                                               (\ y1_aFuo ->
                                                  ww4_sOFE
                                                    (case x1_aFuj of dt5_XEXe { F# ipv2_sILZ ->
                                                     case y1_aFuo of dt6_XEXg { F# ipv3_sIM2 ->
                                                     V2 dt5_XEXe dt6_XEXg
                                                     }
                                                     }))))
                                       (\ x1_aFup ->
                                          ww3_sOFD
                                            (case x1_aFup of { V2 x2_aFuu y1_aFuv ->
                                             case x2_aFuu of { F# x#_sPZB ->
                                             case y1_aFuv of { F# x#1_sPZE ->
                                             ww3_sOFD
                                               (primitive
                                                  $dPrimMonad_aEhm
                                                  (\ s#_aF0u ->
                                                     case writeFloatArray#
                                                            dt2_sPZs
                                                            (+# dt4_sILN o_sNHw)
                                                            x#_sPZB
                                                            s#_aF0u
                                                     of s'#_aF0z
                                                     { __DEFAULT ->
                                                     (# s'#_aF0z, () #)
                                                     }))
                                               (primitive
                                                  $dPrimMonad_aEhm
                                                  (\ s#_aF0u ->
                                                     case writeFloatArray#
                                                            dt2_sPZs
                                                            (+# 1# (+# dt4_sILN o_sNHw))
                                                            x#1_sPZE
                                                            s#_aF0u
                                                     of s'#_aF0z
                                                     { __DEFAULT ->
                                                     (# s'#_aF0z, () #)
                                                     }))
                                             }
                                             }
                                             })
                                            lvl24_sHTF)
                                 }; } in
                         $wdo_copy_sOFL 0#;
                       1# ->
                         ww3_sOFD
                           (let { dt4_sIMb = +# dt_sPZq (*# 2# ww5_sOFV) } in
                            letrec {
                              $wdo_copy_sOFR
                                = \ ww6_sOFP ->
                                    case <# ww6_sOFP ww5_sOFV of {
                                      __DEFAULT -> lvl23_sHTT;
                                      1# ->
                                        let { o_sNHG = *# 2# ww6_sOFP } in
                                        let { lvl24_sHUq = $wdo_copy_sOFR (+# ww6_sOFP 1#) } in
                                        ww2_sOFC
                                          (let {
                                             lvl25_sHU6
                                               = primitive
                                                   $dPrimMonad_aEhm
                                                   ($fPrimCFloat_$creadByteArray#
                                                      dt2_sPZs (+# 1# (+# dt_sPZq o_sNHG))) } in
                                           ww2_sOFC
                                             (primitive
                                                $dPrimMonad_aEhm
                                                ($fPrimCFloat_$creadByteArray#
                                                   dt2_sPZs (+# dt_sPZq o_sNHG)))
                                             (\ x1_aFvb ->
                                                ww2_sOFC
                                                  lvl25_sHU6
                                                  (\ y1_aFvg ->
                                                     ww4_sOFE
                                                       (case x1_aFvb of dt5_XEXe { F# ipv2_sIMn ->
                                                        case y1_aFvg of dt6_XEXg { F# ipv3_sIMq ->
                                                        V2 dt5_XEXe dt6_XEXg
                                                        }
                                                        }))))
                                          (\ x1_aFvh ->
                                             ww3_sOFD
                                               (case x1_aFvh of { V2 x2_aFvm y1_aFvn ->
                                                case x2_aFvm of { F# x#_sPZH ->
                                                case y1_aFvn of { F# x#1_sPZK ->
                                                ww3_sOFD
                                                  (primitive
                                                     $dPrimMonad_aEhm
                                                     (\ s#_aF0u ->
                                                        case writeFloatArray#
                                                               dt2_sPZs
                                                               (+# dt4_sIMb o_sNHG)
                                                               x#_sPZH
                                                               s#_aF0u
                                                        of s'#_aF0z
                                                        { __DEFAULT ->
                                                        (# s'#_aF0z, () #)
                                                        }))
                                                  (primitive
                                                     $dPrimMonad_aEhm
                                                     (\ s#_aF0u ->
                                                        case writeFloatArray#
                                                               dt2_sPZs
                                                               (+# 1# (+# dt4_sIMb o_sNHG))
                                                               x#1_sPZK
                                                               s#_aF0u
                                                        of s'#_aF0z
                                                        { __DEFAULT ->
                                                        (# s'#_aF0z, () #)
                                                        }))
                                                }
                                                }
                                                })
                                               lvl24_sHUq)
                                    }; } in
                            $wdo_copy_sOFR 0#)
                           ($wdo_set_sOFX (*# 2# ww5_sOFV))
                     }; } in
             $wdo_set_sOFX 1#);
        0# -> ww4_sOFE ()
      }
      }
      }
      }

-- RHS size: {terms: 14, types: 2, coercions: 0, joins: 0/0}
$fMVectorMVectorECSVel
  = C:MVector
      $fMVectorMVectorECSVel_$cbasicLength
      $fMVectorMVectorECSVel_$cbasicUnsafeSlice
      $fMVectorMVectorECSVel_$cbasicOverlaps
      $fMVectorMVectorECSVel_$cbasicUnsafeNew
      $fMVectorMVectorECSVel_$cbasicInitialize
      $fMVectorMVectorECSVel_$cbasicUnsafeReplicate
      $fMVectorMVectorECSVel_$cbasicUnsafeRead
      $fMVectorMVectorECSVel_$cbasicUnsafeWrite
      $fMVectorMVectorECSVel_$cbasicClear
      $fMVectorMVectorECSVel_$cbasicSet
      $fMVectorMVectorECSVel_$cbasicUnsafeCopy
      $fMVectorMVectorECSVel_$cbasicUnsafeMove
      $fMVectorMVectorECSVel_$cbasicUnsafeGrow

-- RHS size: {terms: 47, types: 142, coercions: 162, joins: 0/0}
$sarchetypeAdd4
  = \ @ s_XcXd s1_X8xw ->
      case newByteArray# 8192# (s1_X8xw `cast` <Co:19>) of
      { (# ipv_aEYp, ipv1_aEYq #) ->
      case {__pkg_ccall primitive-0.7.0.0 forall s.
                               MutableByteArray# s
                               -> Int#
                               -> Word#
                               -> Word#
                               -> State# RealWorld
                               -> (# State# RealWorld #)}_aF9G
             ipv1_aEYq 0# 8192## 0## (ipv_aEYp `cast` <Co:7>)
      of
      { (# ds9_aF9L #) ->
      case newByteArray# 8192# (ds9_aF9L `cast` <Co:7>) of
      { (# ipv2_XFmd, ipv3_XFmf #) ->
      case {__pkg_ccall primitive-0.7.0.0 forall s.
                               MutableByteArray# s
                               -> Int#
                               -> Word#
                               -> Word#
                               -> State# RealWorld
                               -> (# State# RealWorld #)}_aF9G
             ipv3_XFmf 0# 8192## 0## (ipv2_XFmd `cast` <Co:7>)
      of
      { (# ds3_XFxW #) ->
      (# ds3_XFxW `cast` <Co:24>,
         CCons
           @~ <Co:11>
           ($fMVectorMVectorECSPos `cast` <Co:4>)
           ((MV_V2 1024# ((MVector 0# 2048# ipv1_aEYq) `cast` <Co:11>))
            `cast` <Co:25>)
           (CCons
              @~ <Co:7>
              ($fMVectorMVectorECSVel `cast` <Co:4>)
              ((MV_V2 1024# ((MVector 0# 2048# ipv3_XFmf) `cast` <Co:11>))
               `cast` <Co:25>)
              $WCNil) #)
      }
      }
      }
      }

-- RHS size: {terms: 135, types: 138, coercions: 34, joins: 0/6}
$fVectorVectorECSVel_$cbasicUnsafeCopy
  = \ @ m_aEeG $dPrimMonad_aEeI eta_B1 ds_dEU3 ->
      case eta_B1 `cast` <Co:18> of { MV_V2 ipv_aFbG ipv1_aFbH ->
      case ipv1_aFbH `cast` <Co:7> of
      { MVector dt_sPZS dt1_sPZT dt2_sPZU ->
      case ds_dEU3 `cast` <Co:6> of { V_V2 ipv2_aFbT ipv3_aFbU ->
      case ipv3_aFbU `cast` <Co:3> of
      { Vector dt4_sPZX dt5_sPZY dt6_sPZZ ->
      let { $dMonad_sHOE = $p1PrimMonad $dPrimMonad_aEeI } in
      let { lvl23_sIhy = return $dMonad_sHOE () } in
      letrec {
        $wdo_copy_sOG5
          = \ ww_sOG3 ->
              case <# ww_sOG3 ipv2_aFbT of {
                __DEFAULT -> lvl23_sIhy;
                1# ->
                  let { o_sNIT = *# 2# ww_sOG3 } in
                  let { lvl24_sIi3 = $wdo_copy_sOG5 (+# ww_sOG3 1#) } in
                  >>=
                    $dMonad_sHOE
                    (let {
                       lvl25_sIhJ
                         = case indexFloatArray# dt6_sPZZ (+# 1# (+# dt4_sPZX o_sNIT))
                           of wild2_aFdf
                           { __DEFAULT ->
                           return $dMonad_sHOE (F# wild2_aFdf)
                           } } in
                     >>=
                       $dMonad_sHOE
                       (case indexFloatArray# dt6_sPZZ (+# dt4_sPZX o_sNIT) of wild2_aFdf
                        { __DEFAULT ->
                        return $dMonad_sHOE (F# wild2_aFdf)
                        })
                       (\ x_aFc9 ->
                          >>=
                            $dMonad_sHOE
                            lvl25_sIhJ
                            (\ y_aFce ->
                               return
                                 $dMonad_sHOE
                                 (case x_aFc9 of dt7_XEXe { F# ipv4_sIO4 ->
                                  case y_aFce of dt8_XEXg { F# ipv5_sIO7 -> V2 dt7_XEXe dt8_XEXg }
                                  }))))
                    (\ x_aFcf ->
                       >>
                         $dMonad_sHOE
                         (case x_aFcf of { V2 x1_aFck y_aFcl ->
                          case x1_aFck of { F# x#_sQ02 ->
                          case y_aFcl of { F# x#1_sQ05 ->
                          >>
                            $dMonad_sHOE
                            (primitive
                               $dPrimMonad_aEeI
                               (\ s#_aF0u ->
                                  case writeFloatArray# dt2_sPZU (+# dt_sPZS o_sNIT) x#_sQ02 s#_aF0u
                                  of s'#_aF0z
                                  { __DEFAULT ->
                                  (# s'#_aF0z, () #)
                                  }))
                            (primitive
                               $dPrimMonad_aEeI
                               (\ s#_aF0u ->
                                  case writeFloatArray#
                                         dt2_sPZU (+# 1# (+# dt_sPZS o_sNIT)) x#1_sQ05 s#_aF0u
                                  of s'#_aF0z
                                  { __DEFAULT ->
                                  (# s'#_aF0z, () #)
                                  }))
                          }
                          }
                          })
                         lvl24_sIi3)
              }; } in
      $wdo_copy_sOG5 0#
      }
      }
      }
      }

-- RHS size: {terms: 73, types: 60, coercions: 11, joins: 0/2}
$fVectorVectorECSVel_$cbasicUnsafeIndexM
  = \ @ m_aEeq $dMonad_aEes eta_B1 idx_aE7Y ->
      >>=
        $dMonad_aEes
        (case eta_B1 `cast` <Co:6> of { V_V2 dt_aFe7 v_aFe8 ->
         case v_aFe8 `cast` <Co:3> of { Vector dt1_sQ08 dt2_sQ09 dt3_sQ0a ->
         let {
           o_sHOz = case idx_aE7Y of { I# y_aFec -> I# (*# 2# y_aFec) } } in
         let {
           lvl23_sIif
             = case o_sHOz of { I# x1_aFeh ->
               case indexFloatArray# dt3_sQ0a (+# 1# (+# dt1_sQ08 x1_aFeh))
               of wild3_aFdf
               { __DEFAULT ->
               return $dMonad_aEes (F# wild3_aFdf)
               }
               } } in
         >>=
           $dMonad_aEes
           (case o_sHOz of { I# y_aFdd ->
            case indexFloatArray# dt3_sQ0a (+# dt1_sQ08 y_aFdd) of wild3_aFdf
            { __DEFAULT ->
            return $dMonad_aEes (F# wild3_aFdf)
            }
            })
           (\ x_aFee ->
              >>=
                $dMonad_aEes
                lvl23_sIif
                (\ y_aFej ->
                   return
                     $dMonad_aEes
                     (case x_aFee of dt4_XEXe { F# ipv_sIOJ ->
                      case y_aFej of dt5_XEXg { F# ipv1_sIOM -> V2 dt4_XEXe dt5_XEXg }
                      })))
         }
         })
        (\ x1_aETa -> return $dMonad_aEes (x1_aETa `cast` <Co:2>))

-- RHS size: {terms: 27, types: 20, coercions: 23, joins: 0/0}
$fVectorVectorECSVel_$cbasicUnsafeSlice
  = \ eta_B3 eta1_B2 eta2_B1 ->
      case eta2_B1 `cast` <Co:6> of { V_V2 dt_aFeL v_aFeM ->
      case v_aFeM `cast` <Co:3> of { Vector dt1_sQ0d dt2_sQ0e dt3_sQ0f ->
      case eta1_B2 of { I# dt5_aFbQ ->
      case eta_B3 of { I# y_aFeQ ->
      (V_V2
         dt5_aFbQ
         ((Vector (+# dt1_sQ0d (*# 2# y_aFeQ)) (*# 2# dt5_aFbQ) dt3_sQ0f)
          `cast` <Co:5>))
      `cast` <Co:9>
      }
      }
      }
      }

-- RHS size: {terms: 6, types: 7, coercions: 6, joins: 0/0}
$fVectorVectorECSVel_$cbasicLength
  = \ eta_B1 ->
      case eta_B1 `cast` <Co:6> of { V_V2 dt_aFfo ds1_aFfp ->
      I# dt_aFfo
      }

-- RHS size: {terms: 47, types: 155, coercions: 60, joins: 0/0}
$fVectorVectorECSVel_$cbasicUnsafeThaw
  = \ @ m_aEdT $dPrimMonad_aEdV eta_B1 ->
      case $p1PrimMonad $dPrimMonad_aEdV of
      { C:Monad ww1_sOG8 ww2_sOG9 ww3_sOGa ww4_sOGb ->
      (ww2_sOG9
         (case eta_B1 `cast` <Co:6> of { V_V2 dt_aFfD v_aFfE ->
          case v_aFfE `cast` <Co:3> of { Vector dt1_sQ0i dt2_sQ0j dt3_sQ0k ->
          ww2_sOG9
            (ww2_sOG9
               (ww2_sOG9
                  (primitive
                     $dPrimMonad_aEdV
                     (\ s#_aFfV ->
                        (# s#_aFfV, MutableByteArray (dt3_sQ0k `cast` <Co:6>) #)))
                  (\ x1_aFfW ->
                     ww4_sOGb
                       (case x1_aFfW of { MutableByteArray dt5_af1V ->
                        MVector dt1_sQ0i dt2_sQ0j dt5_af1V
                        })))
               (\ x1_aFfX -> ww4_sOGb (x1_aFfX `cast` <Co:9>)))
            (\ x1_aFfG ->
               ww4_sOGb
                 (case x1_aFfG `cast` <Co:7> of nt_sIPc
                  { MVector ipv_sIPe ipv1_sIPf ipv2_sIPg ->
                  (MV_V2 dt_aFfD (nt_sIPc `cast` <Co:9>)) `cast` <Co:5>
                  }))
          }
          })
         (\ x1_aETa -> ww4_sOGb (x1_aETa `cast` <Co:9>)))
      `cast` <Co:6>
      }

-- RHS size: {terms: 52, types: 131, coercions: 46, joins: 0/0}
$fVectorVectorECSVel_$cbasicUnsafeFreeze
  = \ @ m_aEdA $dPrimMonad_aEdC eta_B1 ->
      case $p1PrimMonad $dPrimMonad_aEdC of
      { C:Monad ww1_sOGA ww2_sOGB ww3_sOGC ww4_sOGD ->
      ww2_sOGB
        (case eta_B1 `cast` <Co:18> of { MV_V2 dt_aFgb v_aFgc ->
         case v_aFgc `cast` <Co:7> of
         { MVector dt1_sQ0n dt2_sQ0o dt3_sQ0p ->
         ww2_sOGB
           (ww2_sOGB
              (ww2_sOGB
                 (primitive
                    $dPrimMonad_aEdC
                    (\ s#_aFgt ->
                       case unsafeFreezeByteArray# dt3_sQ0p s#_aFgt of
                       { (# ipv_aFgw, ipv1_aFgx #) ->
                       (# ipv_aFgw, ByteArray ipv1_aFgx #)
                       }))
                 (\ x1_aFgz ->
                    ww4_sOGD
                      (case x1_aFgz of { ByteArray dt5_aFd5 ->
                       Vector dt1_sQ0n dt2_sQ0o dt5_aFd5
                       })))
              (\ x1_aFgA -> ww4_sOGD (x1_aFgA `cast` <Co:5>)))
           (\ x1_aFge ->
              ww4_sOGD
                (case x1_aFge `cast` <Co:3> of nt_sIPo
                 { Vector ipv_sIPq ipv1_sIPr ipv2_sIPs ->
                 (V_V2 dt_aFgb (nt_sIPo `cast` <Co:5>)) `cast` <Co:3>
                 }))
         }
         })
        (\ x1_aETa -> ww4_sOGD (x1_aETa `cast` <Co:5>))
      }

-- RHS size: {terms: 135, types: 138, coercions: 34, joins: 0/6}
$fVectorVectorECSPos_$cbasicUnsafeCopy
  = \ @ m_aE3u $dPrimMonad_aE3w eta_B1 ds_dETl ->
      case eta_B1 `cast` <Co:18> of { MV_V2 ipv_aFbG ipv1_aFbH ->
      case ipv1_aFbH `cast` <Co:7> of
      { MVector dt_sQ0s dt1_sQ0t dt2_sQ0u ->
      case ds_dETl `cast` <Co:6> of { V_V2 ipv2_aFbT ipv3_aFbU ->
      case ipv3_aFbU `cast` <Co:3> of
      { Vector dt4_sQ0x dt5_sQ0y dt6_sQ0z ->
      let { $dMonad_sHM0 = $p1PrimMonad $dPrimMonad_aE3w } in
      let { lvl23_sIiv = return $dMonad_sHM0 () } in
      letrec {
        $wdo_copy_sOH5
          = \ ww_sOH3 ->
              case <# ww_sOH3 ipv2_aFbT of {
                __DEFAULT -> lvl23_sIiv;
                1# ->
                  let { o_sNK9 = *# 2# ww_sOH3 } in
                  let { lvl24_sIj0 = $wdo_copy_sOH5 (+# ww_sOH3 1#) } in
                  >>=
                    $dMonad_sHM0
                    (let {
                       lvl25_sIiG
                         = case indexFloatArray# dt6_sQ0z (+# 1# (+# dt4_sQ0x o_sNK9))
                           of wild2_aFdf
                           { __DEFAULT ->
                           return $dMonad_sHM0 (F# wild2_aFdf)
                           } } in
                     >>=
                       $dMonad_sHM0
                       (case indexFloatArray# dt6_sQ0z (+# dt4_sQ0x o_sNK9) of wild2_aFdf
                        { __DEFAULT ->
                        return $dMonad_sHM0 (F# wild2_aFdf)
                        })
                       (\ x_aFc9 ->
                          >>=
                            $dMonad_sHM0
                            lvl25_sIiG
                            (\ y_aFce ->
                               return
                                 $dMonad_sHM0
                                 (case x_aFc9 of dt7_XEXe { F# ipv4_sIPK ->
                                  case y_aFce of dt8_XEXg { F# ipv5_sIPN -> V2 dt7_XEXe dt8_XEXg }
                                  }))))
                    (\ x_aFcf ->
                       >>
                         $dMonad_sHM0
                         (case x_aFcf of { V2 x1_aFck y_aFcl ->
                          case x1_aFck of { F# x#_sQ0C ->
                          case y_aFcl of { F# x#1_sQ0F ->
                          >>
                            $dMonad_sHM0
                            (primitive
                               $dPrimMonad_aE3w
                               (\ s#_aF0u ->
                                  case writeFloatArray# dt2_sQ0u (+# dt_sQ0s o_sNK9) x#_sQ0C s#_aF0u
                                  of s'#_aF0z
                                  { __DEFAULT ->
                                  (# s'#_aF0z, () #)
                                  }))
                            (primitive
                               $dPrimMonad_aE3w
                               (\ s#_aF0u ->
                                  case writeFloatArray#
                                         dt2_sQ0u (+# 1# (+# dt_sQ0s o_sNK9)) x#1_sQ0F s#_aF0u
                                  of s'#_aF0z
                                  { __DEFAULT ->
                                  (# s'#_aF0z, () #)
                                  }))
                          }
                          }
                          })
                         lvl24_sIj0)
              }; } in
      $wdo_copy_sOH5 0#
      }
      }
      }
      }

-- RHS size: {terms: 73, types: 60, coercions: 11, joins: 0/2}
$fVectorVectorECSPos_$cbasicUnsafeIndexM
  = \ @ m_aE3e $dMonad_aE3g eta_B1 idx_aDOh ->
      >>=
        $dMonad_aE3g
        (case eta_B1 `cast` <Co:6> of { V_V2 dt_aFe7 v_aFe8 ->
         case v_aFe8 `cast` <Co:3> of { Vector dt1_sQ0I dt2_sQ0J dt3_sQ0K ->
         let {
           o_sHLV = case idx_aDOh of { I# y_aFec -> I# (*# 2# y_aFec) } } in
         let {
           lvl23_sIjc
             = case o_sHLV of { I# x1_aFeh ->
               case indexFloatArray# dt3_sQ0K (+# 1# (+# dt1_sQ0I x1_aFeh))
               of wild3_aFdf
               { __DEFAULT ->
               return $dMonad_aE3g (F# wild3_aFdf)
               }
               } } in
         >>=
           $dMonad_aE3g
           (case o_sHLV of { I# y_aFdd ->
            case indexFloatArray# dt3_sQ0K (+# dt1_sQ0I y_aFdd) of wild3_aFdf
            { __DEFAULT ->
            return $dMonad_aE3g (F# wild3_aFdf)
            }
            })
           (\ x_aFee ->
              >>=
                $dMonad_aE3g
                lvl23_sIjc
                (\ y_aFej ->
                   return
                     $dMonad_aE3g
                     (case x_aFee of dt4_XEXe { F# ipv_sIQp ->
                      case y_aFej of dt5_XEXg { F# ipv1_sIQs -> V2 dt4_XEXe dt5_XEXg }
                      })))
         }
         })
        (\ x1_aETa -> return $dMonad_aE3g (x1_aETa `cast` <Co:2>))

-- RHS size: {terms: 27, types: 20, coercions: 23, joins: 0/0}
$fVectorVectorECSPos_$cbasicUnsafeSlice
  = \ eta_B3 eta1_B2 eta2_B1 ->
      case eta2_B1 `cast` <Co:6> of { V_V2 dt_aFeL v_aFeM ->
      case v_aFeM `cast` <Co:3> of { Vector dt1_sQ0N dt2_sQ0O dt3_sQ0P ->
      case eta1_B2 of { I# dt5_aFbQ ->
      case eta_B3 of { I# y_aFeQ ->
      (V_V2
         dt5_aFbQ
         ((Vector (+# dt1_sQ0N (*# 2# y_aFeQ)) (*# 2# dt5_aFbQ) dt3_sQ0P)
          `cast` <Co:5>))
      `cast` <Co:9>
      }
      }
      }
      }

-- RHS size: {terms: 6, types: 7, coercions: 6, joins: 0/0}
$fVectorVectorECSPos_$cbasicLength
  = \ eta_B1 ->
      case eta_B1 `cast` <Co:6> of { V_V2 dt_aFfo ds1_aFfp ->
      I# dt_aFfo
      }

-- RHS size: {terms: 47, types: 155, coercions: 60, joins: 0/0}
$fVectorVectorECSPos_$cbasicUnsafeThaw
  = \ @ m_aE2H $dPrimMonad_aE2J eta_B1 ->
      case $p1PrimMonad $dPrimMonad_aE2J of
      { C:Monad ww1_sOH8 ww2_sOH9 ww3_sOHa ww4_sOHb ->
      (ww2_sOH9
         (case eta_B1 `cast` <Co:6> of { V_V2 dt_aFfD v_aFfE ->
          case v_aFfE `cast` <Co:3> of { Vector dt1_sQ0S dt2_sQ0T dt3_sQ0U ->
          ww2_sOH9
            (ww2_sOH9
               (ww2_sOH9
                  (primitive
                     $dPrimMonad_aE2J
                     (\ s#_aFfV ->
                        (# s#_aFfV, MutableByteArray (dt3_sQ0U `cast` <Co:6>) #)))
                  (\ x1_aFfW ->
                     ww4_sOHb
                       (case x1_aFfW of { MutableByteArray dt5_af1V ->
                        MVector dt1_sQ0S dt2_sQ0T dt5_af1V
                        })))
               (\ x1_aFfX -> ww4_sOHb (x1_aFfX `cast` <Co:9>)))
            (\ x1_aFfG ->
               ww4_sOHb
                 (case x1_aFfG `cast` <Co:7> of nt_sIQS
                  { MVector ipv_sIQU ipv1_sIQV ipv2_sIQW ->
                  (MV_V2 dt_aFfD (nt_sIQS `cast` <Co:9>)) `cast` <Co:5>
                  }))
          }
          })
         (\ x1_aETa -> ww4_sOHb (x1_aETa `cast` <Co:9>)))
      `cast` <Co:6>
      }

-- RHS size: {terms: 52, types: 131, coercions: 46, joins: 0/0}
$fVectorVectorECSPos_$cbasicUnsafeFreeze
  = \ @ m_aE2k $dPrimMonad_aE2m eta_B1 ->
      case $p1PrimMonad $dPrimMonad_aE2m of
      { C:Monad ww1_sOHA ww2_sOHB ww3_sOHC ww4_sOHD ->
      ww2_sOHB
        (case eta_B1 `cast` <Co:18> of { MV_V2 dt_aFgb v_aFgc ->
         case v_aFgc `cast` <Co:7> of
         { MVector dt1_sQ0X dt2_sQ0Y dt3_sQ0Z ->
         ww2_sOHB
           (ww2_sOHB
              (ww2_sOHB
                 (primitive
                    $dPrimMonad_aE2m
                    (\ s#_aFgt ->
                       case unsafeFreezeByteArray# dt3_sQ0Z s#_aFgt of
                       { (# ipv_aFgw, ipv1_aFgx #) ->
                       (# ipv_aFgw, ByteArray ipv1_aFgx #)
                       }))
                 (\ x1_aFgz ->
                    ww4_sOHD
                      (case x1_aFgz of { ByteArray dt5_aFd5 ->
                       Vector dt1_sQ0X dt2_sQ0Y dt5_aFd5
                       })))
              (\ x1_aFgA -> ww4_sOHD (x1_aFgA `cast` <Co:5>)))
           (\ x1_aFge ->
              ww4_sOHD
                (case x1_aFge `cast` <Co:3> of nt_sIR4
                 { Vector ipv_sIR6 ipv1_sIR7 ipv2_sIR8 ->
                 (V_V2 dt_aFgb (nt_sIR4 `cast` <Co:5>)) `cast` <Co:3>
                 }))
         }
         })
        (\ x1_aETa -> ww4_sOHD (x1_aETa `cast` <Co:5>))
      }

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$fShowECSPos4 = "ECSPos {"#

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$fShowECSPos3 = "unECSPos = "#

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$fShowECSPos2 = "}"#

-- RHS size: {terms: 39, types: 14, coercions: 1, joins: 0/1}
$w$cshowsPrec
  = \ ww_sOI4 w_sOI1 ->
      let {
        f_sHLp
          = case w_sOI1 `cast` <Co:1> of { V2 ww3_aFnB ww4_aFnC ->
            $w$cshowsPrec $fShowFloat 0# ww3_aFnB ww4_aFnC
            } } in
      case >=# ww_sOI4 11# of {
        __DEFAULT ->
          \ x_X6aB ->
            unpackAppendCString#
              $fShowECSPos4
              (unpackAppendCString#
                 $fShowECSPos3
                 (f_sHLp (unpackAppendCString# $fShowECSPos2 x_X6aB)));
        1# ->
          \ x_aESS ->
            : $fShow(,)4
              (unpackAppendCString#
                 $fShowECSPos4
                 (unpackAppendCString#
                    $fShowECSPos3
                    (f_sHLp
                       (unpackAppendCString# $fShowECSPos2 (: $fShow(,)2 x_aESS)))))
      }

-- RHS size: {terms: 8, types: 4, coercions: 0, joins: 0/0}
$fShowECSPos_$cshowsPrec
  = \ w_sOI0 w1_sOI1 ->
      case w_sOI0 of { I# ww1_sOI4 -> $w$cshowsPrec ww1_sOI4 w1_sOI1 }

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$fShowECSPos5 = unpackCString# $fShowECSPos2

-- RHS size: {terms: 27, types: 10, coercions: 1, joins: 0/0}
$fShowECSPos_$cshow
  = \ x_aESW ->
      unpackAppendCString#
        $fShowECSPos4
        (unpackAppendCString#
           $fShowECSPos3
           (case x_aESW `cast` <Co:1> of { V2 ww3_aFnB ww4_aFnC ->
            case ww3_aFnB of { F# ww1_sQ1a ->
            case ww4_aFnC of { F# ww8_sQ1d ->
            unpackAppendCString#
              $fShowV1
              ($w$sshowSignedFloat1
                 $fShowFloat2
                 $fReadV3
                 ww1_sQ1a
                 (: showSpace1
                    ($w$sshowSignedFloat1
                       $fShowFloat2 $fReadV3 ww8_sQ1d $fShowECSPos5)))
            }
            }
            }))

-- RHS size: {terms: 4, types: 1, coercions: 0, joins: 0/0}
$fShowECSPos1 = \ w_sOI1 -> $w$cshowsPrec 0# w_sOI1

-- RHS size: {terms: 6, types: 4, coercions: 0, joins: 0/0}
$fShowECSPos_$cshowList
  = \ ls_aESZ s_aET0 -> showList__ $fShowECSPos1 ls_aESZ s_aET0

-- RHS size: {terms: 4, types: 1, coercions: 0, joins: 0/0}
$fShowECSPos
  = C:Show
      $fShowECSPos_$cshowsPrec
      $fShowECSPos_$cshow
      $fShowECSPos_$cshowList

-- RHS size: {terms: 2, types: 1, coercions: 0, joins: 0/0}
unECSPos1 = \ ds_dESJ -> ds_dESJ

-- RHS size: {terms: 1, types: 0, coercions: 3, joins: 0/0}
unECSPos = unECSPos1 `cast` <Co:3>

-- RHS size: {terms: 5, types: 6, coercions: 0, joins: 0/0}
$fVectorVectorECSPos_$celemseq = \ @ b_aE3J _ _ x_aFob -> x_aFob

-- RHS size: {terms: 9, types: 2, coercions: 4, joins: 0/0}
$fVectorVectorECSPos
  = C:Vector
      ($fMVectorMVectorECSPos `cast` <Co:4>)
      $fVectorVectorECSPos_$cbasicUnsafeFreeze
      $fVectorVectorECSPos_$cbasicUnsafeThaw
      $fVectorVectorECSPos_$cbasicLength
      $fVectorVectorECSPos_$cbasicUnsafeSlice
      $fVectorVectorECSPos_$cbasicUnsafeIndexM
      $fVectorVectorECSPos_$cbasicUnsafeCopy
      $fVectorVectorECSPos_$celemseq

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
$fUnboxECSPos = C:Unbox $fVectorVectorECSPos $fMVectorMVectorECSPos

-- RHS size: {terms: 2, types: 1, coercions: 0, joins: 0/0}
unECSVel1 = \ ds_dESH -> ds_dESH

-- RHS size: {terms: 1, types: 0, coercions: 3, joins: 0/0}
unECSVel = unECSVel1 `cast` <Co:3>

-- RHS size: {terms: 5, types: 6, coercions: 0, joins: 0/0}
$fVectorVectorECSVel_$celemseq = \ @ b_aEeV _ _ x_aFob -> x_aFob

-- RHS size: {terms: 9, types: 2, coercions: 4, joins: 0/0}
$fVectorVectorECSVel
  = C:Vector
      ($fMVectorMVectorECSVel `cast` <Co:4>)
      $fVectorVectorECSVel_$cbasicUnsafeFreeze
      $fVectorVectorECSVel_$cbasicUnsafeThaw
      $fVectorVectorECSVel_$cbasicLength
      $fVectorVectorECSVel_$cbasicUnsafeSlice
      $fVectorVectorECSVel_$cbasicUnsafeIndexM
      $fVectorVectorECSVel_$cbasicUnsafeCopy
      $fVectorVectorECSVel_$celemseq

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
$fUnboxECSVel = C:Unbox $fVectorVectorECSVel $fMVectorMVectorECSVel

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
initWorld_a1 = F# 0.0#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
initWorld_a2 = F# 1.0#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
initWorld2 = F# 0.5#

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
initWorld4 = V2 initWorld_a1 initWorld_a1

-- RHS size: {terms: 3, types: 11, coercions: 9, joins: 0/0}
initWorld_x = HCons @~ <Co:7> (initWorld4 `cast` <Co:2>) $WHNil

-- RHS size: {terms: 104, types: 263, coercions: 109, joins: 4/4}
initWorld3
  = \ s1_Xf1T ->
      case newArray# 1000# uninitialised (s1_Xf1T `cast` <Co:27>) of
      { (# ipv_agbM, ipv1_agbN #) ->
      join {
        exit_X7B ww_sOIP w_sOIM
          = case unsafeFreezeArray# ipv1_agbN (w_sOIM `cast` <Co:29>) of
            { (# ipv2_agcY, ipv3_agcZ #) ->
            (# ipv2_agcY `cast` <Co:3>, Vector 0# ww_sOIP ipv3_agcZ #)
            } } in
      joinrec {
        $wfoldlM'_loop_sOIR w_sOIJ ww_sOIP w1_sOIL w2_sOIM
          = case w_sOIJ of { __DEFAULT ->
            case w1_sOIL of {
              False -> jump exit_X7B ww_sOIP w2_sOIM;
              True ->
                case writeArray#
                       ipv1_agbN ww_sOIP initWorld_x (w2_sOIM `cast` <Co:4>)
                of s'#_aMBo
                { __DEFAULT ->
                join {
                  exit1_X7G ww1_sOIG w3_sOID
                    = case copyMutableArray#
                             ipv1_agbN
                             ww_sOIP
                             ipv1_agbN
                             (+# ww_sOIP ww1_sOIG)
                             (-# 1000# ww1_sOIG)
                             (w3_sOID `cast` <Co:4>)
                      of s'#1_aMB3
                      { __DEFAULT ->
                      jump $wfoldlM'_loop_sOIR
                        SPEC (+# ww_sOIP 1000#) False (s'#1_aMB3 `cast` <Co:3>)
                      } } in
                joinrec {
                  $wdo_set_sOII ww1_sOIG w3_sOID
                    = case <# (*# 2# ww1_sOIG) 1000# of {
                        __DEFAULT -> jump exit1_X7G ww1_sOIG w3_sOID;
                        1# ->
                          case copyMutableArray#
                                 ipv1_agbN
                                 ww_sOIP
                                 ipv1_agbN
                                 (+# ww_sOIP ww1_sOIG)
                                 ww1_sOIG
                                 (w3_sOID `cast` <Co:4>)
                          of s'#1_aMBj
                          { __DEFAULT ->
                          jump $wdo_set_sOII (*# 2# ww1_sOIG) (s'#1_aMBj `cast` <Co:3>)
                          }
                      }; } in
                jump $wdo_set_sOII 1# (s'#_aMBo `cast` <Co:3>)
                }
            }
            }; } in
      jump $wfoldlM'_loop_sOIR SPEC 0# True (ipv_agbM `cast` <Co:29>)
      }

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
initWorld7 = V2 initWorld_a2 initWorld_a2

-- RHS size: {terms: 3, types: 11, coercions: 9, joins: 0/0}
initWorld6 = HCons @~ <Co:7> (initWorld7 `cast` <Co:2>) $WHNil

-- RHS size: {terms: 3, types: 19, coercions: 13, joins: 0/0}
initWorld_x1
  = HCons @~ <Co:11> (initWorld4 `cast` <Co:2>) initWorld6

-- RHS size: {terms: 104, types: 327, coercions: 121, joins: 4/4}
initWorld5
  = \ s1_aexV ->
      case newArray# 1000# uninitialised (s1_aexV `cast` <Co:31>) of
      { (# ipv_agbM, ipv1_agbN #) ->
      join {
        exit_X7q ww_sOIs w_sOIp
          = case unsafeFreezeArray# ipv1_agbN (w_sOIp `cast` <Co:33>) of
            { (# ipv2_agcY, ipv3_agcZ #) ->
            (# ipv2_agcY `cast` <Co:3>, Vector 0# ww_sOIs ipv3_agcZ #)
            } } in
      joinrec {
        $wfoldlM'_loop_sOIu w_sOIm ww_sOIs w1_sOIo w2_sOIp
          = case w_sOIm of { __DEFAULT ->
            case w1_sOIo of {
              False -> jump exit_X7q ww_sOIs w2_sOIp;
              True ->
                case writeArray#
                       ipv1_agbN ww_sOIs initWorld_x1 (w2_sOIp `cast` <Co:4>)
                of s'#_aMBo
                { __DEFAULT ->
                join {
                  exit1_X7v ww1_sOIj w3_sOIg
                    = case copyMutableArray#
                             ipv1_agbN
                             ww_sOIs
                             ipv1_agbN
                             (+# ww_sOIs ww1_sOIj)
                             (-# 1000# ww1_sOIj)
                             (w3_sOIg `cast` <Co:4>)
                      of s'#1_aMB3
                      { __DEFAULT ->
                      jump $wfoldlM'_loop_sOIu
                        SPEC (+# ww_sOIs 1000#) False (s'#1_aMB3 `cast` <Co:3>)
                      } } in
                joinrec {
                  $wdo_set_sOIl ww1_sOIj w3_sOIg
                    = case <# (*# 2# ww1_sOIj) 1000# of {
                        __DEFAULT -> jump exit1_X7v ww1_sOIj w3_sOIg;
                        1# ->
                          case copyMutableArray#
                                 ipv1_agbN
                                 ww_sOIs
                                 ipv1_agbN
                                 (+# ww_sOIs ww1_sOIj)
                                 ww1_sOIj
                                 (w3_sOIg `cast` <Co:4>)
                          of s'#1_aMBj
                          { __DEFAULT ->
                          jump $wdo_set_sOIl (*# 2# ww1_sOIj) (s'#1_aMBj `cast` <Co:3>)
                          }
                      }; } in
                jump $wdo_set_sOIl 1# (s'#_aMBo `cast` <Co:3>)
                }
            }
            }; } in
      jump $wfoldlM'_loop_sOIu SPEC 0# True (ipv_agbM `cast` <Co:33>)
      }

-- RHS size: {terms: 61, types: 574, coercions: 170, joins: 0/0}
initWorld1
  = \ @ s_aEaU s1_X8d2 ->
      case archetypeNew of { Archetype ww10_shBD ww11_shBE ->
      case ww11_shBE of { Vector ww13_sQ1i ww14_sQ1j ww15_sQ1k ->
      case runRW# initWorld5 of { (# ipv1_aey6, ipv2_aey7 #) ->
      case $warchetypeAdd
             $fVectorVectora_$cbasicLength
             $fVectorVectora_$cbasicUnsafeIndexM
             ($sarchetypeAdd4 `cast` <Co:33>)
             ww10_shBD
             ww13_sQ1i
             ww14_sQ1j
             ww15_sQ1k
             ipv2_aey7
             s1_X8d2
      of
      { (# ipv_a885, ipv4_a886 #) ->
      case archetypeNew of { Archetype ww2_XhZR ww3_XhZT ->
      case ww3_XhZT of { Vector ww5_sQ1x ww6_sQ1y ww7_sQ1z ->
      case runRW# initWorld3 of { (# ipv5_XeN8, ipv6_XeNa #) ->
      case $warchetypeAdd
             $fVectorVectora_$cbasicLength
             $fVectorVectora_$cbasicUnsafeIndexM
             ($sarchetypeAdd2 `cast` <Co:25>)
             ww2_XhZR
             ww5_sQ1x
             ww6_sQ1y
             ww7_sQ1z
             ipv6_XeNa
             ipv_a885
      of
      { (# ipv7_X8aI, ipv8_X8aK #) ->
      case newMutVar# (initWorld2 `cast` <Co:2>) ipv7_X8aI of
      { (# ipv9_a87O, ipv10_a87P #) ->
      (# ipv9_a87O,
         (HCons
            @~ <Co:37>
            ipv4_a886
            (HCons
               @~ <Co:21>
               ipv8_X8aK
               (HCons @~ <Co:9> ((STRef ipv10_a87P) `cast` <Co:4>) $WHNil)))
         `cast` <Co:39> #)
      }
      }
      }
      }
      }
      }
      }
      }
      }

-- RHS size: {terms: 1, types: 0, coercions: 8, joins: 0/0}
initWorld = initWorld1 `cast` <Co:8>

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
file3_rQM7 = "./Data/Vector/Generic/Mutable.hs"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
file1_rQM8 = unpackCString# file3_rQM7

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
lvl21_rQM9 = "read"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl22_rQMa = unpackCString# lvl21_rQM9

-- RHS size: {terms: 11, types: 7, coercions: 0, joins: 0/0}
someFunc13
  = \ @ s_aEal ipv_seL3 dt2_af1Y ->
      $wcheckError
        file1_rQM8
        697#
        Bounds
        lvl22_rQMa
        (checkIndex_msg# ipv_seL3 dt2_af1Y)

-- RHS size: {terms: 11, types: 7, coercions: 0, joins: 0/0}
someFunc12
  = \ @ s_aEal i_sHI1 n#_aepI ->
      $wcheckError
        file1_rQM8 697# Bounds lvl22_rQMa (checkIndex_msg# i_sHI1 n#_aepI)

-- RHS size: {terms: 11, types: 7, coercions: 0, joins: 0/0}
someFunc11
  = \ @ s_aEal i_sHI1 n#_aepI ->
      $wcheckError
        file1_rQM8 697# Bounds lvl22_rQMa (checkIndex_msg# i_sHI1 n#_aepI)

-- RHS size: {terms: 299, types: 767, coercions: 190, joins: 7/10}
someFunc10
  = \ @ s_aEal w_XEbG s1_a882 ->
      case (($wreplicateM
               $fApplicativeST
               10#
               (case w_XEbG `cast` <Co:31> of
                { HCons @ a1_a5XA @ as1_a5XB co_a5XC a2_a5Mz as2_a5MA ->
                (\ s2_aevc ->
                   case a2_a5Mz `cast` <Co:4> of { Archetype dt_detX ds_denP ->
                   case ds_denP of { Vector dt1_sQ1J dt2_sQ1K dt3_sQ1L ->
                   joinrec {
                     $wconsume_loop_sOJG w1_sOJz ww_sOJE w2_sOJB
                       = case w1_sOJz of { __DEFAULT ->
                         case >=# ww_sOJE dt2_sQ1K of {
                           __DEFAULT ->
                             case indexArray# dt3_sQ1L (+# dt1_sQ1J ww_sOJE) of
                             { (# ipv_ag9e #) ->
                             case ipv_ag9e of { (header_abIq, chunk_abIr) ->
                             join {
                               exit_X7z wild4_XeW
                                 = case header_abIq `cast` <Co:9> of
                                   { MVector dt4_af1X dt5_af1Y dt6_af1Z ->
                                   case someFunc13 wild4_XeW dt5_af1Y of wild6_00 { }
                                   } } in
                             join {
                               exit1_X7A w3_sOJt
                                 = jump $wconsume_loop_sOJG SPEC (+# ww_sOJE 1#) w3_sOJt } in
                             joinrec {
                               $wgo_sOJy ww1_sOJw w3_sOJt
                                 = case ww1_sOJw of wild4_XeW {
                                     __DEFAULT ->
                                       case >=# wild4_XeW 0# of {
                                         __DEFAULT -> jump exit_X7z wild4_XeW;
                                         1# ->
                                           case header_abIq `cast` <Co:9> of
                                           { MVector dt4_af1X dt5_af1Y dt6_af1Z ->
                                           case <# wild4_XeW dt5_af1Y of {
                                             __DEFAULT ->
                                               case someFunc13 wild4_XeW dt5_af1Y of wild6_00 { };
                                             1# ->
                                               case readWordArray#
                                                      dt6_af1Z
                                                      (+# dt4_af1X wild4_XeW)
                                                      (w3_sOJt `cast` <Co:4>)
                                               of
                                               { (# ipv1_af2z, ipv2_af2A #) ->
                                               let { j'_sHHU = *# wild4_XeW 64# } in
                                               join {
                                                 exit2_X7R i_sHI1
                                                   = case chunk_abIr of
                                                     { CCons @ a4_acxr @ as4_acxs co1_acxt
                                                             $dMVector_acxu ds3_det0 as5_abJp ->
                                                     case as5_abJp `cast` <Co:5> of
                                                     { CCons @ a5_acx9 @ as6_acxa co2_acxb
                                                             $dMVector1_acxc a6_abJl ds4_desA ->
                                                     case a6_abJl `cast` <Co:29> of
                                                     { MV_V2 dt7_aFbs ds6_aFbt ->
                                                     case someFunc12 i_sHI1 dt7_aFbs of wild9_00 { }
                                                     }
                                                     }
                                                     } } in
                                               join {
                                                 exit3_X7S w4_sOJm
                                                   = jump $wgo_sOJy (+# wild4_XeW 1#) w4_sOJm } in
                                               joinrec {
                                                 $wgo1_sOJr ww2_sOJp w4_sOJm
                                                   = case ww2_sOJp of wild6_Xfw {
                                                       __DEFAULT ->
                                                         case >=# wild6_Xfw 0# of {
                                                           __DEFAULT ->
                                                             case overflowError of wild7_00 { };
                                                           1# ->
                                                             case >=# wild6_Xfw 64# of {
                                                               __DEFAULT ->
                                                                 case and#
                                                                        ipv2_af2A
                                                                        (uncheckedShiftL#
                                                                           1## wild6_Xfw)
                                                                 of {
                                                                   __DEFAULT ->
                                                                     let {
                                                                       i_sHI1
                                                                         = +# j'_sHHU wild6_Xfw } in
                                                                     case >=# i_sHI1 0# of {
                                                                       __DEFAULT ->
                                                                         jump exit2_X7R i_sHI1;
                                                                       1# ->
                                                                         case chunk_abIr of
                                                                         { CCons @ a4_acxr
                                                                                 @ as4_acxs co1_acxt
                                                                                 $dMVector_acxu
                                                                                 ds3_det0
                                                                                 as5_abJp ->
                                                                         case as5_abJp `cast` <Co:5>
                                                                         of
                                                                         { CCons @ a5_acx9
                                                                                 @ as6_acxa co2_acxb
                                                                                 $dMVector1_acxc
                                                                                 a6_abJl ds4_desA ->
                                                                         case a6_abJl `cast` <Co:29>
                                                                         of
                                                                         { MV_V2 dt7_aFbs
                                                                                 ds6_aFbt ->
                                                                         case ds6_aFbt `cast` <Co:9>
                                                                         of
                                                                         { MVector dt8_sQ20 dt9_sQ21
                                                                                   dt10_sQ22 ->
                                                                         case <# i_sHI1 dt7_aFbs
                                                                         of {
                                                                           __DEFAULT ->
                                                                             case someFunc12
                                                                                    i_sHI1 dt7_aFbs
                                                                             of wild12_00 {
                                                                             };
                                                                           1# ->
                                                                             let {
                                                                               o_sHI5
                                                                                 = *# 2# i_sHI1 } in
                                                                             case readFloatArray#
                                                                                    dt10_sQ22
                                                                                    (+#
                                                                                       dt8_sQ20
                                                                                       o_sHI5)
                                                                                    (w4_sOJm
                                                                                     `cast` <Co:4>)
                                                                             of
                                                                             { (# ipv4_aF07,
                                                                                  ipv5_aF08 #) ->
                                                                             case readFloatArray#
                                                                                    dt10_sQ22
                                                                                    (+#
                                                                                       1#
                                                                                       (+#
                                                                                          dt8_sQ20
                                                                                          o_sHI5))
                                                                                    ipv4_aF07
                                                                             of
                                                                             { (# ipv6_XFqo,
                                                                                  ipv7_XFqq #) ->
                                                                             case ds3_det0
                                                                                  `cast` <Co:29>
                                                                             of
                                                                             { MV_V2 dt11_XFtg
                                                                                     ds9_XFti ->
                                                                             case ds9_XFti
                                                                                  `cast` <Co:9>
                                                                             of
                                                                             { MVector dt12_sQ25
                                                                                       dt13_sQ26
                                                                                       dt14_sQ27 ->
                                                                             case <#
                                                                                    i_sHI1 dt11_XFtg
                                                                             of {
                                                                               __DEFAULT ->
                                                                                 case someFunc11
                                                                                        i_sHI1
                                                                                        dt11_XFtg
                                                                                 of wild14_00 {
                                                                                 };
                                                                               1# ->
                                                                                 case readFloatArray#
                                                                                        dt14_sQ27
                                                                                        (+#
                                                                                           dt12_sQ25
                                                                                           o_sHI5)
                                                                                        ipv6_XFqo
                                                                                 of
                                                                                 { (# ipv8_XFr3,
                                                                                      ipv9_XFr5 #) ->
                                                                                 case readFloatArray#
                                                                                        dt14_sQ27
                                                                                        (+#
                                                                                           1#
                                                                                           (+#
                                                                                              dt12_sQ25
                                                                                              o_sHI5))
                                                                                        ipv8_XFr3
                                                                                 of
                                                                                 { (# ipv10_XFrc,
                                                                                      ipv11_XFre #) ->
                                                                                 case writeFloatArray#
                                                                                        dt14_sQ27
                                                                                        (+#
                                                                                           dt12_sQ25
                                                                                           o_sHI5)
                                                                                        (plusFloat#
                                                                                           ipv9_XFr5
                                                                                           ipv5_aF08)
                                                                                        ipv10_XFrc
                                                                                 of s'#_aF0z
                                                                                 { __DEFAULT ->
                                                                                 case writeFloatArray#
                                                                                        dt14_sQ27
                                                                                        (+#
                                                                                           1#
                                                                                           (+#
                                                                                              dt12_sQ25
                                                                                              o_sHI5))
                                                                                        (plusFloat#
                                                                                           ipv11_XFre
                                                                                           ipv7_XFqq)
                                                                                        s'#_aF0z
                                                                                 of s'#1_XFKa
                                                                                 { __DEFAULT ->
                                                                                 jump $wgo1_sOJr
                                                                                   (+# wild6_Xfw 1#)
                                                                                   (s'#1_XFKa
                                                                                    `cast` <Co:3>)
                                                                                 }
                                                                                 }
                                                                                 }
                                                                                 }
                                                                             }
                                                                             }
                                                                             }
                                                                             }
                                                                             }
                                                                         }
                                                                         }
                                                                         }
                                                                         }
                                                                         }
                                                                     };
                                                                   0## ->
                                                                     jump $wgo1_sOJr
                                                                       (+# wild6_Xfw 1#) w4_sOJm
                                                                 };
                                                               1# ->
                                                                 jump $wgo1_sOJr
                                                                   (+# wild6_Xfw 1#) w4_sOJm
                                                             }
                                                         };
                                                       64# -> jump exit3_X7S w4_sOJm
                                                     }; } in
                                               jump $wgo1_sOJr 0# (ipv1_af2z `cast` <Co:3>)
                                               }
                                           }
                                           }
                                       };
                                     16# -> jump exit1_X7A w3_sOJt
                                   }; } in
                             jump $wgo_sOJy 0# w2_sOJB
                             }
                             };
                           1# -> (# w2_sOJB, () #)
                         }
                         }; } in
                   jump $wconsume_loop_sOJG SPEC 0# s2_aevc
                   }
                   })
                `cast` <Co:4>
                }))
            `cast` <Co:4>)
             s1_a882
      of
      { (# ipv_a885, ipv1_a886 #) ->
      (# ipv_a885, () #)
      }

-- RHS size: {terms: 1, types: 0, coercions: 10, joins: 0/0}
step = someFunc10 `cast` <Co:10>

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$trModule4 = "ecs-0.1.0.0-6T7cQlWNmHv6W00SuOHZxa"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$trModule3 = TrNameS $trModule4

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$trModule2 = "Lib"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$trModule1 = TrNameS $trModule2

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$trModule = Module $trModule3 $trModule1

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
$krep_rQMb = KindRepTyConApp $tcFloat []

-- RHS size: {terms: 3, types: 2, coercions: 0, joins: 0/0}
$krep1_rQMc = : $krep_rQMb []

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep2_rQMd = KindRepTyConApp $tcV2 $krep1_rQMc

-- RHS size: {terms: 3, types: 2, coercions: 0, joins: 0/0}
$krep3_rQMe = : $krep2_rQMd []

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep4_rQMf = KindRepTyConApp $tcVector $krep3_rQMe

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$krep5_rQMg = KindRepVar 0#

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
$krep6_rQMh = : $krep5_rQMg $krep3_rQMe

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep7_rQMi = KindRepTyConApp $tcMVector $krep6_rQMh

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tcECSPos2 = "ECSPos"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tcECSPos1 = TrNameS $tcECSPos2

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tcECSPos
  = TyCon
      16457145057689062949##
      15936014833676134721##
      $trModule
      $tcECSPos1
      0#
      krep$*

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
$krep8_rQMj = KindRepTyConApp $tcECSPos []

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tc'ECSPos1 = KindRepFun $krep2_rQMd $krep8_rQMj

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tc'ECSPos3 = "'ECSPos"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tc'ECSPos2 = TrNameS $tc'ECSPos3

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tc'ECSPos
  = TyCon
      8452868620195556118##
      16929449198308252610##
      $trModule
      $tc'ECSPos2
      0#
      $tc'ECSPos1

-- RHS size: {terms: 3, types: 2, coercions: 0, joins: 0/0}
$krep9_rQMk = : $krep8_rQMj []

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
$krep10_rQMl = : $krep5_rQMg $krep9_rQMk

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep11_rQMm = KindRepTyConApp $tcMVector $krep10_rQMl

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tc'MV_ECSPos1 = KindRepFun $krep7_rQMi $krep11_rQMm

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep12_rQMn = KindRepTyConApp $tcVector $krep9_rQMk

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tc'V_ECSPos1 = KindRepFun $krep4_rQMf $krep12_rQMn

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tc'MV_ECSPos3 = "'MV_ECSPos"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tc'MV_ECSPos2 = TrNameS $tc'MV_ECSPos3

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tc'MV_ECSPos
  = TyCon
      14035350373738697834##
      2082941156530864794##
      $trModule
      $tc'MV_ECSPos2
      1#
      $tc'MV_ECSPos1

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tc'V_ECSPos3 = "'V_ECSPos"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tc'V_ECSPos2 = TrNameS $tc'V_ECSPos3

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tc'V_ECSPos
  = TyCon
      9783079786152817458##
      6355077876381602632##
      $trModule
      $tc'V_ECSPos2
      0#
      $tc'V_ECSPos1

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tcECSVel2 = "ECSVel"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tcECSVel1 = TrNameS $tcECSVel2

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tcECSVel
  = TyCon
      655903316460265017##
      2029539149141396196##
      $trModule
      $tcECSVel1
      0#
      krep$*

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
$krep13_rQMo = KindRepTyConApp $tcECSVel []

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tc'ECSVel1 = KindRepFun $krep2_rQMd $krep13_rQMo

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tc'ECSVel3 = "'ECSVel"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tc'ECSVel2 = TrNameS $tc'ECSVel3

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tc'ECSVel
  = TyCon
      10824594564663987524##
      18052045322075319433##
      $trModule
      $tc'ECSVel2
      0#
      $tc'ECSVel1

-- RHS size: {terms: 3, types: 2, coercions: 0, joins: 0/0}
$krep14_rQMp = : $krep13_rQMo []

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
$krep15_rQMq = : $krep5_rQMg $krep14_rQMp

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep16_rQMr = KindRepTyConApp $tcMVector $krep15_rQMq

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tc'MV_ECSVel1 = KindRepFun $krep7_rQMi $krep16_rQMr

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$krep17_rQMs = KindRepTyConApp $tcVector $krep14_rQMp

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tc'V_ECSVel1 = KindRepFun $krep4_rQMf $krep17_rQMs

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tc'MV_ECSVel3 = "'MV_ECSVel"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tc'MV_ECSVel2 = TrNameS $tc'MV_ECSVel3

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tc'MV_ECSVel
  = TyCon
      16482556661828382751##
      1874975540904106396##
      $trModule
      $tc'MV_ECSVel2
      1#
      $tc'MV_ECSVel1

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tc'V_ECSVel3 = "'V_ECSVel"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tc'V_ECSVel2 = TrNameS $tc'V_ECSVel3

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tc'V_ECSVel
  = TyCon
      5163686559179571724##
      7160864275974415888##
      $trModule
      $tc'V_ECSVel2
      0#
      $tc'V_ECSVel1

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tcDelta2 = "Delta"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tcDelta1 = TrNameS $tcDelta2

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tcDelta
  = TyCon
      17005596227093214100##
      12960078336930261669##
      $trModule
      $tcDelta1
      0#
      krep$*

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
$krep18_rQMt = KindRepTyConApp $tcDelta []

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$tc'Delta1 = KindRepFun $krep_rQMb $krep18_rQMt

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tc'Delta3 = "'Delta"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tc'Delta2 = TrNameS $tc'Delta3

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tc'Delta
  = TyCon
      15444883978432558688##
      5455717310259969007##
      $trModule
      $tc'Delta2
      0#
      $tc'Delta1

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
someFunc24 = D# 10.0##

-- RHS size: {terms: 12, types: 9, coercions: 3, joins: 0/0}
someFunc23
  = Config
      (defaultConfig5 `cast` <Co:3>)
      someFunc24
      defaultConfig3
      []
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Normal
      defaultConfig1

-- RHS size: {terms: 8, types: 17, coercions: 0, joins: 0/0}
someFunc9
  = \ s1_a882 ->
      case initWorld1 s1_a882 of { (# ipv_a885, ipv1_a886 #) ->
      someFunc10 ipv1_a886 ipv_a885
      }

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
someFunc22 = "pos_vel"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
someFunc21 = unpackCString# someFunc22

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
someFunc20 = "init"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
someFunc19 = unpackCString# someFunc20

-- RHS size: {terms: 7, types: 7, coercions: 6, joins: 0/0}
someFunc18
  = \ _ eta_aESr eta1_aESs ->
      ((whnfIO' (initWorld1 `cast` <Co:4>) eta_aESr) `cast` <Co:2>)
        eta1_aESs

-- RHS size: {terms: 6, types: 1, coercions: 22, joins: 0/0}
someFunc17
  = Benchmarkable
      (nf3 `cast` <Co:3>)
      (nf2 `cast` <Co:5>)
      (nf1 `cast` <Co:7>)
      (someFunc18 `cast` <Co:7>)
      False

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
someFunc16 = Benchmark someFunc19 someFunc17

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
someFunc15 = "step"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
someFunc14 = unpackCString# someFunc15

-- RHS size: {terms: 7, types: 5, coercions: 5, joins: 0/0}
someFunc8
  = \ _ eta_aESr eta1_aESs ->
      ((whnfIO' (someFunc9 `cast` <Co:3>) eta_aESr) `cast` <Co:2>)
        eta1_aESs

-- RHS size: {terms: 6, types: 1, coercions: 22, joins: 0/0}
someFunc7
  = Benchmarkable
      (nf3 `cast` <Co:3>)
      (nf2 `cast` <Co:5>)
      (nf1 `cast` <Co:7>)
      (someFunc8 `cast` <Co:7>)
      False

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
someFunc6 = Benchmark someFunc14 someFunc7

-- RHS size: {terms: 3, types: 2, coercions: 0, joins: 0/0}
someFunc5 = : someFunc6 []

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
someFunc4 = : someFunc16 someFunc5

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
someFunc3 = BenchGroup someFunc21 someFunc4

-- RHS size: {terms: 3, types: 2, coercions: 0, joins: 0/0}
someFunc2 = : someFunc3 []

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
someFunc1 = defaultMain2 someFunc23 someFunc2

-- RHS size: {terms: 1, types: 0, coercions: 3, joins: 0/0}
someFunc = someFunc1 `cast` <Co:3>



Preprocessing executable 'ecs-exe' for ecs-0.1.0.0..
Building executable 'ecs-exe' for ecs-0.1.0.0..
[1 of 2] Compiling Main

==================== Tidy Core ====================
Result size of Tidy Core
  = {terms: 21, types: 20, coercions: 9, joins: 0/0}

-- RHS size: {terms: 1, types: 0, coercions: 3, joins: 0/0}
main = someFunc1 `cast` <Co:3>

-- RHS size: {terms: 2, types: 1, coercions: 3, joins: 0/0}
main1 = runMainIO1 (someFunc1 `cast` <Co:3>)

-- RHS size: {terms: 1, types: 0, coercions: 3, joins: 0/0}
main = main1 `cast` <Co:3>

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$trModule4 = "main"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$trModule3 = TrNameS $trModule4

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$trModule2 = "Main"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$trModule1 = TrNameS $trModule2

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$trModule = Module $trModule3 $trModule1



[2 of 2] Compiling Paths_ecs

==================== Tidy Core ====================
Result size of Tidy Core
  = {terms: 444, types: 938, coercions: 95, joins: 0/0}

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
version5 = I# 0#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
version6 = I# 1#

-- RHS size: {terms: 3, types: 2, coercions: 0, joins: 0/0}
version4 = : version5 []

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
version3 = : version5 version4

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
version2 = : version6 version3

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
version1 = : version5 version2

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
version = Version version1 []

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$trModule4 = "main"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$trModule3 = TrNameS $trModule4

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$trModule2 = "Paths_ecs"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$trModule1 = TrNameS $trModule2

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$trModule = Module $trModule3 $trModule1

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
getBinDir7 = "ecs_bindir"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
getBinDir6 = unpackCString# getBinDir7

-- RHS size: {terms: 23, types: 40, coercions: 8, joins: 0/0}
getBinDir5
  = \ s_a82U ->
      case ((noinline getForeignEncoding) `cast` <Co:2>) s_a82U of
      { (# ipv_a8aT, ipv1_a8aU #) ->
      case charIsRepresentable3
             ipv1_a8aU getBinDir6 (getEnv3 `cast` <Co:6>) ipv_a8aT
      of
      { (# ipv2_a8d1, ipv3_a8d2 #) ->
      case ipv3_a8d2 of {
        Nothing -> getEnv2 getBinDir6 ipv2_a8d1;
        Just x_a8d8 -> (# ipv2_a8d1, x_a8d8 #)
      }
      }
      }

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
getBinDir4
  = "/home/matthias/projects/haskell/ecs/.stack-work/install/x86_64-linux/e1eb528d6f8833bd973f1c3cf17f694ca918906ed63718d4a96c0cee13d0c40a/8.8.2/bin"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
getBinDir3 = unpackCString# getBinDir4

-- RHS size: {terms: 21, types: 63, coercions: 4, joins: 0/0}
getBinDir2
  = \ e1_a7Jx eta_B1 ->
      case e1_a7Jx of wild_a7XO
      { SomeException @ e2_a82i $dException1_a82j e3_a82k ->
      case eqTypeRep
             (($p1Exception $dException1_a82j) `cast` <Co:4>)
             $fExceptionIOException4
      of {
        Nothing -> raiseIO# wild_a7XO eta_B1;
        Just ds1_a82v ->
          case ds1_a82v of { HRefl co_a82A co1_a82B ->
          (# eta_B1, getBinDir3 #)
          }
      }
      }

-- RHS size: {terms: 5, types: 4, coercions: 0, joins: 0/0}
getBinDir1 = \ eta_X7JV -> catch# getBinDir5 getBinDir2 eta_X7JV

-- RHS size: {terms: 1, types: 0, coercions: 3, joins: 0/0}
getBinDir = getBinDir1 `cast` <Co:3>

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
getLibDir7 = "ecs_libdir"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
getLibDir6 = unpackCString# getLibDir7

-- RHS size: {terms: 23, types: 40, coercions: 8, joins: 0/0}
getLibDir5
  = \ s_a82U ->
      case ((noinline getForeignEncoding) `cast` <Co:2>) s_a82U of
      { (# ipv_a8aT, ipv1_a8aU #) ->
      case charIsRepresentable3
             ipv1_a8aU getLibDir6 (getEnv3 `cast` <Co:6>) ipv_a8aT
      of
      { (# ipv2_a8d1, ipv3_a8d2 #) ->
      case ipv3_a8d2 of {
        Nothing -> getEnv2 getLibDir6 ipv2_a8d1;
        Just x_a8d8 -> (# ipv2_a8d1, x_a8d8 #)
      }
      }
      }

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
getLibDir4
  = "/home/matthias/projects/haskell/ecs/.stack-work/install/x86_64-linux/e1eb528d6f8833bd973f1c3cf17f694ca918906ed63718d4a96c0cee13d0c40a/8.8.2/lib/x86_64-linux-ghc-8.8.2/ecs-0.1.0.0-EZIjqhxvpEd7MblyMGvMqC-ecs-exe"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
getLibDir3 = unpackCString# getLibDir4

-- RHS size: {terms: 21, types: 63, coercions: 4, joins: 0/0}
getLibDir2
  = \ e1_a7Jx eta_B1 ->
      case e1_a7Jx of wild_a7XO
      { SomeException @ e2_a82i $dException1_a82j e3_a82k ->
      case eqTypeRep
             (($p1Exception $dException1_a82j) `cast` <Co:4>)
             $fExceptionIOException4
      of {
        Nothing -> raiseIO# wild_a7XO eta_B1;
        Just ds1_a82v ->
          case ds1_a82v of { HRefl co_a82A co1_a82B ->
          (# eta_B1, getLibDir3 #)
          }
      }
      }

-- RHS size: {terms: 5, types: 4, coercions: 0, joins: 0/0}
getLibDir1 = \ eta_X7Ku -> catch# getLibDir5 getLibDir2 eta_X7Ku

-- RHS size: {terms: 1, types: 0, coercions: 3, joins: 0/0}
getLibDir = getLibDir1 `cast` <Co:3>

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
getDynLibDir7 = "ecs_dynlibdir"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
getDynLibDir6 = unpackCString# getDynLibDir7

-- RHS size: {terms: 23, types: 40, coercions: 8, joins: 0/0}
getDynLibDir5
  = \ s_a82U ->
      case ((noinline getForeignEncoding) `cast` <Co:2>) s_a82U of
      { (# ipv_a8aT, ipv1_a8aU #) ->
      case charIsRepresentable3
             ipv1_a8aU getDynLibDir6 (getEnv3 `cast` <Co:6>) ipv_a8aT
      of
      { (# ipv2_a8d1, ipv3_a8d2 #) ->
      case ipv3_a8d2 of {
        Nothing -> getEnv2 getDynLibDir6 ipv2_a8d1;
        Just x_a8d8 -> (# ipv2_a8d1, x_a8d8 #)
      }
      }
      }

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
getDynLibDir4
  = "/home/matthias/projects/haskell/ecs/.stack-work/install/x86_64-linux/e1eb528d6f8833bd973f1c3cf17f694ca918906ed63718d4a96c0cee13d0c40a/8.8.2/lib/x86_64-linux-ghc-8.8.2"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
getDynLibDir3 = unpackCString# getDynLibDir4

-- RHS size: {terms: 21, types: 63, coercions: 4, joins: 0/0}
getDynLibDir2
  = \ e1_a7Jx eta_B1 ->
      case e1_a7Jx of wild_a7XO
      { SomeException @ e2_a82i $dException1_a82j e3_a82k ->
      case eqTypeRep
             (($p1Exception $dException1_a82j) `cast` <Co:4>)
             $fExceptionIOException4
      of {
        Nothing -> raiseIO# wild_a7XO eta_B1;
        Just ds1_a82v ->
          case ds1_a82v of { HRefl co_a82A co1_a82B ->
          (# eta_B1, getDynLibDir3 #)
          }
      }
      }

-- RHS size: {terms: 5, types: 4, coercions: 0, joins: 0/0}
getDynLibDir1
  = \ eta_X7Kx -> catch# getDynLibDir5 getDynLibDir2 eta_X7Kx

-- RHS size: {terms: 1, types: 0, coercions: 3, joins: 0/0}
getDynLibDir = getDynLibDir1 `cast` <Co:3>

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
getDataDir7 = "ecs_datadir"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
getDataDir6 = unpackCString# getDataDir7

-- RHS size: {terms: 23, types: 40, coercions: 8, joins: 0/0}
getDataDir5
  = \ s_a82U ->
      case ((noinline getForeignEncoding) `cast` <Co:2>) s_a82U of
      { (# ipv_a8aT, ipv1_a8aU #) ->
      case charIsRepresentable3
             ipv1_a8aU getDataDir6 (getEnv3 `cast` <Co:6>) ipv_a8aT
      of
      { (# ipv2_a8d1, ipv3_a8d2 #) ->
      case ipv3_a8d2 of {
        Nothing -> getEnv2 getDataDir6 ipv2_a8d1;
        Just x_a8d8 -> (# ipv2_a8d1, x_a8d8 #)
      }
      }
      }

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
getDataDir4
  = "/home/matthias/projects/haskell/ecs/.stack-work/install/x86_64-linux/e1eb528d6f8833bd973f1c3cf17f694ca918906ed63718d4a96c0cee13d0c40a/8.8.2/share/x86_64-linux-ghc-8.8.2/ecs-0.1.0.0"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
getDataDir3 = unpackCString# getDataDir4

-- RHS size: {terms: 21, types: 63, coercions: 4, joins: 0/0}
getDataDir2
  = \ e1_a7Jx eta_B1 ->
      case e1_a7Jx of wild_a7XO
      { SomeException @ e2_a82i $dException1_a82j e3_a82k ->
      case eqTypeRep
             (($p1Exception $dException1_a82j) `cast` <Co:4>)
             $fExceptionIOException4
      of {
        Nothing -> raiseIO# wild_a7XO eta_B1;
        Just ds1_a82v ->
          case ds1_a82v of { HRefl co_a82A co1_a82B ->
          (# eta_B1, getDataDir3 #)
          }
      }
      }

-- RHS size: {terms: 5, types: 4, coercions: 0, joins: 0/0}
getDataDir1 = \ eta_X7KA -> catch# getDataDir5 getDataDir2 eta_X7KA

-- RHS size: {terms: 1, types: 0, coercions: 3, joins: 0/0}
getDataDir = getDataDir1 `cast` <Co:3>

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
getDataFileName2 = "/"#

-- RHS size: {terms: 15, types: 25, coercions: 0, joins: 0/0}
getDataFileName1
  = \ name_a7FA s_a8dD ->
      case catch# getDataDir5 getDataDir2 s_a8dD of
      { (# ipv_a8dG, ipv1_a8dH #) ->
      (# ipv_a8dG,
         ++ ipv1_a8dH (unpackAppendCString# getDataFileName2 name_a7FA) #)
      }

-- RHS size: {terms: 1, types: 0, coercions: 5, joins: 0/0}
getDataFileName = getDataFileName1 `cast` <Co:5>

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
getLibexecDir7 = "ecs_libexecdir"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
getLibexecDir6 = unpackCString# getLibexecDir7

-- RHS size: {terms: 23, types: 40, coercions: 8, joins: 0/0}
getLibexecDir5
  = \ s_a82U ->
      case ((noinline getForeignEncoding) `cast` <Co:2>) s_a82U of
      { (# ipv_a8aT, ipv1_a8aU #) ->
      case charIsRepresentable3
             ipv1_a8aU getLibexecDir6 (getEnv3 `cast` <Co:6>) ipv_a8aT
      of
      { (# ipv2_a8d1, ipv3_a8d2 #) ->
      case ipv3_a8d2 of {
        Nothing -> getEnv2 getLibexecDir6 ipv2_a8d1;
        Just x_a8d8 -> (# ipv2_a8d1, x_a8d8 #)
      }
      }
      }

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
getLibexecDir4
  = "/home/matthias/projects/haskell/ecs/.stack-work/install/x86_64-linux/e1eb528d6f8833bd973f1c3cf17f694ca918906ed63718d4a96c0cee13d0c40a/8.8.2/libexec/x86_64-linux-ghc-8.8.2/ecs-0.1.0.0"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
getLibexecDir3 = unpackCString# getLibexecDir4

-- RHS size: {terms: 21, types: 63, coercions: 4, joins: 0/0}
getLibexecDir2
  = \ e1_a7Jx eta_B1 ->
      case e1_a7Jx of wild_a7XO
      { SomeException @ e2_a82i $dException1_a82j e3_a82k ->
      case eqTypeRep
             (($p1Exception $dException1_a82j) `cast` <Co:4>)
             $fExceptionIOException4
      of {
        Nothing -> raiseIO# wild_a7XO eta_B1;
        Just ds1_a82v ->
          case ds1_a82v of { HRefl co_a82A co1_a82B ->
          (# eta_B1, getLibexecDir3 #)
          }
      }
      }

-- RHS size: {terms: 5, types: 4, coercions: 0, joins: 0/0}
getLibexecDir1
  = \ eta_X7KG -> catch# getLibexecDir5 getLibexecDir2 eta_X7KG

-- RHS size: {terms: 1, types: 0, coercions: 3, joins: 0/0}
getLibexecDir = getLibexecDir1 `cast` <Co:3>

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
getSysconfDir7 = "ecs_sysconfdir"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
getSysconfDir6 = unpackCString# getSysconfDir7

-- RHS size: {terms: 23, types: 40, coercions: 8, joins: 0/0}
getSysconfDir5
  = \ s_a82U ->
      case ((noinline getForeignEncoding) `cast` <Co:2>) s_a82U of
      { (# ipv_a8aT, ipv1_a8aU #) ->
      case charIsRepresentable3
             ipv1_a8aU getSysconfDir6 (getEnv3 `cast` <Co:6>) ipv_a8aT
      of
      { (# ipv2_a8d1, ipv3_a8d2 #) ->
      case ipv3_a8d2 of {
        Nothing -> getEnv2 getSysconfDir6 ipv2_a8d1;
        Just x_a8d8 -> (# ipv2_a8d1, x_a8d8 #)
      }
      }
      }

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
getSysconfDir4
  = "/home/matthias/projects/haskell/ecs/.stack-work/install/x86_64-linux/e1eb528d6f8833bd973f1c3cf17f694ca918906ed63718d4a96c0cee13d0c40a/8.8.2/etc"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
getSysconfDir3 = unpackCString# getSysconfDir4

-- RHS size: {terms: 21, types: 63, coercions: 4, joins: 0/0}
getSysconfDir2
  = \ e1_a7Jx eta_B1 ->
      case e1_a7Jx of wild_a7XO
      { SomeException @ e2_a82i $dException1_a82j e3_a82k ->
      case eqTypeRep
             (($p1Exception $dException1_a82j) `cast` <Co:4>)
             $fExceptionIOException4
      of {
        Nothing -> raiseIO# wild_a7XO eta_B1;
        Just ds1_a82v ->
          case ds1_a82v of { HRefl co_a82A co1_a82B ->
          (# eta_B1, getSysconfDir3 #)
          }
      }
      }

-- RHS size: {terms: 5, types: 4, coercions: 0, joins: 0/0}
getSysconfDir1
  = \ eta_X7KJ -> catch# getSysconfDir5 getSysconfDir2 eta_X7KJ

-- RHS size: {terms: 1, types: 0, coercions: 3, joins: 0/0}
getSysconfDir = getSysconfDir1 `cast` <Co:3>



Linking .stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/ecs-exe/ecs-exe ...
ecs> copy/register
Installing library in /home/matthias/projects/haskell/ecs/.stack-work/install/x86_64-linux/e1eb528d6f8833bd973f1c3cf17f694ca918906ed63718d4a96c0cee13d0c40a/8.8.2/lib/x86_64-linux-ghc-8.8.2/ecs-0.1.0.0-6T7cQlWNmHv6W00SuOHZxa
Installing executable ecs-exe in /home/matthias/projects/haskell/ecs/.stack-work/install/x86_64-linux/e1eb528d6f8833bd973f1c3cf17f694ca918906ed63718d4a96c0cee13d0c40a/8.8.2/bin
Registering library for ecs-0.1.0.0..
