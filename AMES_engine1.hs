{-# LANGUAGE DeriveGeneric, OverloadedStrings, BangPatterns, ScopedTypeVariables #-}

import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Shape as Shape
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Compression.GZip as GZip
import Data.Array.Repa.Repr.ByteString
import Data.Array.Repa.IO.BMP
import Data.Word
import Data.Aeson hiding (Value)
import System.Random
import System.IO
import Data.Time
import System.Locale
import Text.Printf
import GHC.Real
import Data.Csv
import Control.Applicative
import GHC.Generics

import AlignmentUtil
import Alignment
import AlignmentSubstrate
import AlignmentDistribution
import AlignmentApprox
import AlignmentRandom
import AlignmentCoder
import AlignmentPracticable
import AlignmentPracticableIO
import AlignmentAeson
import AlignmentAesonPretty
import AlignmentRepaVShape
import AlignmentRepa
import AlignmentAesonRepa
import AlignmentRandomRepa
import AlignmentPracticableRepa
import AlignmentPracticableIORepa

newtype DoubleOrNull = DoubleOrNull Double deriving Show

instance FromField DoubleOrNull where
    parseField s = case runParser (parseField s :: Parser Double) of
        Left err -> pure $ DoubleOrNull (-1/0)
        Right n  -> pure $ DoubleOrNull n

toDouble (DoubleOrNull d) = d

newtype IntPosOrNull = IntPosOrNull Int deriving Show

instance FromField IntPosOrNull where
    parseField s = case runParser (parseField s :: Parser Int) of
        Left err -> pure $ IntPosOrNull (-1)
        Right n  -> pure $ IntPosOrNull (if n>= 0 then n else (-1))

toInt (IntPosOrNull i) = i

data Train = Train { 
  xID :: (),
  target :: !Int,
  v1 :: !DoubleOrNull,
  v2 :: !DoubleOrNull,
  v3 :: !String,
  v4 :: !DoubleOrNull,
  v5 :: !DoubleOrNull,
  v6 :: !DoubleOrNull,
  v7 :: !DoubleOrNull,
  v8 :: !DoubleOrNull,
  v9 :: !DoubleOrNull,
  v10 :: !DoubleOrNull,
  v11 :: !DoubleOrNull,
  v12 :: !DoubleOrNull,
  v13 :: !DoubleOrNull,
  v14 :: !DoubleOrNull,
  v15 :: !DoubleOrNull,
  v16 :: !DoubleOrNull,
  v17 :: !DoubleOrNull,
  v18 :: !DoubleOrNull,
  v19 :: !DoubleOrNull,
  v20 :: !DoubleOrNull,
  v21 :: !DoubleOrNull,
  v22 :: !String,
  v23 :: !DoubleOrNull,
  v24 :: !String,
  v25 :: !DoubleOrNull,
  v26 :: !DoubleOrNull,
  v27 :: !DoubleOrNull,
  v28 :: !DoubleOrNull,
  v29 :: !DoubleOrNull,
  v30 :: !String,
  v31 :: !String,
  v32 :: !DoubleOrNull,
  v33 :: !DoubleOrNull,
  v34 :: !DoubleOrNull,
  v35 :: !DoubleOrNull,
  v36 :: !DoubleOrNull,
  v37 :: !DoubleOrNull,
  v38 :: !Int,
  v39 :: !DoubleOrNull,
  v40 :: !DoubleOrNull,
  v41 :: !DoubleOrNull,
  v42 :: !DoubleOrNull,
  v43 :: !DoubleOrNull,
  v44 :: !DoubleOrNull,
  v45 :: !DoubleOrNull,
  v46 :: !DoubleOrNull,
  v47 :: !String,
  v48 :: !DoubleOrNull,
  v49 :: !DoubleOrNull,
  v50 :: !DoubleOrNull,
  v51 :: !DoubleOrNull,
  v52 :: !String,
  v53 :: !DoubleOrNull,
  v54 :: !DoubleOrNull,
  v55 :: !DoubleOrNull,
  v56 :: !String,
  v57 :: !DoubleOrNull,
  v58 :: !DoubleOrNull,
  v59 :: !DoubleOrNull,
  v60 :: !DoubleOrNull,
  v61 :: !DoubleOrNull,
  v62 :: !Int,
  v63 :: !DoubleOrNull,
  v64 :: !DoubleOrNull,
  v65 :: !DoubleOrNull,
  v66 :: !String,
  v67 :: !DoubleOrNull,
  v68 :: !DoubleOrNull,
  v69 :: !DoubleOrNull,
  v70 :: !DoubleOrNull,
  v71 :: !String,
  v72 :: !Int,
  v73 :: !DoubleOrNull,
  v74 :: !String,
  v75 :: !DoubleOrNull,
  v76 :: !DoubleOrNull,
  v77 :: !DoubleOrNull,
  v78 :: !DoubleOrNull,
  v79 :: !String,
  v80 :: !DoubleOrNull,
  v81 :: !DoubleOrNull,
  v82 :: !DoubleOrNull,
  v83 :: !DoubleOrNull,
  v84 :: !DoubleOrNull,
  v85 :: !DoubleOrNull,
  v86 :: !DoubleOrNull,
  v87 :: !DoubleOrNull,
  v88 :: !DoubleOrNull,
  v89 :: !DoubleOrNull,
  v90 :: !DoubleOrNull,
  v91 :: !String,
  v92 :: !DoubleOrNull,
  v93 :: !DoubleOrNull,
  v94 :: !DoubleOrNull,
  v95 :: !DoubleOrNull,
  v96 :: !DoubleOrNull,
  v97 :: !DoubleOrNull,
  v98 :: !DoubleOrNull,
  v99 :: !DoubleOrNull,
  v100 :: !DoubleOrNull,
  v101 :: !DoubleOrNull,
  v102 :: !DoubleOrNull,
  v103 :: !DoubleOrNull,
  v104 :: !DoubleOrNull,
  v105 :: !DoubleOrNull,
  v106 :: !DoubleOrNull,
  v107 :: !String,
  v108 :: !DoubleOrNull,
  v109 :: !DoubleOrNull,
  v110 :: !String,
  v111 :: !DoubleOrNull,
  v112 :: !String,
  v113 :: !String,
  v114 :: !DoubleOrNull,
  v115 :: !DoubleOrNull,
  v116 :: !DoubleOrNull,
  v117 :: !DoubleOrNull,
  v118 :: !DoubleOrNull,
  v119 :: !DoubleOrNull,
  v120 :: !DoubleOrNull,
  v121 :: !DoubleOrNull,
  v122 :: !DoubleOrNull,
  v123 :: !DoubleOrNull,
  v124 :: !DoubleOrNull,
  v125 :: !String,
  v126 :: !DoubleOrNull,
  v127 :: !DoubleOrNull,
  v128 :: !DoubleOrNull,
  v129 :: !Int,
  v130 :: !DoubleOrNull,
  v131 :: !DoubleOrNull} 
  deriving (Generic, Show)

instance FromRecord Train

fieldDoublesValues :: Int -> V.Vector a -> (a -> DoubleOrNull) -> UV.Vector Double
fieldDoublesValues valency vv v1 
    | valency>1 && e>0 = UV.fromList $ nub $ ii ++ map (\i -> vvt !! (i-1)) [e,e*2..e*(valency-1)] ++ [(1/0)]
    | valency>1 = UV.fromList $ ii ++ vvt
    | otherwise = UV.empty
    where
      vvs = sort $ V.toList $ V.map (toDouble . v1) vv
      vvt = dropWhile isInfinite vvs
      e = length vvt `div` valency
      ii = if head vvs == (-1/0) then [(-1/0)] else []

fieldIntPossValues :: V.Vector a -> (a -> IntPosOrNull) -> UV.Vector Int
fieldIntPossValues vv v1 = UV.fromList $ nub $ sort $ V.toList $ V.map (toInt . v1) vv

fieldIntsValues :: V.Vector a -> (a -> Int) -> UV.Vector Int
fieldIntsValues vv v1 = UV.fromList $ nub $ sort $ V.toList $ V.map v1 vv

fieldStringsValues :: V.Vector a -> (a -> String) -> V.Vector String
fieldStringsValues vv v1 = V.fromList $ sort $ ii ++ vvt
    where
      vvs = nub $ sort $ V.toList $ V.map v1 vv
      vvt = dropWhile (=="") vvs
      ii = if head vvs == "" then ["null"] else []   

findDouble :: UV.Vector Double -> Double -> Int
findDouble vv a = snd $ UV.ifoldl (\(a,p) i b -> if a>b then (a,i+1) else (a,p)) (a,0) vv

findInt :: UV.Vector Int -> Int -> Int
findInt vv a = snd $ UV.ifoldl (\(a,p) i b -> if a>b then (a,i+1) else (a,p)) (a,0) vv

findString :: V.Vector String -> String -> Int
findString (vv :: V.Vector String) a = snd $ V.ifoldl (\(a,p) i b -> if a>b then (a,i+1) else (a,p)) (a,0) vv

main :: IO ()
main = 
  do
    printf ">>>\n"
    hFlush stdout

    (uu,hh,hht) <- do
      csvData <- BL.readFile "BPCCM/train.csv"
      let vvcsv = either (\_ -> V.empty) id (Data.Csv.decode HasHeader csvData :: Either String (V.Vector Train))
      let z = V.length vvcsv
      let !vvtarget = fieldIntsValues vvcsv target
      let !vvv1 = fieldDoublesValues 8 vvcsv v1
      let !vvv2 = fieldDoublesValues 8 vvcsv v2
      let !vvv3 = fieldStringsValues vvcsv v3
      let !vvv4 = fieldDoublesValues 8 vvcsv v4
      let !vvv5 = fieldDoublesValues 8 vvcsv v5
      let !vvv6 = fieldDoublesValues 8 vvcsv v6
      let !vvv7 = fieldDoublesValues 8 vvcsv v7
      let !vvv8 = fieldDoublesValues 8 vvcsv v8
      let !vvv9 = fieldDoublesValues 8 vvcsv v9
      let !vvv10 = fieldDoublesValues 8 vvcsv v10
      let !vvv11 = fieldDoublesValues 8 vvcsv v11
      let !vvv12 = fieldDoublesValues 8 vvcsv v12
      let !vvv13 = fieldDoublesValues 8 vvcsv v13
      let !vvv14 = fieldDoublesValues 8 vvcsv v14
      let !vvv15 = fieldDoublesValues 8 vvcsv v15
      let !vvv16 = fieldDoublesValues 8 vvcsv v16
      let !vvv17 = fieldDoublesValues 8 vvcsv v17
      let !vvv18 = fieldDoublesValues 8 vvcsv v18
      let !vvv19 = fieldDoublesValues 8 vvcsv v19
      let !vvv20 = fieldDoublesValues 8 vvcsv v20
      let !vvv21 = fieldDoublesValues 8 vvcsv v21
--      let !vvv22 = fieldStringsValues vvcsv v22
      let !vvv23 = fieldDoublesValues 8 vvcsv v23
      let !vvv24 = fieldStringsValues vvcsv v24
      let !vvv25 = fieldDoublesValues 8 vvcsv v25
      let !vvv26 = fieldDoublesValues 8 vvcsv v26
      let !vvv27 = fieldDoublesValues 8 vvcsv v27
      let !vvv28 = fieldDoublesValues 8 vvcsv v28
      let !vvv29 = fieldDoublesValues 8 vvcsv v29
      let !vvv30 = fieldStringsValues vvcsv v30
      let !vvv31 = fieldStringsValues vvcsv v31
      let !vvv32 = fieldDoublesValues 8 vvcsv v32
      let !vvv33 = fieldDoublesValues 8 vvcsv v33
      let !vvv34 = fieldDoublesValues 8 vvcsv v34
      let !vvv35 = fieldDoublesValues 8 vvcsv v35
      let !vvv36 = fieldDoublesValues 8 vvcsv v36
      let !vvv37 = fieldDoublesValues 8 vvcsv v37
      let !vvv38 = fieldIntsValues vvcsv v38
      let !vvv39 = fieldDoublesValues 8 vvcsv v39
      let !vvv40 = fieldDoublesValues 8 vvcsv v40
      let !vvv41 = fieldDoublesValues 8 vvcsv v41
      let !vvv42 = fieldDoublesValues 8 vvcsv v42
      let !vvv43 = fieldDoublesValues 8 vvcsv v43
      let !vvv44 = fieldDoublesValues 8 vvcsv v44
      let !vvv45 = fieldDoublesValues 8 vvcsv v45
      let !vvv46 = fieldDoublesValues 8 vvcsv v46
      let !vvv47 = fieldStringsValues vvcsv v47
      let !vvv48 = fieldDoublesValues 8 vvcsv v48
      let !vvv49 = fieldDoublesValues 8 vvcsv v49
      let !vvv50 = fieldDoublesValues 8 vvcsv v50
      let !vvv51 = fieldDoublesValues 8 vvcsv v51
      let !vvv52 = fieldStringsValues vvcsv v52
      let !vvv53 = fieldDoublesValues 8 vvcsv v53
      let !vvv54 = fieldDoublesValues 8 vvcsv v54
      let !vvv55 = fieldDoublesValues 8 vvcsv v55
--      let !vvv56 = fieldStringsValues vvcsv v56
      let !vvv57 = fieldDoublesValues 8 vvcsv v57
      let !vvv58 = fieldDoublesValues 8 vvcsv v58
      let !vvv59 = fieldDoublesValues 8 vvcsv v59
      let !vvv60 = fieldDoublesValues 8 vvcsv v60
      let !vvv61 = fieldDoublesValues 8 vvcsv v61
      let !vvv62 = fieldIntsValues vvcsv v62
      let !vvv63 = fieldDoublesValues 8 vvcsv v63
      let !vvv64 = fieldDoublesValues 8 vvcsv v64
      let !vvv65 = fieldDoublesValues 8 vvcsv v65
      let !vvv66 = fieldStringsValues vvcsv v66
      let !vvv67 = fieldDoublesValues 8 vvcsv v67
      let !vvv68 = fieldDoublesValues 8 vvcsv v68
      let !vvv69 = fieldDoublesValues 8 vvcsv v69
      let !vvv70 = fieldDoublesValues 8 vvcsv v70
      let !vvv71 = fieldStringsValues vvcsv v71
      let !vvv72 = fieldIntsValues vvcsv v72
      let !vvv73 = fieldDoublesValues 8 vvcsv v73
      let !vvv74 = fieldStringsValues vvcsv v74
      let !vvv75 = fieldDoublesValues 8 vvcsv v75
      let !vvv76 = fieldDoublesValues 8 vvcsv v76
      let !vvv77 = fieldDoublesValues 8 vvcsv v77
      let !vvv78 = fieldDoublesValues 8 vvcsv v78
      let !vvv79 = fieldStringsValues vvcsv v79
      let !vvv80 = fieldDoublesValues 8 vvcsv v80
      let !vvv81 = fieldDoublesValues 8 vvcsv v81
      let !vvv82 = fieldDoublesValues 8 vvcsv v82
      let !vvv83 = fieldDoublesValues 8 vvcsv v83
      let !vvv84 = fieldDoublesValues 8 vvcsv v84
      let !vvv85 = fieldDoublesValues 8 vvcsv v85
      let !vvv86 = fieldDoublesValues 8 vvcsv v86
      let !vvv87 = fieldDoublesValues 8 vvcsv v87
      let !vvv88 = fieldDoublesValues 8 vvcsv v88
      let !vvv89 = fieldDoublesValues 8 vvcsv v89
      let !vvv90 = fieldDoublesValues 8 vvcsv v90
      let !vvv91 = fieldStringsValues vvcsv v91
      let !vvv92 = fieldDoublesValues 8 vvcsv v92
      let !vvv93 = fieldDoublesValues 8 vvcsv v93
      let !vvv94 = fieldDoublesValues 8 vvcsv v94
      let !vvv95 = fieldDoublesValues 8 vvcsv v95
      let !vvv96 = fieldDoublesValues 8 vvcsv v96
      let !vvv97 = fieldDoublesValues 8 vvcsv v97
      let !vvv98 = fieldDoublesValues 8 vvcsv v98
      let !vvv99 = fieldDoublesValues 8 vvcsv v99
      let !vvv100 = fieldDoublesValues 8 vvcsv v100
      let !vvv101 = fieldDoublesValues 8 vvcsv v101
      let !vvv102 = fieldDoublesValues 8 vvcsv v102
      let !vvv103 = fieldDoublesValues 8 vvcsv v103
      let !vvv104 = fieldDoublesValues 8 vvcsv v104
      let !vvv105 = fieldDoublesValues 8 vvcsv v105
      let !vvv106 = fieldDoublesValues 8 vvcsv v106
      let !vvv107 = fieldStringsValues vvcsv v107
      let !vvv108 = fieldDoublesValues 8 vvcsv v108
      let !vvv109 = fieldDoublesValues 8 vvcsv v109
      let !vvv110 = fieldStringsValues vvcsv v110
      let !vvv111 = fieldDoublesValues 8 vvcsv v111
      let !vvv112 = fieldStringsValues vvcsv v112
      let !vvv113 = fieldStringsValues vvcsv v113
      let !vvv114 = fieldDoublesValues 8 vvcsv v114
      let !vvv115 = fieldDoublesValues 8 vvcsv v115
      let !vvv116 = fieldDoublesValues 8 vvcsv v116
      let !vvv117 = fieldDoublesValues 8 vvcsv v117
      let !vvv118 = fieldDoublesValues 8 vvcsv v118
      let !vvv119 = fieldDoublesValues 8 vvcsv v119
      let !vvv120 = fieldDoublesValues 8 vvcsv v120
      let !vvv121 = fieldDoublesValues 8 vvcsv v121
      let !vvv122 = fieldDoublesValues 8 vvcsv v122
      let !vvv123 = fieldDoublesValues 8 vvcsv v123
      let !vvv124 = fieldDoublesValues 8 vvcsv v124
--      let !vvv125 = fieldStringsValues vvcsv v125
      let !vvv126 = fieldDoublesValues 8 vvcsv v126
      let !vvv127 = fieldDoublesValues 8 vvcsv v127
      let !vvv128 = fieldDoublesValues 8 vvcsv v128
      let !vvv129 = fieldIntsValues vvcsv v129
      let !vvv130 = fieldDoublesValues 8 vvcsv v130
      let !vvv131 = fieldDoublesValues 8 vvcsv v131
      let vm = V.fromList 
                 [
                   findInt vvtarget . target,
                   findDouble vvv1 . toDouble . v1,
                   findDouble vvv2 . toDouble . v2,
                   findString vvv3 . v3,
                   findDouble vvv4 . toDouble . v4,
                   findDouble vvv5 . toDouble . v5,
                   findDouble vvv6 . toDouble . v6,
                   findDouble vvv7 . toDouble . v7,
                   findDouble vvv8 . toDouble . v8,
                   findDouble vvv9 . toDouble . v9,
                   findDouble vvv10 . toDouble . v10,
                   findDouble vvv11 . toDouble . v11,
                   findDouble vvv12 . toDouble . v12,
                   findDouble vvv13 . toDouble . v13,
                   findDouble vvv14 . toDouble . v14,
                   findDouble vvv15 . toDouble . v15,
                   findDouble vvv16 . toDouble . v16,
                   findDouble vvv17 . toDouble . v17,
                   findDouble vvv18 . toDouble . v18,
                   findDouble vvv19 . toDouble . v19,
                   findDouble vvv20 . toDouble . v20,
                   findDouble vvv21 . toDouble . v21,
--                   findString vvv22 . v22,
                   findDouble vvv23 . toDouble . v23,
                   findString vvv24 . v24,
                   findDouble vvv25 . toDouble . v25,
                   findDouble vvv26 . toDouble . v26,
                   findDouble vvv27 . toDouble . v27,
                   findDouble vvv28 . toDouble . v28,
                   findDouble vvv29 . toDouble . v29,
                   findString vvv30 . v30,
                   findString vvv31 . v31,
                   findDouble vvv32 . toDouble . v32,
                   findDouble vvv33 . toDouble . v33,
                   findDouble vvv34 . toDouble . v34,
                   findDouble vvv35 . toDouble . v35,
                   findDouble vvv36 . toDouble . v36,
                   findDouble vvv37 . toDouble . v37,
                   findInt vvv38 . v38,
                   findDouble vvv39 . toDouble . v39,
                   findDouble vvv40 . toDouble . v40,
                   findDouble vvv41 . toDouble . v41,
                   findDouble vvv42 . toDouble . v42,
                   findDouble vvv43 . toDouble . v43,
                   findDouble vvv44 . toDouble . v44,
                   findDouble vvv45 . toDouble . v45,
                   findDouble vvv46 . toDouble . v46,
                   findString vvv47 . v47,
                   findDouble vvv48 . toDouble . v48,
                   findDouble vvv49 . toDouble . v49,
                   findDouble vvv50 . toDouble . v50,
                   findDouble vvv51 . toDouble . v51,
                   findString vvv52 . v52,
                   findDouble vvv53 . toDouble . v53,
                   findDouble vvv54 . toDouble . v54,
                   findDouble vvv55 . toDouble . v55,
 --                  findString vvv56 . v56,
                   findDouble vvv57 . toDouble . v57,
                   findDouble vvv58 . toDouble . v58,
                   findDouble vvv59 . toDouble . v59,
                   findDouble vvv60 . toDouble . v60,
                   findDouble vvv61 . toDouble . v61,
                   findInt vvv62 . v62,
                   findDouble vvv63 . toDouble . v63,
                   findDouble vvv64 . toDouble . v64,
                   findDouble vvv65 . toDouble . v65,
                   findString vvv66 . v66,
                   findDouble vvv67 . toDouble . v67,
                   findDouble vvv68 . toDouble . v68,
                   findDouble vvv69 . toDouble . v69,
                   findDouble vvv70 . toDouble . v70,
                   findString vvv71 . v71,
                   findInt vvv72 . v72,
                   findDouble vvv73 . toDouble . v73,
                   findString vvv74 . v74,
                   findDouble vvv75 . toDouble . v75,
                   findDouble vvv76 . toDouble . v76,
                   findDouble vvv77 . toDouble . v77,
                   findDouble vvv78 . toDouble . v78,
                   findString vvv79 . v79,
                   findDouble vvv80 . toDouble . v80,
                   findDouble vvv81 . toDouble . v81,
                   findDouble vvv82 . toDouble . v82,
                   findDouble vvv83 . toDouble . v83,
                   findDouble vvv84 . toDouble . v84,
                   findDouble vvv85 . toDouble . v85,
                   findDouble vvv86 . toDouble . v86,
                   findDouble vvv87 . toDouble . v87,
                   findDouble vvv88 . toDouble . v88,
                   findDouble vvv89 . toDouble . v89,
                   findDouble vvv90 . toDouble . v90,
                   findString vvv91 . v91,
                   findDouble vvv92 . toDouble . v92,
                   findDouble vvv93 . toDouble . v93,
                   findDouble vvv94 . toDouble . v94,
                   findDouble vvv95 . toDouble . v95,
                   findDouble vvv96 . toDouble . v96,
                   findDouble vvv97 . toDouble . v97,
                   findDouble vvv98 . toDouble . v98,
                   findDouble vvv99 . toDouble . v99,
                   findDouble vvv100 . toDouble . v100,
                   findDouble vvv101 . toDouble . v101,
                   findDouble vvv102 . toDouble . v102,
                   findDouble vvv103 . toDouble . v103,
                   findDouble vvv104 . toDouble . v104,
                   findDouble vvv105 . toDouble . v105,
                   findDouble vvv106 . toDouble . v106,
                   findString vvv107 . v107,
                   findDouble vvv108 . toDouble . v108,
                   findDouble vvv109 . toDouble . v109,
                   findString vvv110 . v110,
                   findDouble vvv111 . toDouble . v111,
                   findString vvv112 . v112,
                   findString vvv113 . v113,
                   findDouble vvv114 . toDouble . v114,
                   findDouble vvv115 . toDouble . v115,
                   findDouble vvv116 . toDouble . v116,
                   findDouble vvv117 . toDouble . v117,
                   findDouble vvv118 . toDouble . v118,
                   findDouble vvv119 . toDouble . v119,
                   findDouble vvv120 . toDouble . v120,
                   findDouble vvv121 . toDouble . v121,
                   findDouble vvv122 . toDouble . v122,
                   findDouble vvv123 . toDouble . v123,
                   findDouble vvv124 . toDouble . v124,
--                   findString vvv125 . v125,
                   findDouble vvv126 . toDouble . v126,
                   findDouble vvv127 . toDouble . v127,
                   findDouble vvv128 . toDouble . v128,
                   findInt vvv129 . v129,
                   findDouble vvv130 . toDouble . v130,
                   findDouble vvv131 . toDouble . v131
                 ]
      let n = V.length vm
      let h = R.computeS $ R.fromFunction ((R.Z R.:. z R.:. n) :: R.DIM2) (\(R.Z R.:. i R.:. j) -> (vm V.! j) (vvcsv V.! i)) :: R.Array R.U R.DIM2 Int
      let xx = [
                 (VarStr "target", Set.fromList (map (ValInt . toInteger) (UV.toList vvtarget))),
                 (VarStr "v1", Set.fromList (map ValDouble (UV.toList vvv1))),
                 (VarStr "v2", Set.fromList (map ValDouble (UV.toList vvv2))),
                 (VarStr "v3", Set.fromList (map ValStr (V.toList vvv3))),
                 (VarStr "v4", Set.fromList (map ValDouble (UV.toList vvv4))),
                 (VarStr "v5", Set.fromList (map ValDouble (UV.toList vvv5))),
                 (VarStr "v6", Set.fromList (map ValDouble (UV.toList vvv6))),
                 (VarStr "v7", Set.fromList (map ValDouble (UV.toList vvv7))),
                 (VarStr "v8", Set.fromList (map ValDouble (UV.toList vvv8))),
                 (VarStr "v9", Set.fromList (map ValDouble (UV.toList vvv9))),
                 (VarStr "v10", Set.fromList (map ValDouble (UV.toList vvv10))),
                 (VarStr "v11", Set.fromList (map ValDouble (UV.toList vvv11))),
                 (VarStr "v12", Set.fromList (map ValDouble (UV.toList vvv12))),
                 (VarStr "v13", Set.fromList (map ValDouble (UV.toList vvv13))),
                 (VarStr "v14", Set.fromList (map ValDouble (UV.toList vvv14))),
                 (VarStr "v15", Set.fromList (map ValDouble (UV.toList vvv15))),
                 (VarStr "v16", Set.fromList (map ValDouble (UV.toList vvv16))),
                 (VarStr "v17", Set.fromList (map ValDouble (UV.toList vvv17))),
                 (VarStr "v18", Set.fromList (map ValDouble (UV.toList vvv18))),
                 (VarStr "v19", Set.fromList (map ValDouble (UV.toList vvv19))),
                 (VarStr "v20", Set.fromList (map ValDouble (UV.toList vvv20))),
                 (VarStr "v21", Set.fromList (map ValDouble (UV.toList vvv21))),
--                 (VarStr "v22", Set.fromList (map ValStr (V.toList vvv22))),
                 (VarStr "v23", Set.fromList (map ValDouble (UV.toList vvv23))),
                 (VarStr "v24", Set.fromList (map ValStr (V.toList vvv24))),
                 (VarStr "v25", Set.fromList (map ValDouble (UV.toList vvv25))),
                 (VarStr "v26", Set.fromList (map ValDouble (UV.toList vvv26))),
                 (VarStr "v27", Set.fromList (map ValDouble (UV.toList vvv27))),
                 (VarStr "v28", Set.fromList (map ValDouble (UV.toList vvv28))),
                 (VarStr "v29", Set.fromList (map ValDouble (UV.toList vvv29))),
                 (VarStr "v30", Set.fromList (map ValStr (V.toList vvv30))),
                 (VarStr "v31", Set.fromList (map ValStr (V.toList vvv31))),
                 (VarStr "v32", Set.fromList (map ValDouble (UV.toList vvv32))),
                 (VarStr "v33", Set.fromList (map ValDouble (UV.toList vvv33))),
                 (VarStr "v34", Set.fromList (map ValDouble (UV.toList vvv34))),
                 (VarStr "v35", Set.fromList (map ValDouble (UV.toList vvv35))),
                 (VarStr "v36", Set.fromList (map ValDouble (UV.toList vvv36))),
                 (VarStr "v37", Set.fromList (map ValDouble (UV.toList vvv37))),
                 (VarStr "v38", Set.fromList (map (ValInt . toInteger) (UV.toList vvv38))),
                 (VarStr "v39", Set.fromList (map ValDouble (UV.toList vvv39))),
                 (VarStr "v40", Set.fromList (map ValDouble (UV.toList vvv40))),
                 (VarStr "v41", Set.fromList (map ValDouble (UV.toList vvv41))),
                 (VarStr "v42", Set.fromList (map ValDouble (UV.toList vvv42))),
                 (VarStr "v43", Set.fromList (map ValDouble (UV.toList vvv43))),
                 (VarStr "v44", Set.fromList (map ValDouble (UV.toList vvv44))),
                 (VarStr "v45", Set.fromList (map ValDouble (UV.toList vvv45))),
                 (VarStr "v46", Set.fromList (map ValDouble (UV.toList vvv46))),
                 (VarStr "v47", Set.fromList (map ValStr (V.toList vvv47))),
                 (VarStr "v48", Set.fromList (map ValDouble (UV.toList vvv48))),
                 (VarStr "v49", Set.fromList (map ValDouble (UV.toList vvv49))),
                 (VarStr "v50", Set.fromList (map ValDouble (UV.toList vvv50))),
                 (VarStr "v51", Set.fromList (map ValDouble (UV.toList vvv51))),
                 (VarStr "v52", Set.fromList (map ValStr (V.toList vvv52))),
                 (VarStr "v53", Set.fromList (map ValDouble (UV.toList vvv53))),
                 (VarStr "v54", Set.fromList (map ValDouble (UV.toList vvv54))),
                 (VarStr "v55", Set.fromList (map ValDouble (UV.toList vvv55))),
--                 (VarStr "v56", Set.fromList (map ValStr (V.toList vvv56))),
                 (VarStr "v57", Set.fromList (map ValDouble (UV.toList vvv57))),
                 (VarStr "v58", Set.fromList (map ValDouble (UV.toList vvv58))),
                 (VarStr "v59", Set.fromList (map ValDouble (UV.toList vvv59))),
                 (VarStr "v60", Set.fromList (map ValDouble (UV.toList vvv60))),
                 (VarStr "v61", Set.fromList (map ValDouble (UV.toList vvv61))),
                 (VarStr "v62", Set.fromList (map (ValInt . toInteger) (UV.toList vvv62))),
                 (VarStr "v63", Set.fromList (map ValDouble (UV.toList vvv63))),
                 (VarStr "v64", Set.fromList (map ValDouble (UV.toList vvv64))),
                 (VarStr "v65", Set.fromList (map ValDouble (UV.toList vvv65))),
                 (VarStr "v66", Set.fromList (map ValStr (V.toList vvv66))),
                 (VarStr "v67", Set.fromList (map ValDouble (UV.toList vvv67))),
                 (VarStr "v68", Set.fromList (map ValDouble (UV.toList vvv68))),
                 (VarStr "v69", Set.fromList (map ValDouble (UV.toList vvv69))),
                 (VarStr "v70", Set.fromList (map ValDouble (UV.toList vvv70))),
                 (VarStr "v71", Set.fromList (map ValStr (V.toList vvv71))),
                 (VarStr "v72", Set.fromList (map (ValInt . toInteger) (UV.toList vvv72))),
                 (VarStr "v73", Set.fromList (map ValDouble (UV.toList vvv73))),
                 (VarStr "v74", Set.fromList (map ValStr (V.toList vvv74))),
                 (VarStr "v75", Set.fromList (map ValDouble (UV.toList vvv75))),
                 (VarStr "v76", Set.fromList (map ValDouble (UV.toList vvv76))),
                 (VarStr "v77", Set.fromList (map ValDouble (UV.toList vvv77))),
                 (VarStr "v78", Set.fromList (map ValDouble (UV.toList vvv78))),
                 (VarStr "v79", Set.fromList (map ValStr (V.toList vvv79))),
                 (VarStr "v80", Set.fromList (map ValDouble (UV.toList vvv80))),
                 (VarStr "v81", Set.fromList (map ValDouble (UV.toList vvv81))),
                 (VarStr "v82", Set.fromList (map ValDouble (UV.toList vvv82))),
                 (VarStr "v83", Set.fromList (map ValDouble (UV.toList vvv83))),
                 (VarStr "v84", Set.fromList (map ValDouble (UV.toList vvv84))),
                 (VarStr "v85", Set.fromList (map ValDouble (UV.toList vvv85))),
                 (VarStr "v86", Set.fromList (map ValDouble (UV.toList vvv86))),
                 (VarStr "v87", Set.fromList (map ValDouble (UV.toList vvv87))),
                 (VarStr "v88", Set.fromList (map ValDouble (UV.toList vvv88))),
                 (VarStr "v89", Set.fromList (map ValDouble (UV.toList vvv89))),
                 (VarStr "v90", Set.fromList (map ValDouble (UV.toList vvv90))),
                 (VarStr "v91", Set.fromList (map ValStr (V.toList vvv91))),
                 (VarStr "v92", Set.fromList (map ValDouble (UV.toList vvv92))),
                 (VarStr "v93", Set.fromList (map ValDouble (UV.toList vvv93))),
                 (VarStr "v94", Set.fromList (map ValDouble (UV.toList vvv94))),
                 (VarStr "v95", Set.fromList (map ValDouble (UV.toList vvv95))),
                 (VarStr "v96", Set.fromList (map ValDouble (UV.toList vvv96))),
                 (VarStr "v97", Set.fromList (map ValDouble (UV.toList vvv97))),
                 (VarStr "v98", Set.fromList (map ValDouble (UV.toList vvv98))),
                 (VarStr "v99", Set.fromList (map ValDouble (UV.toList vvv99))),
                 (VarStr "v100", Set.fromList (map ValDouble (UV.toList vvv100))),
                 (VarStr "v101", Set.fromList (map ValDouble (UV.toList vvv101))),
                 (VarStr "v102", Set.fromList (map ValDouble (UV.toList vvv102))),
                 (VarStr "v103", Set.fromList (map ValDouble (UV.toList vvv103))),
                 (VarStr "v104", Set.fromList (map ValDouble (UV.toList vvv104))),
                 (VarStr "v105", Set.fromList (map ValDouble (UV.toList vvv105))),
                 (VarStr "v106", Set.fromList (map ValDouble (UV.toList vvv106))),
                 (VarStr "v107", Set.fromList (map ValStr (V.toList vvv107))),
                 (VarStr "v108", Set.fromList (map ValDouble (UV.toList vvv108))),
                 (VarStr "v109", Set.fromList (map ValDouble (UV.toList vvv109))),
                 (VarStr "v110", Set.fromList (map ValStr (V.toList vvv110))),
                 (VarStr "v111", Set.fromList (map ValDouble (UV.toList vvv111))),
                 (VarStr "v112", Set.fromList (map ValStr (V.toList vvv112))),
                 (VarStr "v113", Set.fromList (map ValStr (V.toList vvv113))),
                 (VarStr "v114", Set.fromList (map ValDouble (UV.toList vvv114))),
                 (VarStr "v115", Set.fromList (map ValDouble (UV.toList vvv115))),
                 (VarStr "v116", Set.fromList (map ValDouble (UV.toList vvv116))),
                 (VarStr "v117", Set.fromList (map ValDouble (UV.toList vvv117))),
                 (VarStr "v118", Set.fromList (map ValDouble (UV.toList vvv118))),
                 (VarStr "v119", Set.fromList (map ValDouble (UV.toList vvv119))),
                 (VarStr "v120", Set.fromList (map ValDouble (UV.toList vvv120))),
                 (VarStr "v121", Set.fromList (map ValDouble (UV.toList vvv121))),
                 (VarStr "v122", Set.fromList (map ValDouble (UV.toList vvv122))),
                 (VarStr "v123", Set.fromList (map ValDouble (UV.toList vvv123))),
                 (VarStr "v124", Set.fromList (map ValDouble (UV.toList vvv124))),
--                 (VarStr "v125", Set.fromList (map ValStr (V.toList vvv125))),
                 (VarStr "v126", Set.fromList (map ValDouble (UV.toList vvv126))),
                 (VarStr "v127", Set.fromList (map ValDouble (UV.toList vvv127))),
                 (VarStr "v128", Set.fromList (map ValDouble (UV.toList vvv128))),
                 (VarStr "v129", Set.fromList (map (ValInt . toInteger) (UV.toList vvv129))),
                 (VarStr "v130", Set.fromList (map ValDouble (UV.toList vvv130))),
                 (VarStr "v131", Set.fromList (map ValDouble (UV.toList vvv131)))
               ]
      let uu = fromJust $ systemFromList xx
      let vv = fst $ unzip xx
      let svv = (UV.fromList (map (val uu) vv)) :: VShape
      let hr = HistoryRepa (V.fromListN n vv) (Map.fromList (zip vv [0..])) svv $ R.computeS $ R.transpose h
      let xx = snd $ unzip $ sort $ zip (randoms (mkStdGen 100) :: [Double]) [0..z-1]
      return (uu, hrev (take 60000 (drop 1000 xx)) hr, hrev (take 1000 xx) hr)

    let vv = uvars uu
    let vvl = Set.singleton (VarStr "target")
    let vvk' = vv `Set.difference` vvl

    let vvk = Set.fromList $ snd $ unzip $ take 15 $ reverse $ sort [(algn aa, v) | v <- Set.toList vvk', let aa = araa uu (hh `hrred` Set.insert v vvl)]

    printf "BPCCM train size: %d\n" $ historyRepasSize hh
    hFlush stdout

    let aa = araa uu $ hh `hrred` vvl
    printf "history label entropy: %.2f\n" $ fromRational (size aa) * histogramsEntropy aa

    let model = "BPCCM_model10"
    let (wmax,lmax,xmax,omax,bmax,mmax,umax,pmax,fmax,mult,seed) = ((9^3), 8, (9^3), 10, (10*3), 3, (9^3), 1, 5, 1, 1)

    printf ">>> %s\n" $ model
    Just (uu',df) <- decomperIO uu vvk hh vvl Set.empty wmax lmax xmax omax bmax mmax umax pmax fmax mult seed
    BL.writeFile (model ++ ".json") $ decompFudsPersistentsEncode $ decompFudsPersistent df
    printf "<<< done %s\n" $ model
    hFlush stdout

    printf "decomp size: %d\n" $ (Set.size $ fvars $ dfff df)

    printf "label entropy: %.2f\n" $ sum $ map (\aa -> fromRational (size aa) * histogramsEntropy aa) $ systemsDecompFudsHistoryRepasSetVariablesListHistogramLeaf uu' df hh vvl

    let (a,ad) = systemsDecompFudsHistoryRepasAlignmentContentShuffleSummation_u mult seed uu' df hh
    printf "alignment: %.2f\n" $ a
    printf "alignment density: %.2f\n" $ ad

    let (_,(_,(a,ad))) = Set.findMin $ treesRoots $ systemsDecompFudsHistoryRepasTreeAlignmentContentShuffleSummation_u mult seed uu' df hh
    printf "root alignment: %.2f\n" $ a
    printf "root alignment density: %.2f\n" $ ad

    printf "BPCCM test size: %d\n" $ historyRepasSize hht
    hFlush stdout

    let rr = systemsDecompFudsHistoryRepasHistoryRepasSetVariablesTest uu' df hh hht vvk'
    printf "test matches: %d\n" $ length $ filter (\(l,(aa,_)) -> l==aamax aa) rr
    printf "ineffective: %d\n" $ sum $ map (\(l,(_,(b,_))) -> (if not b then 1 else 0) :: Int) rr
    rpln $ Map.toList $ Map.map (\(a,b) -> (((a,b),(fromIntegral a/fromIntegral b) :: Double))) $ Map.fromListWith (\(a,b) (c,d) -> (a+c,b+d)) $ map (\(l,(aa,_)) -> (l, (if l==aamax aa then 1 else 0, 1) :: (Int,Int))) rr
    printf "log loss: %.2f\n" $ sum $ [-(log (prob l aa)) | (l,(aa,_)) <- rr]
    printf "log loss2: %.2f\n" $ sum $ [-(log (prob2 l aa)) | (l,(aa,_)) <- rr]

    printf "<<< done\n"
  where 
    decomperIO uu vv hh ll lld wmax lmax xmax omax bmax mmax umax pmax fmax mult seed =
      parametersSystemsHistoryRepasDecomperMaximumRollExcludedSelfHighestFmaxLabelMinEntropyDeLabelIORepa
        wmax lmax xmax omax bmax mmax umax pmax fmax mult seed uu vv hh ll lld 
    aahr uu aa = hhhr uu $ aahh aa 
    aahh aa = fromJust $ histogramsHistory aa
    hhhr uu hh = fromJust $ systemsHistoriesHistoryRepa uu hh
    araa uu rr = fromJust $ systemsHistogramRepasHistogram uu rr
    hrred hh vv = setVarsHistoryRepasReduce 1 vv hh
    hrev = eventsHistoryRepasHistoryRepaSelection
    red hh vv = setVarsHistoryRepasHistoryRepaReduced vv hh
    dfff = decompFudsFud
    fvars = fudsVars
    aamax aa = if size aa > 0 then (snd $ last $ sort [(c,ss) | (ss,c) <- aall aa]) else stateEmpty
    amax aa = if size aa > 0 then (last $ sort $ snd $ unzip $ aall aa) else 0
    prob :: State -> Histogram -> Double
    prob l aa 
      | zza==0 = 1.0e-15
      | zll==0 = 1.0e-15
      | zll==zza = 1.0 - 1.0e-15
      | otherwise = fromRational (zll/zza)
      where
        zza = size aa
        zll = size (aa `mul` unit l)
    prob2 :: State -> Histogram -> Double
    prob2 l aa 
      | zza==0 = undefined
      | zll==0 = fromRational (1/(zza+1))
      | zll==zza = fromRational (zll/(zza+1))
      | otherwise = fromRational (zll/zza)
      where
        zza = size aa
        zll = size (aa `mul` unit l)
    algn = histogramsAlignment
    aall = histogramsList
    llaa ll = fromJust $ listsHistogram ll
    size = histogramsSize
    mul = pairHistogramsMultiply
    unit ss = fromJust $ setStatesHistogramUnit $ Set.singleton ss
    llss = listsState
    val uu v = Set.size $ fromJust $ systemsVarsValues uu v
    uvars = systemsVars
    sys = histogramsSystemImplied 
    rpln ll = mapM_ (print . represent) ll

