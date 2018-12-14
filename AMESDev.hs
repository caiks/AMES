{-# LANGUAGE DeriveGeneric, OverloadedStrings, BangPatterns, ScopedTypeVariables #-}

module AMESDev
where

import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.List hiding (union)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Shape as RS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BL8
import qualified Codec.Compression.GZip as GZip
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
import AlignmentApprox
import AlignmentRandom
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
import AlignmentDevRepa hiding (aahr)

newtype IntOrNull = IntOrNull Int deriving Show

instance FromField IntOrNull where
    parseField s = case runParser (parseField s :: Parser Int) of
        Left err -> pure $ IntOrNull (-1)
        Right n  -> pure $ IntOrNull (if n>= 0 then n else (-1))

toInt (IntOrNull i) = i

data Train = Train { 
  trId :: !IntOrNull,
  trMSSubClass :: !IntOrNull,
  trMSZoning :: !String,
  trLotFrontage :: !String,
  trLotArea :: !IntOrNull,
  trStreet :: !String,
  trAlley :: !String,
  trLotShape :: !String,
  trLandContour :: !String,
  trUtilities :: !String,
  trLotConfig :: !String,
  trLandSlope :: !String,
  trNeighborhood :: !String,
  trCondition1 :: !String,
  trCondition2 :: !String,
  trBldgType :: !String,
  trHouseStyle :: !String,
  trOverallQual :: !IntOrNull,
  trOverallCond :: !IntOrNull,
  trYearBuilt :: !IntOrNull,
  trYearRemodAdd :: !IntOrNull,
  trRoofStyle :: !String,
  trRoofMatl :: !String,
  trExterior1st :: !String,
  trExterior2nd :: !String,
  trMasVnrType :: !String,
  trMasVnrArea :: !String,
  trExterQual :: !String,
  trExterCond :: !String,
  trFoundation :: !String,
  trBsmtQual :: !String,
  trBsmtCond :: !String,
  trBsmtExposure :: !String,
  trBsmtFinType1 :: !String,
  trBsmtFinSF1 :: !IntOrNull,
  trBsmtFinType2 :: !String,
  trBsmtFinSF2 :: !IntOrNull,
  trBsmtUnfSF :: !IntOrNull,
  trTotalBsmtSF :: !IntOrNull,
  trHeating :: !String,
  trHeatingQC :: !String,
  trCentralAir :: !String,
  trElectrical :: !String,
  tr1stFlrSF :: !IntOrNull,
  tr2ndFlrSF :: !IntOrNull,
  trLowQualFinSF :: !IntOrNull,
  trGrLivArea :: !IntOrNull,
  trBsmtFullBath :: !IntOrNull,
  trBsmtHalfBath :: !IntOrNull,
  trFullBath :: !IntOrNull,
  trHalfBath :: !IntOrNull,
  trBedroomAbvGr :: !IntOrNull,
  trKitchenAbvGr :: !IntOrNull,
  trKitchenQual :: !String,
  trTotRmsAbvGrd :: !IntOrNull,
  trFunctional :: !String,
  trFireplaces :: !IntOrNull,
  trFireplaceQu :: !String,
  trGarageType :: !String,
  trGarageYrBlt :: !String,
  trGarageFinish :: !String,
  trGarageCars :: !IntOrNull,
  trGarageArea :: !IntOrNull,
  trGarageQual :: !String,
  trGarageCond :: !String,
  trPavedDrive :: !String,
  trWoodDeckSF :: !IntOrNull,
  trOpenPorchSF :: !IntOrNull,
  trEnclosedPorch :: !IntOrNull,
  tr3SsnPorch :: !IntOrNull,
  trScreenPorch :: !IntOrNull,
  trPoolArea :: !IntOrNull,
  trPoolQC :: !String,
  trFence :: !String,
  trMiscFeature :: !String,
  trMiscVal :: !IntOrNull,
  trMoSold :: !IntOrNull,
  trYrSold :: !IntOrNull,
  trSaleType :: !String,
  trSaleCondition :: !String,
  trSalePrice :: !IntOrNull} 
  deriving (Generic, Show)

instance FromRecord Train

data Test = Test { 
  teId :: !IntOrNull,
  teMSSubClass :: !IntOrNull,
  teMSZoning :: !String,
  teLotFrontage :: !String,
  teLotArea :: !IntOrNull,
  teStreet :: !String,
  teAlley :: !String,
  teLotShape :: !String,
  teLandContour :: !String,
  teUtilities :: !String,
  teLotConfig :: !String,
  teLandSlope :: !String,
  teNeighborhood :: !String,
  teCondition1 :: !String,
  teCondition2 :: !String,
  teBldgType :: !String,
  teHouseStyle :: !String,
  teOverallQual :: !IntOrNull,
  teOverallCond :: !IntOrNull,
  teYearBuilt :: !IntOrNull,
  teYearRemodAdd :: !IntOrNull,
  teRoofStyle :: !String,
  teRoofMatl :: !String,
  teExterior1st :: !String,
  teExterior2nd :: !String,
  teMasVnrType :: !String,
  teMasVnrArea :: !String,
  teExterQual :: !String,
  teExterCond :: !String,
  teFoundation :: !String,
  teBsmtQual :: !String,
  teBsmtCond :: !String,
  teBsmtExposure :: !String,
  teBsmtFinType1 :: !String,
  teBsmtFinSF1 :: !IntOrNull,
  teBsmtFinType2 :: !String,
  teBsmtFinSF2 :: !IntOrNull,
  teBsmtUnfSF :: !IntOrNull,
  teTotalBsmtSF :: !IntOrNull,
  teHeating :: !String,
  teHeatingQC :: !String,
  teCentralAir :: !String,
  teElectrical :: !String,
  te1stFlrSF :: !IntOrNull,
  te2ndFlrSF :: !IntOrNull,
  teLowQualFinSF :: !IntOrNull,
  teGrLivArea :: !IntOrNull,
  teBsmtFullBath :: !IntOrNull,
  teBsmtHalfBath :: !IntOrNull,
  teFullBath :: !IntOrNull,
  teHalfBath :: !IntOrNull,
  teBedroomAbvGr :: !IntOrNull,
  teKitchenAbvGr :: !IntOrNull,
  teKitchenQual :: !String,
  teTotRmsAbvGrd :: !IntOrNull,
  teFunctional :: !String,
  teFireplaces :: !IntOrNull,
  teFireplaceQu :: !String,
  teGarageType :: !String,
  teGarageYrBlt :: !String,
  teGarageFinish :: !String,
  teGarageCars :: !IntOrNull,
  teGarageArea :: !IntOrNull,
  teGarageQual :: !String,
  teGarageCond :: !String,
  tePavedDrive :: !String,
  teWoodDeckSF :: !IntOrNull,
  teOpenPorchSF :: !IntOrNull,
  teEnclosedPorch :: !IntOrNull,
  te3SsnPorch :: !IntOrNull,
  teScreenPorch :: !IntOrNull,
  tePoolArea :: !IntOrNull,
  tePoolQC :: !String,
  teFence :: !String,
  teMiscFeature :: !String,
  teMiscVal :: !IntOrNull,
  teMoSold :: !IntOrNull,
  teYrSold :: !IntOrNull,
  teSaleType :: !String,
  teSaleCondition :: !String} 
  deriving (Generic, Show)

instance FromRecord Test


aahr uu aa = hhhr uu $ aahh aa 

decomperIO uu vv hh wmax lmax xmax omax bmax mmax umax pmax fmax mult seed =
      parametersSystemsHistoryRepasDecomperMaxRollByMExcludedSelfHighestFmaxIORepa 
        wmax lmax xmax omax bmax mmax umax pmax fmax mult seed uu vv hh



