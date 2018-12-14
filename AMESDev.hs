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

toInt :: IntOrNull -> Integer
toInt (IntOrNull i) = toInteger i

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

trmap = [
  (VarStr "Id", ValInt . toInt . trId),
  (VarStr "MSSubClass", ValInt . toInt . trMSSubClass),
  (VarStr "MSZoning", ValStr . trMSZoning),
  (VarStr "LotFrontage", ValStr . trLotFrontage),
  (VarStr "LotArea", ValInt . toInt . trLotArea),
  (VarStr "Street", ValStr . trStreet),
  (VarStr "Alley", ValStr . trAlley),
  (VarStr "LotShape", ValStr . trLotShape),
  (VarStr "LandContour", ValStr . trLandContour),
  (VarStr "Utilities", ValStr . trUtilities),
  (VarStr "LotConfig", ValStr . trLotConfig),
  (VarStr "LandSlope", ValStr . trLandSlope),
  (VarStr "Neighborhood", ValStr . trNeighborhood),
  (VarStr "Condition1", ValStr . trCondition1),
  (VarStr "Condition2", ValStr . trCondition2),
  (VarStr "BldgType", ValStr . trBldgType),
  (VarStr "HouseStyle", ValStr . trHouseStyle),
  (VarStr "OverallQual", ValInt . toInt . trOverallQual),
  (VarStr "OverallCond", ValInt . toInt . trOverallCond),
  (VarStr "YearBuilt", ValInt . toInt . trYearBuilt),
  (VarStr "YearRemodAdd", ValInt . toInt . trYearRemodAdd),
  (VarStr "RoofStyle", ValStr . trRoofStyle),
  (VarStr "RoofMatl", ValStr . trRoofMatl),
  (VarStr "Exterior1st", ValStr . trExterior1st),
  (VarStr "Exterior2nd", ValStr . trExterior2nd),
  (VarStr "MasVnrType", ValStr . trMasVnrType),
  (VarStr "MasVnrArea", ValStr . trMasVnrArea),
  (VarStr "ExterQual", ValStr . trExterQual),
  (VarStr "ExterCond", ValStr . trExterCond),
  (VarStr "Foundation", ValStr . trFoundation),
  (VarStr "BsmtQual", ValStr . trBsmtQual),
  (VarStr "BsmtCond", ValStr . trBsmtCond),
  (VarStr "BsmtExposure", ValStr . trBsmtExposure),
  (VarStr "BsmtFinType1", ValStr . trBsmtFinType1),
  (VarStr "BsmtFinSF1", ValInt . toInt . trBsmtFinSF1),
  (VarStr "BsmtFinType2", ValStr . trBsmtFinType2),
  (VarStr "BsmtFinSF2", ValInt . toInt . trBsmtFinSF2),
  (VarStr "BsmtUnfSF", ValInt . toInt . trBsmtUnfSF),
  (VarStr "TotalBsmtSF", ValInt . toInt . trTotalBsmtSF),
  (VarStr "Heating", ValStr . trHeating),
  (VarStr "HeatingQC", ValStr . trHeatingQC),
  (VarStr "CentralAir", ValStr . trCentralAir),
  (VarStr "Electrical", ValStr . trElectrical),
  (VarStr "1stFlrSF", ValInt . toInt . tr1stFlrSF),
  (VarStr "2ndFlrSF", ValInt . toInt . tr2ndFlrSF),
  (VarStr "LowQualFinSF", ValInt . toInt . trLowQualFinSF),
  (VarStr "GrLivArea", ValInt . toInt . trGrLivArea),
  (VarStr "BsmtFullBath", ValInt . toInt . trBsmtFullBath),
  (VarStr "BsmtHalfBath", ValInt . toInt . trBsmtHalfBath),
  (VarStr "FullBath", ValInt . toInt . trFullBath),
  (VarStr "HalfBath", ValInt . toInt . trHalfBath),
  (VarStr "BedroomAbvGr", ValInt . toInt . trBedroomAbvGr),
  (VarStr "KitchenAbvGr", ValInt . toInt . trKitchenAbvGr),
  (VarStr "KitchenQual", ValStr . trKitchenQual),
  (VarStr "TotRmsAbvGrd", ValInt . toInt . trTotRmsAbvGrd),
  (VarStr "Functional", ValStr . trFunctional),
  (VarStr "Fireplaces", ValInt . toInt . trFireplaces),
  (VarStr "FireplaceQu", ValStr . trFireplaceQu),
  (VarStr "GarageType", ValStr . trGarageType),
  (VarStr "GarageYrBlt", ValStr . trGarageYrBlt),
  (VarStr "GarageFinish", ValStr . trGarageFinish),
  (VarStr "GarageCars", ValInt . toInt . trGarageCars),
  (VarStr "GarageArea", ValInt . toInt . trGarageArea),
  (VarStr "GarageQual", ValStr . trGarageQual),
  (VarStr "GarageCond", ValStr . trGarageCond),
  (VarStr "PavedDrive", ValStr . trPavedDrive),
  (VarStr "WoodDeckSF", ValInt . toInt . trWoodDeckSF),
  (VarStr "OpenPorchSF", ValInt . toInt . trOpenPorchSF),
  (VarStr "EnclosedPorch", ValInt . toInt . trEnclosedPorch),
  (VarStr "3SsnPorch", ValInt . toInt . tr3SsnPorch),
  (VarStr "ScreenPorch", ValInt . toInt . trScreenPorch),
  (VarStr "PoolArea", ValInt . toInt . trPoolArea),
  (VarStr "PoolQC", ValStr . trPoolQC),
  (VarStr "Fence", ValStr . trFence),
  (VarStr "MiscFeature", ValStr . trMiscFeature),
  (VarStr "MiscVal", ValInt . toInt . trMiscVal),
  (VarStr "MoSold", ValInt . toInt . trMoSold),
  (VarStr "YrSold", ValInt . toInt . trYrSold),
  (VarStr "SaleType", ValStr . trSaleType),
  (VarStr "SaleCondition", ValStr . trSaleCondition),
  (VarStr "SalePrice", ValInt . toInt . trSalePrice)]


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

temap = [
  (VarStr "Id", ValInt . toInt . trId),
  (VarStr "MSSubClass", ValInt . toInt . trMSSubClass),
  (VarStr "MSZoning", ValStr . trMSZoning),
  (VarStr "LotFrontage", ValStr . trLotFrontage),
  (VarStr "LotArea", ValInt . toInt . trLotArea),
  (VarStr "Street", ValStr . trStreet),
  (VarStr "Alley", ValStr . trAlley),
  (VarStr "LotShape", ValStr . trLotShape),
  (VarStr "LandContour", ValStr . trLandContour),
  (VarStr "Utilities", ValStr . trUtilities),
  (VarStr "LotConfig", ValStr . trLotConfig),
  (VarStr "LandSlope", ValStr . trLandSlope),
  (VarStr "Neighborhood", ValStr . trNeighborhood),
  (VarStr "Condition1", ValStr . trCondition1),
  (VarStr "Condition2", ValStr . trCondition2),
  (VarStr "BldgType", ValStr . trBldgType),
  (VarStr "HouseStyle", ValStr . trHouseStyle),
  (VarStr "OverallQual", ValInt . toInt . trOverallQual),
  (VarStr "OverallCond", ValInt . toInt . trOverallCond),
  (VarStr "YearBuilt", ValInt . toInt . trYearBuilt),
  (VarStr "YearRemodAdd", ValInt . toInt . trYearRemodAdd),
  (VarStr "RoofStyle", ValStr . trRoofStyle),
  (VarStr "RoofMatl", ValStr . trRoofMatl),
  (VarStr "Exterior1st", ValStr . trExterior1st),
  (VarStr "Exterior2nd", ValStr . trExterior2nd),
  (VarStr "MasVnrType", ValStr . trMasVnrType),
  (VarStr "MasVnrArea", ValStr . trMasVnrArea),
  (VarStr "ExterQual", ValStr . trExterQual),
  (VarStr "ExterCond", ValStr . trExterCond),
  (VarStr "Foundation", ValStr . trFoundation),
  (VarStr "BsmtQual", ValStr . trBsmtQual),
  (VarStr "BsmtCond", ValStr . trBsmtCond),
  (VarStr "BsmtExposure", ValStr . trBsmtExposure),
  (VarStr "BsmtFinType1", ValStr . trBsmtFinType1),
  (VarStr "BsmtFinSF1", ValInt . toInt . trBsmtFinSF1),
  (VarStr "BsmtFinType2", ValStr . trBsmtFinType2),
  (VarStr "BsmtFinSF2", ValInt . toInt . trBsmtFinSF2),
  (VarStr "BsmtUnfSF", ValInt . toInt . trBsmtUnfSF),
  (VarStr "TotalBsmtSF", ValInt . toInt . trTotalBsmtSF),
  (VarStr "Heating", ValStr . trHeating),
  (VarStr "HeatingQC", ValStr . trHeatingQC),
  (VarStr "CentralAir", ValStr . trCentralAir),
  (VarStr "Electrical", ValStr . trElectrical),
  (VarStr "1stFlrSF", ValInt . toInt . tr1stFlrSF),
  (VarStr "2ndFlrSF", ValInt . toInt . tr2ndFlrSF),
  (VarStr "LowQualFinSF", ValInt . toInt . trLowQualFinSF),
  (VarStr "GrLivArea", ValInt . toInt . trGrLivArea),
  (VarStr "BsmtFullBath", ValInt . toInt . trBsmtFullBath),
  (VarStr "BsmtHalfBath", ValInt . toInt . trBsmtHalfBath),
  (VarStr "FullBath", ValInt . toInt . trFullBath),
  (VarStr "HalfBath", ValInt . toInt . trHalfBath),
  (VarStr "BedroomAbvGr", ValInt . toInt . trBedroomAbvGr),
  (VarStr "KitchenAbvGr", ValInt . toInt . trKitchenAbvGr),
  (VarStr "KitchenQual", ValStr . trKitchenQual),
  (VarStr "TotRmsAbvGrd", ValInt . toInt . trTotRmsAbvGrd),
  (VarStr "Functional", ValStr . trFunctional),
  (VarStr "Fireplaces", ValInt . toInt . trFireplaces),
  (VarStr "FireplaceQu", ValStr . trFireplaceQu),
  (VarStr "GarageType", ValStr . trGarageType),
  (VarStr "GarageYrBlt", ValStr . trGarageYrBlt),
  (VarStr "GarageFinish", ValStr . trGarageFinish),
  (VarStr "GarageCars", ValInt . toInt . trGarageCars),
  (VarStr "GarageArea", ValInt . toInt . trGarageArea),
  (VarStr "GarageQual", ValStr . trGarageQual),
  (VarStr "GarageCond", ValStr . trGarageCond),
  (VarStr "PavedDrive", ValStr . trPavedDrive),
  (VarStr "WoodDeckSF", ValInt . toInt . trWoodDeckSF),
  (VarStr "OpenPorchSF", ValInt . toInt . trOpenPorchSF),
  (VarStr "EnclosedPorch", ValInt . toInt . trEnclosedPorch),
  (VarStr "3SsnPorch", ValInt . toInt . tr3SsnPorch),
  (VarStr "ScreenPorch", ValInt . toInt . trScreenPorch),
  (VarStr "PoolArea", ValInt . toInt . trPoolArea),
  (VarStr "PoolQC", ValStr . trPoolQC),
  (VarStr "Fence", ValStr . trFence),
  (VarStr "MiscFeature", ValStr . trMiscFeature),
  (VarStr "MiscVal", ValInt . toInt . trMiscVal),
  (VarStr "MoSold", ValInt . toInt . trMoSold),
  (VarStr "YrSold", ValInt . toInt . trYrSold),
  (VarStr "SaleType", ValStr . trSaleType),
  (VarStr "SaleCondition", ValStr . trSaleCondition)]


aahr uu aa = hhhr uu $ aahh aa 

decomperIO uu vv hh wmax lmax xmax omax bmax mmax umax pmax fmax mult seed =
      parametersSystemsHistoryRepasDecomperMaxRollByMExcludedSelfHighestFmaxIORepa 
        wmax lmax xmax omax bmax mmax umax pmax fmax mult seed uu vv hh



