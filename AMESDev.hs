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

data IntOrNull = IntOrNullInt Int | IntOrNullNull deriving Show

instance FromField IntOrNull where
    parseField s = case runParser (parseField s :: Parser Int) of
        Left err -> pure $ IntOrNullNull
        Right n  -> pure $ IntOrNullInt n

toValue :: IntOrNull -> Value
toValue IntOrNullNull = ValStr "null"
toValue (IntOrNullInt i) = ValInt (toInteger i)

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
  (VarStr "Id", toValue . trId),
  (VarStr "MSSubClass", toValue . trMSSubClass),
  (VarStr "MSZoning", ValStr . trMSZoning),
  (VarStr "LotFrontage", ValStr . trLotFrontage),
  (VarStr "LotArea", toValue . trLotArea),
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
  (VarStr "OverallQual", toValue . trOverallQual),
  (VarStr "OverallCond", toValue . trOverallCond),
  (VarStr "YearBuilt", toValue . trYearBuilt),
  (VarStr "YearRemodAdd", toValue . trYearRemodAdd),
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
  (VarStr "BsmtFinSF1", toValue . trBsmtFinSF1),
  (VarStr "BsmtFinType2", ValStr . trBsmtFinType2),
  (VarStr "BsmtFinSF2", toValue . trBsmtFinSF2),
  (VarStr "BsmtUnfSF", toValue . trBsmtUnfSF),
  (VarStr "TotalBsmtSF", toValue . trTotalBsmtSF),
  (VarStr "Heating", ValStr . trHeating),
  (VarStr "HeatingQC", ValStr . trHeatingQC),
  (VarStr "CentralAir", ValStr . trCentralAir),
  (VarStr "Electrical", ValStr . trElectrical),
  (VarStr "1stFlrSF", toValue . tr1stFlrSF),
  (VarStr "2ndFlrSF", toValue . tr2ndFlrSF),
  (VarStr "LowQualFinSF", toValue . trLowQualFinSF),
  (VarStr "GrLivArea", toValue . trGrLivArea),
  (VarStr "BsmtFullBath", toValue . trBsmtFullBath),
  (VarStr "BsmtHalfBath", toValue . trBsmtHalfBath),
  (VarStr "FullBath", toValue . trFullBath),
  (VarStr "HalfBath", toValue . trHalfBath),
  (VarStr "BedroomAbvGr", toValue . trBedroomAbvGr),
  (VarStr "KitchenAbvGr", toValue . trKitchenAbvGr),
  (VarStr "KitchenQual", ValStr . trKitchenQual),
  (VarStr "TotRmsAbvGrd", toValue . trTotRmsAbvGrd),
  (VarStr "Functional", ValStr . trFunctional),
  (VarStr "Fireplaces", toValue . trFireplaces),
  (VarStr "FireplaceQu", ValStr . trFireplaceQu),
  (VarStr "GarageType", ValStr . trGarageType),
  (VarStr "GarageYrBlt", ValStr . trGarageYrBlt),
  (VarStr "GarageFinish", ValStr . trGarageFinish),
  (VarStr "GarageCars", toValue . trGarageCars),
  (VarStr "GarageArea", toValue . trGarageArea),
  (VarStr "GarageQual", ValStr . trGarageQual),
  (VarStr "GarageCond", ValStr . trGarageCond),
  (VarStr "PavedDrive", ValStr . trPavedDrive),
  (VarStr "WoodDeckSF", toValue . trWoodDeckSF),
  (VarStr "OpenPorchSF", toValue . trOpenPorchSF),
  (VarStr "EnclosedPorch", toValue . trEnclosedPorch),
  (VarStr "3SsnPorch", toValue . tr3SsnPorch),
  (VarStr "ScreenPorch", toValue . trScreenPorch),
  (VarStr "PoolArea", toValue . trPoolArea),
  (VarStr "PoolQC", ValStr . trPoolQC),
  (VarStr "Fence", ValStr . trFence),
  (VarStr "MiscFeature", ValStr . trMiscFeature),
  (VarStr "MiscVal", toValue . trMiscVal),
  (VarStr "MoSold", toValue . trMoSold),
  (VarStr "YrSold", toValue . trYrSold),
  (VarStr "SaleType", ValStr . trSaleType),
  (VarStr "SaleCondition", ValStr . trSaleCondition),
  (VarStr "SalePrice", toValue . trSalePrice)]

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
  (VarStr "Id", toValue . teId),
  (VarStr "MSSubClass", toValue . teMSSubClass),
  (VarStr "MSZoning", ValStr . teMSZoning),
  (VarStr "LotFrontage", ValStr . teLotFrontage),
  (VarStr "LotArea", toValue . teLotArea),
  (VarStr "Street", ValStr . teStreet),
  (VarStr "Alley", ValStr . teAlley),
  (VarStr "LotShape", ValStr . teLotShape),
  (VarStr "LandContour", ValStr . teLandContour),
  (VarStr "Utilities", ValStr . teUtilities),
  (VarStr "LotConfig", ValStr . teLotConfig),
  (VarStr "LandSlope", ValStr . teLandSlope),
  (VarStr "Neighborhood", ValStr . teNeighborhood),
  (VarStr "Condition1", ValStr . teCondition1),
  (VarStr "Condition2", ValStr . teCondition2),
  (VarStr "BldgType", ValStr . teBldgType),
  (VarStr "HouseStyle", ValStr . teHouseStyle),
  (VarStr "OverallQual", toValue . teOverallQual),
  (VarStr "OverallCond", toValue . teOverallCond),
  (VarStr "YearBuilt", toValue . teYearBuilt),
  (VarStr "YearRemodAdd", toValue . teYearRemodAdd),
  (VarStr "RoofStyle", ValStr . teRoofStyle),
  (VarStr "RoofMatl", ValStr . teRoofMatl),
  (VarStr "Exterior1st", ValStr . teExterior1st),
  (VarStr "Exterior2nd", ValStr . teExterior2nd),
  (VarStr "MasVnrType", ValStr . teMasVnrType),
  (VarStr "MasVnrArea", ValStr . teMasVnrArea),
  (VarStr "ExterQual", ValStr . teExterQual),
  (VarStr "ExterCond", ValStr . teExterCond),
  (VarStr "Foundation", ValStr . teFoundation),
  (VarStr "BsmtQual", ValStr . teBsmtQual),
  (VarStr "BsmtCond", ValStr . teBsmtCond),
  (VarStr "BsmtExposure", ValStr . teBsmtExposure),
  (VarStr "BsmtFinType1", ValStr . teBsmtFinType1),
  (VarStr "BsmtFinSF1", toValue . teBsmtFinSF1),
  (VarStr "BsmtFinType2", ValStr . teBsmtFinType2),
  (VarStr "BsmtFinSF2", toValue . teBsmtFinSF2),
  (VarStr "BsmtUnfSF", toValue . teBsmtUnfSF),
  (VarStr "TotalBsmtSF", toValue . teTotalBsmtSF),
  (VarStr "Heating", ValStr . teHeating),
  (VarStr "HeatingQC", ValStr . teHeatingQC),
  (VarStr "CentralAir", ValStr . teCentralAir),
  (VarStr "Electrical", ValStr . teElectrical),
  (VarStr "1stFlrSF", toValue . te1stFlrSF),
  (VarStr "2ndFlrSF", toValue . te2ndFlrSF),
  (VarStr "LowQualFinSF", toValue . teLowQualFinSF),
  (VarStr "GrLivArea", toValue . teGrLivArea),
  (VarStr "BsmtFullBath", toValue . teBsmtFullBath),
  (VarStr "BsmtHalfBath", toValue . teBsmtHalfBath),
  (VarStr "FullBath", toValue . teFullBath),
  (VarStr "HalfBath", toValue . teHalfBath),
  (VarStr "BedroomAbvGr", toValue . teBedroomAbvGr),
  (VarStr "KitchenAbvGr", toValue . teKitchenAbvGr),
  (VarStr "KitchenQual", ValStr . teKitchenQual),
  (VarStr "TotRmsAbvGrd", toValue . teTotRmsAbvGrd),
  (VarStr "Functional", ValStr . teFunctional),
  (VarStr "Fireplaces", toValue . teFireplaces),
  (VarStr "FireplaceQu", ValStr . teFireplaceQu),
  (VarStr "GarageType", ValStr . teGarageType),
  (VarStr "GarageYrBlt", ValStr . teGarageYrBlt),
  (VarStr "GarageFinish", ValStr . teGarageFinish),
  (VarStr "GarageCars", toValue . teGarageCars),
  (VarStr "GarageArea", toValue . teGarageArea),
  (VarStr "GarageQual", ValStr . teGarageQual),
  (VarStr "GarageCond", ValStr . teGarageCond),
  (VarStr "PavedDrive", ValStr . tePavedDrive),
  (VarStr "WoodDeckSF", toValue . teWoodDeckSF),
  (VarStr "OpenPorchSF", toValue . teOpenPorchSF),
  (VarStr "EnclosedPorch", toValue . teEnclosedPorch),
  (VarStr "3SsnPorch", toValue . te3SsnPorch),
  (VarStr "ScreenPorch", toValue . teScreenPorch),
  (VarStr "PoolArea", toValue . tePoolArea),
  (VarStr "PoolQC", ValStr . tePoolQC),
  (VarStr "Fence", ValStr . teFence),
  (VarStr "MiscFeature", ValStr . teMiscFeature),
  (VarStr "MiscVal", toValue . teMiscVal),
  (VarStr "MoSold", toValue . teMoSold),
  (VarStr "YrSold", toValue . teYrSold),
  (VarStr "SaleType", ValStr . teSaleType),
  (VarStr "SaleCondition", ValStr . teSaleCondition)]


aahr uu aa = hhhr uu $ aahh aa 

decomperIO uu vv hh wmax lmax xmax omax bmax mmax umax pmax fmax mult seed =
      parametersSystemsHistoryRepasDecomperMaxRollByMExcludedSelfHighestFmaxIORepa 
        wmax lmax xmax omax bmax mmax umax pmax fmax mult seed uu vv hh



