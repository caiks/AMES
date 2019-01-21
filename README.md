# AMES - House Prices: Advanced Regression Techniques

This repository contains tests of the [AlignmentRepa repository](https://github.com/caiks/AlignmentRepa) using data from the [Ames Housing dataset](http://jse.amstat.org/v19n3/decock.pdf) compiled by Dean De Cock. Full details of the dataset are in [Kaggle Data Set - House Prices: Advanced Regression Techniques](https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data). The AlignmentRepa repository is a fast Haskell implementation of some of the *practicable inducers* described in the paper *The Theory and Practice of Induction by Alignment* at https://greenlake.co.uk/. 

## Documentation

There is an analysis of this dataset [here](https://greenlake.co.uk/pages/dataset_AMES), with sections (a) [predicting sale price without modelling](https://greenlake.co.uk/pages/dataset_AMES#Predicting_sale_price_without_modelling) and (b) [induced modelling of sale price](https://greenlake.co.uk/pages/dataset_AMES#Induced_modelling_of_sale_price). 

## Installation

The `AMES` executables require the `AlignmentRepa` module which is in the [AlignmentRepa repository](https://github.com/caiks/AlignmentRepa). See the AlignmentRepa repository for installation instructions of the Haskell compiler and libraries.

Then download the zip files or use git to get the AMES repository and the underlying Alignment and AlignmentRepa repositories -
```
cd
git clone https://github.com/caiks/Alignment.git
git clone https://github.com/caiks/AlignmentRepa.git
git clone https://github.com/caiks/AMES.git
```

## Usage

The *practicable model induction* is described [here](https://greenlake.co.uk/pages/dataset_AMES_model1).

`AMES_engine1` runs on a Ubuntu 16.04 Pentium CPU G2030 @ 3.00GHz using 1883 MB total memory and takes 6454 seconds,

```
cd ../Alignment
rm *.o *.hi

cd ../AlignmentRepa
rm *.o *.hi

gcc -fPIC -c AlignmentForeign.c -o AlignmentForeign.o -O3

cd ../AMES
rm *.o *.hi

ghc -i../Alignment -i../AlignmentRepa ../AlignmentRepa/AlignmentForeign.o AMES_engine1.hs -o AMES_engine1.exe -rtsopts -O2

./AMES_engine1.exe +RTS -s >AMES_engine1.log 2>&1 &

tail -f AMES_engine1.log

```

To experiment with the dataset in the interpreter,
```
cd ../Alignment
rm *.o *.hi

cd ../AlignmentRepa
rm *.o *.hi

gcc -fPIC -c AlignmentForeign.c -o AlignmentForeign.o -O3

cd ../AMES

ghci -i../Alignment -i../AlignmentRepa ../AlignmentRepa/AlignmentForeign.o
```

```hs
:set -fobject-code
:l AMESDev
```
Then exit the interpreter,
```
rm AMESDev.o

ghci -i../Alignment -i../AlignmentRepa ../AlignmentRepa/AlignmentForeign.o
```

```hs
:l AMESDev

csvtr <- BL.readFile "train.csv"
let vvcsvtr = either (\_ -> V.empty) id (Data.Csv.decode HasHeader csvtr :: Either String (V.Vector Train))
let aatr = llaa [(llss [(VarStr s, fw rr) | (s,fw) <- trmap],1) | rr <- V.toList vvcsvtr]

csvte <- BL.readFile "test.csv"
let vvcsvte = either (\_ -> V.empty) id (Data.Csv.decode HasHeader csvte :: Either String (V.Vector Test))
let aate = llaa [(llss [(VarStr s, fw rr) | (s,fw) <- temap],1) | rr <- V.toList vvcsvte]

let uu = sys aatr `uunion` sys aate
let vv = uvars uu `minus` sgl (VarStr "Id")
let vvl = sgl (VarStr "SalePrice")
let vvk = vv `minus` vvl

let aa = (aatr `red` vvk) `add` (aate `red` vvk)

size aa

let vvo = llqq [w | w <- qqll vv, isOrd uu w, let u = vol uu (sgl w), u > 16]

let vvoz = llqq [w | w <- qqll vv, isOrd uu w, let u = vol uu (sgl w), u > 16, let rr = unit (sgl (llss [(w, ValInt 0)])), let bb = aatr `red` sgl w `mul` rr, size bb > 100]

let xx = Map.fromList $ map (\(v,ww) -> let VarStr s = v in (v, (VarStr (s ++ "B"), ww))) $ [(v, bucket 20 aa v) | v <- qqll (vvo `minus` vvoz)] ++ [(VarStr "SalePrice", bucket 20 aatr (VarStr "SalePrice"))] ++ [(v, bucket 20 aa' v) | v <- qqll vvoz, let rr = unit (sgl (llss [(v, ValInt 0)])), let bb = aa `red` sgl v `mul` rr, let aa' = trim (aa `red` sgl v `sub` bb)]

let aab = reframeb aa xx

size aab

let aatrb = reframeb aatr xx

size aatrb

let aateb = reframeb aate xx

size aateb

let uub = sys aab `uunion` sys aatrb `uunion` sys aateb
let vvb = uvars uub `minus` sgl (VarStr "Id")
let vvbl = sgl (VarStr "SalePriceB")
let vvbk = vvb `minus` vvbl

let hhb = aahr uub aab `hrhrred` vvbk

hrsize hhb

let summation = systemsDecompFudsHistoryRepasAlignmentContentShuffleSummation_u

let sumtree = systemsDecompFudsHistoryRepasTreeAlignmentContentShuffleSummation_u

let (wmax,lmax,xmax,omax,bmax,mmax,umax,pmax,fmax,mult,seed) = (1460, 8, 1460, 10, (10*3), 4, 1460, 1, 10, 3, 5)

Just (uub',dfb') <- decomperIO uub vvbk hhb wmax lmax xmax omax bmax mmax umax pmax fmax mult seed

summation mult seed uub' dfb' hhb
(19003.657461612253,8264.069987311002)

BL.writeFile ("df1.json") $ decompFudsPersistentsEncode $ decompFudsPersistent dfb'

```

