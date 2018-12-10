# AMES - House Prices: Advanced Regression Techniques

This repository contains tests of the [AlignmentRepa repository](https://github.com/caiks/AlignmentRepa) using data from the [Ames Housing dataset](http://jse.amstat.org/v19n3/decock.pdf) compiled by Dean De Cock. Full details of the dataset are in [Kaggle Data Set - House Prices: Advanced Regression Techniques](https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data). The AlignmentRepa repository is a fast Haskell implementation of some of the *practicable inducers* described in the paper *The Theory and Practice of Induction by Alignment* at https://greenlake.co.uk/. 

## Documentation

There is an analysis of this dataset [here](https://greenlake.co.uk/pages/dataset_AMES), with sections (a) [predicting edibility without modelling](https://greenlake.co.uk/pages/dataset_AMES#Predicting_edibility_without_modelling), (b) [predicting odor without modelling](https://greenlake.co.uk/pages/dataset_AMES#Predicting_odor_without_modelling), (c) [manual modelling of edibility](https://greenlake.co.uk/pages/dataset_AMES#Manual_modelling_of_edibility) and (d) [induced modelling of edibility](https://greenlake.co.uk/pages/dataset_AMES#Induced_modelling_of_edibility). 

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

`AMES_engine1` runs on a Ubuntu 16.04 Pentium CPU G2030 @ 3.00GHz using 1784 MB total memory and takes 1166 seconds,

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

mush <- ByteStringChar8.readFile "../AMES/agaricus-lepiota.data"

let aa = llaa $ map (\ll -> (llss ll,1)) $ map (\ss -> (map (\(u,(v,uu)) -> (VarStr v,ValStr (fromJust (lookup u uu)))) (zip ss names))) $ map (\l -> filter (/=',') l) $ lines $ ByteStringChar8.unpack $ mush
let uu = sys aa
let vv = uvars uu
let vvl = Set.singleton (VarStr "edible")
let vvk = vv `minus` vvl
let hh = aahr uu aa

let (wmax,lmax,xmax,omax,bmax,mmax,umax,pmax,fmax,mult,seed) = ((9*9*10), 8, (9*9*10), 10, (10*3), 3, (9*9*10), 1, 3, 3, 5)

Just (uu1,df1) <- decomperIO uu vvk hh wmax lmax xmax omax bmax mmax umax pmax fmax mult seed

ByteString.writeFile ("df1.json") $ decompFudsPersistentsEncode $ decompFudsPersistent df1

systemsDecompFudsHistoryRepasAlignmentContentShuffleSummation_u mult seed uu1 df1 hh

```

