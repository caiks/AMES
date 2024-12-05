# AMES - House Prices: Advanced Regression Techniques

This repository contains tests of the [AlignmentRepa repository](https://github.com/caiks/AlignmentRepa) using data from the [Ames Housing dataset](http://jse.amstat.org/v19n3/decock.pdf) compiled by Dean De Cock. Full details of the dataset are in [Kaggle Data Set - House Prices: Advanced Regression Techniques](https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data). The AlignmentRepa repository is a fast Haskell implementation of some of the *practicable inducers* described in the paper *The Theory and Practice of Induction by Alignment* at https://greenlake.co.uk/. 

## Documentation

There is an analysis of this dataset [here](https://greenlake.co.uk/pages/dataset_AMES), with sections (a) [predicting sale price without modelling](https://greenlake.co.uk/pages/dataset_AMES#Predicting_sale_price_without_modelling) and (b) [induced modelling of sale price](https://greenlake.co.uk/pages/dataset_AMES#Induced_modelling_of_sale_price). 

## Installation

The `AMES` executables require the `AlignmentRepa` module which is in the [AlignmentRepa repository](https://github.com/caiks/AlignmentRepa). The `AlignmentRepa` module requires the [Haskell platform](https://www.haskell.org/downloads#platform) to be installed. The project is managed using [stack](https://docs.haskellstack.org/en/stable/).

Download the zip files or use git to get the AMES repository and the underlying Alignment and AlignmentRepa repositories -
```
cd
git clone https://github.com/caiks/Alignment.git
git clone https://github.com/caiks/AlignmentRepa.git
git clone https://github.com/caiks/AMES.

```
Then build with the following -
```
cd ~/AMES
stack build --ghc-options -w

```
## Usage

The *practicable model induction* is described [here](https://greenlake.co.uk/pages/dataset_AMES_model1).

`AMES_engine1` runs on a Ubuntu 16.04 Pentium CPU G2030 @ 3.00GHz using 1883 MB total memory and takes 6454 seconds,

```
stack exec AMES_engine1.exe +RTS -s >AMES_engine1.log 2>&1 &

tail -f AMES_engine1.log

```
To experiment with the dataset in the interpreter use `stack ghci` or `stack repl` for a run-eval-print loop (REPL) environment, 
```
cd ~/AMES
stack ghci --ghci-options -w

```
Load `AMESDev` to import the modules and define various useful abbreviated functions,
```hs
:l AMESDev

(uub,aab,aatrb,aateb) <- amesBucketedIO 20

let vvb = uvars uub `minus` sgl (VarStr "Id")
let vvbl = sgl (VarStr "SalePriceB")
let vvbk = vvb `minus` vvbl

let hhb = aahr uub aab `hrhrred` vvbk

let (wmax,lmax,xmax,omax,bmax,mmax,umax,pmax,fmax,mult,seed) = (2919, 8, 2919, 10, (10*3), 3, 2919, 1, 3, 3, 5)

Just (uub',dfb') <- decomperIO uub vvbk hhb wmax lmax xmax omax bmax mmax umax pmax fmax mult seed

summation mult seed uub' dfb' hhb
(19003.657461612253,8264.069987311002)

BL.writeFile ("df1.json") $ decompFudsPersistentsEncode $ decompFudsPersistent dfb'

```
Note that if you wish to use compiled code rather than interpreted you may specify the following before loading `AMESDev` -
```
:set -fobject-code

```
Note that some modules may become [unresolved](https://downloads.haskell.org/~ghc/7.10.3-rc1/users_guide/ghci-obj.html), for example,
```hs
rp $ Set.fromList [1,2,3]

<interactive>:9:1: Not in scope: ‘Set.fromList’
```
In this case, re-import the modules explicitly as defined in `AMESDev`, for example,
```hs
import qualified Data.Set as Set
import qualified Data.Map as Map
import Alignment
import AlignmentDevRepa hiding (aahr)

rp $ Set.fromList [1,2,3]
"{1,2,3}"

rp $ fudEmpty
"{}"
```

