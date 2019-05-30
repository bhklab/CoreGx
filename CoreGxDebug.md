# CoreGx Package

This is a record of development on the CoreGx package while it is being prepared for CRAN.

## 28.05.19 R CMD check

### **Build 1**

**Errors**

```[RESOLVED]
ERROR Package suggested but not available for checking: 'RadioGx'
```
- Remove RadioGx from suggestions until it is release on CRAN

```
Namespace dependencies not required: 'lsa', 'methods', 'piano'
``` [RESOLVED]
- Add `Imports: lsa, methods, piano` to `DESCRIPTION`
- Tools > Project Options > Build Tools: uncheck "Use devtools package functions if available"
  - For some reason this makes piano dependency work

**Warnings**

``` [RESOLVED]
Warning: roxygen2 requires Encoding: UTF-8
```
- Added `Encoding: UTF-8` to `DESCRIPTION`
- Caused 40 warnings on package build
  - Warnings due to incorrect format of DESCRIPTION; must end with a return, doesn't support commenting

### **Build 2**

**Errors**

```[RESOLVED]
callingWaterfall : <anonymous>: possible error in distancePointLine(x =
  x[1], y = x[2], slope = slope, intercept = intercept): unused
  arguments (slope = slope, intercept = intercept)
```
- Changed `slope` to `a` and `intercept` to `b` in function call; this matches the parameters in function definition


```
* checking examples ... ERROR
Running examples in 'CoreGx-Ex.R' failed
The error most likely occurred in:

> ### Name: cSetName
> ### Title: cSetName Generic
> ### Aliases: cSetName
> 
> ### ** Examples
> 
> data(CCLEsmall)
Warning in data(CCLEsmall) : data set 'CCLEsmall' not found
> cSetName(CCLEsmall)
Error in cSetName(CCLEsmall) : object 'CCLEsmall' not found
```
- Added `PharmacoGx` to end of `Imports` in `DESCRIPTION`

**Warnings**
```
* checking whether package 'CoreGx' can be installed ... WARNING
Found the following significant warnings:
  Note: possible error in 'distancePointLine(x = x[1], ': unused arguments (slope = slope, intercept = intercept) 
```
- Fixed in above errors section

```[RESOLVED]
* checking dependencies in R code ... WARNING
'::' or ':::' import not declared from: 'Biobase'
```
- Added `Biobase` to end of `Imports` in `DESCRIPTION`

```[RESOLVED]
* checking for missing documentation entries ... WARNING
Undocumented code objects:
  'examineGOF'
All user-level objects in a package should have documentation entries.
```
- Defined preliminary documentation for `examineGOF` function in `examineGOF.R`

```
* checking PDF version of manual ... WARNING
LaTeX errors when creating PDF version.
This typically indicates Rd problems.
```
- Installed MikTex in Windows
- Ran `install.package("latexpdf")`
  - Didn't work

### Build 3

**Errors**

```
* checking examples ... ERROR
Running examples in 'CoreGx-Ex.R' failed
The error most likely occurred in:

> ### Name: cSetName
> ### Title: cSetName Generic
> ### Aliases: cSetName
> 
> ### ** Examples
> 
> data(CCLEsmall)
Warning in data(CCLEsmall) : data set 'CCLEsmall' not found
> cSetName(CCLEsmall)
Error in cSetName(CCLEsmall) : object 'CCLEsmall' not found
Execution halted
```
- Assigned `data(CCLEsmall)` to `CCLEsmall` variable in `@example` of `cSetName Generic` in `CoreSetClass.R`
  - Didn't work
- Added `PharmacoGx` to `Imports` in `DESCRIPTION`
  - Didn't work
- Create `data` directory and downloaded example files from `PharamcoGx` git repo

```[RESOLVED]
* checking PDF version of manual without hyperrefs or index ... ERROR
Re-running with no redirection of stdout/stderr.
Hmm ... looks like a package
Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
  pdflatex is not available
Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
  pdflatex is not available
You may want to clean up by 'rm -Rf C:/Users/CHRISE~1/AppData/Local/Temp/RtmpqarKT7/Rd2pdf20a4288b2048'
Error in running tools::texi2pdf()
```
- Added MikTex directory to path in R with `Sys.setenv`
  - Didn't work
- Ran `install.packages("tinytex")`
- Ran `tinytex::install_tinytex()`
  - Restarted RStudio
  - Note: UHN Wi-fi blocks download of TinyTex for some reason

**Warnings**

## 29.05.19 R CMD check continued


**Errors**

```
Running examples in 'CoreGx-Ex.R' failed
The error most likely occurred in:

> ### Name: cSetName
> ### Title: cSetName Generic
> ### Aliases: cSetName
> 
> ### ** Examples
> 
> data(CCLEsmall)
> cSetName(CCLEsmall)
Loading required package: PharmacoGx

Attaching package: 'PharmacoGx'

The following objects are masked from 'package:CoreGx':

    amcc, cellInfo, cellInfo<-, cellNames, cellNames<-,
    checkPSetStructure, connectivityScore, cosinePerm, dateCreated,
    fNames, featureInfo, featureInfo<-, gwc, intersectList, mDataNames,
    mcc, molecularProfiles, molecularProfiles<-, pertNumber,
    pertNumber<-, phenoInfo, phenoInfo<-, sensNumber, sensNumber<-,
    sensitivityInfo, sensitivityInfo<-, sensitivityMeasures,
    sensitivityProfiles, sensitivityProfiles<-, symSetDiffList,
    unionList

Error in (function (classes, fdef, mtable)  : 
  unable to find an inherited method for function 'cSetName' for signature '"PharmacoSet"'
Calls: cSetName -> <Anonymous>
Execution halted
```
- Error appears to be due to PharmacoGx masking CoreGx function definitions
- When it tries to call a function on a `cSet`, PharmacoGx doesn't accept that arguement type
- Need to figure out how to set CoreGx package priority in all `@example` sections of Roxygen code for `CoreSetClass.R` and `GWC.R`


**Warnings**

```[RESOLVED]
* checking for missing documentation entries ... WARNING
Undocumented code objects:
  'CCLEsmall' 'CMAPsmall' 'GDSCsmall' 'HDAC_genes'
Undocumented data sets:
  'CCLEsmall' 'CMAPsmall' 'GDSCsmall' 'HDAC_genes'
All user-level objects in a package should have documentation entries.
See chapter 'Writing R documentation files' in the 'Writing R
Extensions' manual.
```
- Need to write documentation for the four datasets copied from PharmacoGx
  - This was done to fix error with `data(CCLE)` in `@example` of `CoreSetClass.R`
- Added man files of datasets from PharmacoGx to man folder of CoreGx
  - Didn't work
- Added datasets.R file from PharamcoGx to `R` directory