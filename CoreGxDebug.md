# CoreGx Package

This is a record of development on the CoreGx package while it is being prepared for CRAN.

## 28.05.19 R CMD check

1. Build 1

**Errors**

```
ERROR Package suggested but not available for checking: 'RadioGx'
```
- Remove RadioGx from suggestions until it is release on CRAN

```
Namespace dependencies not required: 'lsa', 'methods', 'piano'
```
- Add `Imports: lsa, methods, piano` to `DESCRIPTION.md`
- Tools > Project Options > Build Tools: uncheck "Use devtools package functions if available"
  - For some reason this makes piano dependency work

**Warnings**

```
Warning: roxygen2 requires Encoding: UTF-8
```
- Added `Encoding: UTF-8` to `DESCRIPTION.md`
- Caused 40 warnings on package build


2. Build 2

**Errors**

```
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
- Fixed in errors section

```
* checking dependencies in R code ... WARNING
'::' or ':::' import not declared from: 'Biobase'
```
- Added `Biobase` to end of `Imports` in `DESCRIPTION`

```
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

1. Build 3
- Now 36 warnings on build

**Errors**


**Warnings**

1. Build 4
**Errors**

**Warnings**
5. Build 5
**Errors**

**Warnings**
6. Build 6
**Errors**

**Warnings**
7. Build 7
**Errors**

**Warnings**
8. Build 8
**Errors**

**Warnings**
9.  Build 9
**Errors**

**Warnings**
10.  Build 10
**Errors**

**Warnings**