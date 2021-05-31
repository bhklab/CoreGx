# CoreGx News File

## 1.5.1
- Fixed some spelling errors and incorrect code chunk configurations in the 
LongTable vignette
- Fix bug in .rebuildProfiles where the function fails if replicate_id is
assigned as a rowID column in the LongTable in @sensitivity

## 1.5.0
- Bioconductor spring 2021 release
- Added the DataMapper abstract class
- Added the LongTableDataMapper concrete class
- Added the metaConstruct method, for making an S4 object from a sub-class of DataMapper
- Updated LongTable vignette with documentation for the DataMapper and LongTableDataMapper
- Refactored various methods to work with a LongTable in @sensititivty
- Refactored various methods to work with a MultiAssayExperiment in @molecularProfiles

## v1.1.5
- Implemented a new class, the `LongTable`, to store the results of a treatment
response experiment. This class provides a flexible and fast data storage 
object which can be subclassed for use in other R packages.
- Added vignette documenting `LongTable` accessors and usage of the new object.

## v1.0.2
- Bug fix: suppress warnings thrown by piano::runGSA inside the connectivitScore
function

## v1.0.1
- Updated the CoreGx vignette to include more information on extending the
CoreSet class for use in other treatment-response experiments.

## v1.0.0
- Implemented molecularProfiles as `SummarizedExperiment`s instead of `ExpressionSet`s
- Modifying generic implementation to add `...`; allows for additional arguments in `setMethods()` for CoreGx generics
- Removing package from CRAN
- Submitting package to Bioconductor