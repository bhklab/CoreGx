# CoreGx News File

## v1.5.5
- Fix bug in .distancePointLine where function fails with no intercept specified (Issue #120)
- Added support for aggregating an assay inside of a LongTable class object
- Some in-progress updates to the CoreSet constructor which will be completed for the Fall release

## v1.5.4
- Fix bug in `$<-` and `[[<-` methods where value was returned instead of updated object
- Fix bug in .sanitize input caused by length > 1 coercing to logical vector

## v1.5.3
- Fix bug in connectivityScore caused by length > 1 coercing to logical vector; this should fix errors in RadioGx and PharmacoGx vignettes that were caused by failed R CMD build

## v1.5.2
- Add subsetBySample method for CoreSet object; this is the first step in
modularizing the subset methods for reuse in dependent packages
- Added a CoreSet-utils documentation section to document subset, intersect,
combine and other set operations for a CoreSet object.

## v1.5.1
- Fixed some spelling errors and incorrect code chunk configurations in the 
LongTable vignette
- Fix bug in .rebuildProfiles where the function fails if replicate_id is
assigned as a rowID column in the LongTable in @sensitivity

## v1.5.0
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