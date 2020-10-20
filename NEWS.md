# CoreGx News File

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
- Removing package from CRAN]
- Submitting package to Bioconductor    