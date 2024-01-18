# CoreGx News File

* v 2.3.6
* Updated a definition of `updateObject` method (Bioc 3.19 bugfix)

# v 2.1.7
* Fixed a bug when deleting a `TreatmentResponseExperiment` assay via `NULL` assignment
* Added a names S4 method fo `TreatmentResponseExperiment` to enable tab autocomplete with `$` access in interative sessions
* Added additional methods for drug combination modelling; these changes will be documented in a new vignette once we have thoroughly tested the new functions
  * For now these are experimental and should not be considered a stable API

# v 2.1.6
* Changed default parallelization strategy inside `aggregate2` (and therefore inside `aggregate,TreatmentResponseExperiment-method` and `endoaggregate`) to split the table into nthread tables instead of using by
  * Result should be (1) parallelization is now always faster than serial computations, which was not true previously
  * Memory usage of parallelization should be much smaller, since we aren't splitting into a very long list of tables
* Optimized the internal representation of the `TreatmentResponseExperiment` assay index to remove storage of `NA` for `rowKey`-`colKey` combinations with no observations in any assay
  * This was causing memory usage to baloon if both `rowKey` and `colKey` were a large sequence
* Prepended a "." to the internal assay index column names for each assay
  * This should reduce name clashes between internal `TRE` metadata and the column names of an assay (specifically, you can now have a column with the same name as the assay)  

# v 2.1.5
* Add error message to CoreSet,show-method which lets users know to use updateObject if the slot names are not valid

# v 2.1.4
* Add `endoaggregate` method to compute `TreatmentResponseExperiment` assay aggregations within the object
* Add `mergeAssays` method to allow joining assaying within a `TreatmentResponseExperiment`

# v 2.1.3
* Updated `CoreSet` vignette to reflect recent changes to the object structure
* Renamed the `LongTable` vignette to `TreatmentResponseExperiment` and updated
the content to reflect the changes in class structure from 2.1.1
* Generated new `TreatmentResponseExperiment` class and structure diagrams and
inlcuded them in the `TreamentResponseExperiment` vignette
* Added new example `TreatmentResponseExperiment` object to package data
* Added various unit tests for the `LongTable` (and therefore also the `TreatmentResponseExperiment`)
* Added proper documentation object for the TREDataMapper-accessors
* Added aggregate methods for `data.table` and `LongTable`
* Added endoaggregate method for `LongTable`, which uses `aggregate` internally
but assigns the result back to the object and returns the updated object.
Thus this method is an endomorphic version of aggregate.
* Added new argument `summarize` to `assay,LongTable-method` which only
attaches columns which have been summarized over if `FALSE`
* Added `assayCols` and `assayKeys` helper methods to retrive valid assay
column names or the key columns for an assay, respectively.


# v 2.1.2
* Fix bug in `logLogisticRegression` causing tests to fail in Bioconductor 3.16
daily builds

# v 2.1.1
* First update since Bioconductor 3.15 release
* Merged rework of the `LongTable` class back into main branch
* The object has now been updated to

# v 2.0.0
* The `@cell` slot has become the `@sample` slot. Associated generics and
accessor methods have been renamed, then aliased to their old names. As such,
old code should still work as expected, but will in fact be calling different
S4 methods.
* Added the `@treatment` slot to the `CoreSet-class`
* Renamed `@sensitivity` slot to `@treatmentResponse`

# v 1.5.8
* Fixed imports for the CoreSet2 constructor to resolve errors in downstream package PharmacoGx

# v1.5.7
* Add TreatmentResponseExperiment class, a simple wrapper around LongTable to make the class syntax more domain specific
* Add CoreSet2 structure to support creation of CoreSets with the modified class structure introducted in BioC 3.13
* CoreSets can now be made with treatment combination experiments via the TreatmentResponseExperiment class!

# v1.5.6
- Fix bug in LongTable -> data.table coerce method that was causing rows of some assays to be dropped (closes issue #)

# v1.5.5
- Fix bug in .distancePointLine where function fails with no intercept specified (Issue #120)
- Added support for aggregating an assay inside of a LongTable class object
- Some in-progress updates to the CoreSet constructor which will be completed for the Fall release
- Fixed an error in treatmentNames example
- Fixed roxygen2 documentation warnings about S4 method documentation
- Overhauled LongTable coerce methods to use the LongTableDataMapper class instead of the deprecated 'LongTable.config' attribute

# v1.5.4
- Fix bug in `$<-` and `[[<-` methods where value was returned instead of updated object
- Fix bug in .sanitize input caused by length > 1 coercing to logical vector

# v1.5.3
- Fix bug in connectivityScore caused by length > 1 coercing to logical vector; this should fix errors in RadioGx and PharmacoGx vignettes that were caused by failed R CMD build

# v1.5.2
- Add subsetBySample method for CoreSet object; this is the first step in
modularizing the subset methods for reuse in dependent packages
- Added a CoreSet-utils documentation section to document subset, intersect,
combine and other set operations for a CoreSet object.

# v1.5.1
- Fixed some spelling errors and incorrect code chunk configurations in the
LongTable vignette
- Fix bug in .rebuildProfiles where the function fails if replicate_id is
assigned as a rowID column in the LongTable in @sensitivity

# v1.5.0
- Bioconductor spring 2021 release
- Added the DataMapper abstract class
- Added the LongTableDataMapper concrete class
- Added the metaConstruct method, for making an S4 object from a sub-class of DataMapper
- Updated LongTable vignette with documentation for the DataMapper and LongTableDataMapper
- Refactored various methods to work with a LongTable in @sensititivty
- Refactored various methods to work with a MultiAssayExperiment in @molecularProfiles

# v1.1.5
- Implemented a new class, the `LongTable`, to store the results of a treatment
response experiment. This class provides a flexible and fast data storage
object which can be subclassed for use in other R packages.
- Added vignette documenting `LongTable` accessors and usage of the new object.

# v1.0.2
- Bug fix: suppress warnings thrown by piano::runGSA inside the connectivitScore
function

# v1.0.1
- Updated the CoreGx vignette to include more information on extending the
CoreSet class for use in other treatment-response experiments.

# v1.0.0
- Implemented molecularProfiles as `SummarizedExperiment`s instead of `ExpressionSet`s
- Modifying generic implementation to add `...`; allows for additional arguments in `setMethods()` for CoreGx generics
- Removing package from CRAN
- Submitting package to Bioconductor