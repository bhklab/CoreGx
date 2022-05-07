#' @include LongTable-class.R
#' @noRd
NULL

#' @title TreatmentResponseExperiment class definition
#'
#' @description Define a private constructor method to be used to build a
#'   `TreatmentResponseExperiment` object.
#'
#' @slot rowData See Slots section.
#' @slot colData See Slots section.
#' @slot assays See Slots section.
#' @slot metadata See Slots section.
#' @slot .intern See Slots section.
#'
#' @section Slots:
#' - *rowData*: A `data.table` containing the metadata associated with the
#'   row dimension of a `TreatmentResponseExperiment`.
#' - *colData*: A `data.table` containing the metadata associated with the
#'   column dimension of a `TreatmentResponseExperiment`.
#' - *assays*: A `list` of `data.table`s, one for each assay in a
#'   `TreatmentResponseExperiment`.
#' - *metadata*: An optional `list` of additional metadata for a
#'   `TreatmentResponseExperiment` which doesn't map to one of the dimensions.
#' - *.intern*: An `environment` that holds internal structural metadata
#'   about a `TreatmentResponseExperiment` object, such as which columns are
#'   required to key the object. An environment has been used to allow locking
#'   items, which can prevent accidental modification of a property required
#'   for the class to work.
#'
#' @return `TreatmentResponseExperiment` object containing the assay data from
#'   a treatment response experiment
#'
#' @md
#' @import data.table
#' @keywords internal
#' @rdname TreatmentResponseExperiment-class
#' @aliases .TreatmentResponseExperiment
#' @exportClass TreatmentResponseExperiment
.TreatmentResponseExperiment <- setClass("TreatmentResponseExperiment",
    contains="LongTable")


#' @title TreatmentResponseExperiment constructor method
#'
#' @rdname TreatmentResponseExperiment
#'
#' @description Builds a `TreatmentResponseExperiment` object from rectangular
#' objects. The `rowData` argument should contain row level metadata, while
#' the `colData` argument should contain column level metadata, for the
#' experimental assays
#' in the `assays` list. The `rowIDs` and `colIDs` lists are used to configure
#' the internal keys mapping rows or columns to rows in the assays. Each list
#' should contain at minimum one character vector, specifying which columns
#' in `rowData` or `colData` are required to uniquely identify each row. An
#' optional second character vector can be included, specifying any metadata
#' columns for either dimension. These should contain information about each
#' row but NOT be required to uniquely identify a row in the `colData` or
#' `rowData` objects. Additional metadata can be attached to a
#' `TreatmentResponseExperiment` by passing a list to the metadata argument.
#'
#' @details
#' For now this class is simply a wrapper around a `LongTable` class. In the
#' future we plan to refactor CoreGx such that the `LongTable` class is in a
#' separate pacakge. We can then specialize the implementation of
#' `TreatmentResponseExperiment` to better capture the biomedical nature of
#' this object.
#'
#' @param rowData `data.table`, `data.frame`, `matrix` A table like object
#'   coercible to a `data.table` containing the a unique `rowID` column which
#'   is used to key assays, as well as additional row metadata to subset on.
#' @param rowIDs `character`, `integer` A vector specifying
#'   the names or integer indexes of the row data identifier columns. These
#'   columns will be pasted together to make up the rownames of the
#'   `TreatmentResponseExperiment` object.
#' @param colData `data.table`, `data.frame`, `matrix` A table like object
#'   coercible to a `data.table` containing the a unique `colID` column which
#'   is used to key assays, as well as additional column metadata to subset on.
#' @param colIDs `character`, `integer` A vector specifying
#'   the names or integer indexes of the column data identifier columns. These
#'   columns will be pasted together to make up the colnames of the
#'   `TreatmentResponseExperiment` object.
#' @param assays A `list` containing one or more objects coercible to a
#'   `data.table`, and keyed by rowIDs and colIDs corresponding to the rowID and
#'   colID columns in colData and rowData.
#' @param metadata A `list` of metadata associated with the
#'   `TreatmentResponseExperiment` object being constructed
#' @param keep.rownames `logical`, `character`
#'   Logical: whether rownames should be added as a column if coercing to a
#'   `data.table`, default is FALSE. If TRUE, rownames are added to the column
#'   'rn'.
#'   Character: specify a custom column name to store the rownames in.
#'
#' @return A `TreatmentResponseExperiment` object containing the data for a
#'   treatment response experiment configured according to the rowIDs and
#'   colIDs arguments.
#'
#'
#' @import data.table
#' @export
TreatmentResponseExperiment <- function(rowData, rowIDs, colData, colIDs,
        assays, metadata=list(), keep.rownames=FALSE) {
    if (!missing(rowData) && is(rowData, "LongTable")) {
        LT <- rowData
    } else {
        LT <- LongTable(rowData=rowData, rowIDs=rowIDs, colData=colData,
            colIDs=colIDs, assays=assays, metadata=metadata,
            keep.rownames=keep.rownames)
    }
    .TreatmentResponseExperiment(
        rowData=LT@rowData,
        colData=LT@colData,
        assays=LT@assays,
        .intern=LT@.intern,
        metadata=LT@metadata
    )
}

#' Coerce a `LongTable` to a `TreatmentResponseExperiment`
#'
#' @param from `LongTable` object to coerce to a `TreatmentResponseExperiment`.
#'
#' @return The data in `object`, as the child-class
#'   `TreatmentResponseExperiment`.
#'
#' @seealso [`TreatmentResponseExperiment`]
#'
#' @examples
#' data(clevelandSmall_cSet)
#' TRE <- as(molecularProfilesSlot(clevelandSmall_cSet),
#'     "TreatmentResponseExperiment")
#' TRE
#' s
#' @md
#' @export
setAs("LongTable", "TreatmentResponseExperiment", function(from) {
    TreatmentResponseExperiment(from)
})