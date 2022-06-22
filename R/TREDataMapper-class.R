#' @include DataMapper-class.R
#' @include LongTableDataMapper-class.R
#' @include TreatmentResponseExperiment-class.R
#' @include LongTableDataMapper-accessors.R
NULL

#' A Class for Mapping Between Raw Data and an `TreatmentResponseExperiment`
#'   Object
#'
#' @slot rawdata See Slots section.
#' @slot rowDataMap See Slots section.
#' @slot colDataMap See Slots section.
#' @slot assayMap See Slots section.
#' @slot metadataMap See Slots section.
#'
#' @inheritSection LongTableDataMapper-class Slots
#'
#' @md
#' @aliases TREDataMapper-class
.TREDataMapper <- setClass("TREDataMapper", contains=c("LongTableDataMapper"))

#' Constructor for the `TREDataMapper` class, which maps from one or
#'   more raw experimental data files to the slots of a `LongTable` object.
#'
#' @details
#' The `guessMapping` method can be used to test hypotheses about the
#' cardinality of one or more sets of identifier columns. This is helpful
#' to determine the id columns for `rowDataMap` and `colDataMap`, as well
#' as identify columns mapping to `assays` or `metadata`.
#'
#' To attach metadata not associated with `rawdata`, please use the `metadata`
#' assignment method on your `TREDataMapper`. This metadata will be
#' merge with any metadata from `metadataMap` and added to the `LongTable`
#' which this object ultimately constructs.
#'
#' @param rawdata A `data.frame` of raw data from a treatment response
#' experiment. This will be coerced to a `data.table` internally. We recommend
#' using joins to aggregate your raw data if it is not present in a single file.
#' @param rowDataMap A list-like object containing two `character` vectors.
#' The first is column names in `rawdata` needed to uniquely identify each row,
#' the second is additional columns which map to rows, but are not required to
#' uniquely identify them. Rows should be treatments.
#' @param colDataMap A list-like object containing two `character` vectors.
#' The first is column names in `rawdata` needed to uniquely identify each
#' column, the second is additional columns which map to rows, but are not
#' required to uniquely identify them. Columns should be samples.
#' @param assayMap A list-like where each item is a `list` with two `character`
#' vectors defining an assay, the first containing the identifier columns in
#' `rawdata` needed to uniquely identify each row an assay, and the second the
#' `rawdata` columns to be mapped to that assay. The names of `assayMap`
#' will be the names of the assays in the `TreatmentResponseExperiment` that
#' is created when calling `metaConstruct` on this `DataMapper` object. If the
#' character vectors have names, the value columns will be renamed accordingly.
#' @param metadataMap A list-like where each item is a `character` vector of
#' `rawdata` column names to assign to the `@metadata` of the `LongTable`,
#' where the name of that assay is the name of the list item. If names are
#' omitted, assays will be numbered by their index in the list.
#'
#' @return A `TREDataMapper` object, with columns mapped to it's slots according
#' to the various maps in the `LongTableDataMapper` object.
#'
#' @seealso [`guessMapping`]
#'
#' @md
#' @importFrom data.table setDT
#' @export
TREDataMapper <- function(rawdata=data.frame(),
        rowDataMap=list(character(), character()),
        colDataMap=list(character(), character()),
        assayMap=list(list(character(), character())),
        metadataMap=list(character())) {

    if (is(rawdata, "LongTableDataMapper")) {
        lt_dm <- rawdata
    } else {
        lt_dm <- LongTableDataMapper(rawdata=rawdata, rowDataMap=rowDataMap,
            colDataMap=colDataMap, assayMap=assayMap, metadataMap=metadataMap)
    }

    .TREDataMapper(
        rawdata=rawdata(lt_dm),
        rowDataMap=rowDataMap(lt_dm),
        colDataMap=colDataMap(lt_dm),
        assayMap=assayMap(lt_dm),
        metadataMap=metadata(lt_dm)
    )
}

## ===========================================
## TREDataMapper Accessors Documentation
## -------------------------------------------

.local_class_4 <- "TREDataMapper"
.local_data_4 <- "exampleDataMapper"

#' @name TREDataMapper-accessors
#'
#' @eval .docs_DataMapper_accessors(class_=.local_class_4)
#' @eval .docs_DataMapper_get_rawdata(class_=.local_class_4)
#'
#' @param value See details.
NULL


## ---------------
## -- rawdata slot


#' @rdname TREDataMapper-accessors
#' @eval .docs_DataMapper_set_rawdata(class_=.local_class_4,
#' class1_='list')
setReplaceMethod("rawdata", signature=c(object="TREDataMapper",
        value="list"), function(object, value) {
    callNextMethod(object=object, value=value)
})


## --------------------
## ---- rowDataMap slot


##
## -- rowDataMap

#' @rdname TREDataMapper-accessors
#' @eval
#' .docs_LongTableDataMapper_get_dimDataMap(dim_='row', class_=.local_class_4,
#' data_=.local_data_4)
#' @aliases rowDataMap
setMethod('rowDataMap', signature(object='TREDataMapper'), function(object) {
    callNextMethod()
})


#' @rdname TREDataMapper-accessors
#' @eval
#' .docs_LongTableDataMapper_set_dimDataMap(dim_='row', class_=.local_class_4,
#' data_=.local_data_4, id_col_='treatmentid')
#' @aliases rowDataMap<-
setReplaceMethod('rowDataMap', signature(object='TREDataMapper',
        value='list_OR_List'), function(object, value) {
    callNextMethod()
})


##
## -- rowData


#' Convenience method to subset the `rowData` out of the `rawdata` slot using
#'   the assigned `rowDataMap` metadata.
#'
#' @param x `TREDataMapper` object with valid data in the `rawdata` and
#'   `colDataMap` slots.
#' @param key `logical(1)` Should the table be keyed according to the
#'   `id_columns` of the `rowDataMap` slot? This will sort the table in memory.
#'   Default is TRUE.
#'
#' @return `data.table` The `rowData` as specified in the `rowDataMap` slot.
#'
#' @export
setMethod("rowData", signature(x="TREDataMapper"), function(x, key=TRUE) {
    callNextMethod()
})


## --------------------
## ---- colDataMap slot


##
## -- colDataMap

#' @rdname TREDataMapper-accessors
#' @eval
#' .docs_LongTableDataMapper_get_dimDataMap(dim_='col', class_=.local_class_4,
#' data_=.local_data_4)
#' @aliases colDataMap
setMethod('colDataMap', signature(object='TREDataMapper'),
        function(object) {
    callNextMethod()
})

#' @rdname TREDataMapper-accessors
#' @eval
#' .docs_LongTableDataMapper_set_dimDataMap(dim_='col', class_=.local_class_4,
#' data_=.local_data_4, id_col_='sampleid')
#' @aliases colDataMap<-
setReplaceMethod('colDataMap',
        signature(object="TREDataMapper", value="list_OR_List"),
        function(object, value) {
    callNextMethod()
})


##
## -- colData


#' Convenience method to subset the `colData` out of the `rawdata` slot using
#'   the assigned `colDataMap` metadata.
#'
#' @param x `TREDataMapper` object with valid data in the `rawdata` and
#'   `colDataMap` slots.
#' @param key `logical(1)` Should the table be keyed according to the
#'   `id_columns` of the `colDataMap` slot? This will sort the table in memory.
#'   Default is TRUE.
#'
#' @return `data.table` The `colData` as specified in the `colDataMap` slot.
#'
#' @export
setMethod("colData", signature(x="TREDataMapper"), function(x, key=TRUE) {
    callNextMethod()
})


## ----------------
## ---- assayMap slot

#' @rdname TREDataMapper-accessors
#' @eval .docs_LongTableDataMapper_get_assayMap(class_=.local_class_3, data_=.local_data_3)
setMethod('assayMap', signature(object='TREDataMapper'),
        function(object) {
    callNextMethod()
})


#' @rdname TREDataMapper-accessors
#' @eval .docs_LongTableDataMapper_set_assayMap(class_=.local_class_4, data_=.local_data_4)
setReplaceMethod('assayMap', signature(object='TREDataMapper',
        value='list_OR_List'), function(object, value) {
    callNextMethod()
})


#' Extract the data for an assay from a `TREDataMapper`
#'
#' @param x `TREDataMapper` The object to retrive assay data form according
#'   to the `assayMap` slot.
#' @param i `character(1)` Name of an assay in the `assayMap` slot of `x`.
#' @param withDimnames `logical(1)` For compatibility with
#'   `SummarizedExperiment::assay` generic. Not used.
#'
#' @return `data.table` Data for the specified assay extracted from the
#'   `rawdata` slot of `x`.
#'
#' @importFrom checkmate assertSubset assertCharacter
#' @keywords internal
setMethod("assay", signature(x="TREDataMapper"),
        function(x, i, withDimnames=TRUE) {
    callNextMethod()
})


#' Extract the data for all assays from a `TREDataMapper`
#'
#' @param x `TREDataMapper` The object to retrive assay data form according
#'   to the `assayMap` slot.
#' @param withDimnames `logical(1)` For compatibility with
#'   `SummarizedExperiment::assay` generic. Not used.
#'
#' @return `list` Data for all assays extracted from the
#'   `rawdata` slot of `x` as a `list` of `data.tables`, where the `keys` for
#'   each table are their `id_columns`.
#'
#' @importFrom checkmate assertSubset assertCharacter
#' @keywords internal
setMethod("assays", signature(x="TREDataMapper"),
        function(x, withDimnames=TRUE) {
    callNextMethod()
})

# -- metadataMap

#' @rdname TREDataMapper-accessors
#' @eval .docs_LongTableDataMapper_get_metadataMap(class_=.local_class_4,
#' data_=.local_data_4)
setMethod('metadataMap', signature(object='TREDataMapper'),
        function(object) {
    callNextMethod()
})


#' @rdname TREDataMapper-accessors
#' @eval .docs_LongTableDataMapper_set_metadataMap(class_=.local_class_4,
#' data_=.local_data_4, col_='metadata')
setReplaceMethod('metadataMap', signature(object='TREDataMapper',
        value='list_OR_List'), function(object, value) {
    callNextMethod()
})