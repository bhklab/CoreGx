#' @include DataMapper-class.R
#' @include LongTableDataMapper-class.R
#' @include TreatmentResponseExperiment-class.R
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
.TREDataMapper <- setClass("TREDataMapper",  contains=c("LongTableDataMapper"))

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
#' @param assayMap A list-like where each item is a `character` vector of
#' `rawdata` column names to assign to an assay, where the name of that assay
#' is the name of the list item. If names are omitted, assays will be numbered
#' by their index in the list
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
TREDataMapper <- function(rawdata=list(), rowDataMap=list(),
    colDataMap=list(), assayMap=list(), metadataMap=list()
) {
    funContext <- "[CoreGx::TREDataMapper]\n\t"

    ## TODO:: input validation with checkmate

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