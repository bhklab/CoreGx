#' Retrieve the assay names from a `LongTable` object.
#'
#' @examples
#' assayNames(merckLongTable)
#'
#' @describeIn LongTable Return the names of the assays contained in a
#'   `LongTable`
#'
#' @param x A `LongTable` object to retrieve the assay names from.
#'
#' @return `character` Names of the assays contained in the `LongTable`.
#'
#' @importMethodsFrom SummarizedExperiment assayNames
#' @export
setMethod('assayNames', signature(x='LongTable'), function(x) {
    return(names(x@assays))
})