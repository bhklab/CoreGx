# ==== LongTable

#' Get the dimensions of a `LongTable` object.
#'
#' @examples
#' dim(merckLongTable)
#'
#' @describeIn LongTable Get the number of row annotations by the number of
#'   column annotations from a LongTable object. Please note that row x columns
#'   does not necessarily equal the number of rows in an assay, since it is
#'   not required for each assay to have every row or column present.
#'
#' @examples
#' dim(merckLongTable)
#'
#' @param x A `LongTable` object to retrieve dimensions for.
#'
#' @return `numeric` Vector of object dimensions.
#'
#' @importMethodsFrom SummarizedExperiment dim
#' @export
setMethod('dim', signature(x='LongTable'), function(x) {
    return(c(nrow(rowData(x)), nrow(colData(x))))
})