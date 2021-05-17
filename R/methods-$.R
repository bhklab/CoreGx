# ==== LongTable Class

#' Select an assay from a LongTable object
#'
#' @examples
#' merckLongTable$sensitivity
#'
#' @param x A `LongTable` object to retrieve an assay from
#' @param name `character` The name of the assay to get.
#'
#' @return `data.frame` The assay object.
#'
#' @export
setMethod('$', signature('LongTable'), function(x, name) {
    # error handling is done inside `[[`
    x[[name]]
})

#' Update an assay from a LongTable object 
#'
#' @examples
#' merckLongTable$sensitivity <- merckLongTable$sensitivity
#'
#' @param x A `LongTable` to update an assay for.
#' @param name `character(1)` The name of the assay to update
#' @param value A `data.frame` or `data.table` to update the assay with.
#'
#' @return Updates the assay `name` in `x` with `value`, returning an invsible
#' NULL.
#'
#' @export
setReplaceMethod('$', signature('LongTable'), function(x, name, value) {
    # error handling done inside `assay<-`
    x[[name]] <- value
})