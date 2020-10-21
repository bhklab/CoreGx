# ==== LongTable Class

#' Select an assay from a LongTable object
#'
#' @examples
#' merckLongTable$viability
#'
#' @param x A [`LongTable`] object to retrieve an assay from
#' @param name [`character`] The name of the assay to get.
#'
#' @return [`data.frame`] The assay object.
#'
#' @export
setMethod('$', signature('LongTable'), function(x, name) {
    # error handling is done inside `[[`
    x[[name]]
})