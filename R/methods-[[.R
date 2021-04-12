# ==== LongTable Class

#' [[ Method for LongTable Class
#'
#' Select an assay from within a LongTable object.
#'
#' @describeIn LongTable Get an assay from a LongTable object. This method
#'   returns the row and column annotations by default to make assignment
#'   and aggregate operations easiers.
#'
#' @examples
#' merckLongTable[['viability']]
#'
#' @param x [`LongTable`] object to retrieve assays from
#' @param i [`character`] name or [`integer`] index of the desired assay.
#' @param withDimnames [`logical`] Should the row and column IDs be joined to
#'    the assay. Default is TRUE to allow easy use of group by arguments when
#'    performing data aggregation using the `data.table` API.
#' @param metadata [`logical`] Should the row and column metadata also
#'    be joined to the to the returned assay. Default is withDimnames.
#' @param keys [`logical`] Should the row and column keys also be returned?
#'    Defaults to !withDimnames.
#'
#' @importFrom crayon cyan magenta
#' @import data.table
#' @export
setMethod('[[', signature('LongTable'),
    function(x, i, withDimnames=TRUE, metadata=withDimnames, keys=!withDimnames) {

    if (metadata && !withDimnames) {
        warning(.warnMsg('\nUnable to return metadata without dimnames, proceeding',
            ' as if withDimnames=TRUE.'))
        withDimnames <- TRUE
    }

    if (length(i) > 1)
        stop(.errorMsg('\nPlease only select one assay! To subset on multiple',
            'assays please see ?subset'))

    if (keys) {
        warning(.warnMsg('\nIgnoring withDimnames and metadata arguments when',
            ' keys=TRUE.'))
        return(assay(x, i))
    } else {
        if (withDimnames || metadata)
            return(assay(x, i, withDimnames, metadata))
        else
            return(assay(x, i)[, -c('rowKey', 'colKey')])
    }
})