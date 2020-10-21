# ==== LongTable Methods

#' Get the symbol(s) x from the object@.intern slot of a LongTable
#'
#' This is used as an alternative to R attributes for storing structural
#' metadata of S4 objects.
#'
#' @examples
#' getIntern(merckLongTable, 'rowIDs')
#' getIntern(merckLongTable, c('colIDs', 'colMeta'))
#'
#' @describeIn LongTable Access structural metadata present within a
#'   LongTable object. This is mostly for developmer use.
#'
#' @param object [`LongTable`]
#' @param x [`character`] One or more symbol name strings to retrieve from
#'     the object@.intern environment.
#' @param ... [`pairlist`] Addtional arguments to get or mget inside of the
#'     function.
#'
#' @return value of x if length(x) == 1 else named list of values for all
#'     symbols in x
#'
#' @include class-LongTable.R
#' @export
setMethod('getIntern', signature(object='LongTable', x='character'),
    function(object, x, ...) {

    if (length(x) > 1)
        tryCatch({ mget(x, envir=object@.intern, ...) },
            error=function(e) {
                message(e); mget(x, envir=object@.intern) })
    else
        tryCatch({ get(x, envir=object@.intern, ...) },
            error=function(e) {
                message(e); get(x, envir=object@.intern) })
})