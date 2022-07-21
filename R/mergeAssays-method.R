#' @include LongTable-class.R
#'
#' @importFrom data.table key merge.data.table
#' @import checkmate
NULL

#' Merge assays with an `S4` object.
#'
#' @param object `S4` An S4 object a list-like slot containing assays for the
#'   object.
#' @param ... Allow new arguments to be defined for this generic.
#'
#' @return A modified version of `object`.
#'
#' @examples
#' "This is a generic method!"
#'
#' @exportMethod mergeAssays
setGeneric("mergeAssays", function(object, ...) standardGeneric("mergeAssays"))


#' Endomorphically merge assays within a `LongTable` or inheriting class
#'
#' @param object A `LongTable` or inheriting class.
#' @param x `character(1)` A valid assay name in `object`.
#' @param y `character(1)` A valid assay name in `object`.
#' @param target `character(1)` Name of the assay to assign the result to.
#' Can be a new or existing assay. Defaults to `x`.
#' @param ... Fallthrough arguments to merge.data.table to specify the join
#'   type. Use this to specify which columns to merge on. If excluded, defaults
#'   to by=assayKeys(objecty, y).
#' @param metadata `logical` A logical vector indicating whether to attach
#' metadata to either assay before the merge occurs. If only one value is
#' passed that value is used for both assays. Defaults to `FALSE`.
#'
#' @return A copy of `object` with assays `x` and `y` merged and assigned to
#' `target`.
#'
#' @seealso [`merge.data.table`]
#'
#' @author
#' Christopher Eeles
#'
#' @export
setMethod("mergeAssays", signature("LongTable"),
        function(object, x, y, target=x, ..., metadata=FALSE) {
    checkmate::qassert(target, "S1")
    z <- .merge_longtable_assays(object, x=x, y=y, ...,
        metadata=metadata)
    object[[target]] <- z
    object
})

#' @noRd
.merge_longtable_assays <- function(object, x, y, ..., metadata=FALSE) {
    # -- input validation
    checkmate::qassert(x, "S1")
    checkmate::qassert(y, "S1")
    checkmate::assertChoice(x, assayNames(object))
    checkmate::assertChoice(x, assayNames(object))
    checkmate::qassert(metadata, c("B1", "B2"))
    if (length(metadata) == 1) metadata <- c(metadata, metadata)

    # -- extract assays to merge
    x_ <- assay(object, x, summarize=TRUE, metadata=metadata[1])
    y_ <- if (x == y) x_ else
        assay(object, y, summarize=TRUE, metadata=metadata[2])

    # -- handle by argument
    by <- NA
    by_args <- c("by", "by.x", "by.y")
    if (!(any(by_args %in% ...names())))  {
        by <- assayKeys(object, y)
    }

    if (!is.character(by))
        merge.data.table(x=x_, y=y_, ...)
    else
        merge.data.table(x=x_, y=y_, by=by, ...)
}