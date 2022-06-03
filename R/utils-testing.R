#' Tests of table 1 is a subset of table2, in which case there will be no rows
#' is the set difference.
#'
#' @param table1,table2 A `data.table` or a table-like object coercible to one 
#' via `as.data.table`.
#'
#' @return `logical(1)` TRUE if `table1` is a subset of `table2`, otherwise 
#' `FALSE`
#'
#' @importFrom data.table fsetdiff as.data.table
#'
#' @noRd
#' @keywords internal
.table_is_subset <- function(table1, table2) {
    if (!is(table1, "data.table")) table1 <- as.data.table(table1)
    if (!is(table2, "data.table")) table2 <- as.data.table(table2)
    nrow(fsetdiff(
        table1,
        table2,
    )) == 0
}