#' Generic for Guessing the Mapping Between Some Raw Data and an S4 Object
#' 
#' @param object An `S4` object containing so raw data to guess data to
#'   object slot mappings for.
#' @param ... Allow new arguments to be defined for this generic.
#' 
#' @return An `S4` object with mapping guesses in the appropriate slots.
#' 
#' @md
#' @export
setGeneric('guessMapping', function(object, ...) standardGeneric('guessMapping'))

#' Guess which columns in raw experiment data map to which dimensions.
#' 
#' Checks for columns which are uniquely identified 
#' 
#' @param mapper A `LongTableDataMapper` object.
#' @param groups A `list` containing one or more vector of column names
#'   to group-by. The function uses these to determine 1:1 mappings between
#'   the combination of columns in each vector and unique values in the raw
#'   data columns.
#' @param subset A `logical` vector indicating whether to to subset out mapped
#'   columns after each grouping. Must be a single `TRUE` or `FALSE` or have
#'   the same length as groups, indicating whether to subset out mapped columns
#'   after each grouping.
#' 
#' 
#' @md
#' @export
setMethod('guessMapping', signature(object='LongTableDataMapper'), 
    function(object, groups, subset)
{
    funContext <- .context(1)
    
    mapData <- rawdata(object)
    if (!is.data.table(mapData)) setDT(mapData)

    if (length(subset) != length(groups) && !length(subset) == 1)
        stop(.errorMsg(funContext, ' The subset parameter must be
            either length 1 or length equal to the groups parameter!'))
    
    if (length(subset) == 1) subset <- rep(subset, length(groups))

    metadataColumns <- names(which(vapply(mapData, FUN=.length_unique, 
        numeric(1)) == 1))
    metadata <- mapData[, .SD, .SDcols=metadataColumns]

    DT <- mapData[, .SD, .SDcols=!metadataColumns]

    for (i in seq_along(groups)) {
        message(funContext, paste0('Mapping to ', 
            paste0(groups[[i]], collapse=', '), ' columns.'))
        mappedCols <- checkColumnCardinality(DT, groups[[i]])
        assign(names(groups)[i], DT[, .SD, .SDcols=mappedCols])
        if (subset[i]) DT <- DT[, .SD, .SDcols=!mappedCols]
    }
    mappings <- mget(c('metadata', names(groups)))
    unmapped <- setdiff(colnames(mapData), 
        unique(c(unlist(groups), Reduce(c, lapply(mappings, colnames)))))
    if (length(unmapped) > 0) mappings[['unmapped']] <- unmapped

    return(mappings)
})

#' Search a data.frame for 1:`cardinality` relationships between a group
#'   of columns (your identifiers) and all other columns.
#' 
#' @param df A `data.frame` to search for 1:`cardinality` mappings with
#'   the columns in `group`.
#' @param group A `character` vector of one or more column names to 
#'   check the cardinality of other columns against.
#' @param cardinality The cardinality of to search for (i.e., 1:`cardinality`)
#'   relationships with the combination of columns in group. Defaults to 1 
#'   (i.e., 1:1 mappings).
#' 
#' @aliases cardinality
#' 
#' @md
#' @export
checkColumnCardinality <- function(df, group, cardinality=1) {
    if (!is.data.table(df)) setDT(df)

    dimDT <- df[, lapply(.SD, FUN=.length_unique), by=group]
    columnsHaveCardinality <- names(which(
        vapply(dimDT, .all_equals, y=cardinality, logical(1))))

    return(columnsHaveCardinality)
}
#' @export
cardinality <- checkColumnCardinality