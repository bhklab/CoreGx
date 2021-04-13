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
#' @param data A `logical` vector indicating whether you would like the data
#'   for mapped columns to be returned instead of their column names. Defaults
#'   to `FALSE` for easy use assigning mapped columns to a `DataMapper` object.
#' 
#' @return
#'
#' @md
#' @export
setMethod('guessMapping', signature(object='LongTableDataMapper'), 
    function(object, groups, subset, data=FALSE)
{
    funContext <- '[CoreGx::guessMapping,LongTableDataMapper-method]\n\t'
    
    # Extract the raw data
    mapData <- copy(rawdata(object))
    if (!is.data.table(mapData)) setDT(mapData)

    # Error handle for subset parameter
    if (!(length(subset) == length(groups) || length(subset) == 1))
        stop(.errorMsg(funContext, ' The subset parameter must be
            either length 1 or length equal to the groups parameter!'))
    
    if (length(subset) == 1) subset <- rep(subset, length(groups))

    # Map unique columns in the data to the metadata slot
    metadataColumns <- names(which(vapply(mapData, FUN=.length_unique, 
        numeric(1)) == 1))
    metadata <- mapData[, .SD, .SDcols=metadataColumns]

    DT <- mapData[, .SD, .SDcols=!metadataColumns]

    # Check the mappings for each group in groups
    for (i in seq_along(groups)) {
        message(funContext, paste0('Mapping for group ', names(groups)[i], 
            ': ', paste0(groups[[i]], collapse=', ')))
        mappedCols <- checkColumnCardinality(DT, groups[[i]])
        assign(names(groups)[i], DT[, .SD, .SDcols=mappedCols])
        if (subset[i]) DT <- DT[, .SD, .SDcols=!mappedCols]
    }

    # Merge the results
    groups <- c(list(metadata=NA), groups)
    mappings <- mget(names(groups))
    unmapped <- setdiff(colnames(mapData), 
        unique(c(unlist(groups), unlist(lapply(mappings, colnames)))))
    if (!data) mappings <- lapply(mappings, colnames)
    mappings <- mapply(list, groups, mappings, SIMPLIFY=FALSE)
    mappings <- lapply(mappings, `names<-`, value=c('id_columns', 'meta_columns'))

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
#' @param ... Fall through arguments to data.table::`[`. For developer use. 
#'   One use case is setting verbose=TRUE to diagnose slow data.table 
#'   operations.
#' 
#' @return A `character` vector with the names of the columns with
#'    cardinality of 1:`cardinality` with the columns lised in `group`.
#'
#' @aliases cardinality
#' 
#' @md
#' @export
checkColumnCardinality <- function(df, group, cardinality=1, ...) {
    
    funContext <- '[CoreGx::checkColumnCardinality]\n\t'

    # Copy to prevent accidental modify by references
    df <- copy(df)
    if (!is.data.table(df)) setDT(df)

    # Intercept slow data.table group by when nrow == .NGRP
    setindexv(df, cols=group)
    groupDT <- df[, .(group_index = .GRP), by=group, ...]
    if (nrow(df) == max(groupDT$group_index)) {
        if (cardinality != 1) stop(.errorMsg(funContext, 'The group argument 
            uniquely identifies each row, so the cardinality is 1:1!'))
        columnsHaveCardinality <- setdiff(colnames(df), group)
    } else {
        dimDT <- df[, lapply(.SD, FUN=.length_unique), by=group, ...]
        columnsHaveCardinality <- names(which(
            vapply(dimDT, .all_equals, y=cardinality, logical(1))))
    }

    return(columnsHaveCardinality)
}
#' @export
cardinality <- checkColumnCardinality