#' Convert the old sensitivity slot format into a `LongTable` and update the
#' `CoreSet` object.
#'
#' @param object Inheriting from `CoreSet`.
#' @param mapper Should the `LongTableDataMapper` object be early returned,
#' instead of the `LongTable` object. This can be useful if the conversion
#' fails or corrupts your data. You can then modify the `DataMapper` as 
#' necessary to fix the sensititivity data.
#'
#' @return A `LongTable` constructed from `object@sensitivty`, or a
#' `LongTableDataMapper` if `mapper==TRUE`.
#'
#' @keywords internal
#' @importFrom data.table data.table as.data.table merge.data.table 
#' melt.data.table
.sensitivityToLongTable <- function(object, mapper=FALSE) {
    
    # -- validate input
    funContext <- .funContext(':::.sensitivitySlotToLongTable')
    if (!is(object, 'CoreSet')) .error(funContext, ' object must inherit from
        the CoreSet class.')
    oldSensitivity <- sensitivitySlot(object)

    if (!is(oldSensitivity, 'list')) .error(funContext, ' @sensitivty slot
        is not a `list`?')

    # -- extact the old data as data.tables

    # sensitivityInfo
    infoDT <- as.data.table(oldSensitivity$info, keep.rownames=TRUE)
    rowCols <- c(drug1id=colnames(infoDT)[3], drug1dose='dose')
    colCols <- colnames(infoDT)[2]
    names(colCols) <- 'cellid'

    # sensitivityProfiles
    profDT <- as.data.table(oldSensitivity$profiles, keep.rownames=TRUE)

    # sensitivityRaw
    doseDT <- as.data.table(oldSensitivity$raw[, , 1], keep.rownames=TRUE)
    meltedDoseDT <- melt.data.table(doseDT, id.vars='rn', 
        variable.name='old_column', value.name='dose')
    viabDT <- as.data.table(oldSensitivity$raw[, , 2], keep.rownames=TRUE)
    meltedViabDT <- melt.data.table(viabDT, id.vars='rn', 
        variable.name='old_column', value.name='viability')
    
    # -- merge into a single long format data.table
    assayDT <- merge.data.table(meltedDoseDT, meltedViabDT, 
        by=c('rn', 'old_column'))
    assayMap <- list(sensitivity=c('viability'), 
        profiles=setdiff(colnames(profDT), 'rn'))

    rawdataDT <- merge.data.table(assayDT, profDT, by='rn')
    rawdataDT <- merge.data.table(rawdataDT, infoDT, by='rn')
    # Find any hidden replicates
    rawdataDT[, replicate_id := seq_len(.N), by=c(rowCols[1], colCols, 
        'old_column')]

    if (max(rawdataDT$replicate_id) > 1) {
        # Handle case where there is only 1 drug (i.e., radiation in RadioGx)
        if (length(unique(rawdataDT[[rowCols[1]]])) == 1) {
            rowCols <- c(rowCols, 'replicate_id')
        } else {
            colCols <- c(colCols, 'replicate_id')
        }
    } else {
        rawdataDT[, replicate_id := NULL]
    }

    groups <- list(
        rowDataMap=rowCols,
        colDataMap=colCols,
        assayMap=c(rowCols, colCols)
    )

    # -- build a LongTableDataMapper object
    LTdataMapper <- LongTableDataMapper(rawdata=rawdataDT)
    guess <- guessMapping(LTdataMapper, groups, subset=TRUE)
    
    assayCols <- unlist(assayMap)

    # do not steal any assay columns for the row or column data
    guess$rowDataMap[[2]] <- setdiff(guess$rowDataMap[[2]], assayCols)
    guess$colDataMap[[2]] <- setdiff(guess$colDataMap[[2]], assayCols)
    guess$metadata[[2]] <- setdiff(guess$metadata[[2]], 
        c(assayCols, guess$rowDataMap[[2]], guess$colDataMap[[2]]))
    assayMap$assay_metadata <- setdiff(guess$assayMap$mapped_columns, assayCols)

    # set the names correctly

    # update the data mapper
    rowDataMap(LTdataMapper) <- guess$rowDataMap
    colDataMap(LTdataMapper) <- guess$colDataMap
    assayMap(LTdataMapper) <- assayMap
    metadataMap(LTdataMapper) <- 
        list(experiment_metadata=guess$metadata$mapped_columns)
    
    # build the object
    return(if (!mapper) metaConstruct(LTdataMapper) else LTdataMapper)
}