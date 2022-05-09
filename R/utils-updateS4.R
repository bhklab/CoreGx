#' Convert the old sensitivity slot format into a `LongTable` and update the
#' `CoreSet` object.
#'
#' @param object Inheriting from `CoreSet`.
#' @param mapper Should the `LongTableDataMapper` object be early returned,
#' instead of the `LongTable` object. This can be useful if the conversion
#' fails or corrupts your data. You can then modify the `DataMapper` as
#' necessary to fix the sensititivity data.
#'
#' @return A `LongTable` constructed from `object@treatmentResponse`, or a
#' `LongTableDataMapper` if `mapper`=TRUE.
#'
#' @keywords internal
#' @noRd
#' @importFrom data.table data.table as.data.table merge.data.table
#' melt.data.table
.sensitivityToTRE <- function(object, mapper=FALSE) {

    # -- validate input
    funContext <- .funContext(':::.sensitivitySlotToLongTable')
    if (!is(object, 'CoreSet')) .error(funContext, ' object must inherit from
        the CoreSet class.')
    oldSensitivity <- sensitivitySlot(object)

    if (!is(oldSensitivity, 'list')) .error(funContext, ' @sensitivty slot
        is not a `list`?')

    # -- extract the old data as data.tables

    # sensitivityInfo
    infoDT <- as.data.table(oldSensitivity$info, keep.rownames=TRUE)
    rowCols <- c(treatment1id="treatmentid", treatment1dose='dose')
    colCols <- c(sampleid="sampleid")

    # sensitivityProfiles
    profDT <- as.data.table(oldSensitivity$profiles, keep.rownames=TRUE)

    # sensitivityRaw
    doseDT <- as.data.table(oldSensitivity$raw[, , 1], keep.rownames=TRUE)
    meltedDoseDT <- na.omit(melt.data.table(doseDT, id.vars='rn',
        variable.name='old_column', value.name='dose'))
    meltedDoseDT[, dose := as.numeric(dose)]
    viabDT <- as.data.table(oldSensitivity$raw[, , 2], keep.rownames=TRUE)
    meltedViabDT <- na.omit(melt.data.table(viabDT, id.vars='rn',
        variable.name='old_column', value.name='viability'))
    meltedViabDT[, viability := as.numeric(viability)]

    # -- merge into a single long format data.table
    assayDT <- merge.data.table(meltedDoseDT, meltedViabDT,
        by=c('rn', 'old_column'))
    assayMap <- list(sensitivity=c('viability'),
        profiles=setdiff(colnames(profDT), 'rn'))

    rawdataDT <- merge.data.table(assayDT, profDT, by='rn')
    rawdataDT <- merge.data.table(rawdataDT, infoDT, by='rn')
    rawdataDT[, replicate_id := seq_len(.N), by=c(rowCols, colCols)]

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

    # -- capute the na rownames to make recreation easier in .rebuildInfo
    missing_rows <- setdiff(infoDT$rn, rawdataDT$rn)
    na_index <- infoDT[rn %in% missing_rows, .(rn, treatmentid, sampleid)]

    # -- build a LongTableDataMapper object
    TREdataMapper <- TREDataMapper(rawdata=rawdataDT)
    guess <- guessMapping(TREdataMapper, groups, subset=TRUE)

    assayCols <- unlist(assayMap)

    # do not steal any assay columns for the row or column data
    guess$rowDataMap[[2]] <- setdiff(guess$rowDataMap[[2]], assayCols)
    guess$colDataMap[[2]] <- setdiff(guess$colDataMap[[2]], assayCols)
    guess$metadata[[2]] <- setdiff(guess$metadata[[2]],
        c(assayCols, guess$rowDataMap[[2]], guess$colDataMap[[2]]))
    assayMap$assay_metadata <- setdiff(guess$assayMap$mapped_columns, assayCols)

    # set the names correctly

    # update the data mapper
    rowDataMap(TREdataMapper) <- guess$rowDataMap
    colDataMap(TREdataMapper) <- guess$colDataMap
    assayMap(TREdataMapper) <- assayMap
    metadataMap(TREdataMapper) <-
        list(experiment_metadata=guess$metadata$mapped_columns)
    metadata(TREdataMapper) <- list(sensitivityInfo_NA=na_index)

    # build the object
    return(if (!mapper) metaConstruct(TREdataMapper) else TREdataMapper)
}


#' Compare the valus of sensitivityInfo before and after use of
#' .sensitivityToTRE
#'
#' @param object `CoreSet` to be updated to the new
#' `TreatmentResponseExperiment` sensitivity format.
#'
#' @return None, displays results of `all.equal` on the sensitivityInfo for
#'   the columns which should be conserved.
#'
#' @keywords internal
#' @noRd
#' @importFrom data.table data.table as.data.table merge.data.table
#' melt.data.table
.compareSensitivityInfo <- function(object) {
    new_object <- copy(object)
    tre <- .sensitivityToTRE(object)
    new_object@treatmentResponse <- tre

    si <- copy(sensitivityInfo(object))
    nsi <- copy(sensitivityInfo(new_object))

    setDT(si, keep.rownames="rownames")
    setDT(nsi, keep.rownames="rownames")

    equal_columns <- setdiff(colnames(si), "rownames")
    all.equal(
        si[order(treatmentid, sampleid), .SD, .SDcols=equal_columns],
        nsi[order(treatmentid, sampleid), .SD, .SDcols=equal_columns]
    )
}