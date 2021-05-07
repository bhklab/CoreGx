#' Generic for preprocessing complex data before being used in the constructor
#'   of an `S4` container object.
#' 
#' This method is intended to abstract away complex constructor arguments
#'   and data preprocessing steps needed to transform raw data, such as that
#'   produced in a treatment-response or next-gen sequencing experiment, and 
#'   automate building of the appropriate `S4` container object. This is
#'   is intended to allow mapping between different experimental designs,
#'   in the form of an `S4` configuration object, and various S4 class
#'   containers in the Bioconductor community and beyond.
#' 
#' @param mapper An `S4` object abstracting arguments to an `S4` class
#'   constructor into a well documented `Mapper` object.
#' @param ... Allow new arguments to be defined for this generic.
#' 
#' @return An `S4` object for which the class corresponds to the type of
#'   the build configuration object passed to this method.
#' 
#' @md
#' @export
setGeneric('metaConstruct', function(mapper, ...) standardGeneric('metaConstruct'))


#' 
#'
#' 
#' @md
#' @export
setMethod('metaConstruct', signature(mapper='LongTableDataMapper'), 
    function(mapper)
{
    funContext <- paste0('[', .S4MethodContext('metaConstruct', class(mapper)[1]))
    
    # -- get the rawdata
    DT <- rawdata(mapper)

    # -- preprocess the row identifiers and metadata
    rowDataDT <- unique(DT[, unlist(rowDataMap(mapper)), with=FALSE])
    rowIDs <- rowDataMap(mapper)[[1]]
    rowDataDT[, c('rowKey') := .GRP, by=rowIDs]
    renameRowCols <- names(rowIDs) != ""
    setnames(rowDataDT, rowIDs[renameRowCols], names(rowIDs)[renameRowCols])
    setnames(DT, rowIDs[renameRowCols], names(rowIDs)[renameRowCols])
    rowIDs <- c(rowIDs[!renameRowCols], names(rowIDs)[renameRowCols])

    # -- preprocess the col identifiers and metadata
    colDataDT <- unique(DT[, unlist(colDataMap(mapper)), with=FALSE])
    colIDs <- colDataMap(mapper)[[1]]
    colDataDT[, c('colKey') := .GRP, by=colIDs]
    renameColCols <- names(colIDs) != ""
    setnames(colDataDT, colIDs[renameColCols], names(colIDs)[renameColCols])
    setnames(DT, colIDs[renameColCols], names(colIDs)[renameColCols])
    colIDs <- c(colIDs[!renameColCols], names(colIDs)[renameColCols])

    # -- extract LongTable level metadata
    metadataL <- lapply(metadataMap(mapper), 
        function(j, x) as.list(unique(x[, j, with=FALSE])), x=DT)

    # -- 
    assayIDs <- c(rowIDs, colIDs)
    assayColumns <- lapply(assayMap(mapper), FUN=c, assayIDs)
    assayDtL <- lapply(assayColumns, FUN=subset, x=DT, subset=TRUE)

    ## TODO:: make this prettier with pipes once R 4.1 launches
    for (i in seq_along(assayDtL)) {
        assayDT <- copy(assayDtL[[i]])
        assayDT <- merge(assayDT, rowDataDT[, c('rowKey', ..rowIDs)], by=rowIDs)
        assayDT <- merge(assayDT, colDataDT[, c('colKey', ..colIDs)], by=colIDs)
        assayDT[, c(rowIDs, colIDs) := NULL]
        setkeyv(assayDT, c('rowKey', 'colKey'))
        notMissingNames <- names(assayColumns[[i]]) != ""
        if (sum(notMissingNames) > 0)
            setnames(assayDT, old=assayColumns[[i]][notMissingNames], 
                new=names(assayColumns[[i]])[notMissingNames])
        assayDtL[[i]] <- assayDT
    }

    CoreGx:::LongTable(
        rowData=rowDataDT, rowIDs=rowIDs,
        colData=colDataDT, colIDs=colIDs,
        assays=assayDtL,
        metadata=metadataL
        )
})