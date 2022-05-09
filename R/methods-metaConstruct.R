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

#' @rdname metaConstruct
#' @title metaConstruct
#'
#' @param mapper An `LongTableDataMapper` object abstracting arguments to an
#'  the `LongTable` constructor.
#'
#' @return A `LongTable` object, as specified in the mapper.
#'
#' @examples
#' data(exampleDataMapper)
#' rowDataMap(exampleDataMapper) <- list(c('treatmentid'), c())
#' colDataMap(exampleDataMapper) <- list(c('sampleid'), c())
#' assayMap(exampleDataMapper) <- list(sensitivity=c('viability'))
#' metadataMap(exampleDataMapper) <- list(experiment_metadata=c('metadata'))
#' longTable <- metaConstruct(exampleDataMapper)
#' longTable
#'
#' @md
#' @importFrom data.table key
#' @export
setMethod('metaConstruct', signature(mapper='LongTableDataMapper'),
        function(mapper) {
    funContext <- paste0('[', .S4MethodContext('metaConstruct', class(mapper)[1]))

    # subset the rawdata slot to build out each component of LongTable
    rowDataDT <- rowData(mapper)
    rowIDs <- key(rowDataDT)
    colDataDT <- colData(mapper)
    colIDs <- key(colDataDT)
    assayDtL <- assays(mapper)
    assayIDs <- lapply(assayMap(mapper), `[[`, i=1)
    for (i in seq_along(assayIDs)) {
        aid_names <- names(assayIDs[[i]])
        notEmptyNames <- !is.null(aid_names) & aid_names != ""
        assayIDs[[i]][notEmptyNames] <- aid_names[notEmptyNames]
    }

    # subset the metadata columns out of raw data and add any additional metadata
    metadataL <- lapply(metadataMap(mapper),
        function(j, x) as.list(unique(x[, j, with=FALSE])), x=rawdata(mapper))
    metadataL <- c(metadataL, metadata(mapper))

    ## FIXME:: Handle TREDataMapper class after updating constructor
    object <- CoreGx::LongTable(
        rowData=rowDataDT, rowIDs=rowIDs,
        colData=colDataDT, colIDs=colIDs,
        assays=assayDtL, assayIDs=assayIDs,
        metadata=metadataL)

    return(object)
})