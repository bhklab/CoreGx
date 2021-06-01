#' @include CoreSet-class.R CoreSet-accessors.R
NULL

.local_class <- 'CoreSet'
.local_data <- 'clevelandSmall_cSet'


#### CoreGx dynamic documentation
####
#### Warning: for dynamic docs to work, you must set
#### Roxygen: list(markdown = TRUE, r6=FALSE)
#### in the DESCRPTION file!


# ===================================
# Utility Method Documentation Object
# -----------------------------------


#' @noRd
.docs_CoreSet_utils <- function(...) .parseToRoxygen(
    "
    @title Utility methods for a `{class_}` object.

    @description
    Documentation for utility methods for a `{class_}` object, such as
    set operations like subset and intersect. See @details for information
    on different types of methods and their implementations.

    @param x A `{class_}` object.
    @param samples A `character` vector of sample names. Must be valid rownames
    from `cellInfo(x)`.

    @return See details.
    ",
    ...
)

#' @name CoreSet-utils
#' @eval .docs_CoreSet_utils(class_=.local_class)
#' @eval .parseToRoxygen("@examples data({data_})", data_=.local_data)
NULL


# ======================================
# Subset Methods
# --------------------------------------


## ===================
## ---- subsetBySample
## -------------------


#' @export
setGeneric('subsetBySample', function(x, samples, ...) 
    standardGeneric('subsetBySample'))

#' @noRd
.docs_CoreSet_subsetBySample <- function(...) .parseToRoxygen(
    "
    @details

    ## subset methods
    __subsetBySample__: Subset a `{class_}` object by sample identifier.
    - value: a `{class_}` object containing only `samples`.

    @examples

    ## subset methods

    ### subsetBySample
    samples <- cellInfo({data_})$cellid[seq_len(10)]
    {data_}_sub <- subsetBySample({data_}, samples)

    @md
    @aliases subsetBySample subsetBySample,CoreSet-method
    @exportMethod subsetBySample
    ",
    ...
)

#' @rdname CoreSet-utils
#' @eval .docs_CoreSet_subsetBySample(class_=.local_class, data_=.local_data)
setMethod('subsetBySample', signature('CoreSet'), function(x, samples) {

    funContext <- .S4MethodContext('subsetBySample', 'CoreSet')

    sampleNames <- rownames(cellInfo(x))
    if (!all(samples %in% sampleNames)) {
        .warning(funContext, 'Samples missing from ', class(x)[1], ': ',
            setdiff(samples, sampleNames), '! Please ensure all specified
            samples are valid rownames of cellInfo(x). Proceeding with
            the valid samples only.')
        samples <- union(samples, sampleNames)
    }

    # -- molecularProfiles slot
    molecSlot <- molecularProfilesSlot(x)
    molecularProfilesSlot(x) <- 
        .subsetMolecularProfilesBySample(molecSlot, samples)

    # -- sensitivity slot
    sensSlot <- sensitivitySlot(x)
    sensitivitySlot(x) <- .subsetSensitivityBySample(sensSlot, samples)

    # -- perturbatiion slot
    ##TODO:: do we still need this?

    # -- curation slot
    sampleCuration <- curation(x)$cell
    curation(x)$cell <- sampleCuration[rownames(sampleCuration) %in% samples, ]

    # -- cell slot
    cellInf <- cellInfo(x)
    cellInfo(x) <- cellInf[rownames(cellInf) %in% samples, ]

    # -- check object is still valid and return
    checkCsetStructure(x)

    return(x)
})

.subsetMolecularProfilesBySample <- function(slotData, samples) {
    funContext <- .funContext(':::.subsetMolecularProfilesBySample')
    if (is(slotData, 'MultiAssayExperiment')) {
        hasSamples <- colData(slotData)$cellid %in% samples
        if (!all(hasSamples)) .warning(funContext, 'Some specified samples are 
            not present in `molecularProfilesSlot(x)`')
        molecProfs <- slotData[, hasSamples]
    } else {
        SEcolData <- lapply(slotData, colData)
        SEsamples <- lapply(SEcolData, FUN=`[[`, i='cellid')
        hasSEsamples <- lapply(SEsamples, FUN=`%in%`, samples)
        molecProfs <- mapply(`[`, x=slotData, j=hasSEsamples)
    }
    return(molecProfs)
}

.subsetSensitivityBySample <- function(slotData, samples) {
    funContext <- .funContext(':::.subsetSensitivityBySample')
    if (is(slotData, 'LongTable')) {
        slotData <- slotData[, samples]
    } else {
        keepSamples <- slotData$info$cellid %in% samples
        slotData$profiles <- slotData[keepSamples, ]
        slotData$raw <- slotData$raw[keepSamples, , ]
        slotData$n <- slotData$n[keepSamples, , ]
        slotData$info <- sensitivityInfo(x)[keepSamples, ]
    }
    return(slotData)
}