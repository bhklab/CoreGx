#' @include CoreSet-class.R CoreSet-accessors.R
#' @importFrom BiocGenerics match %in%
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
    @param samples `character()` vector of sample names. Must be valid rownames
    from `sampleInfo(x)`.
    @param treatments `character()` vector of treatment names. Must be valid
    rownames from `treatmentInfo(x)`. This method does not work with
    `CoreSet` objects yet.
    @param features `character()` vector of feature names. Must be valid feature
    names for a given `mDataType`
    @param mDataTypes `character()` One or more molecular data types to
        to subset features by. Must be valid rownames for the selected
        SummarizedExperiment mDataTypes.

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
    samples <- sampleInfo({data_})$sampleid[seq_len(10)]
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

    sampleNames <- rownames(sampleInfo(x))
    if (!all(samples %in% sampleNames)) {
        .warning(funContext, 'Samples missing from ', class(x)[1], ': ',
            setdiff(samples, sampleNames), '! Please ensure all specified
            samples are valid rownames of sampleInfo(x). Proceeding with
            the valid samples only.')
        samples <- union(samples, sampleNames)
    }

    # -- molecularProfiles slot
    molecSlot <- molecularProfilesSlot(x)
    molecularProfilesSlot(x) <-
        .subsetMolecularProfilesBySample(molecSlot, samples)

    # -- sensitivity slot
    sensSlot <- treatmentResponse(x)
    treatmentResponse(x) <- .subsetSensitivityBySample(sensSlot, samples)

    # -- perturbatiion slot
    ##TODO:: do we still need this?
    
    # -- curation slot
    sampleCuration <- curation(x)$sample
    curation(x)$sample <- sampleCuration[rownames(sampleCuration) %in% samples, ]

    # -- sample slot
    sampleInf <- sampleInfo(x)
    sampleInfo(x) <- sampleInf[rownames(sampleInf) %in% samples, ]

    # -- check object is still valid and return
    tryCatch(checkCsetStructure(x), error = function(e) {})

    return(x)
})

.subsetMolecularProfilesBySample <- function(slotData, samples) {
    funContext <- .funContext(':::.subsetMolecularProfilesBySample')
    if (is(slotData, 'MultiAssayExperiment')) {
        hasSamples <- colData(slotData)$sampleid %in% samples
        if (!all(hasSamples)) .warning(funContext, 'Some specified samples are
            not present in `molecularProfilesSlot(x)`')
        molecProfs <- slotData[, hasSamples]
    } else {
        SEcolData <- lapply(slotData, colData)
        SEsamples <- lapply(SEcolData, FUN=`[[`, i='sampleid')
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
        keepSamples <- slotData$info$sampleid %in% samples
        slotData$profiles <- slotData$profiles[keepSamples, ]
        slotData$raw <- slotData$raw[keepSamples, , ]
        slotData$n <- slotData$n[rownames(slotData$n) %in% samples, ]
        slotData$info <- slotData$info[keepSamples, ]
    }
    return(slotData)
}

## ======================
## ---- subsetByTreatment
## ----------------------

#' @export
setGeneric('subsetByTreatment', function(x, treatments, ...)
    standardGeneric('subsetByTreatment'))

#' @noRd
.docs_CoreSet_subsetByTreatment <- function(...) .parseToRoxygen(
    "
    @details

    ## subset methods
    __subsetByTreatment__: Subset a `{class_}` object by treatment identifier.
    - value: a `{class_}` object containing only `treatments`.

    @examples

    ## subset methods

    ### subsetByTreatment
    #treatments <- {treatment_}Info({data_})${treatment_}id[seq_len(10)]
    #{data_}_sub <- subsetByTreatment({data_}, treatments)

    @md
    @aliases subsetByTreatment subsetByTreatment,{class_}-method
    @exportMethod subsetByTreatment
    ",
    ...
)

#' @rdname CoreSet-utils
#' @eval CoreGx:::.docs_CoreSet_subsetByTreatment(class_=.local_class,
#' data_=.local_data, treatment_='treatment')
setMethod('subsetByTreatment', signature('CoreSet'),
        function(x, treatments) {
    funContext <- .S4MethodContext('subsetByTreatment', 'PharmacoSet')
    treatmentType <- switch(class(x)[1],
        'PharmacoSet'='drug',
        'ToxicoSet'='drug',
        'RadioSet'='radiation',
        'CoreSet'=return(data.frame())
    )
    treatmentNames <- rownames(treatmentInfo(x))
    if (!all(treatments %in% treatmentNames)) {
        .warning(funContext, 'Treatments missing from ', class(x)[1], ': ',
            setdiff(treatments, treatmentNames), '! Please ensure all specified
            treatments are valid rownames of treatmentInfo(x).
            Proceeding with the valid treatments only.')
        treatments <- union(treatments, treatmentNames)
    }
    # -- sensitivity slot
    sensSlot <- treatmentResponse(x)
    treatmentResponse(x) <- .subsetSensitivityByTreatment(sensSlot, treatments,
        treatmentType=treatmentType)

    # -- perturbation slot
    ## TODO: do we still need this?

    # -- curation slot
    treatmentCuration <- curation(x)[[treatmentType]]
    curation(x)[[treatmentType]] <- treatmentCuration[
        rownames(treatmentCuration) %in% treatments, ]

    # -- treatment slot
    treatmentInf <- treatmentInfo(x)
    treatmentInfo(x) <- treatmentInf[rownames(treatmentInf) %in% treatments, ]

    # -- molecularProfiles
    # deal with potential loss of samples when subsetting by treatment
    keepSamples <- sampleNames(x)
    molecSlot <- molecularProfilesSlot(x)
    molecularProfilesSlot(x) <- .subsetMolecularProfilesBySample(molecSlot,
        keepSamples)

    # -- check object is still valid and return
    tryCatch(checkCsetStructure(x), error = function(e) {})

    return(x)
})

.subsetSensitivityByTreatment <- function(slotData, treatments,
        treatmentType) {
    funContext <- .funContext(':::.subsetSensitivityByTreatment')
    treatmentId <- if (treatmentType == 'radiation')
        paste0(treatmentType, '.type') else paste0(treatmentType, 'id')
    if (is(slotData, 'LongTable')) {
        slotData <- slotData[treatments, ]
    } else {
        keepTreatments <- slotData$info[[treatmentId]] %in% treatments
        slotData$profiles <- slotData$profiles[keepTreatments, ]
        slotData$raw <- slotData$raw[keepTreatments, , ]
        slotData$info <- slotData$info[keepTreatments, ]
        slotData$n <- slotData$n[, colnames(slotData$n) %in% treatments]
    }
    return(slotData)
}


## ====================
## ---- subsetByFeature
## --------------------


#' @export
setGeneric('subsetByFeature', function(x, features, ...)
    standardGeneric('subsetByFeature'))

#' @noRd
.docs_CoreSet_subsetByFeature <- function(...) .parseToRoxygen(
    "
    @details

    ## subset methods
    __subsetByFeature__: Subset a `{class_}` object by molecular feature
        identifier.
    - value: a `{class_}` object containing only `features`.

    @examples

    ## subset methods

    ### subsetByFeature
    features <- fNames({data_}, 'rna')[seq_len(5)]
    {data_}_sub <- subsetByFeature({data_}, features, 'rna')

    @md
    @aliases subsetByFeature subsetByFeature,{class_}-method
    @importFrom MultiAssayExperiment MultiAssayExperiment
    @exportMethod subsetByFeature
    ",
    ...
)

#' @rdname CoreSet-utils
#' @eval .docs_CoreSet_subsetByFeature(class_=.local_class, data_=.local_data)
setMethod('subsetByFeature', signature(x='CoreSet'),
        function(x, features, mDataTypes) {
    slotData <- molecularProfilesSlot(x)
    MAE <- if (!is(slotData, 'MultiAssayExperiment'))
        MultiAssayExperiment(slotData) else slotData
    if (missing(mDataTypes)) mDataTypes <- names(MAE)
    suppressMessages({
        suppressWarnings({
            MAE_sub <- MAE[, , mDataTypes]
        })
    })
    keepFeatures <- rownames(MAE_sub) %in% features
    subsetMAE <- MAE[keepFeatures, drop=TRUE]
    newSlotData <- if (is(slotData, 'MultiAssayExperiment')) subsetMAE else
        as.list(experiments(subsetMAE))
    molecularProfilesSlot(x) <- newSlotData
    ## TODO:: What if this drops samples from the PSet?
    return(x)
})
