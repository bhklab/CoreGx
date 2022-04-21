#' @include CoreSet-class.R LongTable-class.R
#' @import checkmate
NULL

#' @importClassesFrom MultiAssayExperiment MultiAssayExperiment
#' @export
setClassUnion('list_OR_MAE', c('list', 'MultiAssayExperiment'))

.local_class <- 'CoreSet'
.local_data <- "ClevelandSmall_cSet"

#' @title
#' CoreSet - A generic data container for molecular profiles and
#'   treatment response data
#'
#' @slot annotation See Slots section.
#' @slot molecularProfiles See Slots section.
#' @slot sample See Slots section.
#' @slot curation See Slots section.
#' @slot sensitivity See Slots section.
#' @slot perturbation See Slots section.
#' @slot curation See Slots section.
#' @slot datasetType See Slots section.
#'
#' @details
#' The CoreSet (CSet) class was developed as a superclass for pSets in the
#' PharmacoGx and RadioGx packages to contain the data generated in screens
#' of cancer sample lines for their genetic profile and sensitivities to therapy
#' (Pharmacological or Radiation). This class is meant to be a superclass which
#' is contained within the PharmacoSet (pSet) and RadioSet (RSet) objects
#' exported by PharmacoGx and RadioGx. The format of the data is similar for
#' both pSets and rSets, allowing much of the code to be abstracted into
#' the CoreSet super-class. However, the models involved with quantifying
#' sampleular response to Pharmacological and Radiation therapy are widely
#' different, and extension of the cSet class allows the
#' packages to apply the correct model for the given data.
#'
#' @section Slots:
#' * annotation: A `list` of annotation data about the ``r .local_class``,
#'   including the `$name` and the session information for how the object
#'   was created, detailing the exact versions of R and all the packages used.
#' * molecularProfiles: A `list` or `MultiAssayExperiment` containing
#    a set of `SummarizedExperiment`s with molecular profile data for a given
#'   ``r .local_class`` object.
#' * sample: A `data.frame` containg the annotations for all the sample
#'   lines profiled in the data set, across all molecular data types and
#'   treatment response experiments.
#' * sensitivity: A `list` or `LongTable` containing all the data for the
#'   sensitivity experiments, including `$info`, a `data.frame` containing the
#'   experimental info, `$raw` a 3D `array` containing raw data,
#'   `$profiles`, a `data.frame` containing sensitivity profiles
#'   statistics, and `$n`, a `data.frame` detailing the number of
#'   experiments for each sample-drug/radiationInfo pair
#' * perturbation: `list` containing `$n`, a `data.frame`
#'   summarizing the available perturbation data. This slot is currently
#'   being deprecated.
#' * curation: `list` containing mappings for
#'   `sample`, `tissue` names used in the data set to universal
#'   identifiers used between different ``r .local_class`` objects
#' * datasetType: `character` string of 'sensitivity',
#'   'perturbation', or both detailing what type of data can be found in the
#'   CoreSet, for proper processing of the data
#'
#' @seealso [`CoreSet-accessors`]
#'
#' @md
#' @importClassesFrom MultiAssayExperiment MultiAssayExperiment
#' @aliases CoreSet-class
#' @exportClass CoreSet
.CoreSet <- setClass("CoreSet",
    slots=list(
        sensitivity="list_OR_LongTable",
        annotation="list",
        molecularProfiles="list_OR_MAE",
        sample="data.frame",
        treatment="data.frame",
        datasetType="character",
        perturbation="list",
        curation="list"
    )
)

# The default constructor above does a poor job of explaining the required structure of a CoreSet.
# The constructor function defined below guides the user into providing the required components of the curation and senstivity lists
# and hides the annotation slot which the user does not need to manually fill.
# This also follows the design of the Expression Set class.

## ==========================
## CONSTRUCTOR
## --------------------------

#' CoreSet constructor
#'
#' A constructor that simplifies the process of creating CoreSets, as well
#' as creates empty objects for data not provided to the constructor. Only
#' objects returned by this constructor are expected to work with the CoreSet
#' methods.
#'
#' @param name A \code{character} string detailing the name of the dataset
#' @param molecularProfiles A \code{list} of SummarizedExperiment objects containing
#'   molecular profiles for each molecular data type.
#' @param sample A \code{data.frame} containing the annotations for all the sample
#'   profiled in the data set, across all data types
#' @param sensitivityInfo A \code{data.frame} containing the information for the
#'   sensitivity experiments
#' @param sensitivityRaw A 3 Dimensional \code{array} contaning the raw drug
#'   dose response data for the sensitivity experiments
#' @param sensitivityProfiles \code{data.frame} containing drug sensitivity profile
#'   statistics such as IC50 and AUC
#' @param sensitivityN,perturbationN A \code{data.frame} summarizing the
#'   available sensitivity/perturbation data
#' @param curationSample,curationTissue A \code{data.frame} mapping
#'   the names for samples and tissues used in the data set to universal
#'   identifiers used between different CoreSet objects
#' @param datasetType A \code{character} string of 'sensitivity',
#'   'preturbation', or both detailing what type of data can be found in the
#'   CoreSet, for proper processing of the data
#' @param verify \code{boolean} Should the function verify the CoreSet and
#'   print out any errors it finds after construction?
#'
#' @return An object of class CoreSet
#'
#' @examples
#' data(clevelandSmall_cSet)
#' clevelandSmall_cSet
#'
#' @export
#'
#' @include LongTable-class.R
#' @import methods
#' @importFrom utils sessionInfo
#' @importFrom stats na.omit
#' @importFrom SummarizedExperiment rowData colData assays
CoreSet <- function(name, molecularProfiles=list(), sample=data.frame(),
    sensitivityInfo=data.frame(), sensitivityRaw=array(dim=c(0,0,0)),
    sensitivityProfiles=matrix(), sensitivityN=matrix(nrow=0, ncol=0),
    perturbationN=array(NA, dim=c(0,0,0)), curationSample=data.frame(),
    curationTissue=data.frame(), treatment=data.frame(),
    datasetType=c("sensitivity", "perturbation", "both"), verify=TRUE
) {

    .Deprecated("CoreSet2", package=packageName(), msg="The CoreSet class is
        being redesigned. Please use the new constructor to ensure forwards
        compatibility with future releases!", old="CoreSet")

    datasetType <- match.arg(datasetType)

    annotation <- list()
    annotation$name <- as.character(name)
    annotation$dateCreated <- date()
    annotation$sessionInfo <- sessionInfo()
    annotation$call <- match.call()

    for (i in seq_len(length(molecularProfiles))){
        if (!is(molecularProfiles[[i]], "SummarizedExperiment")) {
            stop(sprintf("Please provide the %s data as a SummarizedExperiment",
                names(molecularProfiles[i])))
        } else {
            rowData(molecularProfiles[[i]]) <-
                rowData(molecularProfiles[[i]])[
                    rownames(assays(molecularProfiles[[i]])[[1]]), , drop=FALSE
            ]
            colData(molecularProfiles[[i]]) <- colData(molecularProfiles[[i]])[
                colnames(assays(molecularProfiles[[i]])[[1]]), , drop=FALSE
            ]
        }
    }

    sensitivity <- list()

    if (!all(rownames(sensitivityInfo) == rownames(sensitivityProfiles) &
        rownames(sensitivityInfo) == dimnames(sensitivityRaw)[[1]])) {
        stop("Please ensure all the row names match between the sensitivity data.")
    }

    sensitivity$info <- as.data.frame(sensitivityInfo, stringsAsFactors=FALSE)
    sensitivity$raw <- sensitivityRaw
    sensitivity$profiles <- as.data.frame(sensitivityProfiles,
        stringsAsFactors=FALSE)
    sensitivity$n <- sensitivityN

    curation <- list()
    curation$sample <- as.data.frame(curationSample, stringsAsFactors=FALSE)
    curation$tissue <- as.data.frame(curationTissue, stringsAsFactors=FALSE)

    perturbation <- list()
    perturbation$n <- perturbationN
    if (datasetType == "perturbation" || datasetType == "both") {
        perturbation$info <- "The metadata for the perturbation experiments is
            available for each molecular type by calling the appropriate info
            function. \n For example, for RNA transcriptome perturbations, the
            metadata can be accessed using rnaInfo(cSet)."
    } else {
        perturbation$info <- "Not a perturbation dataset."
    }

    object  <- .CoreSet(annotation=annotation,
        molecularProfiles=molecularProfiles,
        sample=as.data.frame(sample), datasetType=datasetType,
        sensitivity=sensitivity, perturbation=perturbation,
        curation=curation, treatment=treatment)
    if (verify) { checkCsetStructure(object)}

    if(length(sensitivityN) == 0 &&
            datasetType %in% c("sensitivity", "both")) {
        sensNumber(object) <- .summarizeSensitivityNumbers(object)
    }
    if(length(perturbationN) == 0  &&
            datasetType %in% c("perturbation", "both")) {
        pertNumber(object) <- .summarizePerturbationNumbers(object)
    }
    return(object)
}


#' @noRd
.docs_CoreSet2_constructor <- function(...) .parseToRoxygen(
    "
    @title Make a CoreSet with the updated class structure

    @description
    New implementation of the CoreSet constructor to support MAE and TRE. This
    constructor will be swapped with the original `CoreSet` constructor as
    part of an overhaul of the CoreSet class structure.

    @param name A `character(1)` vector with the `{class_}` objects name.
    @param treatment A `data.frame` with treatment level metadata. {tx_}
    @param sample A `data.frame` with sample level metadata for the union
        of samples in `treatmentResponse` and `molecularProfiles`. {sx_}
    @param molecularProfiles A `MultiAssayExperiment` containing one
        `SummarizedExperiment` object for each molecular data type.
    @param treatmentResponse A `LongTable` or `LongTableDataMapper` object
        containing all treatment response data associated with the `{class_}`
        object.
    @param curation {cx_}

    @examples
    data({data_})
    {data_}

    @return A `CoreSet` object storing standardized and curated treatment
        response and multiomic profile data associated with a given publication.

    @importFrom MultiAssayExperiment MultiAssayExperiment
    @importFrom checkmate assertCharacter assertDataFrame assertClass assert
    assertList assertSubset
    ",
    ...
)

#' @eval .docs_CoreSet2_constructor(class_=.local_class,
#' tx_="This slot is not implemented for a CoreSet object yet.",
#' sx_="",
#' cx_="A `list(2)` object with two items named `treatment` and `sample` with mappings from publication identifiers to standardized identifiers for both annotations, respectively.",
#' data_=.local_data)
#' @md
#' @export
CoreSet2 <- function(name="emptySet", treatment=data.frame(),
    sample=data.frame(), molecularProfiles=MultiAssayExperiment(),
    treatmentResponse=LongTable(), perturbation=list(), datasetType="sensitivity",
    curation=list(sample=data.frame(), treatment=data.frame())
) {

    # -- update old curation names
    names(curation) <- gsub("drug|radiation", "treatment", names(curation))
    names(curation) <- gsub("cell", "sample", names(curation))

    ## -- input validation
    assertCharacter(name, len=1)
    assertDataFrame(treatment)
    assertDataFrame(sample)
    assertClass(molecularProfiles, "MultiAssayExperiment")
    assert(
        checkClass(treatmentResponse, "LongTable"),
        checkClass(treatmentResponse, "LongTableDataMapper")
    )
    assertList(curation, min.len=2)
    assertSubset(c("sample", "treatment"), choices=names(curation))

    ## -- capture object creation environment
    annotation <- list(name=name, dateCreated=date(),
        sessionInfo=sessionInfo(), call=match.call())

    ## -- conditionally materialize DataMapper
    if (is(treatmentResponse, 'LongTableDataMapper'))
        treatmentResponse <- metaConstruct(treatmentResponse)


    ## -- handle missing rownames for sample
    if (!all(sample$sampleid == rownames(sample)))
        rownames(sample) <- sample$sampleid

    object <- .CoreSet(
        annotation=annotation,
        sample=sample,
        treatment=treatment,
        molecularProfiles=molecularProfiles,
        sensitivity=treatmentResponse,
        datasetType=datasetType,
        curation=curation,
        perturbation=perturbation
    )

    ## -- data integrity checks
    # molecularProfiles
    validProfiles <- .checkMolecularProfiles(object)

    # treatmentResponse
    validTreatments <- .checkTreatmentResponse(object)

    diagnosis <- c(!isTRUE(validProfiles), !isTRUE(validTreatments))
    if (any(diagnosis)) {
        .error(paste0(list(validProfiles, validTreatments)[diagnosis],
            collapse="\n", sep="\n"))
    }
    return(object)
}

#' Show a CoreSet
#'
#' @param object `CoreSet` object to show via `cat`.
#'
#' @seealso [`cat`]
#'
#' @examples
#' show(clevelandSmall_cSet)
#'
#' @return Prints the CoreSet object to the output stream, and returns
#'   invisible NULL.
#'
#' @md
#' @export
setMethod("show", signature=signature(object="CoreSet"), function(object) {
    cat(paste0("<", class(object)[1], ">\n"))
    space <- "  "
    cat("Name: ", name(object), "\n")
    cat("Date Created: ", dateCreated(object), "\n")
    cat("Number of samples: ", nrow(sampleInfo(object)), "\n")
    mProfiles <- molecularProfilesSlot(object)
    mProfileNames <- names(mProfiles)
    cat("Molecular profiles:\n")
    if (is(mProfiles, "MultiAssayExperiment")) {
        showMAE <- capture.output(show(mProfiles))
        dropAfter <- which(grepl("Functionality", showMAE)) - 1
        showCompactMAE <- showMAE[1:dropAfter]
        cat(space, paste0(showCompactMAE, collapse="\n  "), "\n")
    } else {
        if (!length(mProfileNames)) cat(space, "None\n")
        for (item in mProfileNames) {
            title <- switch(item,
                "dna"="DNA",
                "rna"="RNA",
                "rnaseq"="RNAseq",
                "snp"="SNP",
                "cnv"="CNV",
                item
            )
            cat(title, ":\n")
            cat(paste0(space, "Dim: ", dim(molecularProfiles(object, mDataType=item)),
                "\n"))
        }
    }
    cat("Treatment response:\n")
    if (is(sensitivitySlot(object), "LongTable")) {
        showLT <- capture.output(show(sensitivitySlot(object)))
        cat(space, paste0(showLT, collapse="\n  "), "\n")
    } else {
        cat("Drug pertubation:\n")
        cat(space,
            "Please look at pertNumber(cSet) to determine number of experiments",
            " for each drug-sample combination.\n")
        cat("Drug sensitivity:\n")
        cat(space, "Number of Experiments: ", nrow(sensitivityInfo(object)),"\n")
        cat(space, "Please look at sensNumber(cSet) to determine number of ",
            "experiments for each drug-sample combination.\n")
    }
})


#' Update the sample ids in a cSet object
#'
#' @examples
#' updateSampleId(clevelandSmall_cSet, sampleNames(clevelandSmall_cSet))
#'
#' @param object The object for which the sample ids will be updated
#' @param new.ids The new ids to assign to the object
#'
#' @return \code{CoreSet} The modified CoreSet object
#'
#' @keywords internal
#' @importFrom S4Vectors endoapply
#' @importFrom SummarizedExperiment colData rowData
#' @export
updateSampleId <- function(object, new.ids=vector("character")) {

    if (length(new.ids) != nrow(sampleInfo(object))){
        stop("Wrong number of sample identifiers")
    }

    if (datasetType(object) == "sensitivity" || datasetType(object) == "both") {
        myx <- match(sensitivityInfo(object)[, "sampleid"],
            rownames(sampleInfo(object)))
        if (is(sensitivitySlot(object), 'LongTable')) {
            LT <- sensitivitySlot(object)
            whichSampleIds <- which(colData(LT)$sampleid %in% sampleNames(object))
            colData(LT)$sampleid <- new.ids[whichSampleIds]
            sensitivitySlot(object) <- LT
        } else {
            sensitivityInfo(object)[, "sampleid"] <- new.ids[myx]
        }
    }

    molecularProfilesSlot(object) <- lapply(molecularProfilesSlot(object), function(SE) {
        myx <- match(colData(SE)[["sampleid"]],
            rownames(sampleInfo(object)))
        colData(SE)[["sampleid"]]  <- new.ids[myx]
        return(SE)
    })

    if (any(duplicated(new.ids))) {
        warning("Duplicated ids passed to updateSampleId. Merging old ids into",
            " the same identifier")

        if(ncol(sensNumber(object)) > 0) {
            sensMatch <- match(rownames(sensNumber(object)),
                rownames(sampleInfo(object)))
        }
        if(dim(pertNumber(object))[[2]] > 0) {
            pertMatch <- match(dimnames(pertNumber(object))[[1]],
                rownames(sampleInfo(object)))
        }

        curMatch <- match(rownames(curation(object)$sample),
            rownames(sampleInfo(object)))
        duplId <- unique(new.ids[duplicated(new.ids)])

        for(id in duplId){
            if (ncol(sensNumber(object)) > 0) {
                myx <- which(new.ids[sensMatch] == id)
                sensNumber(object)[myx[1],] <- apply(sensNumber(object)[myx, ],
                    2, sum)
                sensNumber(object) <- sensNumber(object)[-myx[-1], ]
                # sensMatch <- sensMatch[-myx[-1]]
        }
        if (dim(pertNumber(object))[[1]] > 0) {
            myx <- which(new.ids[pertMatch] == id)
            pertNumber(object)[myx[1], , ] <- apply(pertNumber(object)[myx, , ],
                c(1,3), sum)
            pertNumber(object) <- pertNumber(object)[-myx[-1], , ]
        }

        myx <- which(new.ids[curMatch] == id)
        curation(object)$sample[myx[1],] <- apply(curation(object)$sample[myx, ], 2,
            FUN=paste, collapse="///")
        curation(object)$sample <- curation(object)$sample[-myx[-1], ]
        curation(object)$tissue[myx[1],] <- apply(curation(object)$tissue[myx, ],
            2, FUN=paste, collapse="///")
        curation(object)$tissue <- curation(object)$tissue[-myx[-1], ]

        myx <- which(new.ids == id)
        sampleInfo(object)[myx[1],] <- apply(sampleInfo(object)[myx,], 2,
            FUN=paste, collapse="///")
        sampleInfo(object) <- sampleInfo(object)[-myx[-1], ]
        new.ids <- new.ids[-myx[-1]]
        if(ncol(sensNumber(object)) > 0){
            sensMatch <- match(rownames(sensNumber(object)),
                rownames(sampleInfo(object)))
        }
        if(dim(pertNumber(object))[[1]] > 0){
            pertMatch <- match(dimnames(pertNumber(object))[[1]],
                rownames(sampleInfo(object)))
        }
        curMatch <- match(rownames(curation(object)$sample),
            rownames(sampleInfo(object)))
        }
    } else {
        if (dim(pertNumber(object))[[1]] > 0) {
            pertMatch <- match(dimnames(pertNumber(object))[[1]],
                rownames(sampleInfo(object)))
        }
        if (ncol(sensNumber(object)) > 0) {
            sensMatch <- match(rownames(sensNumber(object)),
                rownames(sampleInfo(object)))
        }
        curMatch <- match(rownames(curation(object)$sample),
            rownames(sampleInfo(object)))
    }
    if (dim(pertNumber(object))[[1]] > 0) {
        dimnames(pertNumber(object))[[1]] <- new.ids[pertMatch]
    }
    if (ncol(sensNumber(object)) > 0) {
        rownames(sensNumber(object)) <- new.ids[sensMatch]
    }
    rownames(curation(object)$sample) <- new.ids[curMatch]
    rownames(curation(object)$tissue) <- new.ids[curMatch]
    rownames(sampleInfo(object)) <- new.ids
    return(object)
}

# updateFeatureNames <- function(object, new.ids=vector("character")){
#
#   if (length(new.ids)!=nrow(sampleInfo(object))){
#     stop("Wrong number of sample identifiers")
#   }
#
#   if(datasetType(object)=="sensitivity"|datasetType(object)=="both"){
#     myx <- match(sensitivityInfo(object)[,"sampleid"],rownames(sampleInfo(object)))
#     sensitivityInfo(object)[,"sampleid"] <- new.ids[myx]
#
#   }
#
#   molecularProfilesSlot(object) <- lapply(molecularProfilesSlot(object), function(eset){
#
#     myx <- match(colData(eset)[["sampleid"]],rownames(sampleInfo(object)))
#     colData(eset)[["sampleid"]]  <- new.ids[myx]
#     return(eset)
#       })
#   myx <- match(rownames(curation(object)$sample),rownames(sampleInfo(object)))
#   rownames(curation(object)$sample) <- new.ids[myx]
#   rownames(curation(object)$tissue) <- new.ids[myx]
#   if (dim(pertNumber(object))[[1]]>0){
#     myx <- match(dimnames(pertNumber(object))[[1]], rownames(sampleInfo(object)))
#     dimnames(pertNumber(object))[[1]] <- new.ids[myx]
#   }
#   if (nrow(sensNumber(object))>0){
#     myx <- match(rownames(sensNumber(object)), rownames(sampleInfo(object)))
#     rownames(sensNumber(object)) <- new.ids[myx]
#   }
#   rownames(sampleInfo(object)) <- new.ids
#   return(object)
#
# }


### TODO:: Add updating of sensitivity Number tables
#' Update the treatment ids in a cSet object
#'
#' @examples
#' updateTreatmentId(clevelandSmall_cSet, treatmentNames(clevelandSmall_cSet))
#'
#' @param object The object for which the treatment ids will be updated
#' @param new.ids The new ids to assign to the object
#'
#' @return `CoreSet` The modified CoreSet object
#'
#' @keywords internal
#' @importFrom S4Vectors endoapply
#' @importFrom SummarizedExperiment colData rowData
#' @export
updateTreatmentId <- function(object, new.ids = vector('character')){

    if (nrow(treatmentInfo(object)) < 1) {
        message("No treatments in this object! Returning without modification.")
        return(object)
    }

    if (length(new.ids) != nrow(treatmentInfo(object))) {
        stop('Wrong number of drug identifiers')
    }
    if (datasetType(object) == 'sensitivity' || datasetType(object) == 'both') {
        myx <- match(sensitivityInfo(object)[, "treatmentid"], rownames(treatmentInfo(object)))
        sensitivityInfo(object)[, "treatmentid"] <- new.ids[myx]
    }
    if (datasetType(object) == 'perturbation' || datasetType(object) == 'both') {
        molecularProfilesSlot(object) <- lapply(molecularProfilesSlot(object),
                function(SE) {
            myx <- match(
                SummarizedExperiment::colData(SE)[["treatmentid"]],
                rownames(treatmentInfo(object))
            )
            SummarizedExperiment::colData(SE)[["treatmentid"]] <- new.ids[myx]
            return(SE)
        })
    }
    if (any(duplicated(new.ids))) {
        warning('Duplicated ids passed to updateTreatmentId. Merging old ids ',
            'into the same identifier')
        if (ncol(sensNumber(object)) > 0){
            sensMatch <- match(colnames(sensNumber(object)),
                rownames(treatmentInfo(object)))
        }
        if (dim(pertNumber(object))[[2]] > 0) {
            pertMatch <- match(dimnames(pertNumber(object))[[2]],
                rownames(treatmentInfo(object)))
        }
        if ("treatment" %in% names(curation(object))) {
            curMatch <- match(rownames(curation(object)$treatment),
                rownames(treatmentInfo(object)))
        }
        duplId <- unique(new.ids[duplicated(new.ids)])
        for(id in duplId) {
            if (ncol(sensNumber(object))>0){
                myx <- which(new.ids[sensMatch] == id)
                sensNumber(object)[, myx[1]] <- apply(sensNumber(object)[, myx], 1, sum)
                sensNumber(object) <- sensNumber(object)[, -myx[-1]]
                # sensMatch <- sensMatch[-myx[-1]]
            }
            if (dim(pertNumber(object))[[2]] > 0) {
                myx <- which(new.ids[pertMatch] == id)
                pertNumber(object)[,myx[1],] <- apply(pertNumber(object)[,myx,],
                    c(1,3), sum)
                pertNumber(object) <- pertNumber(object)[,-myx[-1], ]
                # pertMatch <- pertMatch[-myx[-1]]
            }
            if ("treatment" %in% names(curation(object))) {
                myx <- which(new.ids[curMatch] == id)
                curation(object)$treatment[myx[1], ] <-
                    apply(curation(object)$treatment[myx, ], 2, paste,
                        collapse='///')
                curation(object)$treatment <- curation(object)$treatment[-myx[-1], ]
                # curMatch <- curMatch[-myx[-1]]
            }

            myx <- which(new.ids == id)
            treatmentInfo(object)[myx[1],] <- apply(treatmentInfo(object)[myx,],
                2, paste, collapse='///')
            treatmentInfo(object) <- treatmentInfo(object)[-myx[-1], ]
            new.ids <- new.ids[-myx[-1]]
            if (ncol(sensNumber(object)) > 0) {
                sensMatch <- match(colnames(sensNumber(object)),
                    rownames(treatmentInfo(object)))
            }
            if (dim(pertNumber(object))[[2]] > 0) {
                pertMatch <- match(dimnames(pertNumber(object))[[2]],
                    rownames(treatmentInfo(object)))
            }
            if ("treatment" %in% names(curation(object))) {
                curMatch <- match(rownames(curation(object)$treatment),
                    rownames(treatmentInfo(object)))
            }
        }
    } else {
        if (dim(pertNumber(object))[[2]]>0){
            pertMatch <- match(dimnames(pertNumber(object))[[2]],
                rownames(treatmentInfo(object)))
        }
        if (ncol(sensNumber(object))>0){
            sensMatch <- match(colnames(sensNumber(object)),
                rownames(treatmentInfo(object)))
        }
        if ("treatment" %in% names(curation(object))) {
            curMatch <- match(rownames(curation(object)$treatment),
                rownames(treatmentInfo(object)))
        }
    }
    if (dim(pertNumber(object))[[2]]>0){
        dimnames(pertNumber(object))[[2]] <- new.ids[pertMatch]
    }
    if (ncol(sensNumber(object))>0){
        colnames(sensNumber(object)) <- new.ids[sensMatch]
    }
    if ("treatment" %in% names(curation(object))) {
        rownames(curation(object)$treatment) <- new.ids[curMatch]
    }
    rownames(treatmentInfo(object)) <- new.ids
    return(object)
}


.summarizeSensitivityNumbers <- function(object) {

    if (datasetType(object) != "sensitivity" && datasetType(object) != "both") {
        stop ("Data type must be either sensitivity or both")
    }

    ## unique drug identifiers
    # drugn <- sort(unique(sensitivitySlot(object)$info[ , "drugid"]))

    ## consider all drugs
    drugn <- rownames(treatmentInfo(object))

    ## unique drug identifiers
    # samplen <- sort(unique(sensitivitySlot(object)$info[ , "sampleid"]))

    ## consider all sample
    samplen <- rownames(sampleInfo(object))

    sensitivity.info <- matrix(0, nrow=length(samplen), ncol=length(drugn),
        dimnames=list(samplen, drugn))
    drugids <- sensitivityInfo(object)[, "drugid"]
    sampleids <- sensitivityInfo(object)[, "sampleid"]
    sampleids <- sampleids[grep("///", drugids, invert=TRUE)]
    drugids <- drugids[grep("///", drugids, invert=TRUE)]

    tt <- table(sampleids, drugids)
    sensitivity.info[rownames(tt), colnames(tt)] <- tt

    return(sensitivity.info)
}

#' @export
#' @keywords internal
.summarizeMolecularNumbers <- function(object) {

    ## consider all molecular types
    mDT <- mDataNames(object)

    ## consider all sample lines
    samplen <- rownames(sampleInfo(object))

    molecular.info <- matrix(0, nrow=length(samplen), ncol=length(mDT),
        dimnames=list(samplen, mDT))

    for(mDataType in mDT) {
        tt <- table(phenoInfo(object, mDataType)$sampleid)
        molecular.info[names(tt), mDataType] <- tt
    }
    return(molecular.info)
}

#' @importFrom SummarizedExperiment colData rowData
.summarizePerturbationNumbers <- function(object) {

    if (datasetType(object) != "perturbation" && datasetType(object) != "both") {
        stop ("Data type must be either perturbation or both")
    }

    ## consider all drugs
    drugn <- rownames(treatmentInfo(object))

    ## consider all sample lines
    samplen <- rownames(sampleInfo(object))

    perturbation.info <- array(0, dim=c(length(samplen), length(drugn),
        length(molecularProfilesSlot(object))),
        dimnames=list(samplen, drugn, names((molecularProfilesSlot(object)))))

    for (i in seq_len(length(molecularProfilesSlot(object)))) {
        if (nrow(colData(molecularProfilesSlot(object)[[i]])) > 0 &&
                all(is.element(c("sampleid", "drugid"),
                    colnames(colData(molecularProfilesSlot(object)[[i]]))))) {
            tt <- table(colData(molecularProfilesSlot(object)[[i]])[ , "sampleid"],
                colData(molecularProfilesSlot(object)[[i]])[ , "drugid"])
            perturbation.info[rownames(tt), colnames(tt),
                names(molecularProfilesSlot(object))[i]] <- tt
        }
    }

    return(perturbation.info)
}

#' A function to verify the structure of a CoreSet
#'
#' This function checks the structure of a PharamcoSet, ensuring that the
#' correct annotations are in place and all the required slots are filled so
#' that matching of samples and drugs can be properly done across different types
#' of data and with other studies.
#'
#' @examples
#' checkCsetStructure(clevelandSmall_cSet)
#'
#' @param object A `CoreSet` to be verified
#' @param plotDist Should the function also plot the distribution of molecular
#'   data?
#' @param result.dir The path to the directory for saving the plots as a string.
#'   Defaults to this R sessions `tempdir()`.
#'
#' @return Prints out messages whenever describing the errors found in the
#'   structure of the cSet object passed in.
#'
#' @export
#'
#' @md
#' @importFrom graphics hist
#' @importFrom grDevices dev.off pdf
#' @importFrom SummarizedExperiment assay rowData colData
#' @importFrom S4Vectors metadata
checkCsetStructure <- function(object, plotDist=FALSE, result.dir=tempdir()) {

    msg <- c()

    # Make directory to store results if it doesn't exist
    if (!file.exists(result.dir) && plotDist) {
        dir.create(result.dir, showWarnings=FALSE, recursive=TRUE)
    }

    ####
    ## Checking molecularProfiles
    ####
    for (i in seq_along(molecularProfilesSlot(object))) {
        profile <- molecularProfilesSlot(object)[[i]]
        nn <- names(molecularProfilesSlot(object))[i]

        # Testing plot rendering for rna and rnaseq
        if ((metadata(profile)$annotation == "rna" ||
                metadata(profile)$annotation == "rnaseq") && plotDist) {
            pdf(file=file.path(result.dir, sprintf("%s.pdf", nn)))
            hist(assay(profile, 'exprs'), breaks=100)
            dev.off()
        }

        ## Test if sample and feature annotations dimensions match the assay
        if (nrow(rowData(profile)) != nrow(assays(profile)$exprs)) {
            msg <- c(msg, paste0(nn, " number of features in rowData is ",
                "different from SummarizedExperiment slots"))
        }
        if (nrow(colData(profile)) != ncol(assays(profile)$exprs)) {
            msg <- c(msg, paste0(nn, "number of samples in colData is ",
                "different from expression slots", nn))
        }

        # Checking sample metadata for required columns
        if (!("sampleid" %in% colnames(colData(profile)))) {
            msg <- c(msg, paste0(nn, " sampleid does not exist in colData ",
                "(samples) columns"))
        }
        if (!("batchid" %in% colnames(colData(profile)))) {
            msg <- c(msg, sprintf(nn, " batchid does not exist in colData ",
                "(samples) columns"))
        }

        # Checking mDataType of the SummarizedExperiment for required columns
        if (metadata(profile)$annotation == "rna" ||
                metadata(profile)$annotation == "rnaseq") {
            if (!("BEST" %in% colnames(rowData(profile)))) {
                msg <- c(msg, paste0(nn, " BEST does not exist in rowData ",
                    "(features) columns"))
            }
            if (!("Symbol" %in% colnames(rowData(profile)))) {
                msg <- c(msg, paste0(nn, " Symbol does not exist in rowData ",
                    "(features) columns"))
            }
        }

        # Check that all sampleids from the cSet are included in molecularProfiles
        if ("sampleid" %in% colnames(rowData(profile))) {
            if (!all(colData(profile)[, "sampleid"] %in% rownames(sampleInfo(object)))) {
                msg <- c(msg, paste0(nn, " not all the sample lines in this ",
                    "profile are in sample lines slot"))
            }
        } else {
            msg <- c(msg, paste0(nn, " sampleid does not exist in colData ",
                "(samples)"))
        }
    }

    #####
    # Checking sample
    #####
    if ("tissueid" %in% colnames(sampleInfo(object))) {
        if ("unique.tissueid" %in% colnames(curation(object)$tissue)) {
            if (length(intersect(rownames(curation(object)$tissue),
                    rownames(sampleInfo(object)))) != nrow(sampleInfo(object))) {
                msg <- c(msg, paste0("rownames of curation tissue slot should",
                    " be the same as sample slot (curated sample ids)"))
            } else {
                if (length(intersect(sampleInfo(object)$tissueid,
                        curation(object)$tissue$unique.tissueid)) !=
                            length(table(sampleInfo(object)$tissueid))) {
                    msg <- c(msg, paste0("tissueid should be the same as unique",
                        " tissue id from tissue curation slot"))
                }
            }
        } else {
            msg <- c(msg, paste0("unique.tissueid which is curated tissue id",
                " across data set should be a column of tissue curation slot"))
        }
        if (any(is.na(sampleInfo(object)[,"tissueid"]) |
                sampleInfo(object)[, "tissueid"] == "", na.rm=TRUE)) {
            msg <- c(msg, paste0(
                    "There is no tissue type for these samples",
                    paste(
                        rownames(sampleInfo(object))[
                            which(is.na(sampleInfo(object)[,"tissueid"]) |
                                sampleInfo(object)[,"tissueid"] == "")
                            ],
                        collapse=" ")))
        }
    } else {
        msg <- c(msg, "tissueid does not exist in sample slot")
    }

    if("unique.sampleid" %in% colnames(curation(object)$sample)) {
        if (length(intersect(curation(object)$sample$unique.sampleid,
                rownames(sampleInfo(object)))) != nrow(sampleInfo(object))) {
            msg <- c(msg, "rownames of sample slot should be curated sample ids")
        }
    } else {
        msg <- c(msg, paste0("unique.sampleid which is curated sample id across",
            " data set should be a column of sample curation slot"))
    }

    if (length(intersect(rownames(curation(object)$sample),
            rownames(sampleInfo(object)))) != nrow(sampleInfo(object))) {
        msg <- c(msg, paste0("rownames of curation sample slot should be the",
            " same as sample slot (curated sample ids)"))
    }

    if (!is(sampleInfo(object), "data.frame")) {
        msg <- c(msg, "sample slot class type should be dataframe")
    }
    if (length(msg)) return(paste0(msg, collapse="\n")) else TRUE
}

#' @importFrom MultiAssayExperiment MultiAssayExperiment experiments
#' @importFrom S4Vectors List
#' @importFrom BiocGenerics %in% match
.checkMolecularProfiles <- function(object) {
    msg <- character()
    # ---- Make a MutliAssayExperiment, if it isn't one already
    molecProf <- molecularProfilesSlot(object)
    isSummarizedExperiment <- all(as(lapply(experiments(molecProf), is,
        'SummarizedExperiment'), 'List'))
    if (!all(isSummarizedExperiment)) {
        nmsg <- .formatMessage('All molecular profiles must be stored as
            SummarizedExperiment objects. The following are not ',
            paste(names(which(!isSummarizedExperiment)), collapse=', '))
        msg <- c(msg, nmsg)
    }
    tryCatch({
        MAE <- if (is(molecProf, 'MultiAssayExperiment')) molecProf else
            MultiAssayExperiment(molecProf)
    }, error=function(e) msg <- c(msg, paste0('Failed coercing to
        MultiAssayExperiment: ', as.character(e))))

    # ---- Check for correct metadata columns
    # -- sample identifiers
    colDataL <- lapply(experiments(MAE), FUN=colData)
    colColNameL <- as(lapply(colDataL, FUN=colnames), 'List')
    hasSampleId <- any(colColNameL %in% 'sampleid')
    if (!all(hasSampleId)) {
        nmsg <- .formatMessage('All SummarizedExperiments must have a sampleid
            column. This is not the case for ',
            paste(names(which(!hasSampleId)), collapse=', '), '!')
        msg <- c(msg, nmsg)
    }
    hasBatchId <- any(colColNameL %in% 'batchid')
    if (!all(hasBatchId)) {
        nmsg <- .formatMessage('All SummarizedExpeirments must have a batchid
            column. This is not the case for ',
            paste(names(which(!hasBatchId)), collapse=', '), '!')
        msg <- c(msg, nmsg)
    }
    # -- feature identifiers
    rowDataL <- lapply(experiments(MAE), FUN=rowData)
    rowColNameL <- as(lapply(rowDataL, colnames), 'List')
    # hasGeneId <- rowColNameL %in% 'geneid'
    hasSymbol <- rowColNameL %in% 'Symbol'
    hasBEST <- rowColNameL %in% 'BEST'
    # hasEnsemblId <- rowColNamesL %in% 'ensemblid'

    # ---- Check all samples are in the @sample slot
    samples <- sampleNames(object)
    sampleIdL <- as(lapply(colDataL, `[[`, i='sampleid'), 'List')
    hasValidSamples <- sampleIdL %in% samples
    if (!all(all(hasValidSamples))) {
        nmsg <- .formatMessage('All sampleids in the @molecularProfiles slot
            must also be in the @sample slot. This is not the case
            for ', paste(names(which(all(hasValidSamples))), collapse=', '))
        msg <- c(msg, nmsg)
    }

    # ---- Return messages if something is wrong, or TRUE if everything is good
    return(if (length(msg)) msg else TRUE)
}

.checkTreatmentResponse <- function(object) {
    msg <- character()
    # ---- Extract sensitivity data
    samples <- sampleNames(object)
    sensSlot <- sensitivitySlot(object)
    if (!is(sensSlot, "TreatmentResponseExperiment")) {
        nmsg <- "The treatmentReponse parameter must be a
            TreatmentResponseExperiment!"
        msg <- c(msg, nmsg)
        return(msg)
    }
    return(if (length(msg)) msg else TRUE)
}