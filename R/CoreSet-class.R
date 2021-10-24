#' @include CoreSet-class.R LongTable-class.R
#' @import checkmate
NULL

#' @importClassesFrom MultiAssayExperiment MultiAssayExperiment
setClassUnion('list_or_MAE', c('list', 'MultiAssayExperiment'))

.local_class <- 'CoreSet'
.local_data <- "ClevelandSmall_cSet"

#' @title
#' CoreSet - A generic data container for molecular profiles and 
#'   treatment response data
#' 
#' @slot annotation See Slots section.
#' @slot molecularProfiles See Slots section.
#' @slot cell See Slots section.
#' @slot curation See Slots section.
#' @slot sensitivity See Slots section.
#' @slot perturbation See Slots section.
#' @slot curation See Slots section.
#' @slot datasetType See Slots section.
#'
#' @details  
#' The CoreSet (CSet) class was developed as a superclass for pSets in the 
#' PharmacoGx and RadioGx packages to contain the data generated in screens 
#' of cancer cell lines for their genetic profile and sensitivities to therapy
#' (Pharmacological or Radiation). This class is meant to be a superclass which 
#' is contained within the PharmacoSet (pSet) and RadioSet (RSet) objects 
#' exported by PharmacoGx and RadioGx. The format of the data is similar for 
#' both pSets and rSets, allowing much of the code to be abstracted into 
#' the CoreSet super-class. However, the models involved with quantifying 
#' cellular response to Pharmacological and Radiation therapy are widely 
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
#' * cell: A `data.frame` containg the annotations for all the cell 
#'   lines profiled in the data set, across all molecular data types and
#'   treatment response experiments.
#' * sensitivity: A `list` or `LongTable` containing all the data for the 
#'   sensitivity experiments, including `$info`, a `data.frame` containing the 
#'   experimental info, `$raw` a 3D `array` containing raw data,
#'   `$profiles`, a `data.frame` containing sensitivity profiles 
#'   statistics, and `$n`, a `data.frame` detailing the number of 
#'   experiments for each cell-drug/radiationInfo pair
#' * perturbation: `list` containing `$n`, a `data.frame` 
#'   summarizing the available perturbation data. This slot is currently
#'   being deprecated.
#' * curation: `list` containing mappings for
#'   `cell`, `tissue` names used in the data set to universal 
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
        sensitivity="list_or_LongTable",
        annotation="list",
        molecularProfiles="list_or_MAE",
        cell="data.frame",
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
#' @param cell A \code{data.frame} containing the annotations for all the cell
#'   lines profiled in the data set, across all data types
#' @param sensitivityInfo A \code{data.frame} containing the information for the
#'   sensitivity experiments
#' @param sensitivityRaw A 3 Dimensional \code{array} contaning the raw drug
#'   dose response data for the sensitivity experiments
#' @param sensitivityProfiles \code{data.frame} containing drug sensitivity profile 
#'   statistics such as IC50 and AUC
#' @param sensitivityN,perturbationN A \code{data.frame} summarizing the
#'   available sensitivity/perturbation data
#' @param curationCell,curationTissue A \code{data.frame} mapping
#'   the names for cells and tissues used in the data set to universal
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
CoreSet <- function(name, molecularProfiles=list(), cell=data.frame(), 
    sensitivityInfo=data.frame(), sensitivityRaw=array(dim=c(0,0,0)), 
    sensitivityProfiles=matrix(), sensitivityN=matrix(nrow=0, ncol=0), 
    perturbationN=array(NA, dim=c(0,0,0)), curationCell=data.frame(), 
    curationTissue=data.frame(), 
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
    curation$cell <- as.data.frame(curationCell, stringsAsFactors=FALSE)
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
        cell=as.data.frame(cell), datasetType=datasetType, 
        sensitivity=sensitivity, perturbation=perturbation, 
        curation=curation)
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
    ",
    ...
)

#' @eval .docs_CoreSet2_constructor(class_=.local_class, tx_="This slot is not implemented for a CoreSet object yet.", sx_="", cx_="A `list(2)` object with two items named `treatment` and `sample` with mappings from publication identifiers to standardized identifiers for both annotations, respectively.", data_=.local_data)
#' @md
#' @export
CoreSet2 <- function(name="emptySet", treatment=data.frame(), 
    sample=data.frame(), molecularProfiles=MultiAssayExperiment(), 
    treatmentResponse=LongTable(), 
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
    longTable <- if (is(treatmentResponse, 'LongTableDataMapper')) {
        metaConstruct(treatmentResponse)
    } else {
        treatmentResponse
    }

    object <- .CoreSet(
        annotation=annotation,
        cell=sample,
        molecularProfiles=molecularProfiles,
        sensitivity=treatmentResponse,
        datasetType="sensitivity",
        curation=curation
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
    cat("Number of cell lines: ", nrow(cellInfo(object)), "\n")
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
            " for each drug-cell combination.\n")
        cat("Drug sensitivity:\n")
        cat(space, "Number of Experiments: ", nrow(sensitivityInfo(object)),"\n")
        cat(space, "Please look at sensNumber(cSet) to determine number of ",
            "experiments for each drug-cell combination.\n")
    }
})


#' Update the cell ids in a cSet object
#'
#' @examples
#' updateCellId(clevelandSmall_cSet, cellNames(clevelandSmall_cSet))
#'
#' @param object The object for which the cell ids will be updated
#' @param new.ids The new ids to assign to the object
#' 
#' @return \code{CoreSet} The modified CoreSet object
#'
#' @keywords internal
#' @importFrom S4Vectors endoapply
#' @importFrom SummarizedExperiment colData rowData
#' @export
updateCellId <- function(object, new.ids=vector("character")) {

    if (length(new.ids) != nrow(cellInfo(object))){
        stop("Wrong number of cell identifiers")
    }

    if (datasetType(object) == "sensitivity" || datasetType(object)=="both") {
        myx <- match(sensitivityInfo(object)[, "cellid"], 
            rownames(cellInfo(object)))
        if (is(sensitivitySlot(object), 'LongTable')) {
            LT <- sensitivitySlot(object)
            whichCellIds <- which(colData(LT)$cellid %in% cellNames(object))
            colData(LT)$cellid <- new.ids[whichCellIds]
            sensitivitySlot(object) <- LT
        } else {
            sensitivityInfo(object)[,"cellid"] <- new.ids[myx]
        }
    }

    molecularProfilesSlot(object) <- lapply(molecularProfilesSlot(object), function(SE) {
        myx <- match(colData(SE)[["cellid"]], 
            rownames(cellInfo(object)))
        colData(SE)[["cellid"]]  <- new.ids[myx]
        return(SE)
    })

    if (any(duplicated(new.ids))) {
        warning("Duplicated ids passed to updateCellId. Merging old ids into",
            " the same identifier")
    
        if(ncol(sensNumber(object)) > 0) {
            sensMatch <- match(rownames(sensNumber(object)), 
                rownames(cellInfo(object)))
        }
        if(dim(pertNumber(object))[[2]] > 0) {
            pertMatch <- match(dimnames(pertNumber(object))[[1]], 
                rownames(cellInfo(object)))
        }

        curMatch <- match(rownames(curation(object)$cell), 
            rownames(cellInfo(object)))  
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
        curation(object)$cell[myx[1],] <- apply(curation(object)$cell[myx, ], 2, 
            FUN=paste, collapse="///")
        curation(object)$cell <- curation(object)$cell[-myx[-1], ]
        curation(object)$tissue[myx[1],] <- apply(curation(object)$tissue[myx, ], 
            2, FUN=paste, collapse="///")
        curation(object)$tissue <- curation(object)$tissue[-myx[-1], ]
        
        myx <- which(new.ids == id)
        cellInfo(object)[myx[1],] <- apply(cellInfo(object)[myx,], 2, 
            FUN=paste, collapse="///")
        cellInfo(object) <- cellInfo(object)[-myx[-1], ]
        new.ids <- new.ids[-myx[-1]]
        if(ncol(sensNumber(object)) > 0){
            sensMatch <- match(rownames(sensNumber(object)), 
                rownames(cellInfo(object)))
        }
        if(dim(pertNumber(object))[[1]] > 0){
            pertMatch <- match(dimnames(pertNumber(object))[[1]], 
                rownames(cellInfo(object)))
        }
        curMatch <- match(rownames(curation(object)$cell), 
            rownames(cellInfo(object)))
        }
    } else {
        if (dim(pertNumber(object))[[1]] > 0) {
            pertMatch <- match(dimnames(pertNumber(object))[[1]], 
                rownames(cellInfo(object)))
        }
        if (ncol(sensNumber(object)) > 0) {
            sensMatch <- match(rownames(sensNumber(object)), 
                rownames(cellInfo(object)))
        }
        curMatch <- match(rownames(curation(object)$cell), 
            rownames(cellInfo(object)))
    } 
    if (dim(pertNumber(object))[[1]] > 0) {
        dimnames(pertNumber(object))[[1]] <- new.ids[pertMatch]
    }
    if (ncol(sensNumber(object)) > 0) {
        rownames(sensNumber(object)) <- new.ids[sensMatch]
    }
    rownames(curation(object)$cell) <- new.ids[curMatch]
    rownames(curation(object)$tissue) <- new.ids[curMatch]
    rownames(cellInfo(object)) <- new.ids   
    return(object)  
}

# updateFeatureNames <- function(object, new.ids=vector("character")){
#
#   if (length(new.ids)!=nrow(cellInfo(object))){
#     stop("Wrong number of cell identifiers")
#   }
#
#   if(datasetType(object)=="sensitivity"|datasetType(object)=="both"){
#     myx <- match(sensitivityInfo(object)[,"cellid"],rownames(cellInfo(object)))
#     sensitivityInfo(object)[,"cellid"] <- new.ids[myx]
#
#   }
#
#   molecularProfilesSlot(object) <- lapply(molecularProfilesSlot(object), function(eset){
#
#     myx <- match(colData(eset)[["cellid"]],rownames(cellInfo(object)))
#     colData(eset)[["cellid"]]  <- new.ids[myx]
#     return(eset)
#       })
#   myx <- match(rownames(curation(object)$cell),rownames(cellInfo(object)))
#   rownames(curation(object)$cell) <- new.ids[myx]
#   rownames(curation(object)$tissue) <- new.ids[myx]
#   if (dim(pertNumber(object))[[1]]>0){
#     myx <- match(dimnames(pertNumber(object))[[1]], rownames(cellInfo(object)))
#     dimnames(pertNumber(object))[[1]] <- new.ids[myx]
#   }
#   if (nrow(sensNumber(object))>0){
#     myx <- match(rownames(sensNumber(object)), rownames(cellInfo(object)))
#     rownames(sensNumber(object)) <- new.ids[myx]
#   }
#   rownames(cellInfo(object)) <- new.ids
#   return(object)
#
# }

.summarizeSensitivityNumbers <- function(object) {

    if (datasetType(object) != "sensitivity" && datasetType(object) != "both") {
        stop ("Data type must be either sensitivity or both")
    }

    ## unique drug identifiers
    # drugn <- sort(unique(sensitivitySlot(object)$info[ , "drugid"]))

    ## consider all drugs
    drugn <- rownames(drugInfo(object))

    ## unique drug identifiers
    # celln <- sort(unique(sensitivitySlot(object)$info[ , "cellid"]))

    ## consider all cell lines
    celln <- rownames(cellInfo(object))

    sensitivity.info <- matrix(0, nrow=length(celln), ncol=length(drugn), 
        dimnames=list(celln, drugn))
    drugids <- sensitivityInfo(object)[, "drugid"]
    cellids <- sensitivityInfo(object)[, "cellid"]
    cellids <- cellids[grep("///", drugids, invert=TRUE)]
    drugids <- drugids[grep("///", drugids, invert=TRUE)]

    tt <- table(cellids, drugids)
    sensitivity.info[rownames(tt), colnames(tt)] <- tt

    return(sensitivity.info)
}

#' @export
#' @keywords internal
.summarizeMolecularNumbers <- function(object) {

    ## consider all molecular types
    mDT <- mDataNames(object)

    ## consider all cell lines
    celln <- rownames(cellInfo(object))

    molecular.info <- matrix(0, nrow=length(celln), ncol=length(mDT), 
        dimnames=list(celln, mDT))

    for(mDataType in mDT) {
        tt <- table(phenoInfo(object, mDataType)$cellid)
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
    drugn <- rownames(drugInfo(object))

    ## consider all cell lines
    celln <- rownames(cellInfo(object))

    perturbation.info <- array(0, dim=c(length(celln), length(drugn), 
        length(molecularProfilesSlot(object))),
        dimnames=list(celln, drugn, names((molecularProfilesSlot(object)))))

    for (i in seq_len(length(molecularProfilesSlot(object)))) {
        if (nrow(colData(molecularProfilesSlot(object)[[i]])) > 0 && 
                all(is.element(c("cellid", "drugid"), 
                    colnames(colData(molecularProfilesSlot(object)[[i]]))))) {
            tt <- table(colData(molecularProfilesSlot(object)[[i]])[ , "cellid"],
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
#' that matching of cells and drugs can be properly done across different types
#' of data and with other studies.
#' 
#' @examples
#' checkCsetStructure(clevelandSmall_cSet)
#' 
#' @param cSet A `CoreSet` to be verified
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
            msg <- c(msg, paste0(nn, "number of cell lines in colData is ",
                "different from expression slots", nn))
        }

        # Checking sample metadata for required columns
        if (!("cellid" %in% colnames(colData(profile)))) {
            msg <- c(msg, paste0(nn, " cellid does not exist in colData ",
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

        # Check that all cellids from the cSet are included in molecularProfiles
        if ("cellid" %in% colnames(rowData(profile))) {
            if (!all(colData(profile)[, "cellid"] %in% rownames(cellInfo(object)))) {
                msg <- c(msg, paste0(nn, " not all the cell lines in this ",
                    "profile are in cell lines slot"))
            }
        } else {
            msg <- c(msg, paste0(nn, " cellid does not exist in colData ",
                "(samples)"))
        }
    }

    #####
    # Checking cell
    #####
    if ("tissueid" %in% colnames(cellInfo(object))) {
        if ("unique.tissueid" %in% colnames(curation(object)$tissue)) {
            if (length(intersect(rownames(curation(object)$tissue), 
                    rownames(cellInfo(object)))) != nrow(cellInfo(object))) {
                msg <- c(msg, paste0("rownames of curation tissue slot should",
                    " be the same as cell slot (curated cell ids)"))
            } else {
                if (length(intersect(cellInfo(object)$tissueid, 
                        curation(object)$tissue$unique.tissueid)) != 
                            length(table(cellInfo(object)$tissueid))) {
                    msg <- c(msg, paste0("tissueid should be the same as unique",
                        " tissue id from tissue curation slot"))
                }
            }
        } else {
            msg <- c(msg, paste0("unique.tissueid which is curated tissue id",
                " across data set should be a column of tissue curation slot"))
        }
        if (any(is.na(cellInfo(object)[,"tissueid"]) || 
                cellInfo(object)[, "tissueid"] == "", na.rm=TRUE)) {
            msg <- c(msg, paste0(
                    "There is no tissue type for this cell line(s)",
                    paste(
                        rownames(cellInfo(object))[
                            which(is.na(cellInfo(object)[,"tissueid"]) | 
                                cellInfo(object)[,"tissueid"] == "")
                            ], 
                        collapse=" ")))
        }
    } else {
        msg <- c(msg, "tissueid does not exist in cell slot")
    }

    if("unique.cellid" %in% colnames(curation(object)$cell)) {
        if (length(intersect(curation(object)$cell$unique.cellid, 
                rownames(cellInfo(object)))) != nrow(cellInfo(object))) {
            msg <- c(msg, "rownames of cell slot should be curated cell ids")
        }
    } else {
        msg <- c(msg, paste0("unique.cellid which is curated cell id across",
            " data set should be a column of cell curation slot"))
    }

    if (length(intersect(rownames(curation(object)$cell), 
            rownames(cellInfo(object)))) != nrow(cellInfo(object))) {
        msg <- c(msg, paste0("rownames of curation cell slot should be the",
            " same as cell slot (curated cell ids)"))
    }

    if (!is(cellInfo(object), "data.frame")) {
        msg <- c(msg, "cell slot class type should be dataframe")
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
    hasCellId <- any(colColNameL %in% 'cellid')
    if (!all(hasCellId)) {
        nmsg <- .formatMessage('All SummarizedExperiments must have a cellid
            column. This is not the case for ', 
            paste(names(which(!hasCellId)), collapse=', '), '!')
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

    # ---- Check all samples are in the @cell slot
    samples <- cellNames(object)
    cellIdL <- as(lapply(colDataL, `[[`, i='cellid'), 'List')
    hasValidSamples <- cellIdL %in% samples
    if (!all(all(hasValidSamples))) {
        nmsg <- .formatMessage('All cellids in the @molecularProfiles slot
            must also be in the @cell slot. This is not the case
            for ', paste(names(which(all(hasValidSamples))), collapse=', '))
        msg <- c(msg, nmsg)
    }

    # ---- Return messages if something is wrong, or TRUE if everything is good
    return(if (length(msg)) msg else TRUE)
}

.checkTreatmentResponse <- function(object) {
    msg <- character()
    # ---- Extract sensitivity data
    samples <- cellNames(object)
    sensSlot <- sensitivitySlot(object)
    if (!is(sensSlot, "TreatmentResponseExperiment")) {
        nmsg <- "The treatmentReponse parameter must be a 
            TreatmentResponseExperiment!"
        msg <- c(msg, nmsg)
        return(msg)
    }
    return(if (length(msg)) msg else TRUE)
}