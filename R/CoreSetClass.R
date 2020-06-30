#' A Superclass to Contain Data for Genetic Profiling and Viability Screens of Cancer Cell Lines
#' 
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
#' @param mDataType A \code{character} with the type of molecular data to return/update
#' @param object A \code{CoreSet} object
#' @param value A replacement value
#' 
#' @slot annotation A \code{list} of annotation data about the CoreSet,
#'    including the \code{$name} and the session information for how the object
#'    was creating, detailing the exact versions of R and all the packages used
#' @slot molecularProfiles A \code{list} containing \code{SummarizedExperiment}s 
#'   type object for holding data for RNA, DNA, SNP and Copy Number Variation 
#'   measurements respectively, with associated \code{rowData} and \code{colData} 
#'   containing the row and column metadata
#' @slot cell A \code{data.frame} containg the annotations for all the cell 
#'   lines profiled in the data set, across all data types
#' @slot sensitivity A \code{list} containing all the data for the sensitivity 
#'   experiments, including \code{$info}, a \code{data.frame} containing the 
#'   experimental info,\code{$raw} a 3D \code{array} containing raw data, 
#'   \code{$profiles}, a \code{data.frame} containing sensitivity profiles 
#'   statistics, and \code{$n}, a \code{data.frame} detailing the number of 
#'   experiments for each cell-drug/radiationInfo pair
#' @slot perturbation A \code{list} containting \code{$n}, a \code{data.frame} 
#'   summarizing the available perturbation data,
#' @slot curation A \code{list} containing mappings for
#'   \code{cell}, \code{tissue} names  used in the data set to universal 
#'   identifiers used between different CoreSet objects
#' @slot datasetType A \code{character} string of 'sensitivity', 
#'   'perturbation', or both detailing what type of data can be found in the 
#'   CoreSet, for proper processing of the data
#' 
#' @return An object of the CoreSet class
#' 
#' @exportClass CoreSet
#' @export
.CoreSet <- setClass("CoreSet", slots = list(
                                            annotation = "list",
                                            molecularProfiles = "list",
                                            cell="data.frame", 
                                            datasetType="character", 
                                            sensitivity="list",
                                            perturbation="list",
                                            curation="list"
                                            ))


# The default constructor above does a poor job of explaining the required structure of a CoreSet. 
# The constructor function defined below guides the user into providing the required components of the curation and senstivity lists
# and hides the annotation slot which the user does not need to manually fill. 
# This also follows the design of the Expression Set class.

#####
## CONSTRUCTOR ----
#####

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
#' @return An object of class CoreSet
#' @export
#' 
#' @import methods
#' @importFrom utils sessionInfo
#' @importFrom stats na.omit
#' @importFrom SummarizedExperiment rowData colData assays
CoreSet <-  function(name, 
                          molecularProfiles=list(), 
                          cell=data.frame(), 
                          sensitivityInfo=data.frame(),
                          sensitivityRaw=array(dim=c(0,0,0)), 
                          sensitivityProfiles=matrix(), 
                          sensitivityN=matrix(nrow=0, ncol=0), 
                          perturbationN=array(NA, dim=c(0,0,0)), 
                          curationCell = data.frame(), 
                          curationTissue = data.frame(), 
                          datasetType=c("sensitivity", "perturbation", "both"),
                          verify = TRUE)
{
    datasetType <- match.arg(datasetType)
    
    annotation <- list()
    annotation$name <- as.character(name)
    annotation$dateCreated <- date()
    annotation$sessionInfo <- sessionInfo()
    annotation$call <- match.call()
    
    for (i in seq_len(length(molecularProfiles))){
        if (!is(molecularProfiles[[i]], "SummarizedExperiment")) {
            stop(sprintf("Please provide the %s data as a SummarizedExperiment", names(molecularProfiles[i])))
        } else {
          rowData(molecularProfiles[[i]]) <- 
            rowData(molecularProfiles[[i]])[rownames(assays(molecularProfiles[[i]])[[1]]), , drop=FALSE]
          colData(molecularProfiles[[i]]) <- 
            colData(molecularProfiles[[i]])[colnames(assays(molecularProfiles[[i]])[[1]]), , drop=FALSE]
        }
    }
    
    sensitivity <- list()
    
    if (!all(rownames(sensitivityInfo) == rownames(sensitivityProfiles) & 
             rownames(sensitivityInfo) == dimnames(sensitivityRaw)[[1]])){
        stop("Please ensure all the row names match between the sensitivity data.")
    }
    
    sensitivity$info <- as.data.frame(sensitivityInfo, stringsAsFactors = FALSE)
    sensitivity$raw <- sensitivityRaw
    sensitivity$profiles <- as.data.frame(sensitivityProfiles, stringsAsFactors = FALSE)
    sensitivity$n <- sensitivityN
    
    curation <- list()
    curation$cell <- as.data.frame(curationCell, stringsAsFactors = FALSE)
    curation$tissue <- as.data.frame(curationTissue, stringsAsFactors = FALSE)
    ### TODO:: Make sure to fix the curation to check for matching row names to the drug and cell line matrices!!!!!!
    
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

  if(length(sensitivityN) == 0 & datasetType %in% c("sensitivity", "both")) {
    sensNumber(object) <- .summarizeSensitivityNumbers(object)
  }
    if(length(perturbationN) == 0  & datasetType %in% c("perturbation", "both")) {
      pertNumber(object) <- .summarizePerturbationNumbers(object)
    }
  return(object)
}


#' Show a CoreSet
#' 
#' @param object \code{CoreSet}
#' 
#' @examples
#' show(clevelandSmall_cSet)
#' 
#' @return Prints the CoreSet object to the output stream, and returns invisible NULL. 
#' @export
setMethod("show", signature=signature(object="CoreSet"), 
    function(object) {
        cat("Name: ", name(object), "\n")
        cat("Date Created: ", dateCreated(object), "\n")
    cat("Number of cell lines: ", nrow(cellInfo(object)), "\n")
        if("dna" %in% names(object@molecularProfiles)){cat("DNA: \n");cat("\tDim: ", dim(molecularProfiles(object, mDataType="dna")), "\n")}
      if("rna" %in% names(object@molecularProfiles)){cat("RNA: \n");cat("\tDim: ", dim(molecularProfiles(object, mDataType="rna")), "\n")}
      if("rnaseq" %in% names(object@molecularProfiles)){cat("RNASeq: \n");cat("\tDim: ", dim(molecularProfiles(object, mDataType="rnaseq")), "\n")}
      if("snp" %in% names(object@molecularProfiles)){cat("SNP: \n");cat("\tDim: ", dim(molecularProfiles(object, mDataType="snp")), "\n")}
      if("cnv" %in% names(object@molecularProfiles)){cat("CNV: \n");cat("\tDim: ", dim(molecularProfiles(object, mDataType="cnv")), "\n")}
        cat("Drug pertubation: \n")
        cat("\tPlease look at pertNumber(cSet) to determine number of experiments for each drug-cell combination.\n")
        cat("Drug sensitivity: \n")
        cat("\tNumber of Experiments: ", nrow(sensitivityInfo(object)),"\n")
        cat("\tPlease look at sensNumber(cSet) to determine number of experiments for each drug-cell combination.\n")
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
#' @export
updateCellId <- function(object, new.ids = vector("character")){
  
  if (length(new.ids)!=nrow(cellInfo(object))){
    stop("Wrong number of cell identifiers")
  }

  if(object@datasetType=="sensitivity"|object@datasetType=="both"){
    myx <- match(sensitivityInfo(object)[,"cellid"],rownames(cellInfo(object)))
    sensitivityInfo(object)[,"cellid"] <- new.ids[myx]

  }
  
  
  object@molecularProfiles <- lapply(object@molecularProfiles, function(SE){
    
    myx <- match(SummarizedExperiment::colData(SE)[["cellid"]], rownames(cellInfo(object)))
    SummarizedExperiment::colData(SE)[["cellid"]]  <- new.ids[myx]
    return(SE)
  })

  if(any(duplicated(new.ids))){
    warning("Duplicated ids passed to updateCellId. Merging old ids into the same identifier")
    
    if(ncol(sensNumber(object))>0) {
      sensMatch <- match(rownames(sensNumber(object)), rownames(cellInfo(object)))
    }
    if(dim(pertNumber(object))[[2]]>0) {
      pertMatch <- match(dimnames(pertNumber(object))[[1]], rownames(cellInfo(object)))
    }
    curMatch <- match(rownames(object@curation$cell),rownames(cellInfo(object)))

    duplId <- unique(new.ids[duplicated(new.ids)])
    for(id in duplId){

      if (ncol(sensNumber(object))>0){
        myx <- which(new.ids[sensMatch] == id)
        sensNumber(object)[myx[1],] <- apply(sensNumber(object)[myx,], 2, sum)
        sensNumber(object) <- sensNumber(object)[-myx[-1],]
        # sensMatch <- sensMatch[-myx[-1]]
      }
      if (dim(pertNumber(object))[[1]]>0){
        myx <- which(new.ids[pertMatch] == id)
        pertNumber(object)[myx[1],,] <- apply(pertNumber(object)[myx,,], c(1,3), sum)
        pertNumber(object) <- pertNumber(object)[-myx[-1],,]
        # pertMatch <- pertMatch[-myx[-1]]
      }

      myx <- which(new.ids[curMatch] == id)
      object@curation$cell[myx[1],] <- apply(object@curation$cell[myx,], 2, paste, collapse="///")
      object@curation$cell <- object@curation$cell[-myx[-1],]
      object@curation$tissue[myx[1],] <- apply(object@curation$tissue[myx,], 2, paste, collapse="///")
      object@curation$tissue <- object@curation$tissue[-myx[-1],]
      # curMatch <- curMatch[-myx[-1]]

      myx <- which(new.ids == id)
      cellInfo(object)[myx[1],] <- apply(cellInfo(object)[myx,], 2, paste, collapse="///")
      cellInfo(object) <- cellInfo(object)[-myx[-1],]
      new.ids <- new.ids[-myx[-1]]
      if(ncol(sensNumber(object))>0){
        sensMatch <- match(rownames(sensNumber(object)), rownames(cellInfo(object)))
      }
      if(dim(pertNumber(object))[[1]]>0){
        pertMatch <- match(dimnames(pertNumber(object))[[1]], rownames(cellInfo(object)))
      }
      curMatch <- match(rownames(object@curation$cell),rownames(cellInfo(object)))
    }
  } else {
    if (dim(pertNumber(object))[[1]]>0){
      pertMatch <- match(dimnames(pertNumber(object))[[1]], rownames(cellInfo(object)))
    }
    if (ncol(sensNumber(object))>0){
      sensMatch <- match(rownames(sensNumber(object)), rownames(cellInfo(object)))
    }
    curMatch <- match(rownames(object@curation$cell),rownames(cellInfo(object)))
  }

  if (dim(pertNumber(object))[[1]]>0){
    dimnames(pertNumber(object))[[1]] <- new.ids[pertMatch]
  }
  if (ncol(sensNumber(object))>0){
    rownames(sensNumber(object)) <- new.ids[sensMatch]
  }
  rownames(object@curation$cell) <- new.ids[curMatch]
  rownames(object@curation$tissue) <- new.ids[curMatch]
  rownames(cellInfo(object)) <- new.ids





  # myx <- match(rownames(object@curation$cell),rownames(cellInfo(object)))
  # rownames(object@curation$cell) <- new.ids[myx]
  # rownames(object@curation$tissue) <- new.ids[myx]
  # if (dim(pertNumber(object))[[1]]>0){
  #   myx <- match(dimnames(pertNumber(object))[[1]], rownames(cellInfo(object)))
  #   dimnames(pertNumber(object))[[1]] <- new.ids[myx]
  # }
  # if (nrow(sensNumber(object))>0){
  #   myx <- match(rownames(sensNumber(object)), rownames(cellInfo(object)))
  #   rownames(sensNumber(object)) <- new.ids[myx]
  # }
  # rownames(cellInfo(object)) <- new.ids
  return(object)

}

# updateFeatureNames <- function(object, new.ids = vector("character")){
#
#   if (length(new.ids)!=nrow(cellInfo(object))){
#     stop("Wrong number of cell identifiers")
#   }
#
#   if(object@datasetType=="sensitivity"|object@datasetType=="both"){
#     myx <- match(sensitivityInfo(object)[,"cellid"],rownames(cellInfo(object)))
#     sensitivityInfo(object)[,"cellid"] <- new.ids[myx]
#
#   }
#
#   object@molecularProfiles <- lapply(object@molecularProfiles, function(eset){
#
#     myx <- match(colData(eset)[["cellid"]],rownames(cellInfo(object)))
#     colData(eset)[["cellid"]]  <- new.ids[myx]
#     return(eset)
#       })
#   myx <- match(rownames(object@curation$cell),rownames(cellInfo(object)))
#   rownames(object@curation$cell) <- new.ids[myx]
#   rownames(object@curation$tissue) <- new.ids[myx]
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

  if (object@datasetType != "sensitivity" && object@datasetType != "both") {
    stop ("Data type must be either sensitivity or both")
  }
  
  ## unique drug identifiers
  # drugn <- sort(unique(object@sensitivity$info[ , "drugid"]))
  
  ## consider all drugs
  drugn <- rownames(object@drug)
  
  ## unique drug identifiers
  # celln <- sort(unique(object@sensitivity$info[ , "cellid"]))
  
  ## consider all cell lines
  celln <- rownames(object@cell)
  
  sensitivity.info <- matrix(0, nrow=length(celln), ncol=length(drugn), dimnames=list(celln, drugn))
  drugids <- object@sensitivity$info[ , "drugid"]
  cellids <- object@sensitivity$info[ , "cellid"]
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
  celln <- rownames(object@cell)
  
  molecular.info <- matrix(0, nrow=length(celln), ncol=length(mDT), dimnames=list(celln, mDT))
  
  for(mDataType in mDT) {
    tt <- table(phenoInfo(object, mDataType)$cellid)
    molecular.info[names(tt), mDataType] <- tt
  }
  return(molecular.info)
}

.summarizePerturbationNumbers <- function(object) {

  if (object@datasetType != "perturbation" && object@datasetType != "both") {
    stop ("Data type must be either perturbation or both")
  }
  
  ## unique drug identifiers
  # drugn <- sort(unique(unlist(lapply(object@molecularProfiles, function (x) {
  #   res <- NULL
  #   if (nrow(colData(x)) > 0 & "drugid" %in% colnames(colData(x))) {
  #     res <- colData(x)[ , "drugid"]
  #   }
  #   return (res)
  # }))))
  
  ## consider all drugs
  drugn <- rownames(object@drug)
  
  ## unique cell line identifiers
  # celln <- sort(unique(unlist(lapply(object@molecularProfiles, function (x) {
  #   res <- NULL
  #   if (nrow(colData(x)) > 0 & "cellid" %in% colnames(colData(x))) {
  #     res <- colData(x)[ , "cellid"]
  #   }
  #   return (res)
  # }))))
  
  ## consider all cell lines
  celln <- rownames(object@cell)
  
  perturbation.info <- array(0, dim=c(length(celln), length(drugn), length(object@molecularProfiles)), dimnames=list(celln, drugn, names((object@molecularProfiles))))
  
  for (i in seq_len(length(object@molecularProfiles))) {
    if (nrow(SummarizedExperiment::colData(object@molecularProfiles[[i]])) > 0 && all(is.element(c("cellid", "drugid"), colnames(SummarizedExperiment::colData(object@molecularProfiles[[i]]))))) {
      tt <- table(SummarizedExperiment::colData(object@molecularProfiles[[i]])[ , "cellid"], SummarizedExperiment::colData(object@molecularProfiles[[i]])[ , "drugid"])
      perturbation.info[rownames(tt), colnames(tt), names(object@molecularProfiles)[i]] <- tt
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
#' @param cSet A \code{CoreSet} to be verified
#' @param plotDist Should the function also plot the distribution of molecular data?
#' @param result.dir The path to the directory for saving the plots as a string
#' 
#' @return Prints out messages whenever describing the errors found in the structure of the cSet object passed in.
#' 
#' @export
#' 
#' @importFrom graphics hist
#' @importFrom grDevices dev.off pdf
checkCsetStructure <-
  function(cSet, plotDist=FALSE, result.dir=".") {
    
    # Make directory to store results if it doesn't exist
    if(!file.exists(result.dir) & plotDist) { dir.create(result.dir, showWarnings=FALSE, recursive=TRUE) }
    
    ####
    ## Checking molecularProfiles
    ####
    for( i in seq_along(cSet@molecularProfiles)) {
      profile <- cSet@molecularProfiles[[i]]
      nn <- names(cSet@molecularProfiles)[i]
      
      # Testing plot rendering for rna and rnaseq
      if( (S4Vectors::metadata(profile)$annotation == "rna" | S4Vectors::metadata(profile)$annotation == "rnaseq") & plotDist)
      {
        pdf(file=file.path(result.dir, sprintf("%s.pdf", nn)))
        hist(SummarizedExperiment::assay(profile, 'exprs'), breaks = 100)
        dev.off()
      }
      
      ## Test if sample and feature annotations dimensions match the assay
      warning(ifelse(nrow(rowData(profile)) != nrow(assays(profile)$exprs),
                     sprintf("%s: number of features in rowData is different from SummarizedExperiment slots", nn),
                     sprintf("%s: rowData dimension is OK", nn)
                    )
              )
      warning(ifelse(nrow(colData(profile)) != ncol(assays(profile)$exprs),
                     sprintf("%s: number of cell lines in colData is different from expression slots", nn),
                     sprintf("%s: colData dimension is OK", nn)
                    )
              )
      
      # Checking sample metadata for required columns
      warning(ifelse("cellid" %in% colnames(colData(profile)), "", sprintf("%s: cellid does not exist in colData (samples) columns", nn)))
      warning(ifelse("batchid" %in% colnames(colData(profile)), "", sprintf("%s: batchid does not exist in colData (samples) columns", nn)))
      
      # Checking mDataType of the SummarizedExperiment for required columns
      if(S4Vectors::metadata(profile)$annotation == "rna" | S4Vectors::metadata(profile)$annotation == "rnaseq")
      {
        warning(ifelse("BEST" %in% colnames(rowData(profile)), "BEST is OK", sprintf("%s: BEST does not exist in rowData (features) columns", nn)))
        warning(ifelse("Symbol" %in% colnames(rowData(profile)), "Symbol is OK", sprintf("%s: Symbol does not exist in rowData (features) columns", nn)))
      }

      # Check that all cellids from the cSet are included in molecularProfiles
      if("cellid" %in% colnames(rowData(profile))) {
        if(!all(colData(profile)[,"cellid"] %in% rownames(cSet@cell))) {
          warning(sprintf("%s: not all the cell lines in this profile are in cell lines slot", nn))
        }
      }else {
        warning(sprintf("%s: cellid does not exist in colData (samples)", nn))
      }
    }
    
    #####
    # Checking cell
    #####
    if("tissueid" %in% colnames(cSet@cell)) {
      if("unique.tissueid" %in% colnames(cSet@curation$tissue))
      {
        if(length(intersect(rownames(cSet@curation$tissue), rownames(cSet@cell))) != nrow(cSet@cell)) {
          message("rownames of curation tissue slot should be the same as cell slot (curated cell ids)")
        } else{
          if(length(intersect(cSet@cell$tissueid, cSet@curation$tissue$unique.tissueid)) != length(table(cSet@cell$tissueid))){
            message("tissueid should be the same as unique tissue id from tissue curation slot")
          }
        }
      } else {
        message("unique.tissueid which is curated tissue id across data set should be a column of tissue curation slot")
      }
      if(any(is.na(cSet@cell[,"tissueid"]) | cSet@cell[,"tissueid"]=="", na.rm=TRUE)){
        message(sprintf("There is no tissue type for this cell line(s): %s", paste(rownames(cSet@cell)[which(is.na(cSet@cell[,"tissueid"]) | cSet@cell[,"tissueid"]=="")], collapse=" ")))
      }
    } else {
      warning("tissueid does not exist in cell slot")
    }
    
    if("unique.cellid" %in% colnames(cSet@curation$cell)) {
      if(length(intersect(cSet@curation$cell$unique.cellid, rownames(cSet@cell))) != nrow(cSet@cell)) {
        print("rownames of cell slot should be curated cell ids")
      }
    } else {
      print("unique.cellid which is curated cell id across data set should be a column of cell curation slot")
    }
    
    if(length(intersect(rownames(cSet@curation$cell), rownames(cSet@cell))) != nrow(cSet@cell)) {
      print("rownames of curation cell slot should be the same as cell slot (curated cell ids)")
    }
    
    if(!is(cSet@cell, "data.frame")) {
      warning("cell slot class type should be dataframe")
    }
    if(cSet@datasetType %in% c("sensitivity", "both"))
    {
      if(!is(cSet@sensitivity$info, "data.frame")) {
        warning("sensitivity info slot class type should be dataframe")
      }
      if("cellid" %in% colnames(cSet@sensitivity$info)) {
        if(!all(cSet@sensitivity$info[,"cellid"] %in% rownames(cSet@cell))) {
          warning("not all the cell lines in sensitivity data are in cell slot")
        }
      }else {
        warning("cellid does not exist in sensitivity info")
      }
    
      if(any(!is.na(cSet@sensitivity$raw))) {
        if(!all(dimnames(cSet@sensitivity$raw)[[1]] %in% rownames(cSet@sensitivity$info))) {
          warning("For some experiments there is raw sensitivity data but no experimet information in sensitivity info")
        }
      }
      if(!all(rownames(cSet@sensitivity$profiles) %in% rownames(cSet@sensitivity$info))) {
        warning("For some experiments there is sensitivity profiles but no experimet information in sensitivity info")
      }
    }
  }
