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
# @param cSet A \code{CoreSet} object ##TODO:: Is this needed?
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
                                            ##TODO:: Do we want these in the constuctor?
                                            # tables="array",
                                            # table.summary="list",
                                            # dateCreated="character",
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
    
    #molecularProfiles <- list("dna"=dna, "rna"=rna, "snp"=snp, "cnv"=cnv)
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
    
    if (!all(rownames(sensitivityInfo) == rownames(sensitivityProfiles) & rownames(sensitivityInfo) == dimnames(sensitivityRaw)[[1]])){
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
        perturbation$info <- "The metadata for the perturbation experiments is available for each molecular type by calling the appropriate info function. \n For example, for RNA transcriptome perturbations, the metadata can be accessed using rnaInfo(cSet)."
    } else {
        perturbation$info <- "Not a perturbation dataset."
    }
    
    object  <- .CoreSet(annotation=annotation, molecularProfiles=molecularProfiles, cell=as.data.frame(cell), datasetType=datasetType, sensitivity=sensitivity, perturbation=perturbation, curation=curation)
    if (verify) { checkCsetStructure(object)}
  if(length(sensitivityN) == 0 & datasetType %in% c("sensitivity", "both")) {
    object@sensitivity$n <- .summarizeSensitivityNumbers(object)
  }
    if(length(perturbationN) == 0  & datasetType %in% c("perturbation", "both")) {
      object@perturbation$n <- .summarizePerturbationNumbers(object)
    }
  return(object)
}
    
###
# CELL SLOT GETTERS/SETTERS ----
###

#' cellInfo Getter
#' 
#' Get cell line information from a PharmacoSet object
#' 
#' @examples
#' data(clevelandSmall_cSet)
#' cellInf <- cellInfo(clevelandSmall_cSet)
#' 
#' @param object The \code{CoreSet} to retrieve cell info from
#' @param ... \code{list} Fall through arguments to allow generic 
#'   to be defined with different parameters
#' 
#' @return a \code{data.frame} with the cell annotations
#'
#' @import methods
#'
#' @export
setGeneric("cellInfo", function(object, ...) standardGeneric("cellInfo"))
#' @describeIn CoreSet Returns the annotations for all the cell lines tested on in the CoreSet
#' @export
setMethod(cellInfo, "CoreSet", function(object){
  object@cell
})


#' cellInfo<- Generic
#' 
#' Generic for cellInfo replace method
#' 
#' @examples
#' cellInfo(clevelandSmall_cSet) <- cellInfo(clevelandSmall_cSet)
#' 
#' @param object The \code{CoreSet} to replace cell info in
#' @param value A \code{data.frame} with the new cell annotations
#' 
#' @return Updated \code{CoreSet}
#' 
#' @export
setGeneric("cellInfo<-", function(object, value) standardGeneric("cellInfo<-"))
#' @describeIn CoreSet Update the cell line annotations
#' @export
setReplaceMethod("cellInfo", signature = signature(object="CoreSet", value="data.frame"), function(object, value){
  if(is.null(rownames(value))){
    stop("Please provide the cell_id as rownames for the cell line annotations")
  }
  object@cell <- value
  object
})

#####
# MOLECULAR PROFILES SLOT GETTERS/SETTERS ---------------------------------
#####

#' phenoInfo Generic
#' 
#' Generic for phenoInfo method 
#' 
#' @examples
#' phenoInfo(clevelandSmall_cSet, mDataType="rna")
#' 
#' @param object The \code{CoreSet} to retrieve rna annotations from
#' @param mDataType the type of molecular data
#' @param ... Fallthrough argument for defining new parameters in other S4 methods
#' 
#' @return a \code{data.frame} with the experiment info
#' 
setGeneric("phenoInfo", function(object, mDataType, ...) standardGeneric("phenoInfo"))
#'
#' @importFrom SummarizedExperiment colData
#' @describeIn CoreSet Return the experiment info from the given type of molecular data in CoreSet 
#' @export
setMethod(phenoInfo, "CoreSet", function(object, mDataType){
    
  if(mDataType %in% names(object@molecularProfiles)){ # Columns = Samples
    return(colData(object@molecularProfiles[[mDataType]]))
  }else{
    return(NULL)
  }
})

#' phenoInfo<- Generic
#' 
#' Generic for phenoInfo replace method
#' 
#' @examples
#' data(clevelandSmall_cSet)
#' phenoInfo(clevelandSmall_cSet, mDataType="rna") <- phenoInfo(clevelandSmall_cSet, mDataType="rna")
#' 
#' @param object The \code{CoreSet} to retrieve molecular experiment annotations from
#' @param mDataType the type of molecular data 
#' @param value a \code{dataframe}  with the new experiment annotations
#' 
#' @return The updated \code{CoreSet}
#' 
setGeneric("phenoInfo<-", function(object, mDataType, value) standardGeneric("phenoInfo<-"))
#' @describeIn CoreSet Update the given type of molecular data experiment info in the CoreSet 
#' @importFrom SummarizedExperiment colData colData<-
#' @importFrom S4Vectors DataFrame
#' @export
setReplaceMethod("phenoInfo", signature = signature(object="CoreSet", mDataType ="character", value="data.frame"), function(object, mDataType, value){
  if(mDataType %in% names(object@molecularProfiles)){
    SummarizedExperiment::colData(object@molecularProfiles[[mDataType]]) <- S4Vectors::DataFrame(value, rownames = rownames(value))
  }
  object
})
#' @describeIn CoreSet Update the given type of molecular data experiment info in the CoreSet
#' @export 
setReplaceMethod("phenoInfo", signature = signature(object="CoreSet", mDataType ="character", value="DataFrame"), function(object, mDataType, value){
  if(mDataType %in% names(object@molecularProfiles)){
    SummarizedExperiment::colData(object@molecularProfiles[[mDataType]]) <- value
  }
  object
})

#' molecularProfiles Generic
#' 
#' Generic for molecularProfiles method 
#' 
#' @examples
#' data(clevelandSmall_cSet)
#' molecularProfiles(clevelandSmall_cSet, "rna")
#' 
#' @param object The \code{CoreSet} to retrieve molecular profiles from
#' @param mDataType \code{character} The type of molecular data
#' @param assay \code{character} Name of the desired assay; if excluded 
#'   defaults to first assay in the SummarizedExperiment for the given 
#'   mDataType. Use \code{assayNames(molecularProfiles(object, dataType)}
#'   to check which assays are available for a given molecular datatype.
#' @param ... Fallthrough arguements for defining new methods
#' 
#' @return a \code{matrix} of data for the given mDataType and assay
#' 
#' @importClassesFrom S4Vectors DataFrame SimpleList
#' @importFrom S4Vectors DataFrame
#' @importFrom SummarizedExperiment colData assay assayNames
setGeneric("molecularProfiles", function(object, mDataType, assay, ...) standardGeneric("molecularProfiles"))
#' @describeIn CoreSet Return the given type of molecular data from the CoreSet 
#' @inheritParams molecularProfiles
#' @export
setMethod(molecularProfiles, "CoreSet", function(object, mDataType, assay){
  ## TODO:: Add an all option that returns a list?
  if(mDataType %in% names(object@molecularProfiles)){
    if (!missing(assay)) {
      if (assay %in% assayNames(object@molecularProfiles[[mDataType]])) {
        return(SummarizedExperiment::assay(object@molecularProfiles[[mDataType]], assay))
      } else {
        stop(paste('Assay', assay, 'not found in the SummarizedExperiment object!'))
      }
    } else {
      return(SummarizedExperiment::assay(object@molecularProfiles[[mDataType]], 1))
    }
  } else {
    stop(paste0('mDataType ', mDataType, ' not found the object!'))
  }
})

#' molecularProfiles<- Generic
#' 
#' Generic for molecularProfiles replace method
#' 
#' @examples
#' data(clevelandSmall_cSet)
#' molecularProfiles(clevelandSmall_cSet, "rna") <- molecularProfiles(clevelandSmall_cSet, "rna")
#' 
#' @param object The \code{CoreSet} to replace molecular profiles in
#' @param mDataType The type of molecular data to be updated
#' @param assay \code{character} Name or index of the assay data to return
#' @param value A \code{matrix} with the new profiles
#' 
#' @return Updated \code{CoreSet}
#' 
setGeneric("molecularProfiles<-", function(object, mDataType, assay, value) standardGeneric("molecularProfiles<-"))
#' @describeIn CoreSet Update the given type of molecular data from the CoreSet 
#' @importFrom SummarizedExperiment assay
#' @inheritParams molecularProfiles<-
#' @export
setReplaceMethod("molecularProfiles", signature = signature(object="CoreSet", mDataType ="character", assay="character", value="matrix"), function(object, mDataType, assay, value){
  if (mDataType %in% names(object@molecularProfiles)) {
    SummarizedExperiment::assay(object@molecularProfiles[[mDataType]], assay) <- value
  }
  object
})
#' @describeIn CoreSet Update the given type of molecular data from the CoreSet 
#' @inheritParams molecularProfiles<-
#' @export
setReplaceMethod("molecularProfiles", signature = signature(object="CoreSet", mDataType ="character", assay="missing", value="matrix"), function(object, mDataType, assay, value){
  if (mDataType %in% names(object@molecularProfiles)) {
    SummarizedExperiment::assay(object@molecularProfiles[[mDataType]], 1) <- value
  }
  object
})

#' featureInfo Generic
#' 
#' Generic for featureInfo method 
#' 
#' @examples
#' featureInfo(clevelandSmall_cSet, "rna")
#' 
#' @param object The \code{CoreSet} to retrieve feature annotations from
#' @param mDataType the type of molecular data
#' @param ... Fallthrough arguements for defining new methods
#' 
#' @return a \code{data.frame} with the feature annotations
#' 
setGeneric("featureInfo", function(object, mDataType, ...) standardGeneric("featureInfo"))
#' 
#' @describeIn CoreSet Return the feature info for the given molecular data 
#' @importFrom SummarizedExperiment rowData rowData<-
#' @export
setMethod(featureInfo, "CoreSet", function(object, mDataType) {
  if(mDataType %in% names(object@molecularProfiles)){
    return(rowData(object@molecularProfiles[[mDataType]]))
  }else{
    return(NULL)
  }
})

#' featureInfo<- Generic
#' 
#' Generic for featureInfo replace method
#' 
#' @examples
#' featureInfo(clevelandSmall_cSet, "rna") <- featureInfo(clevelandSmall_cSet, "rna")
#' 
#' @param object The \code{CoreSet} to replace gene annotations in
#' @param mDataType The type of molecular data to be updated
#' @param value A \code{data.frame} with the new feature annotations
#' 
#' @return Updated \code{CoreSet}
#' 
#' @export
setGeneric("featureInfo<-", function(object, mDataType, value) standardGeneric("featureInfo<-"))
#' @describeIn CoreSet Replace the gene info for the molecular data
#' @importFrom SummarizedExperiment rowData rowData<-
#' @importFrom S4Vectors DataFrame
#' 
#' @export
setReplaceMethod("featureInfo", signature = signature(object="CoreSet", mDataType ="character",value="data.frame"), function(object, mDataType, value){
  
  if(mDataType %in% names(object@molecularProfiles)){
    rowData(object@molecularProfiles[[mDataType]]) <- 
      S4Vectors::DataFrame(value, rownames = rownames(value))
  }
  object
})
#' @describeIn CoreSet Replace the gene info for the molecular data
#'@export
setReplaceMethod("featureInfo", signature = signature(object="CoreSet", mDataType ="character",value="DataFrame"), function(object, mDataType, value){
  
  if(mDataType %in% names(object@molecularProfiles)){
    rowData(object@molecularProfiles[[mDataType]]) <- 
      S4Vectors::DataFrame(value, rownames = rownames(value))
  }
  object
})


#####
# SENSITIVITY SLOT GETTERS/SETTERS
#####

#' sensitivityInfo Generic
#' 
#' Generic for sensitivityInfo method 
#' 
#' @examples
#' sensitivityInfo(clevelandSmall_cSet)
#' 
#' @param object The \code{CoreSet} to retrieve sensitivity experiment annotations from
#' @param ... Fallthrough arguments for defining new methods
#' 
#' @return a \code{data.frame} with the experiment info
setGeneric("sensitivityInfo", function(object, ...) standardGeneric("sensitivityInfo"))
#' @describeIn CoreSet Return the drug dose sensitivity experiment info
#' @export
setMethod(sensitivityInfo, "CoreSet", function(object){
    return(object@sensitivity$info)
})

#' sensitivityInfo<- Generic
#' 
#' A generic for the sensitivityInfo replacement method
#' 
#' 
#' @examples
#' sensitivityInfo(clevelandSmall_cSet) <- sensitivityInfo(clevelandSmall_cSet)
#' 
#' @param object The \code{CoreSet} to update
#' @param value A \code{data.frame} with the new sensitivity annotations
#' @return Updated \code{CoreSet} 
#' 
setGeneric("sensitivityInfo<-", function(object, value) standardGeneric("sensitivityInfo<-"))
#' @describeIn CoreSet Update the sensitivity experiment info
#' @export
setReplaceMethod("sensitivityInfo", 
                 signature = signature(object="CoreSet", 
                                       value="data.frame"), 
                 function(object, value){
    object@sensitivity$info <- value
    object
})


#' sensitivityProfiles Generic
#' 
#' Generic for sensitivityProfiles method 
#' 
#' @examples
#' sensitivityProfiles(clevelandSmall_cSet)
#' 
#' @param object The \code{CoreSet} to retrieve sensitivity experiment data from
#' @param ... Fallthrough arguements for defining new methods
#' 
#' @return a \code{data.frame} with the experiment info
#' @export
setGeneric("sensitivityProfiles", function(object, ...) standardGeneric("sensitivityProfiles"))
#' @describeIn CoreSet Return the phenotypic data for the drug dose sensitivity
#' @export
setMethod(sensitivityProfiles, "CoreSet", function(object){
    return(object@sensitivity$profiles)
})

#' sensitivityProfiles<- Generic
#' 
#' A generic for the sensitivityProfiles replacement method
#' 
#' @examples
#' sensitivityProfiles(clevelandSmall_cSet) <- sensitivityProfiles(clevelandSmall_cSet)
#' 
#' @param object The \code{CoreSet} to update
#' @param value A \code{data.frame} with the new sensitivity profiles. If a matrix object is passed in, converted to data.frame before assignment
#' 
#' @return Updated \code{CoreSet} 
#' 
#' @export
setGeneric("sensitivityProfiles<-", function(object, value) standardGeneric("sensitivityProfiles<-"))
#' @describeIn CoreSet Update the phenotypic data for the drug dose
#'   sensitivity
#' @export
setReplaceMethod("sensitivityProfiles", 
                 signature = signature(object="CoreSet", 
                                       value="data.frame"), 
                 function(object, value){
    object@sensitivity$profiles <- value
    object
})
#' @describeIn CoreSet Update the phenotypic data for the drug dose
#'   sensitivity
#' @export
setReplaceMethod("sensitivityProfiles", 
                 signature = signature(object="CoreSet",
                                       value="matrix"), 
                 function(object, value) {
    object@sensitivity$profiles <- as.data.frame(value)
    object
})


#' sensitivityMeasures Generi
#' 
#' A generic for the sensitivityMeasures  method
#' 
#' @examples
#' sensitivityMeasures(clevelandSmall_cSet)
#' 
#' @param object The \code{CoreSet}
#' @param ... Fallthrough arguements for defining new methods
#'  
#' @return A \code{character} vector of all the available sensitivity measures
setGeneric("sensitivityMeasures", function(object, ...) standardGeneric("sensitivityMeasures"))
#' @describeIn CoreSet Returns the available sensitivity profile
#'   summaries, for example, whether there are IC50 values available
#' @export
setMethod(sensitivityMeasures, "CoreSet", function(object){
    
    return(colnames(sensitivityProfiles(object)))
})

#' cellNames Generic
#' 
#' A generic for the cellNames method
#' 
#' @examples
#' cellNames(clevelandSmall_cSet)
#' 
#' @param object The \code{CoreSet} to return cell names from
#' @param ... Fallthrough arguements for defining new methods
#'  
#' @return A vector of the cell names used in the CoreSet
setGeneric("cellNames", function(object, ...) standardGeneric("cellNames"))
#' @describeIn CoreSet Return the cell names used in the dataset
#' @export
setMethod(cellNames, "CoreSet", function(object){
  rownames(cellInfo(object))
})

#' cellNames<- Generic
#' 
#' A generic for the cellNames replacement method
#' 
#' @examples
#' cellNames(clevelandSmall_cSet) <- cellNames(clevelandSmall_cSet)
#' 
#' @param object The \code{CoreSet} to update
#' @param value A \code{character} vector of the new cell names
#' 
#' @return Updated \code{CoreSet} 
#' @export
setGeneric("cellNames<-", function(object, value) standardGeneric("cellNames<-"))
#' @describeIn CoreSet Update the cell names used in the dataset
#' @export
setReplaceMethod("cellNames", signature = signature(object="CoreSet",value="character"), function(object, value){
    object <- updateCellId(object, value)
    return(object)
    })


#' fNames Generic
#' 
#' A generic for the fNames method
#' 
#' @examples
#' fNames(clevelandSmall_cSet, "rna")
#' 
#' @param object The \code{CoreSet} 
#' @param mDataType The molecular data type to return feature names for
#' @param ... Fallthrough arguements for defining new methods
#' 
#' @return A \code{character} vector of the feature names
setGeneric("fNames", function(object, mDataType, ...) standardGeneric("fNames"))
#' @describeIn CoreSet Return the feature names used in the dataset
#' @export
setMethod(fNames, "CoreSet", function(object, mDataType){
  if (mDataType %in% names(object@molecularProfiles)) {
    rownames(featureInfo(object, mDataType))
  } else {
    stop(paste0("Molecular data type ", mDataType ," is not part of this ", class(object)))
  }
})

#' fNames<- Generic
#' 
#' A generic for the fNames replacement method
#' 
#' @examples
#' data(clevelandSmall_cSet)
#' fNames(clevelandSmall_cSet, "rna") <- fNames(clevelandSmall_cSet, "rna")
#' 
#' @param object The \code{CoreSet} to update
#' @param mDataType The molecular data type to update
#' @param value A \code{character} vector of the new cell names
#' 
#' @return Updated \code{CoreSet}
#' @export
setGeneric("fNames<-", function(object, mDataType, value)
  standardGeneric("fNames<-"))
#' @describeIn CoreSet Update the feature names used in a molecular profile
#' @export
setReplaceMethod("fNames", 
                 signature = signature(object="CoreSet", mDataType='character', 
                                       value="character"), 
                 function(object, mDataType, value)
{
  if (mDataType %in% names(object@molecularProfiles)) {
    rownames(featureInfo(object, mDataType)) <- as.character(value)
    object
  } else {
    stop(paste0(mDataType, " is not in the object. Possible mDataTypes are: ", names(object@molecularProfiles)))
  }
})

#' dateCreated Generic
#' 
#' A generic for the dateCreated method
#' 
#' @examples
#' dateCreated(clevelandSmall_cSet)
#' 
#' @param object A \code{CoreSet} 
#' @param ... Fallthrough arguements for defining new methods
#'  
#' @return The date the CoreSet was created
setGeneric("dateCreated", function(object, ...) standardGeneric("dateCreated"))
#' @describeIn CoreSet Return the date the CoreSet was created
#' @export
setMethod(dateCreated, "CoreSet", function(object) {
  object@annotation$dateCreated
})

#' name Generic
#' 
#' A generic for the name method
#' 
#' @examples
#' name(clevelandSmall_cSet)
#' 
#' @param object A \code{CoreSet} 
#' @param ... Fallthrough arguements for defining new methods
#' 
#' @return The name of the CoreSet
#' 
setGeneric("name", function(object, ...) standardGeneric("name"))
#' @describeIn CoreSet Return the name of the CoreSet 
#' @export
setMethod(name, "CoreSet", function(object){
    return(object@annotation$name)
})

#' pertNumber Generic
#' 
#' A generic for the pertNumber method
#' 
#' @examples
#' pertNumber(clevelandSmall_cSet)
#' 
#' @param object A \code{CoreSet}
#' @param ... Fallthrough arguements for defining new methods
#'
#' @return A 3D \code{array} with the number of perturbation experiments per drug and cell line, and data type
#' @export
setGeneric("pertNumber", function(object, ...) standardGeneric("pertNumber"))
#' 
#' @describeIn CoreSet Return the summary of available perturbation
#'   experiments
#' @export
setMethod(pertNumber, "CoreSet", function(object){
    return(object@perturbation$n)
})

#' sensNumber Generic
#' 
#' A generic for the sensNumber method
#' 
#' @examples
#' sensNumber(clevelandSmall_cSet)
#' 
#' @param object A \code{CoreSet}
#' @param ... Fallthrough arguements for defining new methods
#' 
#' @return A \code{data.frame} with the number of sensitivity experiments per drug and cell line
#' @export
setGeneric("sensNumber", function(object, ...) standardGeneric("sensNumber"))
#' @describeIn CoreSet Return the summary of available sensitivity
#'   experiments
#' @export
setMethod(sensNumber, "CoreSet", function(object){
  return(object@sensitivity$n)
})

#' pertNumber<- Generic
#' 
#' A generic for the pertNumber method
#' 
#' @examples
#' pertNumber(clevelandSmall_cSet) <- pertNumber(clevelandSmall_cSet)
#' 
#' @param object A \code{CoreSet} 
#' @param value A new 3D \code{array} with the number of perturbation experiments per drug and cell line, and data type
#' 
#' @return The updated \code{CoreSet}
#' 
#' @export
setGeneric("pertNumber<-", function(object, value) standardGeneric("pertNumber<-"))
#' @describeIn CoreSet Update the summary of available perturbation
#'   experiments
#' @export
setReplaceMethod('pertNumber', signature = signature(object="CoreSet", value="array"), function(object, value){
  object@perturbation$n <- value
  object
})

#' sensNumber<- Generic
#' 
#' A generic for the sensNumber method
#' 
#' @examples
#' sensNumber(clevelandSmall_cSet) <- sensNumber(clevelandSmall_cSet)
#' 
#' @param object A \code{CoreSet} 
#' @param value A new \code{data.frame} with the number of sensitivity experiments per drug and cell line
#' 
#' @return The updated \code{CoreSet} 
#' 
#' @export
setGeneric("sensNumber<-", function(object, value) standardGeneric("sensNumber<-"))
#' @describeIn CoreSet Update the summary of available sensitivity
#'   experiments
#' @export
setReplaceMethod('sensNumber', signature = signature(object="CoreSet", value="matrix"), function(object, value){
  object@sensitivity$n <- value
  object
})

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

#' mDataNames Generic
#' 
#' A generic for the mDataNames method
#' 
#' 
#' @examples
#' mDataNames(clevelandSmall_cSet)
#' 
#' @param object CoreSet object
#' @param ... Fallthrough arguements for defining new methods
#'  
#' @return Vector of names of the molecular data types
#' 
#' @export
setGeneric("mDataNames", function(object, ...) standardGeneric("mDataNames"))

#' mDataNames
#' 
#' Returns the molecular data names for the CoreSet.
#' 
#' @examples
#' data(clevelandSmall_cSet)
#' mDataNames(clevelandSmall_cSet)
#' 
#' @param object CoreSet object
#' 
#' @return Vector of names of the molecular data types
#' 
#' @export
setMethod("mDataNames", "CoreSet", function(object){
  return(names(object@molecularProfiles))
})


### TODO:: Add updating of sensitivity Number tables
##TODO:: Add documentation!

#' Update the cell ids in a cSet object
#'
#' @param object The object for witch the cell ids will be updated
#' @param new.ids The new ids to assign to the object
#' 
#' @return \code{CoreSet} The modified CoreSet object
#' 
#' @export
#' @keywords internal
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
