#' A Superclass to Contain Data for Genetic Profiling and Viability Screens of Cancer Cell Lines
#' 
#' The CoreSet (CSet) class was developed as a superclass for objects in the 
#' PharmacoGx and RadioGx packages to contain the data generated in screens 
#' of cancer cell lines for their genetic profile and sensitivities to therapy
#' (Pharmacological or Radiation). This class is meant to be a superclass which 
#' is contained within the PharmacoSet (PSet) and RadioSet (RSet) objects 
#' exported by PharmacoGx and RadioGx. The format of the data is similar for 
#' both data PSets and RSets, allowing much of the code to be abstracted into 
#' the CoreSet super-class. However, the models involved with quantifying 
#' cellular response to Pharmacological and Radiation therapy are widely 
#' different, and two seperate implementations of the CSet class allows the
#' packages to apply the correct model for the given data. 
#' 
#' @param cSet A \code{CoreSet} object
#' @param mDataType A \code{character} with the type of molecular data to return/update
#' @param object A \code{CoreSet} object
#' @param value A replacement value
#' 
#' @slot annotation A \code{list} of annotation data about the CoreSet,
#'    including the \code{$name} and the session information for how the object
#'    was creating, detailing the exact versions of R and all the packages used
#' @slot molecularProfiles A \code{list} containing 4 \code{Biobase::ExpressionSet} 
#'   type object for holding data for RNA, DNA, SNP and Copy Number Variation 
#'   measurements respectively, with associated \code{fData} and \code{pData} 
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
#' @return An object of the CoreSet class
#' @export
.CoreSet <- setClass("CoreSet", slots = list(
                                            annotation = "list",
                                            molecularProfiles = "list",
                                            cell="data.frame", 
                                            datasetType="character", 
                                            sensitivity="list",
                                            perturbation="list",
                                            curation="list"
                                            # tables="array",
                                            # table.summary="list",
                                            # dateCreated="character",
                                            ))


# The default constructor above does a poor job of explaining the required structure of a CoreSet. 
# The constructor function defined below guides the user into providing the required components of the curation and senstivity lists
# and hides the annotation slot which the user does not need to manually fill. 
# This also follows the design of the Expression Set class.

#' CoreSet constructor
#' 
#' A constructor that simplifies the process of creating CoreSets, as well 
#' as creates empty objects for data not provided to the constructor. Only
#' objects returned by this constructor are expected to work with the CoreSet
#' methods. For a much more detailed instruction on creating CoreSets, please
#' see the "CreatingCoreSet" vignette.
#' 
#' @examples  
#' ## For help creating a CoreSet object, please see the following vignette:
#' browseVignettes("PharmacoGx")
#' 
#' @param name A \code{character} string detailing the name of the dataset
#' @param molecularProfiles A \code{list} of ExpressionSet objects containing
#'   molecular profiles 
#' @param cell A \code{data.frame} containg the annotations for all the cell
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
#' @import methods
#' @importFrom utils sessionInfo
#' @importFrom stats na.omit
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
        if (!is(molecularProfiles[[i]], "ExpressionSet")) {
            stop(sprintf("Please provide the %s data as an ExpressionSet", names(molecularProfiles[i])))
        }else{
      Biobase::fData(molecularProfiles[[i]]) <- Biobase::fData(molecularProfiles[[i]])[rownames(Biobase::exprs(molecularProfiles[[i]])), , drop=FALSE]
      Biobase::pData(molecularProfiles[[i]]) <- Biobase::pData(molecularProfiles[[i]])[colnames(Biobase::exprs(molecularProfiles[[i]])), , drop=FALSE]
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
    
    cSet  <- .CoreSet(annotation=annotation, molecularProfiles=molecularProfiles, cell=as.data.frame(cell), datasetType=datasetType, sensitivity=sensitivity, perturbation=perturbation, curation=curation)
    if (verify) { checkCSetStructure(cSet)}
  if(length(sensitivityN) == 0 & datasetType %in% c("sensitivity", "both")) {
    cSet@sensitivity$n <- .summarizeSensitivityNumbers(cSet)
  }
    if(length(perturbationN) == 0  & datasetType %in% c("perturbation", "both")) {
      cSet@perturbation$n <- .summarizePerturbationNumbers(cSet)
    }
  return(cSet)
}
    

#' cellInfo Generic
#' 
#' Generic for cellInfo method 
#' 
#' @examples
#' data(Cleveland_small)
#' cellInfo(Cleveland_small)
#' 
#' @param cSet The \code{CoreSet} to retrieve cell info from
#' 
#' @return a \code{data.frame} with the cell annotations
setGeneric("cellInfo", function(cSet) standardGeneric("cellInfo"))

#' cellInfo<- Generic
#' 
#' Generic for cellInfo replace method
#' 
#' @examples
#' cellInfo(Cleveland_small) <- cellInfo(Cleveland_small)
#' 
#' @param object The \code{CoreSet} to replace cell info in
#' @param value A \code{data.frame} with the new cell annotations
#' @return Updated \code{CoreSet}
setGeneric("cellInfo<-", function(object, value) standardGeneric("cellInfo<-"))
#' @describeIn CoreSet Returns the annotations for all the cell lines tested on in the CoreSet
#' @export
setMethod(cellInfo, "CoreSet", function(cSet){
  cSet@cell
})
#' @describeIn CoreSet Update the cell line annotations
#' @export
setReplaceMethod("cellInfo", signature = signature(object="CoreSet",value="data.frame"), function(object, value){
  if(is.null(rownames(value))){
    stop("Please provide the cell_id as rownames for the cell line annotations")
  }
  
  object@cell <- value
  object
})

#' phenoInfo Generic
#' 
#' Generic for phenoInfo method 
#' 
#' @examples
#' phenoInfo(Cleveland_small, mDataType="rna")
#' 
#' @param cSet The \code{CoreSet} to retrieve rna annotations from
#' @param mDataType the type of molecular data 
#' @return a \code{data.frame} with the experiment info
setGeneric("phenoInfo", function(cSet, mDataType) standardGeneric("phenoInfo"))
#' @describeIn CoreSet Return the experiment info from the given type of molecular data in CoreSet 
#' @export
setMethod(phenoInfo, "CoreSet", function(cSet, mDataType){
    
  if(mDataType %in% names(cSet@molecularProfiles)){
    return(Biobase::pData(cSet@molecularProfiles[[mDataType]]))}else{
      return(NULL)
    }
    
})

#' phenoInfo<- Generic
#' 
#' Generic for phenoInfo replace method 
#' 
#' @examples
#' 
#' phenoInfo(Cleveland_small, mDataType="rna") <- phenoInfo(Cleveland_small, mDataType="rna")
#' 
#' @param object The \code{CoreSet} to retrieve molecular experiment annotations from
#' @param mDataType the type of molecular data 
#' @param value a \code{data.frame} with the new experiment annotations
#' @return The updated \code{CoreSet}
setGeneric("phenoInfo<-", function(object, mDataType, value) standardGeneric("phenoInfo<-"))
#' @describeIn CoreSet Update the the given type of molecular data experiment info in the CoreSet 
#' @export
setReplaceMethod("phenoInfo", signature = signature(object="CoreSet", mDataType ="character",value="data.frame"), function(object, mDataType, value){

  if(mDataType %in% names(object@molecularProfiles)){Biobase::pData(object@molecularProfiles[[mDataType]]) <- value}
    object
})

#' molecularProfiles Generic
#' 
#' Generic for molecularProfiles method 
#' 
#' @examples
#' molecularProfiles(Cleveland_small, "rna")
#' 
#' @param cSet The \code{CoreSet} to retrieve molecular profiles from
#' @param mDataType the type of molecular data 
#' @return a \code{data.frame} with the experiment info
setGeneric("molecularProfiles", function(cSet, mDataType) standardGeneric("molecularProfiles"))
#' @describeIn CoreSet Return the given type of molecular data from the CoreSet 
#' @export
setMethod(molecularProfiles, signature("CoreSet", "character"), function(cSet, mDataType){
    
  if(mDataType %in% names(cSet@molecularProfiles)){
    return(Biobase::exprs(cSet@molecularProfiles[[mDataType]]))}else{
      return(NULL)
    }
    
})

#' molecularProfiles<- Generic
#' 
#' Generic for molecularProfiles replace method
#' 
#' @examples
#' molecularProfiles(Cleveland_small, "rna") <- molecularProfiles(Cleveland_small, "rna")
#' 
#' @param object The \code{CoreSet} to replace molecular profiles in
#' @param mDataType The type of molecular data to be updated
#' @param value A \code{matrix} with the new profiles
#' @return Updated \code{CoreSet}
setGeneric("molecularProfiles<-", function(object, mDataType, value) standardGeneric("molecularProfiles<-"))
#' @describeIn CoreSet Update the given type of molecular data from the CoreSet 
#' @export
setReplaceMethod("molecularProfiles", signature = signature(object="CoreSet", mDataType ="character",value="matrix"), function(object, mDataType, value){

  if (mDataType %in% names(object@molecularProfiles)) {
    Biobase::exprs(object@molecularProfiles[[mDataType]]) <- value
  }
    object
})

#' featureInfo Generic
#' 
#' Generic for featureInfo method 
#' 
#' @examples

#' featureInfo(Cleveland_small, "rna")
#' 
#' @param cSet The \code{CoreSet} to retrieve feature annotations from
#' @param mDataType the type of molecular data 
#' @return a \code{data.frame} with the experiment info
setGeneric("featureInfo", function(cSet, mDataType) standardGeneric("featureInfo"))
#' @describeIn CoreSet Return the feature info for the given molecular data 
#' @export
setMethod(featureInfo, "CoreSet", function(cSet, mDataType){
  if(mDataType %in% names(cSet@molecularProfiles)){
    return(Biobase::fData(cSet@molecularProfiles[[mDataType]]))}else{
      return(NULL)
    }
  
})

#' featureInfo<- Generic
#' 
#' Generic for featureInfo replace method
#' 
#' @examples

#' featureInfo(Cleveland_small, "rna") <- featureInfo(Cleveland_small, "rna")
#' 
#' @param object The \code{CoreSet} to replace gene annotations in
#' @param mDataType The type of molecular data to be updated
#' @param value A \code{data.frame} with the new feature annotations
#' @return Updated \code{CoreSet}
setGeneric("featureInfo<-", function(object, mDataType, value) standardGeneric("featureInfo<-"))
#' @describeIn CoreSet Replace the gene info for the molecular data
#' @export
setReplaceMethod("featureInfo", signature = signature(object="CoreSet", mDataType ="character",value="data.frame"), function(object, mDataType, value){
  
  if(mDataType %in% names(object@molecularProfiles)){Biobase::fData(object@molecularProfiles[[mDataType]]) <- value}
  
  object
})

#' sensitivityInfo Generic
#' 
#' Generic for sensitivityInfo method 
#' 
#' @examples

#' sensitivityInfo(Cleveland_small)
#' 
#' @param cSet The \code{CoreSet} to retrieve sensitivity experiment annotations from
#' @return a \code{data.frame} with the experiment info
setGeneric("sensitivityInfo", function(cSet) standardGeneric("sensitivityInfo"))
#' @describeIn CoreSet Return the drug dose sensitivity experiment info
#' @export
setMethod(sensitivityInfo, "CoreSet", function(cSet){
    
    return(cSet@sensitivity$info)
    
})

#' sensitivityInfo<- Generic
#' 
#' A generic for the sensitivityInfo replacement method
#' 
#' 
#' @examples
#' sensitivityInfo(Cleveland_small) <- sensitivityInfo(Cleveland_small)
#' 
#' @param object The \code{CoreSet} to update
#' @param value A \code{data.frame} with the new sensitivity annotations
#' @return Updated \code{CoreSet} 
setGeneric("sensitivityInfo<-", function(object, value) standardGeneric("sensitivityInfo<-"))
#' @describeIn CoreSet Update the sensitivity experiment info
#' @export
setReplaceMethod("sensitivityInfo", signature = signature(object="CoreSet",value="data.frame"), function(object, value){

    object@sensitivity$info <- value
    object
})


#' sensitivityProfiles Generic
#' 
#' Generic for sensitivityProfiles method 
#' 
#' @examples
#' sensitivityProfiles(Cleveland_small)
#' 
#' @param cSet The \code{CoreSet} to retrieve sensitivity experiment data from
#' @return a \code{data.frame} with the experiment info
setGeneric("sensitivityProfiles", function(cSet) standardGeneric("sensitivityProfiles"))
#' @describeIn CoreSet Return the phenotypic data for the drug dose sensitivity
#' @export
setMethod(sensitivityProfiles, "CoreSet", function(cSet){
    
    return(cSet@sensitivity$profiles)
    
})

#' sensitivityProfiles<- Generic
#' 
#' A generic for the sensitivityProfiles replacement method
#' 
#' @examples
#' sensitivityProfiles(Cleveland_small) <- sensitivityProfiles(Cleveland_small)
#' 
#' @param object The \code{CoreSet} to update
#' @param value A \code{data.frame} with the new sensitivity profiles. If a matrix object is passed in, converted to data.frame before assignment
#' @return Updated \code{CoreSet} 
setGeneric("sensitivityProfiles<-", function(object, value) standardGeneric("sensitivityProfiles<-"))
#' @describeIn CoreSet Update the phenotypic data for the drug dose
#'   sensitivity
#' @export
setReplaceMethod("sensitivityProfiles", signature = signature(object="CoreSet",value="data.frame"), function(object, value){

    object@sensitivity$profiles <- value
    object
})
#' @describeIn CoreSet Update the phenotypic data for the drug dose
#'   sensitivity
#' @export
setReplaceMethod("sensitivityProfiles", signature = signature(object="CoreSet",value="matrix"), function(object, value){

    object@sensitivity$profiles <- as.data.frame(value)
    object
})
#' sensitivityMeasures Generic
#' 
#' A generic for the sensitivityMeasures  method
#' 
#' @examples
#' sensitivityMeasures(Cleveland_small)
#' 
#' @param cSet The \code{CoreSet} 
#' @return A \code{character} vector of all the available sensitivity measures
setGeneric("sensitivityMeasures", function(cSet) standardGeneric("sensitivityMeasures"))
#' @describeIn CoreSet Returns the available sensitivity profile
#'   summaries, for example, whether there are IC50 values available
#' @export
setMethod(sensitivityMeasures, "CoreSet", function(cSet){
    
    return(colnames(sensitivityProfiles(cSet)))
    
})

#' cellNames Generic
#' 
#' A generic for the cellNames method
#' 
#' @examples
#' cellNames(Cleveland_small)
#' 
#' @param cSet The \code{CoreSet} to return cell names from
#' @return A vector of the cell names used in the CoreSet
setGeneric("cellNames", function(cSet) standardGeneric("cellNames"))
#' @describeIn CoreSet Return the cell names used in the dataset
#' @export
setMethod(cellNames, "CoreSet", function(cSet){
  
  rownames(cellInfo(cSet))
  
})

#' cellNames<- Generic
#' 
#' A generic for the cellNames replacement method
#' 
#' @examples
#' cellNames(Cleveland_small) <- cellNames(Cleveland_small)
#' 
#' @param object The \code{CoreSet} to update
#' @param value A \code{character} vector of the new cell names
#' @return Updated \code{CoreSet} 
setGeneric("cellNames<-", function(object, value) standardGeneric("cellNames<-"))
#' @describeIn CoreSet Update the cell names used in the dataset
#' @export
setReplaceMethod("cellNames", signature = signature(object="CoreSet",value="character"), function(object, value){
    
    object <- updateCellId(object, value)
    return(object)
    })


    #### TODO:: set replace method for genenames
#' fNames Generic
#' 
#' A generic for the fNames method
#' 
#' @examples
#' fNames(Cleveland_small, "rna")
#' 
#' @param cSet The \code{CoreSet} 
#' @param mDataType The molecular data type to return feature names for
#' @return A \code{character} vector of the feature names
setGeneric("fNames", function(cSet, mDataType) standardGeneric("fNames"))
#' @describeIn CoreSet Return the feature names used in the dataset
#' @export
setMethod(fNames, "CoreSet", function(cSet, mDataType){
  if (mDataType %in% names(cSet@molecularProfiles)) {
    rownames(featureInfo(cSet, mDataType))
  } else {
    stop("Molecular data type name specified is not part of this CoreSet")
  }
})

#' dateCreated Generic
#' 
#' A generic for the dateCreated method
#' 
#' @examples

#' dateCreated(Cleveland_small)
#' 
#' @param cSet A \code{CoreSet} 
#' @return The date the CoreSet was created
setGeneric("dateCreated", function(cSet) standardGeneric("dateCreated"))
#' @describeIn CoreSet Return the date the CoreSet was created
#' @export
setMethod(dateCreated, "CoreSet", function(cSet) {
  cSet@annotation$dateCreated
})

#' cSetName Generic
#' 
#' A generic for the cSetName method
#' 
#' @examples
#' cSetName(Cleveland_small)
#' 
#' @param cSet A \code{CoreSet} 
#' @return The name of the CoreSet
setGeneric("cSetName", function(cSet) standardGeneric("cSetName"))
#' @describeIn CoreSet Return the name of the CoreSet 
#' @export
setMethod(cSetName, "CoreSet", function(cSet){
    
    return(cSet@annotation$name)
    
})

#' pertNumber Generic
#' 
#' A generic for the pertNumber method
#' 
#' @examples
#' pertNumber(Cleveland_small)
#' 
#' @param cSet A \code{CoreSet} 
#' @return A 3D \code{array} with the number of perturbation experiments per drug and cell line, and data type
setGeneric("pertNumber", function(cSet) standardGeneric("pertNumber"))
#' @describeIn CoreSet Return the summary of available perturbation
#'   experiments
#' @export
setMethod(pertNumber, "CoreSet", function(cSet){
    
    return(cSet@perturbation$n)
    
})


#' sensNumber Generic
#' 
#' A generic for the sensNumber method
#' 
#' @examples
#' sensNumber(Cleveland_small)
#' 
#' @param cSet A \code{CoreSet} 
#' @return A \code{data.frame} with the number of sensitivity experiments per drug and cell line
setGeneric("sensNumber", function(cSet) standardGeneric("sensNumber"))
#' @describeIn CoreSet Return the summary of available sensitivity
#'   experiments
#' @export
setMethod(sensNumber, "CoreSet", function(cSet){
  
  return(cSet@sensitivity$n)
  
})

#' pertNumber<- Generic
#' 
#' A generic for the pertNumber method
#' 
#' @examples
#' pertNumber(Cleveland_small) <- pertNumber(Cleveland_small)
#' 
#' @param object A \code{CoreSet} 
#' @param value A new 3D \code{array} with the number of perturbation experiments per drug and cell line, and data type
#' @return The updated \code{CoreSet} 
setGeneric("pertNumber<-", function(object, value) standardGeneric("pertNumber<-"))
#' @describeIn CoreSet Update the summary of available perturbation
#'   experiments
#' @export
setReplaceMethod('pertNumber', signature = signature(object="CoreSet",value="array"), function(object, value){
  
  object@perturbation$n <- value
  object
  
})

#' sensNumber<- Generic
#' 
#' A generic for the sensNumber method
#' 
#' 
#' @examples

#' sensNumber(Cleveland_small) <- sensNumber(Cleveland_small)
#' 
#' @param object A \code{CoreSet} 
#' @param value A new \code{data.frame} with the number of sensitivity experiments per drug and cell line
#' @return The updated \code{CoreSet} 
setGeneric("sensNumber<-", function(object, value) standardGeneric("sensNumber<-"))
#' @describeIn CoreSet Update the summary of available sensitivity
#'   experiments
#' @export
setReplaceMethod('sensNumber', signature = signature(object="CoreSet",value="matrix"), function(object, value){
  
  object@sensitivity$n <- value
  object
  
})

#' Show a CoreSet
#' 
#' @param object \code{CoreSet}
#' 
#' @examples
#' show(Cleveland_small)
#' 
#' @return Prints the CoreSet object to the output stream, and returns invisible NULL. 
#' @export
setMethod("show", signature=signature(object="CoreSet"), 
    function(object) {
        cat("Name: ", cSetName(object), "\n")
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
        cat("\tNumber of Experiments: ",nrow(sensitivityInfo(object)),"\n")
        cat("\tPlease look at sensNumber(cSet) to determine number of experiments for each drug-cell combination.\n")
    })

#' mDataNames Generic
#' 
#' A generic for the mDataNames method
#' 
#' 
#' @examples
#' mDataNames(Cleveland_small)
#' 
#' @param cSet CoreSet object
#' @return Vector of names of the molecular data types
#' @export
setGeneric("mDataNames", function(cSet) standardGeneric("mDataNames"))


#' mDataNames
#' 
#' Returns the molecular data names for the CoreSet.
#' 
#' @examples
#' data(cleveland_small)
#' mDataNames(Cleveland_small)
#' 
#' @param cSet CoreSet object
#' @return Vector of names of the molecular data types
#' @export
setMethod("mDataNames", "CoreSet", function(cSet){

  return(names(cSet@molecularProfiles))

})


### TODO:: Add updating of sensitivity Number tables
updateCellId <- function(cSet, new.ids = vector("character")){
  
  if (length(new.ids)!=nrow(cellInfo(cSet))){
    stop("Wrong number of cell identifiers")
  }

  if(cSet@datasetType=="sensitivity"|cSet@datasetType=="both"){
    myx <- match(sensitivityInfo(cSet)[,"cellid"],rownames(cellInfo(cSet)))
    sensitivityInfo(cSet)[,"cellid"] <- new.ids[myx]

  }
  
  
  cSet@molecularProfiles <- lapply(cSet@molecularProfiles, function(eset){
          
      myx <- match(Biobase::pData(eset)[["cellid"]],rownames(cellInfo(cSet)))
      Biobase::pData(eset)[["cellid"]]  <- new.ids[myx]
      return(eset)
        })





  if(any(duplicated(new.ids))){
    warning("Duplicated ids passed to updateCellId. Merging old ids into the same identifier")
    
    if(ncol(sensNumber(cSet))>0){
      sensMatch <- match(rownames(sensNumber(cSet)), rownames(cellInfo(cSet)))
    }
    if(dim(pertNumber(cSet))[[2]]>0){
      pertMatch <- match(dimnames(pertNumber(cSet))[[1]], rownames(cellInfo(cSet)))
    }
    curMatch <- match(rownames(cSet@curation$cell),rownames(cellInfo(cSet)))

    duplId <- unique(new.ids[duplicated(new.ids)])
    for(id in duplId){

      if (ncol(sensNumber(cSet))>0){
        myx <- which(new.ids[sensMatch] == id)
        sensNumber(cSet)[myx[1],] <- apply(sensNumber(cSet)[myx,], 2, sum)
        sensNumber(cSet) <- sensNumber(cSet)[-myx[-1],]
        # sensMatch <- sensMatch[-myx[-1]]
      }
      if (dim(pertNumber(cSet))[[1]]>0){
        myx <- which(new.ids[pertMatch] == id)
        pertNumber(cSet)[myx[1],,] <- apply(pertNumber(cSet)[myx,,], c(1,3), sum)
        pertNumber(cSet) <- pertNumber(cSet)[-myx[-1],,]
        # pertMatch <- pertMatch[-myx[-1]]
      }

      myx <- which(new.ids[curMatch] == id)
      cSet@curation$cell[myx[1],] <- apply(cSet@curation$cell[myx,], 2, paste, collapse="///")
      cSet@curation$cell <- cSet@curation$cell[-myx[-1],]
      cSet@curation$tissue[myx[1],] <- apply(cSet@curation$tissue[myx,], 2, paste, collapse="///")
      cSet@curation$tissue <- cSet@curation$tissue[-myx[-1],]
      # curMatch <- curMatch[-myx[-1]]

      myx <- which(new.ids == id)
      cellInfo(cSet)[myx[1],] <- apply(cellInfo(cSet)[myx,], 2, paste, collapse="///")
      cellInfo(cSet) <- cellInfo(cSet)[-myx[-1],]
      new.ids <- new.ids[-myx[-1]]
      if(ncol(sensNumber(cSet))>0){
        sensMatch <- match(rownames(sensNumber(cSet)), rownames(cellInfo(cSet)))
      }
      if(dim(pertNumber(cSet))[[1]]>0){
        pertMatch <- match(dimnames(pertNumber(cSet))[[1]], rownames(cellInfo(cSet)))
      }
      curMatch <- match(rownames(cSet@curation$cell),rownames(cellInfo(cSet)))
    }
  } else {
    if (dim(pertNumber(cSet))[[1]]>0){
      pertMatch <- match(dimnames(pertNumber(cSet))[[1]], rownames(cellInfo(cSet)))
    }
    if (ncol(sensNumber(cSet))>0){
      sensMatch <- match(rownames(sensNumber(cSet)), rownames(cellInfo(cSet)))
    }
    curMatch <- match(rownames(cSet@curation$cell),rownames(cellInfo(cSet)))
  }

  if (dim(pertNumber(cSet))[[1]]>0){
    dimnames(pertNumber(cSet))[[1]] <- new.ids[pertMatch]
  }
  if (ncol(sensNumber(cSet))>0){
    rownames(sensNumber(cSet)) <- new.ids[sensMatch]
  }
  rownames(cSet@curation$cell) <- new.ids[curMatch]
  rownames(cSet@curation$tissue) <- new.ids[curMatch]
  rownames(cellInfo(cSet)) <- new.ids





  # myx <- match(rownames(cSet@curation$cell),rownames(cellInfo(cSet)))
  # rownames(cSet@curation$cell) <- new.ids[myx]
  # rownames(cSet@curation$tissue) <- new.ids[myx]
  # if (dim(pertNumber(cSet))[[1]]>0){
  #   myx <- match(dimnames(pertNumber(cSet))[[1]], rownames(cellInfo(cSet)))
  #   dimnames(pertNumber(cSet))[[1]] <- new.ids[myx]
  # }
  # if (nrow(sensNumber(cSet))>0){
  #   myx <- match(rownames(sensNumber(cSet)), rownames(cellInfo(cSet)))
  #   rownames(sensNumber(cSet)) <- new.ids[myx]
  # }
  # rownames(cellInfo(cSet)) <- new.ids
  return(cSet)

}

# updateFeatureNames <- function(cSet, new.ids = vector("character")){
#
#   if (length(new.ids)!=nrow(cellInfo(cSet))){
#     stop("Wrong number of cell identifiers")
#   }
#
#   if(cSet@datasetType=="sensitivity"|cSet@datasetType=="both"){
#     myx <- match(sensitivityInfo(cSet)[,"cellid"],rownames(cellInfo(cSet)))
#     sensitivityInfo(cSet)[,"cellid"] <- new.ids[myx]
#
#   }
#
#   cSet@molecularProfiles <- lapply(cSet@molecularProfiles, function(eset){
#
#     myx <- match(pData(eset)[["cellid"]],rownames(cellInfo(cSet)))
#     pData(eset)[["cellid"]]  <- new.ids[myx]
#     return(eset)
#       })
#   myx <- match(rownames(cSet@curation$cell),rownames(cellInfo(cSet)))
#   rownames(cSet@curation$cell) <- new.ids[myx]
#   rownames(cSet@curation$tissue) <- new.ids[myx]
#   if (dim(pertNumber(cSet))[[1]]>0){
#     myx <- match(dimnames(pertNumber(cSet))[[1]], rownames(cellInfo(cSet)))
#     dimnames(pertNumber(cSet))[[1]] <- new.ids[myx]
#   }
#   if (nrow(sensNumber(cSet))>0){
#     myx <- match(rownames(sensNumber(cSet)), rownames(cellInfo(cSet)))
#     rownames(sensNumber(cSet)) <- new.ids[myx]
#   }
#   rownames(cellInfo(cSet)) <- new.ids
#   return(cSet)
#
# }


.summarizeSensitivityNumbers <- function(cSet) {

  if (cSet@datasetType != "sensitivity" && cSet@datasetType != "both") {
    stop ("Data type must be either sensitivity or both")
  }
  
  ## unique drug identifiers
  # drugn <- sort(unique(cSet@sensitivity$info[ , "drugid"]))
  
  ## consider all drugs
  drugn <- rownames(cSet@drug)
  
  ## unique drug identifiers
  # celln <- sort(unique(cSet@sensitivity$info[ , "cellid"]))
  
  ## consider all cell lines
  celln <- rownames(cSet@cell)
  
  sensitivity.info <- matrix(0, nrow=length(celln), ncol=length(drugn), dimnames=list(celln, drugn))
  drugids <- cSet@sensitivity$info[ , "drugid"]
  cellids <- cSet@sensitivity$info[ , "cellid"]
  cellids <- cellids[grep("///", drugids, invert=TRUE)]
  drugids <- drugids[grep("///", drugids, invert=TRUE)]
  
  
  tt <- table(cellids, drugids)
  sensitivity.info[rownames(tt), colnames(tt)] <- tt
  
    return(sensitivity.info)
}


.summarizeMolecularNumbers <- function(cSet) {
  
  ## consider all molecular types
  mDT <- mDataNames(cSet)
  
  ## consider all cell lines
  celln <- rownames(cSet@cell)
  
  molecular.info <- matrix(0, nrow=length(celln), ncol=length(mDT), dimnames=list(celln, mDT))
  
  for(mDataType in mDT) {
    tt <- table(phenoInfo(cSet, mDataType)$cellid)
    molecular.info[names(tt), mDataType] <- tt

  }
  return(molecular.info)
}


.summarizePerturbationNumbers <- function(cSet) {

  if (cSet@datasetType != "perturbation" && cSet@datasetType != "both") {
    stop ("Data type must be either perturbation or both")
  }
  
  ## unique drug identifiers
  # drugn <- sort(unique(unlist(lapply(cSet@molecularProfiles, function (x) {
  #   res <- NULL
  #   if (nrow(pData(x)) > 0 & "drugid" %in% colnames(pData(x))) {
  #     res <- pData(x)[ , "drugid"]
  #   }
  #   return (res)
  # }))))
  
  ## consider all drugs
  drugn <- rownames(cSet@drug)
  
  ## unique cell line identifiers
  # celln <- sort(unique(unlist(lapply(cSet@molecularProfiles, function (x) {
  #   res <- NULL
  #   if (nrow(pData(x)) > 0 & "cellid" %in% colnames(pData(x))) {
  #     res <- pData(x)[ , "cellid"]
  #   }
  #   return (res)
  # }))))
  
  ## consider all cell lines
  celln <- rownames(cSet@cell)
  
  perturbation.info <- array(0, dim=c(length(celln), length(drugn), length(cSet@molecularProfiles)), dimnames=list(celln, drugn, names((cSet@molecularProfiles))))

    for (i in seq_len(length(cSet@molecularProfiles))) {
      if (nrow(Biobase::pData(cSet@molecularProfiles[[i]])) > 0 && all(is.element(c("cellid", "drugid"), colnames(Biobase::pData(cSet@molecularProfiles[[i]]))))) {
      tt <- table(Biobase::pData(cSet@molecularProfiles[[i]])[ , "cellid"], Biobase::pData(cSet@molecularProfiles[[i]])[ , "drugid"])
        perturbation.info[rownames(tt), colnames(tt), names(cSet@molecularProfiles)[i]] <- tt
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
#' checkCSetStructure(Cleveland_small)
#' 
#' @param cSet A \code{CoreSet} to be verified
#' @param plotDist Should the function also plot the distribution of molecular data?
#' @param result.dir The path to the directory for saving the plots as a string
#' @return Prints out messages whenever describing the errors found in the structure of the pset object passed in. 
#' @export
#' @importFrom graphics hist
#' @importFrom grDevices dev.off pdf

checkCSetStructure <-
  function(cSet, plotDist=FALSE, result.dir=".") {
    if(!file.exists(result.dir) & plotDist) { dir.create(result.dir, showWarnings=FALSE, recursive=TRUE) }
    for( i in seq_len(length(cSet@molecularProfiles))) {
      profile <- cSet@molecularProfiles[[i]]
      nn <- names(cSet@molecularProfiles)[i]
      if((Biobase::annotation(profile) == "rna" | Biobase::annotation(profile) == "rnaseq") & plotDist)
      {
        pdf(file=file.path(result.dir, sprintf("%s.pdf", nn)))
        hist(Biobase::exprs(profile), breaks = 100)
        dev.off()
      }
      warning(ifelse(nrow(Biobase::fData(profile)) != nrow(Biobase::exprs(profile)), sprintf("%s: number of features in fData is different from expression slots", nn), sprintf("%s: fData dimension is OK", nn)))
      warning(ifelse(nrow(Biobase::pData(profile)) != ncol(Biobase::exprs(profile)), sprintf("%s: number of cell lines in pData is different from expression slots", nn), sprintf("%s: pData dimension is OK", nn)))
      warning(ifelse("cellid" %in% colnames(Biobase::pData(profile)), "", sprintf("%s: cellid does not exist in pData columns", nn)))
      warning(ifelse("batchid" %in% colnames(Biobase::pData(profile)), "", sprintf("%s: batchid does not exist in pData columns", nn)))
      if(Biobase::annotation(profile) == "rna" | Biobase::annotation(profile) == "rnaseq")
      {
        warning(ifelse("BEST" %in% colnames(Biobase::fData(profile)), "BEST is OK", sprintf("%s: BEST does not exist in fData columns", nn)))
        warning(ifelse("Symbol" %in% colnames(Biobase::fData(profile)), "Symbol is OK", sprintf("%s: Symbol does not exist in fData columns", nn)))
      }
      if("cellid" %in% colnames(Biobase::pData(profile))) {
        if(!all(Biobase::pData(profile)[,"cellid"] %in% rownames(cSet@cell))) {
          warning(sprintf("%s: not all the cell lines in this profile are in cell lines slot", nn))
        }
      }else {
        warning(sprintf("%s: cellid does not exist in pData", nn))
      }
    }
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
