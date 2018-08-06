setClassUnion("df_or_DF", c("data.frame", "DataFrame"))




#' A Superclass to Contain Data for Genetic Profiling and Phenotyping data (eg. Drug Sensitivity) for Cancer Cell Lines
#' 
#' The PhenoGenoExperiment (CSet) class was developed as a superclass for objects in the 
#' PharmacoGx and RadioGx packages to contain the data generated in screens 
#' of cancer cell lines for their genetic profile and sensitivities to therapy
#' (Pharmacological or Radiation). This class is meant to be a superclass which 
#' is contained within the PharmacoSet (PSet) and RadioSet (RSet) objects 
#' exported by PharmacoGx and RadioGx. The format of the data is similar for 
#' both data PSets and RSets, allowing much of the code to be abstracted into 
#' the PhenoGenoExperiment super-class. However, the models involved with quantifying 
#' cellular response to Pharmacological and Radiation therapy are widely 
#' different, and two seperate implementations of the CSet class allows the
#' packages to applu the correct model for the given data. 
#' 
#' @param cSet A \code{PhenoGenoExperiment} object
#' @param mDataType A \code{character} with the type of molecular data to return/update
#' @param object A \code{PhenoGenoExperiment} object
#' @param value A replacement value
#' 
#' @slot annotation A \code{list} of annotation data about the PhenoGenoExperiment,
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
#'   identifiers used between different PhenoGenoExperiment objects
#' @slot datasetType A \code{character} string of 'sensitivity', 
#'   'perturbation', or both detailing what type of data can be found in the 
#'   PhenoGenoExperiment, for proper processing of the data
#' @return An object of the PhenoGenoExperiment class
#' @export
# setClass("PGExperimentList", contains = "ExperimentList")
.PhenoGenoExperiment <- setClass("PhenoGenoExperiment", slots = list(
                                            sensitivity = "longArray.DT", 
                                            treatmentData = "DataFrame"
                                            ),
                                contains = "MultiAssayExperiment")


# The default constructor above does a poor job of explaining the required structure of a PhenoGenoExperiment. 
# The constructor function defined below guides the user into providing the required components of the curation and sensitivity lists
# and hides the annotation slot which the user does not need to manually fill. 
# This also follows the design of the Expression Set class.



PhenoGenoExperiment <- function(molecularProfiles = MultiAssayExperiment::ExperimentList(),
             colData = S4Vectors::DataFrame(),
             treatmentData = S4Vectors::DataFrame(),
             sensitivity,
             sampleMap =
                S4Vectors::DataFrame(
                    assay = factor(),
                    primary = character(),
                    colname = character()),
             metadata = NULL,
             drops = list()) {


        if(!missing(sensitivity)){
          sensitivity.columns <- colnames(sensitivity)
          fakeSensAssay <- new("PlaceHolderAssay", .Data=matrix(NA_real_, ncol=length(sensitivity.columns)))
          colnames(fakeSensAssay) <- sensitivity.columns
          if(!missing(sampleMap)){
            newSampleMapRows <- S4Vectors::DataFrame(assay="sensitivity", primary=sensitivity.columns, colname = sensitivity.columns)
            sampleMap <- S4Vectors::DataFrame(plyr::rbind.fill(as.data.frame(sampleMap), as.data.frame(newSampleMapRows)))
          }
          molecularProfiles = c(molecularProfiles, "sensitivity" = list(fakeSensAssay))
        }

        if (missing(molecularProfiles))
            experiments <- ExperimentList()
        else
            experiments <- ExperimentList(molecularProfiles)

        if(!is(sensitivity, "longArray")) { stop("Only longArray sensitivity is implemented ATM.")}
        
        if (missing(colData)){
          stop("Please provide explicit colData.")
        #     allsamps <- unique(unlist(unname(colnames(experiments))))
        #     colData <- S4Vectors::DataFrame(row.names = allsamps)
        } else if (!is(colData, "DataFrame"))
            colData <- S4Vectors::DataFrame(colData)


        if (missing(sampleMap)) {
            sampleMap <- MultiAssayExperiment:::.sampleMapFromData(colData, experiments)
        } else {
            sampleMap <- S4Vectors::DataFrame(sampleMap)
            if (!all(c("assay", "primary", "colname") %in% colnames(sampleMap)))
                stop("'sampleMap' does not have required columns")
            if (!is.factor(sampleMap[["assay"]]))
                sampleMap[["assay"]] <- factor(sampleMap[["assay"]])
            if (!is.character(sampleMap[["primary"]])) {
                warning("sampleMap[['primary']] coerced to character()")
                sampleMap[["primary"]] <- as.character(sampleMap[["primary"]])
            }
            if (!is.character(sampleMap[["colname"]])) {
                warning("sampleMap[['colname']] coerced to character()")
                sampleMap[["colname"]] <- as.character(sampleMap[["colname"]])
            }
        }

        bliss <- MultiAssayExperiment:::.harmonize(experiments, colData, sampleMap)

        newMultiAssay <- new("PhenoGenoExperiment",
                             ExperimentList = bliss[["experiments"]],
                             colData = bliss[["colData"]],
                             sampleMap = bliss[["sampleMap"]],
                             treatmentData = treatmentData,
                             sensitivity = sensitivity,
                             metadata = metadata)
        return(newMultiAssay)
    }


#' Show a PhenoGenoExpriment
#' 
#' @param object \code{PhenoGenoExperiment}
#' 
#' @examples
#' data(CCLEsmall)
#' CCLEsmall
#' 
#' @return Prints the PhenoGenoExperiment object to the output stream, and returns invisible NULL. 
#' @export
setMethod("show", signature=signature(object="PhenoGenoExperiment"), 
    function(object) {
      callNextMethod(suppressMessages(object[,,names(object)]))
})

setMethod("dimnames", signature=signature(x="PhenoGenoExperiment"), 
    function(x) {
      callNextMethod(suppressMessages(x[,,names(x)]))
})

## TODO:: this method should use the MAE to get the common columns, and then subset the sensitivity slot appropriately. 
setMethod("complete.cases", "PhenoGenoExperiment", function(...) {
    args <- list(...)

    if (length(args) == 1L) {
      callNextMethod(...)
    } else { stop("Provide only a 'PhenoGenoExperiment'") }
})


setMethod("names", "PhenoGenoExperiment", function(x){
  return(setdiff(callNextMethod(x), "sensitivity"))
})

longFormat <- function(object, colDataCols = NULL, i = 1L) {
    if (is(object, "PhenoGenoExperiment"))
        return(MultiAssayExperiment::longFormat(suppressMessages(object[,,names(object)]), colDataCols = colDataCols, i = i))
    else
      return(MultiAssayExperiment::longFormat(object, colDataCols = colDataCols, i = i))
}



#' cellInfo Generic
#' 
#' Generic for cellInfo method 
#' 
#' @examples
#' data(CCLEsmall)
#' cellInfo(CCLEsmall)
#' 
#' @param cSet The \code{PhenoGenoExperiment} to retrieve cell info from
#' 
#' @return a \code{data.frame} with the cell annotations
setGeneric("cellInfo", function(x) standardGeneric("cellInfo"))
NULL

#' cellInfo<- Generic
#' 
#' Generic for cellInfo replace method
#' 
#' @examples
#' data(CCLEsmall)
#' cellInfo(CCLEsmall) <- cellInfo(CCLEsmall)
#' 
#' @param object The \code{PhenoGenoExperiment} to replace cell info in
#' @param value A \code{data.frame} with the new cell annotations
#' @return Updated \code{PhenoGenoExperiment}
setGeneric("cellInfo<-", function(object, value) standardGeneric("cellInfo<-"))
NULL

#' @describeIn PhenoGenoExperiment Returns the annotations for all the cell lines tested on in the PhenoGenoExperiment
#' @export
setMethod("cellInfo", "PhenoGenoExperiment", function(x){
  colData(x)
})

#' @describeIn PhenoGenoExperiment Update the cell line annotations
#' @export
setReplaceMethod("cellInfo", signature = signature(object="PhenoGenoExperiment",value="data.frame"), function(object, value){
  if(is.null(rownames(value))){
    stop("Please provide the cell_id as rownames for the cell line annotations")
  }
  colData(object) <- S4Vectors::DataFrame(value)
  object
})


#' @describeIn PhenoGenoExperiment Update the cell line annotations
#' @export
setReplaceMethod("cellInfo", signature = signature(object="PhenoGenoExperiment",value="DataFrame"), function(object, value){
  if(is.null(rownames(value))){
    stop("Please provide the cell_id as rownames for the cell line annotations")
  }
  colData(object) <- value
  object
})

#' phenoInfo Generic
#' 
#' Generic for phenoInfo method 
#' 
#' @examples
#' data(CCLEsmall)
#' phenoInfo(CCLEsmall, mDataType="rna")
#' 
#' @param x The \code{PhenoGenoExperiment} to retrieve rna annotations from
#' @param mDataType the type of molecular data 
#' @return a \code{data.frame} with the experiment info
setGeneric("phenoInfo", function(x, mDataType) standardGeneric("phenoInfo"))
#' @describeIn PhenoGenoExperiment Return the experiment info from the given type of molecular data in PhenoGenoExperiment 
#' @export
setMethod(phenoInfo, "PhenoGenoExperiment", function(x, mDataType){
    
  if(mDataType %in% names(x)){
    return(colData(x[[mDataType]]))}else{
      return(NULL)
    }
    
})

#' phenoInfo<- Generic
#' 
#' Generic for phenoInfo replace method 
#' 
#' @examples
#' 
#' data(CCLEsmall)
#' phenoInfo(CCLEsmall, mDataType="rna") <- phenoInfo(CCLEsmall, mDataType="rna")
#' 
#' @param object The \code{PhenoGenoExperiment} to retrieve molecular experiment annotations from
#' @param mDataType the type of molecular data 
#' @param value a \code{data.frame} with the new experiment annotations
#' @return The updated \code{PhenoGenoExperiment}
setGeneric("phenoInfo<-", function(object, mDataType, value) standardGeneric("phenoInfo<-"))
#' @describeIn PhenoGenoExperiment Update the the given type of molecular data experiment info in the PhenoGenoExperiment 
#' @export
setReplaceMethod("phenoInfo", signature = signature(object="PhenoGenoExperiment", mDataType ="character", value="df_or_DF"), function(object, mDataType, value){

if(mDataType %in% names(object)){
    colData(object[[mDataType]]) <- S4Vectors::DataFrame(value)
    }else{
      stop(paste0("Molecular Data Type: ", mDataType, " not found."))
    }
  object
})

#' molecularProfiles Generic
#' 
#' Generic for molecularProfiles method 
#' 
#' @examples
#' data(CCLEsmall)
#' molecularProfiles(CCLEsmall, "rna")
#' 
#' @param x The \code{PhenoGenoExperiment} to retrieve molecular profiles from
#' @param mDataType the type of molecular data 
#' @return A matrix storing the specified molecular profiles
setGeneric("molecularProfiles", function(x, mDataType) standardGeneric("molecularProfiles"))
#' @describeIn PhenoGenoExperiment Return the given type of molecular data from the PhenoGenoExperiment 
#' @export
setMethod(molecularProfiles, signature("PhenoGenoExperiment", "character"), function(x, mDataType){
    
  if(mDataType %in% names(x)){
    return(assay(PGE, mDataType))}else{
      return(NULL)
    }
    
})

#' molecularProfiles<- Generic
#' 
#' Generic for molecularProfiles replace method
#' 
#' @examples
#' data(CCLEsmall)
#' molecularProfiles(CCLEsmall, "rna") <- molecularProfiles(CCLEsmall, "rna")
#' 
#' @param object The \code{PhenoGenoExperiment} to replace molecular profiles in
#' @param mDataType The type of molecular data to be updated
#' @param value A \code{matrix} with the new profiles
#' @return Updated \code{PhenoGenoExperiment}
setGeneric("molecularProfiles<-", function(object, mDataType, value) standardGeneric("molecularProfiles<-"))
#' @describeIn PhenoGenoExperiment Update the given type of molecular data from the PhenoGenoExperiment 
#' @export
setReplaceMethod("molecularProfiles", signature = signature(object="PhenoGenoExperiment", mDataType ="character",value="matrix"), function(object, mDataType, value){

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
#' data(CCLEsmall)
#' featureInfo(CCLEsmall, "rna")
#' 
#' @param cSet The \code{PhenoGenoExperiment} to retrieve feature annotations from
#' @param mDataType the type of molecular data 
#' @return a \code{data.frame} with the experiment info
setGeneric("featureInfo", function(cSet, mDataType) standardGeneric("featureInfo"))
#' @describeIn PhenoGenoExperiment Return the feature info for the given molecular data 
#' @export
setMethod(featureInfo, "PhenoGenoExperiment", function(cSet, mDataType){
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
#' data(CCLEsmall)
#' featureInfo(CCLEsmall, "rna") <- featureInfo(CCLEsmall, "rna")
#' 
#' @param object The \code{PhenoGenoExperiment} to replace gene annotations in
#' @param mDataType The type of molecular data to be updated
#' @param value A \code{data.frame} with the new feature annotations
#' @return Updated \code{PhenoGenoExperiment}
setGeneric("featureInfo<-", function(object, mDataType, value) standardGeneric("featureInfo<-"))
#' @describeIn PhenoGenoExperiment Replace the gene info for the molecular data
#' @export
setReplaceMethod("featureInfo", signature = signature(object="PhenoGenoExperiment", mDataType ="character",value="data.frame"), function(object, mDataType, value){
  
  if(mDataType %in% names(object@molecularProfiles)){Biobase::fData(object@molecularProfiles[[mDataType]]) <- value}
  
  object
})

#' sensitivityInfo Generic
#' 
#' Generic for sensitivityInfo method 
#' 
#' @examples
#' data(CCLEsmall)
#' sensitivityInfo(CCLEsmall)
#' 
#' @param cSet The \code{PhenoGenoExperiment} to retrieve sensitivity experiment annotations from
#' @return a \code{data.frame} with the experiment info
setGeneric("sensitivityInfo", function(cSet) standardGeneric("sensitivityInfo"))
#' @describeIn PhenoGenoExperiment Return the drug dose sensitivity experiment info
#' @export
setMethod(sensitivityInfo, "PhenoGenoExperiment", function(cSet){
    
    return(cSet@sensitivity$info)
    
})

#' sensitivityInfo<- Generic
#' 
#' A generic for the sensitivityInfo replacement method
#' 
#' 
#' @examples
#' data(CCLEsmall)
#' sensitivityInfo(CCLEsmall) <- sensitivityInfo(CCLEsmall)
#' 
#' @param object The \code{PhenoGenoExperiment} to update
#' @param value A \code{data.frame} with the new sensitivity annotations
#' @return Updated \code{PhenoGenoExperiment} 
setGeneric("sensitivityInfo<-", function(object, value) standardGeneric("sensitivityInfo<-"))
#' @describeIn PhenoGenoExperiment Update the sensitivity experiment info
#' @export
setReplaceMethod("sensitivityInfo", signature = signature(object="PhenoGenoExperiment",value="data.frame"), function(object, value){

    object@sensitivity$info <- value
    object
})


#' sensitivityProfiles Generic
#' 
#' Generic for sensitivityProfiles method 
#' 
#' @examples
#' data(CCLEsmall)
#' sensitivityProfiles(CCLEsmall)
#' 
#' @param cSet The \code{PhenoGenoExperiment} to retrieve sensitivity experiment data from
#' @return a \code{data.frame} with the experiment info
setGeneric("sensitivityProfiles", function(cSet) standardGeneric("sensitivityProfiles"))
#' @describeIn PhenoGenoExperiment Return the phenotypic data for the drug dose sensitivity
#' @export
setMethod(sensitivityProfiles, "PhenoGenoExperiment", function(cSet){
    
    return(cSet@sensitivity$profiles)
    
})

#' sensitivityProfiles<- Generic
#' 
#' A generic for the sensitivityProfiles replacement method
#' 
#' @examples
#' data(CCLEsmall)
#' sensitivityProfiles(CCLEsmall) <- sensitivityProfiles(CCLEsmall)
#' 
#' @param object The \code{PhenoGenoExperiment} to update
#' @param value A \code{data.frame} with the new sensitivity profiles. If a matrix object is passed in, converted to data.frame before assignment
#' @return Updated \code{PhenoGenoExperiment} 
setGeneric("sensitivityProfiles<-", function(object, value) standardGeneric("sensitivityProfiles<-"))
#' @describeIn PhenoGenoExperiment Update the phenotypic data for the drug dose
#'   sensitivity
#' @export
setReplaceMethod("sensitivityProfiles", signature = signature(object="PhenoGenoExperiment",value="data.frame"), function(object, value){

    object@sensitivity$profiles <- value
    object
})
#' @describeIn PhenoGenoExperiment Update the phenotypic data for the drug dose
#'   sensitivity
#' @export
setReplaceMethod("sensitivityProfiles", signature = signature(object="PhenoGenoExperiment",value="matrix"), function(object, value){

    object@sensitivity$profiles <- as.data.frame(value)
    object
})
#' sensitivityMeasures Generic
#' 
#' A generic for the sensitivityMeasures  method
#' 
#' @examples
#' data(CCLEsmall)
#' sensitivityMeasures(CCLEsmall)
#' 
#' @param cSet The \code{PhenoGenoExperiment} 
#' @return A \code{character} vector of all the available sensitivity measures
setGeneric("sensitivityMeasures", function(cSet) standardGeneric("sensitivityMeasures"))
#' @describeIn PhenoGenoExperiment Returns the available sensitivity profile
#'   summaries, for example, whether there are IC50 values available
#' @export
setMethod(sensitivityMeasures, "PhenoGenoExperiment", function(cSet){
    
    return(colnames(sensitivityProfiles(cSet)))
    
})

#' cellNames Generic
#' 
#' A generic for the cellNames method
#' 
#' @examples
#' data(CCLEsmall)
#' cellNames(CCLEsmall)
#' 
#' @param cSet The \code{PhenoGenoExperiment} to return cell names from
#' @return A vector of the cell names used in the PhenoGenoExperiment
setGeneric("cellNames", function(cSet) standardGeneric("cellNames"))
#' @describeIn PhenoGenoExperiment Return the cell names used in the dataset
#' @export
setMethod(cellNames, "PhenoGenoExperiment", function(cSet){
  
  rownames(cellInfo(cSet))
  
})

#' cellNames<- Generic
#' 
#' A generic for the cellNames replacement method
#' 
#' @examples
#' data(CCLEsmall)
#' cellNames(CCLEsmall) <- cellNames(CCLEsmall)
#' 
#' @param object The \code{PhenoGenoExperiment} to update
#' @param value A \code{character} vector of the new cell names
#' @return Updated \code{PhenoGenoExperiment} 
setGeneric("cellNames<-", function(object, value) standardGeneric("cellNames<-"))
#' @describeIn PhenoGenoExperiment Update the cell names used in the dataset
#' @export
setReplaceMethod("cellNames", signature = signature(object="PhenoGenoExperiment",value="character"), function(object, value){
    
    object <- updateCellId(object, value)
    return(object)
    })


    #### TODO:: set replace method for genenames
#' fNames Generic
#' 
#' A generic for the fNames method
#' 
#' @examples
#' data(CCLEsmall)
#' fNames(CCLEsmall, "rna")
#' 
#' @param cSet The \code{PhenoGenoExperiment} 
#' @param mDataType The molecular data type to return feature names for
#' @return A \code{character} vector of the feature names
setGeneric("fNames", function(cSet, mDataType) standardGeneric("fNames"))
#' @describeIn PhenoGenoExperiment Return the feature names used in the dataset
#' @export
setMethod(fNames, "PhenoGenoExperiment", function(cSet, mDataType){
  if (mDataType %in% names(cSet@molecularProfiles)) {
    rownames(featureInfo(cSet, mDataType))
  } else {
    stop("Molecular data type name specified is not part of this PhenoGenoExperiment")
  }
})

#' dateCreated Generic
#' 
#' A generic for the dateCreated method
#' 
#' @examples
#' data(CCLEsmall)
#' dateCreated(CCLEsmall)
#' 
#' @param cSet A \code{PhenoGenoExperiment} 
#' @return The date the PhenoGenoExperiment was created
setGeneric("dateCreated", function(cSet) standardGeneric("dateCreated"))
#' @describeIn PhenoGenoExperiment Return the date the PhenoGenoExperiment was created
#' @export
setMethod(dateCreated, "PhenoGenoExperiment", function(cSet) {
  cSet@annotation$dateCreated
})

#' cSetName Generic
#' 
#' A generic for the cSetName method
#' 
#' @examples
#' data(CCLEsmall)
#' cSetName(CCLEsmall)
#' 
#' @param cSet A \code{PhenoGenoExperiment} 
#' @return The name of the PhenoGenoExperiment
setGeneric("cSetName", function(cSet) standardGeneric("cSetName"))
#' @describeIn PhenoGenoExperiment Return the name of the PhenoGenoExperiment 
#' @export
setMethod(cSetName, "PhenoGenoExperiment", function(cSet){
    
    return(cSet@annotation$name)
    
})

#' pertNumber Generic
#' 
#' A generic for the pertNumber method
#' 
#' @examples
#' data(CCLEsmall)
#' pertNumber(CCLEsmall)
#' 
#' @param cSet A \code{PhenoGenoExperiment} 
#' @return A 3D \code{array} with the number of perturbation experiments per drug and cell line, and data type
setGeneric("pertNumber", function(cSet) standardGeneric("pertNumber"))
#' @describeIn PhenoGenoExperiment Return the summary of available perturbation
#'   experiments
#' @export
setMethod(pertNumber, "PhenoGenoExperiment", function(cSet){
    
    return(cSet@perturbation$n)
    
})


#' sensNumber Generic
#' 
#' A generic for the sensNumber method
#' 
#' @examples
#' data(CCLEsmall)
#' sensNumber(CCLEsmall)
#' 
#' @param cSet A \code{PhenoGenoExperiment} 
#' @return A \code{data.frame} with the number of sensitivity experiments per drug and cell line
setGeneric("sensNumber", function(cSet) standardGeneric("sensNumber"))
#' @describeIn PhenoGenoExperiment Return the summary of available sensitivity
#'   experiments
#' @export
setMethod(sensNumber, "PhenoGenoExperiment", function(cSet){
  
  return(cSet@sensitivity$n)
  
})

#' pertNumber<- Generic
#' 
#' A generic for the pertNumber method
#' 
#' @examples
#' data(CCLEsmall)
#' pertNumber(CCLEsmall) <- pertNumber(CCLEsmall)
#' 
#' @param object A \code{PhenoGenoExperiment} 
#' @param value A new 3D \code{array} with the number of perturbation experiments per drug and cell line, and data type
#' @return The updated \code{PhenoGenoExperiment} 
setGeneric("pertNumber<-", function(object, value) standardGeneric("pertNumber<-"))
#' @describeIn PhenoGenoExperiment Update the summary of available perturbation
#'   experiments
#' @export
setReplaceMethod('pertNumber', signature = signature(object="PhenoGenoExperiment",value="array"), function(object, value){
  
  object@perturbation$n <- value
  object
  
})

#' sensNumber<- Generic
#' 
#' A generic for the sensNumber method
#' 
#' 
#' @examples
#' data(CCLEsmall)
#' sensNumber(CCLEsmall) <- sensNumber(CCLEsmall)
#' 
#' @param object A \code{PhenoGenoExperiment} 
#' @param value A new \code{data.frame} with the number of sensitivity experiments per drug and cell line
#' @return The updated \code{PhenoGenoExperiment} 
setGeneric("sensNumber<-", function(object, value) standardGeneric("sensNumber<-"))
#' @describeIn PhenoGenoExperiment Update the summary of available sensitivity
#'   experiments
#' @export
setReplaceMethod('sensNumber', signature = signature(object="PhenoGenoExperiment",value="matrix"), function(object, value){
  
  object@sensitivity$n <- value
  object
  
})

#' mDataNames Generic
#' 
#' A generic for the mDataNames method
#' 
#' 
#' @examples
#' data(CCLEsmall)
#' mDataNames(CCLEsmall)
#' 
#' @param cSet PharamcoSet object
#' @return Vector of names of the molecular data types
#' @export
setGeneric("mDataNames", function(cSet) standardGeneric("mDataNames"))


#' mDataNames
#' 
#' Returns the molecular data names for the PhenoGenoExperiment.
#' 
#' @examples
#' data(CCLEsmall)
#' mDataNames(CCLEsmall)
#' 
#' @param cSet PharamcoSet object
#' @return Vector of names of the molecular data types
#' @export
setMethod("mDataNames", "PhenoGenoExperiment", function(cSet){

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

    for (i in 1:length(cSet@molecularProfiles)) {
      if (nrow(Biobase::pData(cSet@molecularProfiles[[i]])) > 0 && all(is.element(c("cellid", "drugid"), colnames(Biobase::pData(cSet@molecularProfiles[[i]]))))) {
      tt <- table(Biobase::pData(cSet@molecularProfiles[[i]])[ , "cellid"], Biobase::pData(cSet@molecularProfiles[[i]])[ , "drugid"])
        perturbation.info[rownames(tt), colnames(tt), names(cSet@molecularProfiles)[i]] <- tt
      }
    }
  
    return(perturbation.info)
}

#' A function to verify the structure of a PhenoGenoExperiment
#' 
#' This function checks the structure of a PharamcoSet, ensuring that the
#' correct annotations are in place and all the required slots are filled so
#' that matching of cells and drugs can be properly done across different types
#' of data and with other studies.
#' 
#' @examples
#' data(CCLEsmall)
#' 
#' checkCSetStructure(CCLEsmall)
#' 
#' @param cSet A \code{PhenoGenoExperiment} to be verified
#' @param plotDist Should the function also plot the distribution of molecular data?
#' @param result.dir The path to the directory for saving the plots as a string
#' @return Prints out messages whenever describing the errors found in the structure of the pset object passed in. 
#' @export
#' @importFrom graphics hist
#' @importFrom grDevices dev.off pdf

checkPSetStructure <-
  function(cSet, plotDist=FALSE, result.dir=".") {
    if(!file.exists(result.dir) & plotDist) { dir.create(result.dir, showWarnings=FALSE, recursive=TRUE) }
    for( i in 1:length(cSet@molecularProfiles)) {
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
#     if("cellid" %in% colnames(cSet@cell)) {
#       if(length(intersect(cSet@curation$cell$cellid, rownames(cSet@cell))) != nrow(cSet@cell)) {
#         print("values of cellid column should be curated cell line ids")
#       }
#     } else {
#       print("cellid which is curated cell id across data set should be a column of cell slot")
#     }
    
    if(length(intersect(rownames(cSet@curation$cell), rownames(cSet@cell))) != nrow(cSet@cell)) {
      print("rownames of curation cell slot should be the same as cell slot (curated cell ids)")
    }
    
    if(class(cSet@cell) != "data.frame") {
      warning("cell slot class type should be dataframe")
    }
    if(cSet@datasetType %in% c("sensitivity", "both"))
    {
      if(class(cSet@sensitivity$info) != "data.frame") {
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

