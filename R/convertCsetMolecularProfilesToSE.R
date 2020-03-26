#' CSet molecularProfiles from ESets to SEs
#'
#' Converts all ExpressionSet objects within the molecularProfiles slot of a 
#'   CoreSet to SummarizedExperiments
#'
#' @param cSet \code{S4} A CoreSet containing molecular data in ExpressionSets
#'
#' @return \code{S4} A CoreSet containing molecular data in a SummarizedExperiments
#' 
#' @importFrom parallel mclapply
#' @importFrom SummarizedExperiment SummarizedExperiment Assays assay assayNames assayNames<-
#' @importFrom Biobase exprs fData pData annotation protocolData assayDataElementNames
#' @importFrom S4Vectors SimpleList DataFrame
#' @importFrom stats setNames
#' 
#' @export
convertCSetMolecularProfilesToSE <- function(cSet) {
  
  eSets <- cSet@molecularProfiles # Extract eSet data
  
  cSet@molecularProfiles <-
    lapply(eSets,
           function(eSet){
             
             # Change rownames from probes to EnsemblGeneId for rna data type
             if (grepl("^rna$", Biobase::annotation(eSet))) {
               rownames(eSet) <- Biobase::fData(eSet)$EnsemblGeneId
             }
             
             # Build summarized experiment from eSet
             SE <- SummarizedExperiment::SummarizedExperiment(
               ## TODO:: Do we want to pass an environment for better memory efficiency?
               assays=SimpleList(as.list(Biobase::assayData(eSet))
               ),
               # Switch rearrange columns so that IDs are first, probes second
               rowData=S4Vectors::DataFrame(Biobase::fData(eSet),
                                            rownames=rownames(Biobase::fData(eSet)) 
               ),
               colData=S4Vectors::DataFrame(Biobase::pData(eSet),
                                            rownames=rownames(Biobase::pData(eSet))
               ),
               metadata=list("experimentData" = eSet@experimentData, 
                             "annotation" = Biobase::annotation(eSet), 
                             "protocolData" = Biobase::protocolData(eSet)
               )
             )
             ## TODO:: Determine if this can be done in the SE constructor?
             # Extract names from expression set
             assayNames(SE) <- assayDataElementNames(eSet)
             # Assign SE to cSet
             mDataType <- Biobase::annotation(eSet)
             cSet@molecularProfiles[[mDataType]] <- SE
           })
  setNames(cSet@molecularProfiles, names(eSets))
  cSet
}