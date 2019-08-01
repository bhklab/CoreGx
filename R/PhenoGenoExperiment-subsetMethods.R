
#' @importFrom MultiAssayExperiment subsetByColumn
#' @export
setMethod("subsetByColumn", c("PhenoGenoExperiment", "ANY"), function(x, y) {
    x <- callNextMethod(x, y)
    BiocGenerics:::replaceSlots(x, phenotype = x@phenotype[,colnames(x[["phenotype"]])], check = FALSE)
})

#' @importFrom MultiAssayExperiment subsetByColData
#' @export
setMethod("subsetByColData", c("PhenoGenoExperiment", "ANY"), function(x, y) {
    x <- callNextMethod(x, y)
    BiocGenerics:::replaceSlots(x, phenotype = x@phenotype[,colnames(x[["phenotype"]])], check = FALSE)
})

## TODO:: move to separate file and write WideFormat

longFormat <- function(object, colDataCols = NULL, i = 1L) {
    if (is(object, "PhenoGenoExperiment"))
        return(MultiAssayExperiment::longFormat(suppressMessages(object[,,names(object)]), colDataCols = colDataCols, i = i))
    else
      return(MultiAssayExperiment::longFormat(object, colDataCols = colDataCols, i = i))
}