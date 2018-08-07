setMethod("subsetByColumn", c("PhenoGenoExperiment", "ANY"), function(x, y) {
    x <- callNextMethod(x, y)
    BiocGenerics:::replaceSlots(x, sensitivity = x@sensitivity[,colnames(x[["sensitivity"]])], check = FALSE)
})

setMethod("subsetByColData", c("PhenoGenoExperiment", "ANY"), function(x, y) {
    x <- callNextMethod(x, y)
    BiocGenerics:::replaceSlots(x, sensitivity = x@sensitivity[,colnames(x[["sensitivity"]])], check = FALSE)
})


