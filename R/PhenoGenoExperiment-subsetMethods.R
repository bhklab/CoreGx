setMethod("subsetByColumn", c("PhenoGenoExperiment", "ANY"), function(x, y) {
    x <- callNextMethod(x, y)
    BiocGenerics:::replaceSlots(x, phenotype = x@phenotype[,colnames(x[["phenotype"]])], check = FALSE)
})

setMethod("subsetByColData", c("PhenoGenoExperiment", "ANY"), function(x, y) {
    x <- callNextMethod(x, y)
    BiocGenerics:::replaceSlots(x, phenotype = x@phenotype[,colnames(x[["phenotype"]])], check = FALSE)
})


