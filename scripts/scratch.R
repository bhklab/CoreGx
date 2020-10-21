library(CoreGx)

newAssays <- assays(merckLongTable, withDimnames=TRUE)

assays(merckLongTable) <- newAssays