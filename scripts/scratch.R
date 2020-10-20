assayList <- assays(longTable, withDimnames=TRUE, metadata=TRUE, key=FALSE)
assayList$new_viability <- assayList$viability  # Add a fake additional assay
assayCols$new_viability <-  assayCols$viability  # Add column names for fake assay
longTable2 <- buildLongTable(from=assayList, lapply(rowDataCols, names), lapply(colDataCols, names), assayCols)