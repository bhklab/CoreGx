library(CoreGx)
library(data.table)

# ---- NCI-ALMANAC

# -- Read in data
exp_DT <- fread('combos/NCI_ComboDrugGrowth_Nov2017.csv')
drug_DT <- fread('combos/NCI_compound_names.tsv')
colnames(drug_DT) <- c('NSC', 'drug_name')

# -- Drop drugs with duplicated NSC ids
duplicatedDrugs <- drug_DT[duplicated(NSC) | duplicated(drug_name), ]
drug_DT <- drug_DT[!(duplicated(NSC) | duplicated(drug_name)), ]

# -- Join to map to names of drug1 and drug2 in combo
nci_DT <- merge.data.table(exp_DT, drug_DT, by.x='NSC1', by.y='NSC')
setnames(nci_DT, 'drug_name', 'drug1_name', skip_absent=TRUE)

nci_DT <- merge.data.table(nci_DT, drug_DT, by.x='NSC2', by.y='NSC')
setnames(nci_DT, 'drug_name', 'drug2_name', skip_absent=TRUE)

# -- Keep a backup of the original
.nci_DT <- nci_DTmaps

# -- Explore the dimensonality
.length_unique <- function(x) length(unique(x))
.list_unique <- function(x) list(unique(x))
.all_equals <- function(x, y) all(x == y)

# -- Identify already unique columns
onlyOneValueCols <- names(which(vapply(nci_DT, .length_unique, numeric(1)) == 1))
expMetadata <- as.list(unique(nci_DT[, ..onlyOneValueCols]))
nci_DT <- nci_DT[, .SD, .SDcols=!onlyOneValueCols]


# -- Identify columns uniquely identified by drug combo
dimDrugsDT <- nci_DT[, lapply(.SD, .length_unique), by=.(drug1_name, drug2_name)]
mapToDrugsCols <- names(which(vapply(dimDrugsDT, .all_equals, y=1, logical(1))))

# -- Identify columns uniquely identified by cell-line 
dimCellsDT <- nci_DT[, lapply(.SD, .length_unique), by=.(CELLNAME)]
mapToCellsCols <- names(which(vapply(dimCellsDT, FUN=.all_equals, y=1, logical(1))))

# -- Determine if any column map to cells and drugs
ambiguiousDrugsOrCells <- intersect(mapToCellsCols, mapToDrugsCols)
if (length(ambiguiousDrugsOrCells) > 0) 
    stop('The columns ', paste0(ambiguiousDrugsOrCells, collapse=', '), 
        ' can map to either drugs or cells!')

# -- Subset mapped columsn to correct data.table
expDrugDT <- nci_DT[, .SD, .SDcols=mapToDrugsCols]
expCellsDT <- nci_DT[, .SD, .SDcols=mapToCellsCols]

# -- Remove mapped columns
nci_DT_drop <- nci_DT[, .SD, .SDcols=!unique(c(mapToCellsCols, mapToDrugsCols))]

# -- Identify columns uniquely identified by drug combo + concentrations
dimDrugsConcDT <- nci_DT_drop[, lapply(.SD, .length_unique), 
    by=.(drug1_name, drug2_name, CONC1, CONC2)]
DT <- nci_DT_drop[, lapply(.SD, .list_unique), 
    by=.(drug1_name, drug2_name, CONC1, CONC2)]
DT <- DT[which(dimDrugsConcDT$CONCINDEX2 == 2), 
    .(drug1_name, drug2_name, CONC1, CONC2, CONCINDEX1, CONCINDEX2)]
mapsToDrugsConcCols <- names(which(vapply(dimDrugsConcDT, FUN=.all_equals, y=1, 
    logical(1))))


# -- Identify columns uniquely identified by drug combo and cell line
dimCellDrugDT <- nci_DT_drop[, lapply(.SD, .length_unique), 
    by=.(drug1_name, drug2_name, CELLNAME)]
mapsToCellDrugCols <- names(which(vapply(dimCellDrugDT, FUN=.all_equals, y=1, 
    logical(1))))

ambiguousCellDrugOrDrugConc <- intersect(mapsToDrugsConcCols, mapsToCellDrugCols)
if (length(ambiguousCellDrugOrDrugConc) > 0) 
    stop('The columns ', paste0(ambiguousCellDrugOrDrugConc, collapse=', '), 
        ' can map to either drugs by cells or drugs by concentrations!')

# -- Extract the mapped column data.tables
expDrugsByConcDT <- nci_DT_drop[, .SD, .SDcols=mapsToDrugsConcCols]
expDrugsByCellsDT <-  nci_DT_drop[, .SD, .SDcols=mapsToDrugsConcCols]

# -- Drop mapped columns
nci_DT_drop1 <- nci_DT_drop[, .SD, .SDcols=!unique(c(mapsToDrugsConcCols, mapsToCellDrugCols))]

# -- Identify columns uniquely identified by drug combo, cell line and concentrations
dimDrugConcCellDT <- nci_DT_drop1[, lapply(.SD, .length_unique), 
    by=.(drug1_name, drug2_name, CELLNAME, CONC1, CONC2)]
mapsToExpCondition <- names(which(vapply(dimDrugConcCellDT, FUN=.all_equals, 
    y=1, logical(1))))
expAssaysDT <- nci_DT_drop1[, .SD, .SDcols=mapsToExpCondition]

# -- Setup our build configuration