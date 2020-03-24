##' Validate cSet molecularProfiles Conversion
##' 
##' Checks that all the information contained in an ExpressionSet molecularProfile 
##'   was successfully tranferred to the SummarizedExperiment molecularProfile
##'   
##' @param cSet \code{CoreSet} a cSet containing molecularProfiles as SummarizedExperiments
##' @param cSet \code{CoreSet} a cSet containing molecularProfiles as ExpressionSets
##' 
##' @return \code{message} Any slots which are not the same
##' 
##' @importFrom testthat expect_equal test_that
##' 
##' @export
##'
validateCsetMolecularProfilesToSEConversion <- function(cSet_old, cSet_new) {
  
  # Testing that cSets are in correct order
  print("Checking is cSet structures are correct")
  
  testthat::expect_true(
    all(vapply(cSet_old@molecularProfiles, function(x) { is(x, "ExpressionSet") }, FUN.VALUE = logical(1))),
    info = "Old cSet doesn't contain ExpressionSet objects, maybe argument order is wrong?"
  )
  
  testthat::expect_true(
    all(vapply(cSet_new@molecularProfiles, function(x) { is(x, "SummarizedExperiment") }, FUN.VALUE = logical(1))),
    info = "New cSet doesn't contain SummarizedExperiment objects, maybe argument order is wrong?"
  )
  
  # Comparing molecularProfiles slot data  
  print("Checking molecularProfiles slots hold equivalent data.")
  
  for (i in seq_len(length(cSet_old@molecularProfiles))) {
    for (j in seq_along(assays(cSet_new@molecularProfiles[[i]]))) {
      testthat::expect_true(
        all(
          as.list(assayData(cSet_old@molecularProfiles[[i]]))[[j]] == 
            assay(cSet_new@molecularProfiles[[i]], j),
          na.rm = TRUE
        ),
        info = "The assay data is not equivalent"
      )
    }
  }
  ## TODO:: Rewrite this as an apply statement
  for (i in seq_len(length(cSet_old@molecularProfiles))) { # Have to compare like this due to NAs in data
    # Checking phenoData
    testthat::expect_true(
      if (nrow(pData(cSet_old@molecularProfiles[[i]])) > 0) {
        all(
          as(cSet_old@molecularProfiles[[i]]@phenoData, "data.frame") == 
            as.data.frame(cSet_new@molecularProfiles[[i]]@colData[
              seq_len(length(cSet_new@molecularProfiles[[i]]@colData) -1)]),
          na.rm = TRUE)
      } else { TRUE },
      info = "The phenoData is not equivalent",
    )
    # Checking featureData
    testthat::expect_true(
      if (nrow(fData(cSet_old@molecularProfiles[[i]])) > 0) {
        all(
          as(cSet_old@molecularProfiles[[i]]@featureData, "data.frame") == 
            as.data.frame(cSet_new@molecularProfiles[[i]]@elementMetadata[
              seq_len(length(cSet_new@molecularProfiles[[i]]@elementMetadata) -1)]),
          na.rm = TRUE)
      } else { TRUE },
      info = "The featureData is not equivalent",
    )
    # Checking protocolData
    testthat::expect_true(
      all(
        as(cSet_old@molecularProfiles[[i]]@protocolData, "data.frame") ==
          as(cSet_new@molecularProfiles[[i]]@metadata$protocolData, "data.frame"),
        na.rm = TRUE),
      info = "The protocolData is not equivalent"
    )
  }
  
  testthat::expect_equal(
    lapply(cSet_old@molecularProfiles, function(x) { x@annotation }), 
    lapply(cSet_new@molecularProfiles, function(x) { x@metadata$annotation }),
    info = "The annotation is not equivalent"
  )
  
  testthat::expect_equal(
    lapply(cSet_old@molecularProfiles, function(x) { x@experimentData }), 
    lapply(cSet_new@molecularProfiles, function(x) { x@metadata$experimentData }),
    info = "The experimentData is not equivalent"
  )
  
  ##TODO:: Removed .__classVersion__ from SE as it is a property specific to eSet
  # testthat::expect_equal(
  #   lapply(cSet_old@molecularProfiles, function(x) { x@.__classVersion__ }), 
  #   lapply(cSet_new@molecularProfiles, function(x) { x@metadata$.__classVersion__}),
  #   info = "The .__classVersion__ is not equivalent"
  # )
  
  # Comparing remainder of cSet slots; should not be affect by conversion
  print("Comparing remainder of cSet slots")
  
  testthat::test_that("Checking cSet@annotation slot is unchanged.", {
    testthat::expect_equal(cSet_old@annotation, cSet_new@annotation)
  })

  testthat::test_that("Checking cSet@cell slot is unchanged.", {
    testthat::expect_equal(cSet_old@cell, cSet_new@cell)
  })
  
  ## No drug slot in cSet
  # testthat::test_that("Checking cSet@drug slot is unchanged.", {
  #   testthat::expect_equal(cSet_old@drug, cSet_new@drug)
  # })
  
  testthat::test_that("Checking cSet@sensitivity slot is unchanged.", {
    testthat::expect_equal(cSet_old@sensitivity, cSet_new@sensitivity)
  })
  
  testthat::test_that("Checking cSet@datasetType slot is unchanged.", {
    testthat::expect_equal(cSet_old@datasetType, cSet_new@datasetType)
  })
  
  testthat::test_that("Checking cSet@perturbation slot is unchanged.", {
    testthat::expect_equal(cSet_old@perturbation, cSet_new@perturbation)
  })
  
  testthat::test_that("Checking cSet@curation slot is unchanged.", {
    testthat::expect_equal(cSet_old@curation, cSet_new@curation)
  })
  message("Tests pass!")
}

## Doesn't work; issue with scoping of get() function
resaveAllExampleDatasets <- function(datasets) {
  for (dataset in datasets) {
    dataDir <- paste0(grep('data', list.dirs(), value=TRUE))
    load(paste0(dataDir, '/', dataset, '_old.rda'))
    assign(dataset, convertcSetMolecularProfilesToSE(get(dataset)))
    save(list=dataset, file=paste0(dataDir, '/', dataset, '.rda'), compress='xz')
  }
}