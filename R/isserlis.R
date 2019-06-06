isserlis <- function(lowerBounds, #1-by-N matrix of lower bounds of box of integration
                     upperBounds, #1-by-N matrix of upper bounds of box of integration
                     growthSlopes, #N-by-N symmetric matrix of growth slopes in the presence of drug combinations
                     nEvalPts) { #1-by-N matrix of number of evaluation points in each dimension of box of integration
  #SANITY CHECKS GO HERE#
  
  if (missing(nEvalPts)) {
    nEvalPts <- rep.int(101, times = length(lowerBounds))
  }
  
  domainVectors <- list()
  for (dimension in seq_along(lowerBounds)) {
    domainVectors <- c(domainVectors, seq(from = lowerBounds[dimension], to = upperBounds[dimension], length.out = nEvalPts[dimension]))
  }
  
  initialGuessValues <- c(rep.int(1, times = length(lowerBounds)), rep.int(0, times = choose(length(lowerBounds), 2)))
  optimizedParameters <- optim(par = initialGuessValues,
                               method = "L-BFGS-B",
                               fn = function(x) {
                                 latticePointInds <- seq(from = 1, to = prod(nEvalPts))
                                 fnValues <- matrix(0, nrow = 1, ncol = length(latticePointInds))
                                 for (latticePoint in latticePointInds) {
                                   matInd <- .matInd(linInd = latticePoint, dimsizes = nEvalPts)
                                   for (dimension1 in seq_along(lowerBounds)) {
                                     for (dimension2 in seq_along(lowerBounds)) {
                                       if (dimesion1 > dimension2) {
                                         fnValues[latticePoint] <- fnValues[latticePoint] + x[NCOL(lowerBounds) + dimension1 * (dimension1 - 1) / 2 + dimension2] * domainVectors[[dimension1]][matInd[dimension1]] * domainVectors[[dimension2]][matInd[dimension2]]
                                       } else if (dimension1 == dimension2) {
                                         fnValues[latticePoint] <- fnValues[latticePoint] + x[dimension1] * domainVectors[[dimension1]][matInd[dimension1]]
                                       } else {
                                       }
                                     }
                                   }
                                 }
                                 fnValues <- exp(fnValues)
                                 return(log(trapznD(fnDomain = domainVectors, fnRange = fnValues)) + sum(growthSlopes * x))
                                 },
                               lower = lowerBounds,
                               upper = upperBounds)
  
  return(optimizedParameters)
}