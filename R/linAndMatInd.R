.linInd <- function(matInd, #array indices
                    dimsizes) { #array containing size of array of interest in each dimension
  y <- matInd[1]
  if (length(dimsizes) > 1) {
    for (i in 2:length(dimsizes)) {
      y <- y + (matInd[i] - 1) * prod(dimsizes[1:(i - 1)])
    }
  }
  return(y)
}

.matInd <- function(linInd, #linear index
                    dimsizes) { #array containing size of array of interest in each dimension
  y <- matrix(0, nrow = length(dimsizes), ncol = 1)
  if (NROW(y) > 1) {
    for (i in seq(from = length(dimsizes), to = 2)) {
      y[i] <- ceiling(linInd / prod(dimsizes[1:(i - 1)]))
      linInd <- linInd - (y[i] - 1) * prod(dimsizes[1:(i - 1)])
    }
  }
  y[1] <- linInd
  return(y)
}