## get vector of interpolated concentrations for graphing purposes
.GetSupportVec <- function(x, output_length = 1001) {
  return(seq(from = min(x), to = max(x), length.out = output_length))
}