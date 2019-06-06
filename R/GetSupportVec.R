## get vector of interpolated concentrations for graphing purposes
#' .GetSupportVec 
#'
#' @param x An input vector of dosages
#' @param output_length The length of the returned support vector 
#' @export 
.GetSupportVec <- function(x, output_length = 1001) {
  return(seq(from = min(x), to = max(x), length.out = output_length))
}