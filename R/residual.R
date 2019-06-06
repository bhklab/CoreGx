.residual <- function (x, y, n, pars, f, scale = 0.07, family = c("normal", "Cauchy"), trunc = FALSE) {
  family <- match.arg(family)
  diffs <- do.call(f, list(x, pars)) - y
  
  if (family != "Cauchy") {
    if (trunc == FALSE) {

      if (n == 1){
        return(sum(diffs^2))
      }

      # if(!all(is.finite(sum(-log(.dmednnormals(diffs, n, scale)))))){
      #   browser()
      # }

      return(sum(-log(.dmednnormals(diffs, n, scale))))
    }
    else {
      down_truncated <- abs(y) >= 1
      up_truncated <- abs(y) <= 0
      return(sum(-log(.dmednnormals(diffs[!(down_truncated | up_truncated)], n, scale))) +
               sum(-log(.edmednnormals(-diffs[up_truncated | down_truncated], n, scale))))
    }
  }
  else {
    if (trunc == FALSE) {
      return(sum(-log(.dmedncauchys(diffs, n, scale))))
    }
    else {
      down_truncated <- abs(y) >= 1
      up_truncated <- abs(y) <= 0
      return(sum(-log(.dmedncauchys(diffs[!(down_truncated | up_truncated)], n, scale))) +
               sum(-log(.edmedncauchys(-diffs[up_truncated | down_truncated], n, scale))))
    }
  }
}