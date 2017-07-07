function (x, y, n, pars, f, scale = 0.07, family = c("normal", "Cauchy"), trunc = FALSE) 
{
  family <- match.arg(family)
  Cauchy_flag = (family == "Cauchy")
  diffs <- do.call(f, list(x, pars)) - y
  
  if (Cauchy_flag == FALSE) {
    if (trunc == FALSE) {
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