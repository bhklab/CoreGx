.multinom<-function(x, y) {
  coeff <- 1
  for (i in seq_len(length(y))) {
    coeff <- coeff * choose(x, y[i])
    x <- x - y[i]
  }
  return(coeff)
}