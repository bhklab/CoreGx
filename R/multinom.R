.multinom<-function(x, y) {
  coeff <- 1
  for (i in 1:length(y)) {
    coeff <- coeff * choose(x, y[i])
    x <- x - y[i]
  }
  return(coeff)
}