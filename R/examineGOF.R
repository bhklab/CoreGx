examineGOF <- function(pars) {
	return(c("Rsquare" = attr(pars, "Rsquare")))
}

### If more GOF stats are implemented, they can be added above