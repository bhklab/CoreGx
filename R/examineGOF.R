###TODO:: Determine type of objects intended for this function
#' Getter for attributes of an object
#'
#' @param pars The object for which attributes are to be returned
#' @return A named vector where index `Rsquare` contains the attributes of the object
#' @export
examineGOF <- function(pars) {
	return(c("Rsquare" = attr(pars, "Rsquare")))
}

### If more GOF stats are implemented, they can be added above