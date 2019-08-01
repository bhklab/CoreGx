setClass("PlaceHolderAssay", contains="array")


setMethod("[", c("PlaceHolderAssay", "ANY", "ANY"), function(x, i, j, ..., drop=TRUE){
	mdrop <- missing(drop)
    Narg <- nargs() - !mdrop
    if(missing(j)){
        if(Narg == 2) {
            j <- i
        } else {
            j <- seq_len(NCOL(x@.Data))
        }
    }
    x@.Data <- x@.Data[,j, drop=FALSE]
	return(x)
})

setMethod("show", "PlaceHolderAssay", function(object){
	cat("A PlaceHolderAssay containing no observed data but recording the presence of", ncol(object), "samples.\n")
	cat("\nSubsetting this object works only by columns to subset samples.\nSubsetting by rows does nothing.\n")
	})

test <- S4Vectors::DataFrame(
                    assay = factor(),
                    primary = character(),
                    colname = character(),
                    extra = character())