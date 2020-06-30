# ---- Generics -----------------------------------------------------------------
#' curation Slot Getter
#'
#' @examples
#' data(clevelandSmall_cSet)
#' curation(clevelandSmall_cSet)
#'
#' @param object A \code{object}
#' @param ... A \code{list} to allow definition of new parameters on this generic
#'
#' @return A \code{list} of unique cell and tissue identifiers to check validity
#'   of an rSet
#'
#' @export
setGeneric("curation", function(object, ...) standardGeneric("curation"))

#' curation<- Slot Setter
#'
#' #' @examples
#' data(clevelandSmall_cSet)
#' curation(clevelandSmall_cSet) <- curation(clevelandSmall_cSet)
#'
#' @param object A \code{object}
#' @param ... A \code{list} to allow definition of new parameters on this generic
#' @param value A \code{list} of curations for the cell and tissues types in the
#'   rSet object
#'
#' @return A copy of the \code{RadioSet} with the updated curation slot
#'
#' @export
setGeneric("curation<-", function(object, ..., value) standardGeneric("curation<-"))


# ---- Methods -----------------------------------------------------------------
#' curation Slot Getter
#'
#' @examples
#' data(clevelandSmall_cSet)
#' curation(clevelandSmall_cSet)
#'
#' @param object A \code{CoreSet} object to retrive the curation slot from
#'
#' @return A \code{list} of unique cell and tissue identifiers to check validity
#'   of a cSet
#'
#' @describeIn CoreSet Retrieve the curation slot form a cSet
#' @export
setMethod('curation', signature(object="CoreSet"), function(object) {
    object@curation
})


#' curation<- Slot Setter
#'
#' @examples
#' data(clevelandSmall_cSet)
#' curation(clevelandSmall_cSet) <- curation(clevelandSmall_cSet)
#'
#' @param object A \code{CoreSet}
#' @param value A \code{list} of curations for the cell and tissues types in the cSet object
#'
#' @return A copy of the \code{RadioSet} with the updated curation slot
#'
#' @describeIn CoreSet Update the curation slot of a cSet
#' @export
setReplaceMethod("curation", signature(object="CoreSet", value="list"), function(object, value) {
    object@curation <- value
    object
})