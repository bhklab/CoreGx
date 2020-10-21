#' molecularProfilesSlot Generic
#'
#' @examples
#' data(clevelandSmall_cSet)
#' molecularProfilesSlot(clevelandSmall_cSet)
#'
#' @param object A \code{CoreSet} from which to return a list of all availble
#'   SummarizedExperiment objects
#' @param ... A \code{list} of additional parameters; included to allow adding
#'   arguments to methods on this generc
#'
#' @return A \code{list} containing the molecularProfiles from a cSet
#'
#' Generic for molecularProfilesSlot
setGeneric("molecularProfilesSlot", function(object, ...) standardGeneric("molecularProfilesSlot"))
#' @describeIn CoreSet Return a list containing all molecularProfiles in the cSet
#' @inheritParams molecularProfilesSlot
#' @export
setMethod("molecularProfilesSlot", signature("CoreSet"), function(object) {
  object@molecularProfiles
})

#' molecularProfilesSlot<-
#'
#' Replace method for the molecular profiles slot of a cSet
#'
#' @examples
#' data(clevelandSmall_cSet)
#' molecularProfilesSlot(clevelandSmall_cSet) <- molecularProfilesSlot(clevelandSmall_cSet)
#'
#' @param object A \code{CoreSet} object for which values will be replaced
#' @param value A \code{list} containing molecular profiles as SummarizedExperiments
#'
#' @return A copy of the \code{CoreSet} with the molecularProfiles slot updated
#'
#' @export
setGeneric("molecularProfilesSlot<-",
           function(object, value) standardGeneric("molecularProfilesSlot<-"))
#' @describeIn CoreSet Update the contents of the molecularProfiles slot in a
#'   CoreSet and returns an update copy
#' @inheritParams molecularProfilesSlot<-
#' @export
setReplaceMethod("molecularProfilesSlot", signature("CoreSet", "list"),
                 function(object, value) {
  ##TODO:: Implement error handling for this function
  object@molecularProfiles <- value
  object
})
