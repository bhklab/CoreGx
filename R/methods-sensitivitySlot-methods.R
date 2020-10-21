#' sensitivitySlot Generic
#'
#' @examples
#' data(clevelandSmall_cSet)
#' sensitivitySlot(clevelandSmall_cSet)
#'
#' @param object A \code{CoreSet} to extract the raw sensitivity data from
#' @param ... A \code{list} to allow new parameters in specific methods
#'
#' @return A \code{list} of the sensitivity slot contents
#'
#' @export
setGeneric("sensitivitySlot", function(object, ...) standardGeneric("sensitivitySlot"))
#' @describeIn CoreSet Retrieve the contents of the sensitivity slot
#' @inheritParams sensitivitySlot
#' @export
setMethod("sensitivitySlot", signature("CoreSet"), function(object) {
  object@sensitivity
})

##TODO:: Migrate this to CoreGx
#' sensitivitySlot<- Replacement Generic
#'
#' @examples
#' data(clevelandSmall_cSet)
#' sensitivitySlot(clevelandSmall_cSet) <- sensitivitySlot(clevelandSmall_cSet)
#'
#' @param object A \code{CoreSet} to extract the raw sensitivity data from
#' @param ... A \code{list} to allow new parameters in specific methods
#' @param value A \code{list} of new sensitivity slot data for the rSet
#'
#' @return A copy of the \code{CoreSet} containing the updated sensitivty slot
#'
#' @export
setGeneric("sensitivitySlot<-", function(object, ..., value) standardGeneric("sensitivitySlot<-"))
#' @describeIn CoreSet Set the raw dose and viability data for a cSet and return
#'   and updated copty
#' @inheritParams sensitivitySlot<-
#' @export
setReplaceMethod("sensitivitySlot", signature("CoreSet", "list"),
                 function(object, value) {
                   ##TODO:: Implement error handinlg for this slot
                   object@sensitivity <- value
                   object
                 })