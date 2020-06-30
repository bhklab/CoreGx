#' sensitivityRaw Generic
#'
#' @examples
#' data(clevelandSmall_cSet)
#' sensRaw <- sensitivityRaw(clevelandSmall_cSet)
#' head(sensRaw)
#'
#' @param object An \code{object} to extract the raw sensitivity data from
#' @param ... A \code{list} to allow new parameters in specific methods
#'
#' @return A \code{array} containing the raw sensitivity data as experiment by dose level by metric
#'
#' @export
setGeneric("sensitivityRaw", function(object, ...) standardGeneric("sensitivityRaw"))
#'
#' @describeIn CoreSet Get the raw dose and vaibility data for a cSet
#' @export
setMethod('sensitivityRaw', signature("CoreSet"), function(object){
  object@sensitivity$raw
})

#' sensitivityRaw<- Generic
#'
#' #' @examples
#' data(clevelandSmall_cSet)
#' sensitivityRaw(clevelandSmall_cSet) <- sensitivityRaw(clevelandSmall_cSet)
#'
#' @param object An \code{object} to extract the raw sensitivity data from
#' @param ... A \code{list} to allow new parameters in specific methods
#' @param value A 3D \code{array} containing dose and viability metrics to update the object with
#'
#' @return A \code{array} containing the raw sensitivity data as experiment by dose level by metric
#'
#' @export
setGeneric("sensitivityRaw<-", function(object, ..., value) standardGeneric("sensitivityRaw<-"))
#' @describeIn CoreSet Set the raw dose and viability data for a cSet and return
#'   and updated copty
#' @export
setReplaceMethod("sensitivityRaw", signature("CoreSet", "array"),
                 function(object, value) {
  object@sensitivity$raw <- value
  object
})