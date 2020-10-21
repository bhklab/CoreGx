#' sensitivityRaw CoreSet Getter Method
#'
#' @describeIn CoreSet Get the raw dose and vaibility data from a CoreSet object.
#'
#' @examples
#' data(clevelandSmall_cSet)
#' sensRaw <- sensitivityRaw(clevelandSmall_cSet)
#' head(sensRaw)
#'
#' @param object A [`CoreSet`] to extract the raw sensitivity data from
#'
#' @return A [`array`] containing the raw sensitivity data as experiment by
#'     dose level by metric.
#'
#' @export
setMethod('sensitivityRaw', signature("CoreSet"), function(object){

  if (is(sensitivitySlot(object), 'LongTable'))
      stop(.errorMsg('\n[CoreGx::sensitivityRaw] This getter has not been
          implemented for a CoreSet. Please define a method using setMethod()
          on the subclass of CoreSet in your current package!'))

  object@sensitivity$raw
})

#' sensitivityRaw<- CoreSet Setter Method
#'
#' @describeIn CoreSet Set the raw dose and viability data for a cSet and return
#'   and updated copty
#'
#' @examples
#' data(clevelandSmall_cSet)
#' sensitivityRaw(clevelandSmall_cSet) <- sensitivityRaw(clevelandSmall_cSet)
#'
#' @param object An [`CoreSet`] to extract the raw sensitivity data from.
#' @param value A 3D [`array`] containing the raw dose and viability
#'    measurements to update the object with.
#'
#' @return A 3D [`array`] containing the raw sensitivity data as experiment by
#'    dose level by metric.
#'
#' @export
setReplaceMethod("sensitivityRaw", signature("CoreSet", "array"),
                 function(object, value) {

  if (is(sensitivitySlot(object), 'LongTable'))
      stop(.errorMsg('\n[CoreGx::sensitivityRaw<-] This setter has not been ',
          'implemented for a CoreSet. Please define a method using ',
          'setReplaceMethod() on the subclass of CoreSet in your current ',
          'package!'))

  object@sensitivity$raw <- value
  object
})