#' Generic for preprocessing complex data before being used in the constructor
#'   of an `S4` container object.
#' 
#' This method is intended to abstract away complex constructor arguments
#'   and data preprocessing steps needed to transform raw data, such as that
#'   produced in a treatment-response or next-gen sequencing experiment, and 
#'   automate building of the appropriate `S4` container object. This is
#'   is intended to allow mapping between different experimental designs,
#'   in the form of an `S4` configuration object, and various S4 class
#'   containers in the Bioconductor community and beyond.
#' 
#' @param mapper An `S4` object abstracting arguments to an `S4` class
#'   constructor into a well documented `Mapper` object.
#' @param ... Allow new arguments to be defined for this generic.
#' 
#' @return An `S4` object for which the class corresponds to the type of
#'   the build configuration object passed to this method.
#' 
#' @md
#' @export
setGeneric('metaConstruct', function(mapper, ...) standardGeneric('metaConstruct'))


#' 
#' @md
#' @export
setMethod('metaConstruct', signature(mapper='LongTableDataMapper'), 
    function(mapper)
{
    DT <- rawdata(mapper)

})


#' 
#' 
#' 
#' 
#' 
