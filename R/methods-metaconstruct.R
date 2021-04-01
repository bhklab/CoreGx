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
setGeneric('metaconstruct', function(mapper, ...) standardGeneric('metaconstruct'))

#' Guess which columns in raw experiment data map to which dimensions.
#' 
#' Checks for columns which are uniquely identified 
#' 
#' @param mapper A `LongTableDataMapper` object.
#' @param groups A `list` containing one or more vector of column names
#'   to group-by. The function uses these to determine 1:1 mappings between
#'   the combination of columns in each vector and unique values in the raw
#'   data columns.
#' @param subset A `logical` vector indicating whether to to subset out mapped
#'   columns after each grouping. Must be a single `TRUE` or `FALSE` or have
#'   the same length as groups, indicating whether to subset out mapped columns
#'   after each grouping.
#' 
#' @return The `mapper` object, with predicted mappings.
#' 
#' @md
#' @export
setMethod('metaconstruct', signature(mapper='LongTableDataMapper'), 
    function(mapper, groups, subset=TRUE)
{
    dataL <- rawdata(mapper)

    # -- parse data into long format


    # -- 

})


#'
#' 
#' 
#' 
#' 
