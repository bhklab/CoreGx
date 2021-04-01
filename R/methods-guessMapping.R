#' Generic for Guessing the Mapping Between Some Raw Data and an S4 Object
#' 
#' @param object An `S4` object containing so raw data to guess data to
#'   object slot mappings for.
#' @param ... Allow new arguments to be defined for this generic.
#' 
#' @return An `S4` object with mapping guesses in the appropriate slots.
#' 
#' @md
#' @export
setGeneric('guessMapping', function(object, ...) standardGeneric('guessMapping'))

#'
#' 
#' @param object A `LongTableDataMapper`, containing raw data to guess mappings
#'   for but all other slots as empty lists.
#' 
#' @md
#' @export
setMethod('guessMapping', signature(object='LongTableDataMapper'), 
    function(object)
{
    
})