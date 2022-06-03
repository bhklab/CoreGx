# ======================================================
# Utilty functions for message, warnings, errors and cat
# ------------------------------------------------------

#' @title .formatMessage
#'
#' @description Format one or more strings to fit nicely displayed in the
#' R console at any given width.
#'
#' @param ... One or more `character` vectors containing the strings to be
#' formatted
#' @param collapse `character(1)` A string of characters to collapse vectors
#' inside `...` with.
#'
#' @md
#' @keywords internal
#' @export
#' @noRd
.formatMessage <- function(..., collapse=', ') {
    paste0(strwrap(paste0(..., collapse=collapse)), collapse='\n')
}

#' @title .message
#'
#' @description Alternative to message which respects the local package
#' settings for verbosity in `options()`. The message is displayed if either
#' the general R option 'verbose' is TRUE, or if '<packageName()>.verbose'
#' is TRUE.
#'
#' @details
#' Defaults for package verbosity are determined in zzz.R via the .onAttach
#' function. When loading the package, if the session is interactive the
#' default verbosity is TRUE, otherwise it is FALSE.
#'
#' @md
#' @importFrom crayon blue
#' @keywords internal
#' @export
#' @noRd
.message <- function(...) {
    optionName <- paste0(packageName(), '.verbose')
    optionIsTRUE <- !is.null(getOption(optionName)) && getOption(optionName)
    verboseIsTRUE <- getOption('verbose')
    if (optionIsTRUE || verboseIsTRUE)
        message(crayon::blue$bold(.formatMessage(...)))
}

#' @title .warning
#'
#' @description Alternative to message which respects the local package
#' settings for verbosity in `options()`. The message is displayed if either
#' the general R option 'verbose' is TRUE, or if '<packageName()>.verbose'
#' is TRUE.
#'
#' @details
#' Defaults for package verbosity are determined in zzz.R via the .onAttach
#' function. When loading the package, if the session is interactive the
#' default verbosity is TRUE, otherwise it is FALSE.
#'
#' @md
#' @importFrom crayon cyan
#' @keywords internal
#' @export
#' @noRd
.warning <- function(...) {
    warning(crayon::cyan$bold(.formatMessage(...)), call.=FALSE)
}

#' @title .error
#'
#' @description Alternative to error which formats the error to fit the
#' console and prints it in magenta.
#'
#' @details
#' Defaults for package verbosity are determined in zzz.R via the .onAttach
#' function. When loading the package, if the session is interactive the
#' default verbosity is TRUE, otherwise it is FALSE.
#'
#' @md
#' @importFrom crayon magenta
#' @keywords internal
#' @export
#' @noRd
.error <- function(...) {
    stop(crayon::magenta$bold(.formatMessage(...)), call.=FALSE)
}

#' @title .funContext
#'
#' @description Build a string with the package and function name for a current
#'   function. Prepended to error message to make it easier to determine where
#'   the error or warning came from.
#'
#' @param funName `character(1)` A string with the function name, prepended
#'   with the correct connection to the package NAMESPACE. For exported functions
#'   use '::', for non-exported functions use ':::'.
#'
#' @keywords internal
#' @export
#' @noRd
.funContext <- function(funName) paste0('[', packageName(), funName, ']\n')


#' @title .parseToRoxygen
#'
#' @description Takes a string block of roxygen2 tags sepearated by new-line
#'   characteres and parses it to the appropriate format for the @eval tag,
#'   subtituting any string in { } for the argument of the same name in `...`.
#'
#' @keywords internal
#' @export
#' @noRd
.parseToRoxygen <- function(string, ...) {
    unlist(strsplit(
        with(list(...), glue::glue(string)),
    '\n'))
}

#' @keywords internal
.paste_ <- function(x, y) paste(x, y, sep='_')

#' @keywords internal
.paste_colon <- function(x, y) paste(x, y, sep=':')

#' @keywords interanl
.paste_slashes <- function(...) paste(..., sep='///')