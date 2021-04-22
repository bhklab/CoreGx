# ======================================================
# Utilty functions for message, warnings, errors and cat
# ------------------------------------------------------

#' @name utils-messages
#'
#'
#' 
NULL

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
#' settings for verbosity in `options()`.
#'
#' @md
#' @keywords internal
#' @export
#' @noRd
.message <- function(...) {
    paste0(strwrap(paste0(..., collapse=collapse)), collapse='\n')
}

