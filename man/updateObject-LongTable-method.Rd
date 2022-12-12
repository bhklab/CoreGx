% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/updateObject-methods.R
\name{updateObject,LongTable-method}
\alias{updateObject,LongTable-method}
\title{Update the \code{LongTable} class after changes in it struture or API}
\usage{
\S4method{updateObject}{LongTable}(object, verify = FALSE)
}
\arguments{
\item{object}{A \code{LongTable} object to update the class structure for.}

\item{verify}{A \code{logical(1)} indicating is \code{validObject} should be called
after updating the object. Defaults to \code{TRUE}, only set \code{FALSE} for debugging.}
}
\value{
\code{LongTable} with update class structure.
}
\description{
Update the \code{LongTable} class after changes in it struture or API
}