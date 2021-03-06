% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{is.items}
\alias{is.items}
\title{Get the types of all items in a list}
\usage{
is.items(list, ..., FUN = is)
}
\arguments{
\item{list}{A [`list`] to get the types from}

\item{...}{[`pairlist`] Additional arguments to FUN}

\item{FUN}{[`function`] or [`character`] Either a function, or the name
of a function which returns a single logical value. The default function
uses `is`, specify the desired type in `...`. You can also use other
type checking functions such as is.character, is.numeric, or is.data.frame.}
}
\value{
[`logical`] A vector indicating if the list item is the specified
  type.
}
\description{
Get the types of all items in a list
}
\examples{
list <- list(c(1,2,3), c('a','b','c'))
is.items(list, 'character')

}
