% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods-mDataNames.R
\name{mDataNames<-}
\alias{mDataNames<-}
\title{mDataNames<- Generic}
\usage{
mDataNames(object, ...) <- value
}
\arguments{
\item{object}{CoreSet object}

\item{...}{Fallthrough arguements for defining new methods}

\item{value}{A \code{character} vector with names to be assigned to each list item in the `molecularProfiles` slot}
}
\value{
An updated copy of the CoreSet object
}
\description{
A generic for the mDataNames method
}
\examples{
mDataNames(clevelandSmall_cSet) <- mDataNames(clevelandSmall_cSet)

}
