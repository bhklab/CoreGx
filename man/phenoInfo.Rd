% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods-phenoInfo.R
\name{phenoInfo}
\alias{phenoInfo}
\title{phenoInfo Generic}
\usage{
phenoInfo(object, mDataType, ...)
}
\arguments{
\item{object}{The \code{CoreSet} to retrieve rna annotations from}

\item{mDataType}{the type of molecular data}

\item{...}{Fallthrough argument for defining new parameters in other S4 methods}
}
\value{
a \code{data.frame} with the experiment info
}
\description{
Generic for phenoInfo method
}
\examples{
phenoInfo(clevelandSmall_cSet, mDataType="rna")

}
