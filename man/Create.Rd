% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods-buildLongTable.R
\name{buildLongTable,list-method}
\alias{buildLongTable,list-method}
\title{LongTable build method from list}
\usage{
\S4method{buildLongTable}{list}(from, rowDataCols, colDataCols, assayCols)
}
\arguments{
\item{from}{[`list`] A list containing any combination of character file paths,
data.tables and data.frames which will be used to construct the LongTable.}

\item{rowDataCols}{[`list`] List with two `character` vectors, the first
specifying one or more columns to be used as cell identifiers (e.g.,
cell-line name columns) and the second containing any additional metadata
columns related to the cell identifiers.}

\item{colDataCols}{[`list`] List with two `character` vectors, the first
specifying one or more columns to be used as column identifiers (e.g.,
drug name columns) and the second containing any additional metadata
columns related to the column identifiers.}

\item{assayCols}{[`list`] A named list of character vectors specifying how to
parse assay columns into a list of `data.table`s. Each list data.table
will be named for the name of corresponding list item and contain the columns
specified in the character vector of column names in each list item.}
}
\value{
A [`LongTable`] object constructed with the data in `from`.
}
\description{
LongTable build method from list
}
\section{Functions}{
\itemize{
\item \code{buildLongTable,list-method}: a LongTable object from a list containing file paths,
data.frames and data.tables.
}}

\examples{
assayList <- assays(merckLongTable, withDimnames=TRUE)
rowDataCols <- list(rowIDs(merckLongTable))
colDataCols <- list(colIDs(merckLongTable), colMeta(merckLongTable))
assayCols <- assayCols(merckLongTable)
longTable <- buildLongTable(from=assayList, rowDataCols, colDataCols, assayCols)

}
