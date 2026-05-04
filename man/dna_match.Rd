\name{dna_match}
\alias{dna_match}
\title{Match a data set against a DNA library}
\usage{
dna_match(x, library, top_n = 5L, sample_size = 10000L)
}
\arguments{
\item{x}{A data frame or \code{data_dna} object to match.}
\item{library}{A list of data frames or \code{data_dna} objects.}
\item{top_n}{Maximum number of matches to return.}
\item{sample_size}{Maximum number of rows used when profiling raw data frames.}
}
\value{
A \code{dna_match} object.
}
\description{
Finds the closest relatives of a query data set by comparing its data DNA
against a named library of data frames or \code{data_dna} objects.
}
\examples{
demo <- dna_example_customers()
lib <- list(old = data_dna(demo$customers_old), new = data_dna(demo$customers_new))
dna_match(demo$customers_new, lib)
}
