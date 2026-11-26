\name{dna_diff}
\alias{dna_diff}
\title{Explain mutations between two data DNA profiles}
\usage{
dna_diff(x, y)
}
\arguments{
\item{x}{A data frame or \code{data_dna} object.}
\item{y}{A data frame or \code{data_dna} object.}
}
\value{
A \code{dna_diff} object containing \code{changes} and \code{renames} tables.
}
\description{
Detects schema changes, likely column renames, type and role transitions,
missingness shifts, uniqueness changes, numeric distribution drift, category
shifts, and temporal window drift between two data sets.
}
\examples{
demo <- dna_example_customers()
dna_diff(demo$customers_old, demo$customers_new)
}
