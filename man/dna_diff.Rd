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
A \code{dna_diff} object containing a mutation table.
}
\description{
Explain mutations between two data DNA profiles.
}
\examples{
demo <- dna_example_customers()
dna_diff(demo$customers_old, demo$customers_new)
}
