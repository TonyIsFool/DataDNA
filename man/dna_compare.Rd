\name{dna_compare}
\alias{dna_compare}
\title{Compare two data DNA profiles}
\usage{
dna_compare(x, y)
}
\arguments{
\item{x}{A data frame or \code{data_dna} object.}
\item{y}{A data frame or \code{data_dna} object.}
}
\value{
A \code{dna_comparison} object.
}
\description{
Compare two data DNA profiles.
}
\examples{
demo <- dna_example_customers()
dna_compare(demo$customers_old, demo$customers_new)
}
