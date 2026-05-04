\name{data_dna}
\alias{data_dna}
\title{Create a data DNA profile}
\usage{
data_dna(df, name = NULL, sample_size = 10000L)
}
\arguments{
\item{df}{A data frame.}
\item{name}{Optional data set name shown on cards and print output.}
\item{sample_size}{Maximum number of rows used for profiling.}
}
\value{
A \code{data_dna} object.
}
\description{
Profiles an R data frame into a compact identity object that records schema,
shape, missingness, distributions, categories, uniqueness, time signals, and
stable fingerprints.
}
\examples{
demo <- dna_example_customers()
dna <- data_dna(demo$customers_new, name = "customers_new")
dna
}
