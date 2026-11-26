\name{dna_library}
\alias{dna_library}
\title{Build or load a compact DataDNA library}
\usage{
dna_library(x, sample_size = 10000L, recursive = FALSE, file = NULL)
}
\arguments{
\item{x}{A list of data frames/\code{data_dna} objects, a directory path, or a character vector of file paths. Passing a single \code{.rds} path that stores a previously saved \code{dna_library} loads it directly.}
\item{sample_size}{Maximum number of rows sampled per table when profiling.}
\item{recursive}{Logical. Search subdirectories when \code{x} is a directory.}
\item{file}{Optional \code{.rds} path where the resulting \code{dna_library} is saved.}
}
\value{
A \code{dna_library} object (a named list of \code{data_dna} objects).
}
\description{
Profiles a list of data frames/\code{data_dna} objects, or scans tabular/R data
files (\code{.csv}, \code{.tsv}, \code{.rds}, \code{.rda}, \code{.RData}) from
disk one by one so raw tables do not stay resident in memory.
}
\examples{
demo <- dna_example_customers()
lib <- dna_library(demo)
lib
}
