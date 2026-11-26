\name{dna_diff_plot}
\alias{dna_diff_plot}
\title{Draw a paper-style drift and mutation figure}
\usage{
dna_diff_plot(x, y = NULL, file = NULL, width = 10.8, height = 6.8, dpi = 200)
}
\arguments{
\item{x}{A data frame, \code{data_dna}, \code{dna_comparison}, or \code{dna_diff} object.}
\item{y}{Optional second data frame or \code{data_dna} object when \code{x} is a data frame or \code{data_dna} profile.}
\item{file}{Optional output path (\code{.png}, \code{.pdf}, or \code{.svg}).}
\item{width}{Plot width in inches.}
\item{height}{Plot height in inches.}
\item{dpi}{Resolution used for PNG output.}
}
\value{
A \code{dna_comparison} object (or \code{x} when \code{x} is a \code{dna_diff}), invisibly.
}
\description{
Visualizes similarity across fingerprint dimensions alongside ranked column and
schema mutations between two data sets (or from an existing
\code{dna_comparison} or \code{dna_diff} object).
}
\examples{
demo <- dna_example_customers()
dna_diff_plot(demo$customers_old, demo$customers_new)
}
