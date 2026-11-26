\name{dna_plot}
\alias{dna_plot}
\title{Draw a paper-style specimen profile figure}
\usage{
dna_plot(x, file = NULL, width = 10.8, height = 6.8, dpi = 200)
}
\arguments{
\item{x}{A data frame or \code{data_dna} object.}
\item{file}{Optional output path (\code{.png}, \code{.pdf}, or \code{.svg}).}
\item{width}{Plot width in inches.}
\item{height}{Plot height in inches.}
\item{dpi}{Resolution used for PNG output.}
}
\value{
The \code{data_dna} object, invisibly.
}
\description{
Creates a clean, publication-ready static figure summarizing a single data frame
or \code{data_dna} profile (role genome, column signals, fingerprints, and
diagnostic warnings) using base R grid graphics.
}
\examples{
demo <- dna_example_customers()
dna_plot(demo$customers_new)
}
