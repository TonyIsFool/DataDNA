\name{dna_match_plot}
\alias{dna_match_plot}
\title{Draw a paper-style lineage figure}
\usage{
dna_match_plot(match, file = NULL, width = 11, height = 7, dpi = 144)
}
\arguments{
\item{match}{A \code{dna_match} object.}
\item{file}{Optional output path. Supported extensions are \code{.png} and \code{.pdf}.}
\item{width}{Plot width in inches.}
\item{height}{Plot height in inches.}
\item{dpi}{Resolution used for PNG output.}
}
\value{
The input \code{dna_match} object, invisibly.
}
\description{
Creates a print-friendly, paper-style lineage figure for a \code{dna_match}
object using base R grid graphics. The figure can be drawn on the current
graphics device or saved directly to PNG or PDF.
}
\examples{
demo <- dna_example_customers()
lib <- list(old = data_dna(demo$customers_old), new = data_dna(demo$customers_new))
match <- dna_match(demo$customers_new, lib)
dna_match_plot(match)
}
