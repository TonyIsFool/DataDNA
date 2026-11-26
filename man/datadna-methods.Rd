\name{datadna-methods}
\alias{datadna-methods}
\alias{summary.data_dna}
\alias{plot.data_dna}
\alias{plot.dna_match}
\alias{plot.dna_comparison}
\alias{plot.dna_diff}
\alias{as.data.frame.data_dna}
\alias{as.data.frame.dna_comparison}
\alias{as.data.frame.dna_diff}
\alias{as.data.frame.dna_match}
\alias{as.data.frame.dna_library}
\title{S3 methods for DataDNA objects}
\usage{
\method{summary}{data_dna}(object, ...)

\method{plot}{data_dna}(x, y = NULL, ...)

\method{plot}{dna_match}(x, y = NULL, ...)

\method{plot}{dna_comparison}(x, y = NULL, ...)

\method{plot}{dna_diff}(x, y = NULL, ...)

\method{as.data.frame}{data_dna}(x, row.names = NULL, optional = FALSE, ...)

\method{as.data.frame}{dna_comparison}(x, row.names = NULL, optional = FALSE, ...)

\method{as.data.frame}{dna_diff}(x, row.names = NULL, optional = FALSE, ...)

\method{as.data.frame}{dna_match}(x, row.names = NULL, optional = FALSE, ...)

\method{as.data.frame}{dna_library}(x, row.names = NULL, optional = FALSE, ...)
}
\arguments{
\item{object}{A \code{data_dna} object.}
\item{x}{A DataDNA object (\code{data_dna}, \code{dna_comparison}, \code{dna_diff}, \code{dna_match}, or \code{dna_library}).}
\item{y}{Optional second data frame or \code{data_dna} object when plotting a \code{data_dna} specimen comparison.}
\item{row.names}{Optional row names passed to \code{as.data.frame()}.}
\item{optional}{Logical passed to \code{as.data.frame()}.}
\item{...}{Additional arguments passed to underlying plotting or conversion functions.}
}
\value{
\itemize{
\item \code{summary.data_dna()} returns a \code{summary.data_dna} list.
\item \code{plot.*} methods draw a paper-style figure and return their primary object invisibly.
\item \code{as.data.frame.*} methods return flat, export-ready \code{data.frame} tables.
}
}
\description{
Standard R \code{summary()}, \code{plot()}, and \code{as.data.frame()} methods
for \code{data_dna}, \code{dna_comparison}, \code{dna_diff}, \code{dna_match},
and \code{dna_library} objects.
}
\examples{
demo <- dna_example_customers()
dna <- data_dna(demo$customers_new)
summary(dna)
head(as.data.frame(dna))
}
