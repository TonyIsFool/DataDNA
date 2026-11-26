\name{dna_card}
\alias{dna_card}
\title{Render a laboratory-style data DNA card}
\usage{
dna_card(x, file = NULL, open = FALSE)
}
\arguments{
\item{x}{A data frame or \code{data_dna} object.}
\item{file}{Optional HTML file path. If supplied, the card is saved there.}
\item{open}{Logical. Open the saved file in a browser when \code{file} is supplied.}
}
\value{
A \code{dna_html} document object, invisibly when saved to \code{file}.
}
\description{
Builds a self-contained HTML specimen card for a data frame or \code{data_dna}
object without external runtime dependencies.
}
\examples{
demo <- dna_example_customers()
card <- dna_card(demo$customers_new)
}
