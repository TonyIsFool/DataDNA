\name{dna_match_card}
\alias{dna_match_card}
\title{Render a DataDNA lineage match card}
\usage{
dna_match_card(match, file = NULL, open = FALSE)
}
\arguments{
\item{match}{A \code{dna_match} object.}
\item{file}{Optional HTML file path. If supplied, the card is saved there.}
\item{open}{Logical. Open the saved file in a browser when \code{file} is supplied.}
}
\value{
A \code{dna_html} document object, invisibly when saved to \code{file}.
}
\description{
Creates a self-contained HTML/SVG lineage diagram for a \code{dna_match} object.
}
\examples{
demo <- dna_example_customers()
lib <- list(old = data_dna(demo$customers_old), new = data_dna(demo$customers_new))
match <- dna_match(demo$customers_new, lib)
dna_match_card(match)
}
