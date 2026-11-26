\name{dna_example_customers}
\alias{dna_example_customers}
\title{Example customer tables}
\usage{
dna_example_customers()
}
\value{
A list with \code{customers_old} and \code{customers_new} data frames.
}
\description{
Creates two small customer data frames designed to demonstrate DataDNA cards,
comparison, and mutation reports without altering the caller's random number
generator state.
}
\examples{
demo <- dna_example_customers()
str(demo$customers_old)
}
