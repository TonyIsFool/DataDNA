\name{dna_species}
\alias{dna_species}
\title{Guess the species of a data frame}
\usage{
dna_species(df)
}
\arguments{
\item{df}{A data frame.}
}
\value{
A character label such as \code{customer_table}, \code{event_stream}, or
\code{wide_feature_matrix}.
}
\description{
Guess the species of a data frame.
}
\examples{
dna_species(dna_example_customers()$customers_new)
}
