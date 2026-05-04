#' Example customer tables
#'
#' Creates two small customer data frames designed to demonstrate DataDNA cards,
#' comparison, and mutation reports.
#'
#' @return A list with `customers_old` and `customers_new` data frames.
#' @export
#'
#' @examples
#' demo <- dna_example_customers()
#' str(demo$customers_old)
dna_example_customers <- function() {
  set.seed(42)
  n <- 180
  customers_old <- data.frame(
    customer_id = sprintf("C%04d", seq_len(n)),
    signup_date = as.Date("2025-01-01") + sample(0:240, n, replace = TRUE),
    region = sample(c("North", "South", "East", "West"), n, replace = TRUE, prob = c(0.28, 0.26, 0.24, 0.22)),
    segment = sample(c("Starter", "Growth", "Enterprise"), n, replace = TRUE, prob = c(0.62, 0.28, 0.10)),
    age = round(stats::rnorm(n, 38, 11)),
    income = round(stats::rlnorm(n, log(62000), 0.35)),
    active = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.76, 0.24)),
    churn_target = sample(c("no", "yes"), n, replace = TRUE, prob = c(0.83, 0.17)),
    stringsAsFactors = FALSE
  )

  customers_new <- customers_old
  customers_new$income <- round(customers_new$income * stats::runif(n, 1.02, 1.22))
  customers_new$region[sample(seq_len(n), 18)] <- "International"
  customers_new$age[sample(seq_len(n), 28)] <- NA
  customers_new$loyalty_score <- round(stats::runif(n, 0, 100), 1)
  customers_new$active <- sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.69, 0.31))

  list(customers_old = customers_old, customers_new = customers_new)
}
