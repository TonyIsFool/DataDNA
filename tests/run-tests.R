library(DataDNA)

demo <- dna_example_customers()
stopifnot(is.data.frame(customers_old))
stopifnot(is.data.frame(customers_new))

dna <- data_dna(demo$customers_new, name = "customers_new")
stopifnot(inherits(dna, "data_dna"))
stopifnot(grepl("^DNA-", dna$dna_id))
stopifnot(dna$n_rows == nrow(demo$customers_new))
stopifnot(any(dna$columns$role == "id"))
stopifnot(dna_species(demo$customers_new) %in% c("customer_table", "unknown_table"))

shuffled <- demo$customers_new[sample(seq_len(nrow(demo$customers_new))), ]
same <- dna_compare(demo$customers_new, shuffled)
stopifnot(same$overall > 0.95)

changed <- dna_compare(demo$customers_old, demo$customers_new)
stopifnot(changed$overall < 1)
stopifnot(changed$overall > 0.5)

mutations <- dna_diff(demo$customers_old, demo$customers_new)
stopifnot(inherits(mutations, "dna_diff"))
stopifnot(any(grepl("loyalty_score", mutations$changes$message)))
stopifnot(any(mutations$changes$type %in% c("missingness", "distribution", "category")))

tmp <- tempfile(fileext = ".html")
invisible(dna_card(dna, file = tmp))
html <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
stopifnot(file.exists(tmp))
stopifnot(grepl("DataDNA specimen card", html))
stopifnot(grepl(dna$dna_id, html, fixed = TRUE))
stopifnot(grepl("fingerprint-chip", html))
stopifnot(grepl("legend", html))

set.seed(101)
n <- 500
customer_base <- data.frame(
  customer_id = sprintf("C%05d", seq_len(n)),
  signup_date = as.Date("2025-01-01") + sample(0:365, n, TRUE),
  region = sample(c("North", "South", "East", "West"), n, TRUE, c(.30, .25, .25, .20)),
  plan = sample(c("Free", "Pro", "Team", "Enterprise"), n, TRUE, c(.52, .28, .15, .05)),
  age = round(rnorm(n, 39, 10)),
  spend = round(rlnorm(n, log(900), .55), 2),
  visits = rpois(n, 12),
  churn_target = sample(c("no", "yes"), n, TRUE, c(.82, .18)),
  stringsAsFactors = FALSE
)
customer_mutated <- customer_base
customer_mutated$spend <- round(customer_mutated$spend * runif(n, 1.12, 1.45), 2)
customer_mutated$region[sample(seq_len(n), 70)] <- "International"
customer_mutated$age[sample(seq_len(n), 90)] <- NA
customer_mutated$visits <- customer_mutated$visits + rpois(n, 4)
customer_mutated$loyalty_score <- round(runif(n, 0, 100), 1)

sensor_stream <- data.frame(
  sensor_id = sprintf("S%03d", sample(1:90, n, TRUE)),
  timestamp = as.POSIXct("2026-01-01", tz = "UTC") + sort(sample(1:900000, n)),
  temperature = rnorm(n, 22, 3),
  vibration = rgamma(n, 2, .8),
  pressure = rnorm(n, 101, 2),
  fault_flag = sample(c(TRUE, FALSE), n, TRUE, c(.04, .96)),
  batch = sample(LETTERS[1:12], n, TRUE),
  stringsAsFactors = FALSE
)

base_vs_mutated <- dna_compare(customer_base, customer_mutated)
base_vs_sensor <- dna_compare(customer_base, sensor_stream)
stress_diff <- dna_diff(customer_base, customer_mutated)

stopifnot(dna_species(customer_base) == "customer_table")
stopifnot(dna_species(customer_mutated) == "customer_table")
stopifnot(dna_species(sensor_stream) == "event_stream")
stopifnot(base_vs_mutated$overall > 0.8)
stopifnot(base_vs_mutated$likely_relation == "same dataset, changed version")
stopifnot(all(c("shape", "species", "role_structure") %in% names(base_vs_mutated$scores)))
stopifnot(base_vs_mutated$scores[["species"]] == 1)
stopifnot(length(base_vs_mutated$top_mutations) >= 1)
stopifnot(base_vs_sensor$overall < 0.25)
stopifnot(base_vs_sensor$likely_relation == "different dataset")
stopifnot(base_vs_sensor$scores[["schema"]] == 0)
stopifnot(base_vs_sensor$scores[["species"]] == 0)
stopifnot(any(grepl("loyalty_score", stress_diff$changes$message)))
stopifnot(any(grepl("spend", stress_diff$changes$message)))
stopifnot(any(grepl("visits", stress_diff$changes$message)))

lineage_library <- list(
  customer_base = customer_base,
  customer_mutated = customer_mutated,
  sensor_stream = sensor_stream
)
lineage <- dna_match(customer_mutated, lineage_library, top_n = 10)
stopifnot(inherits(lineage, "dna_match"))
stopifnot(nrow(lineage$matches) == 3)
stopifnot(lineage$matches$name[[1L]] == "customer_base")
stopifnot(lineage$matches$overall[[1L]] > lineage$matches$overall[lineage$matches$name == "sensor_stream"])
stopifnot(lineage$matches$relation[[1L]] %in% c("same dataset, changed version", "same dataset, near-identical version"))

unnamed_lineage <- dna_match(customer_mutated, list(customer_base, sensor_stream), top_n = 10)
stopifnot(all(grepl("^candidate_", unnamed_lineage$matches$name)))
stopifnot(nrow(unnamed_lineage$matches) == 2)

too_many <- dna_match(customer_mutated, list(base = customer_base), top_n = 99)
stopifnot(nrow(too_many$matches) == 1)
stopifnot(too_many$top_n == 1)

empty_error <- tryCatch(
  dna_match(customer_mutated, list()),
  error = function(e) conditionMessage(e)
)
stopifnot(grepl("at least one", empty_error))

lineage_html_file <- tempfile(fileext = ".html")
invisible(dna_match_card(lineage, file = lineage_html_file))
lineage_html <- paste(readLines(lineage_html_file, warn = FALSE), collapse = "\n")
stopifnot(file.exists(lineage_html_file))
stopifnot(grepl("<svg", lineage_html, fixed = TRUE))
stopifnot(grepl(lineage$query$dna_id, lineage_html, fixed = TRUE))
stopifnot(grepl("customer_base", lineage_html, fixed = TRUE))
stopifnot(grepl("Similarity", lineage_html, fixed = TRUE))
stopifnot(grepl("lineage-report", lineage_html, fixed = TRUE))
stopifnot(grepl("lineage-rank", lineage_html, fixed = TRUE))
stopifnot(grepl("lineage-summary", lineage_html, fixed = TRUE))

lineage_png_file <- tempfile(fileext = ".png")
invisible(dna_match_plot(lineage, file = lineage_png_file, width = 9, height = 6))
stopifnot(file.exists(lineage_png_file))
stopifnot(file.info(lineage_png_file)$size > 1000)

lineage_pdf_file <- tempfile(fileext = ".pdf")
invisible(dna_match_plot(lineage, file = lineage_pdf_file, width = 9, height = 6))
stopifnot(file.exists(lineage_pdf_file))
stopifnot(file.info(lineage_pdf_file)$size > 1000)

lineage_large_png_file <- tempfile(fileext = ".png")
invisible(dna_match_plot(lineage, file = lineage_large_png_file, width = 11, height = 7))
stopifnot(file.exists(lineage_large_png_file))
stopifnot(file.info(lineage_large_png_file)$size > 1000)

long_name_lineage <- dna_match(
  customer_mutated,
  list(
    customer_base_2024_archive_version_with_extra_long_name = customer_base,
    sensor_stream_operational_reference_with_extra_long_name = sensor_stream
  ),
  top_n = 2
)
long_name_png_file <- tempfile(fileext = ".png")
invisible(dna_match_plot(long_name_lineage, file = long_name_png_file, width = 9, height = 6))
stopifnot(file.exists(long_name_png_file))
stopifnot(file.info(long_name_png_file)$size > 1000)

single_match <- dna_match(customer_mutated, list(customer_base = customer_base), top_n = 1)
single_match_png_file <- tempfile(fileext = ".png")
invisible(dna_match_plot(single_match, file = single_match_png_file, width = 9, height = 6))
stopifnot(file.exists(single_match_png_file))
stopifnot(file.info(single_match_png_file)$size > 1000)
