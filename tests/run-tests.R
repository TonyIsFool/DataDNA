library(DataDNA)

# 1. RNG preservation check for dna_example_customers()
set.seed(2026)
expected_next <- runif(1)
set.seed(2026)
demo <- dna_example_customers()
actual_next <- runif(1)
stopifnot(identical(expected_next, actual_next))

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

# 2. Zero-dependency HTML specimen card
tmp <- tempfile(fileext = ".html")
invisible(dna_card(dna, file = tmp))
html <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
stopifnot(file.exists(tmp))
stopifnot(grepl("DataDNA specimen card", html))
stopifnot(grepl(dna$dna_id, html, fixed = TRUE))
stopifnot(grepl("fingerprint-chip", html))
stopifnot(grepl("legend", html))
stopifnot(grepl("^<!DOCTYPE html>", as.character(dna_card(dna))))

# 3. Edge cases: 0-row, 0-col, 1-row, and all-NA data frames
dna_zero_rows <- data_dna(customers_old[0, ])
stopifnot(dna_zero_rows$n_rows == 0L)
stopifnot(dna_zero_rows$n_cols == ncol(customers_old))
stopifnot(is.finite(dna_zero_rows$scores$identity_strength))
stopifnot(any(grepl("0 rows", dna_zero_rows$warnings)))

dna_zero_cols <- data_dna(data.frame())
stopifnot(dna_zero_cols$n_rows == 0L)
stopifnot(dna_zero_cols$n_cols == 0L)
stopifnot(dna_zero_cols$species == "empty_table")
stopifnot(is.finite(dna_zero_cols$scores$identity_strength))

all_na_df <- data.frame(
  a = c(NA_real_, NA_real_, NA_real_),
  b = c(NA_character_, NA_character_, NA_character_),
  d = as.Date(c(NA, NA, NA)),
  stringsAsFactors = FALSE
)
dna_all_na <- data_dna(all_na_df)
stopifnot(is.finite(dna_all_na$scores$identity_strength))
stopifnot(dna_all_na$scores$drift_risk == "high")
stopifnot(is.finite(dna_compare(dna_zero_cols, dna_all_na)$overall))
stopifnot(inherits(dna_diff(dna_zero_rows, dna), "dna_diff"))
invisible(dna_card(dna_zero_cols, file = tempfile(fileext = ".html")))
invisible(dna_plot(dna_zero_cols, file = tempfile(fileext = ".png")))

# 4. Role inference accuracy: low-cardinality *_code vs true keys
role_test_df <- data.frame(
  row_seq = seq_len(60),
  country_code = rep(c("US", "AU", "GB"), 20),
  cust_no = sprintf("CUST-%04d", seq_len(60)),
  churn_rate = runif(60, 0.02, 0.08),
  stringsAsFactors = FALSE
)
role_dna <- data_dna(role_test_df)
roles_map <- setNames(role_dna$columns$role, role_dna$columns$name)
stopifnot(roles_map[["country_code"]] == "category")
stopifnot(roles_map[["cust_no"]] == "id")
stopifnot(roles_map[["row_seq"]] == "id")
stopifnot(roles_map[["churn_rate"]] == "measure")
stopifnot(role_dna$species == "customer_table")

# 5. Rename detection, temporal drift, role transitions, and small-scale numeric shift
role_test_v2 <- role_test_df
names(role_test_v2)[names(role_test_v2) == "cust_no"] <- "customer_number"
role_test_v2$churn_rate <- runif(60, 0.30, 0.48)
role_test_v2$signup_dt <- as.Date("2025-01-01") + seq_len(60)
role_test_v1_dated <- role_test_df
role_test_v1_dated$signup_dt <- as.Date("2024-01-01") + seq_len(60)

rename_diff <- dna_diff(role_test_v1_dated, role_test_v2)
rename_comp <- dna_compare(role_test_v1_dated, role_test_v2)
stopifnot(nrow(rename_diff$renames) >= 1L)
stopifnot(any(rename_diff$renames$old_name == "cust_no" & rename_diff$renames$new_name == "customer_number"))
stopifnot(any(rename_diff$changes$type == "rename"))
stopifnot(any(rename_diff$changes$type == "temporal"))
stopifnot(any(rename_diff$changes$type == "distribution" & grepl("churn_rate", rename_diff$changes$message)))
stopifnot(is.finite(rename_comp$scores[["temporal"]]))

# 6. Stress comparison & lineage tests
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
stopifnot(all(c("shape", "species", "role_structure", "temporal") %in% names(base_vs_mutated$scores)))
stopifnot(base_vs_mutated$scores[["species"]] == 1)
stopifnot(length(base_vs_mutated$top_mutations) >= 1)
stopifnot(base_vs_sensor$overall < 0.25)
stopifnot(base_vs_sensor$likely_relation == "different dataset")
stopifnot(base_vs_sensor$scores[["schema"]] == 0)
stopifnot(base_vs_sensor$scores[["species"]] == 0)
stopifnot(any(grepl("loyalty_score", stress_diff$changes$message)))
stopifnot(any(grepl("spend", stress_diff$changes$message)))
stopifnot(any(grepl("visits", stress_diff$changes$message)))

lineage_library <- dna_library(list(
  customer_base = customer_base,
  customer_mutated = customer_mutated,
  sensor_stream = sensor_stream
))
stopifnot(inherits(lineage_library, "dna_library"))
stopifnot(nrow(as.data.frame(lineage_library)) == 3L)

lineage <- dna_match(customer_mutated, lineage_library, top_n = 10)
stopifnot(inherits(lineage, "dna_match"))
stopifnot(nrow(lineage$matches) == 3)
stopifnot(lineage$matches$name[[1L]] == "customer_base")
stopifnot(lineage$matches$overall[[1L]] > lineage$matches$overall[lineage$matches$name == "sensor_stream"])
stopifnot(lineage$matches$relation[[1L]] %in% c("same dataset, changed version", "same dataset, near-identical version"))

# Self-only library wording check
self_only <- dna_match(customer_base, list(customer_base = customer_base))
self_html <- as.character(dna_match_card(self_only))
stopifnot(!grepl("nearest non-self relative", self_html, fixed = TRUE))

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

# 7. Directory-based dna_library() and RDS persistence
lib_dir <- tempfile("dna_lib_dir_")
dir.create(lib_dir)
write.csv(demo$customers_old, file.path(lib_dir, "customers_2024.csv"), row.names = FALSE)
saveRDS(demo$customers_new, file.path(lib_dir, "customers_2025.rds"))
lib_from_dir <- dna_library(lib_dir)
stopifnot(length(lib_from_dir) == 2L)
lib_rds <- tempfile(fileext = ".rds")
invisible(dna_library(lib_from_dir, file = lib_rds))
reloaded_lib <- dna_library(lib_rds)
stopifnot(inherits(reloaded_lib, "dna_library"))
stopifnot(length(reloaded_lib) == 2L)
unlink(c(lib_dir, lib_rds), recursive = TRUE)

# 8. HTML match card, static figures (PNG / PDF / SVG), 8-candidate layout, and S3 generics
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

for (ext in c(".png", ".pdf", ".svg")) {
  f_match <- tempfile(fileext = ext)
  f_spec <- tempfile(fileext = ext)
  f_diff <- tempfile(fileext = ext)
  invisible(dna_match_plot(lineage, file = f_match, width = 9, height = 6))
  invisible(dna_plot(dna, file = f_spec, width = 9.5, height = 6.2))
  invisible(dna_diff_plot(demo$customers_old, demo$customers_new, file = f_diff, width = 9.5, height = 6.2))
  stopifnot(file.exists(f_match), file.info(f_match)$size > 500)
  stopifnot(file.exists(f_spec), file.info(f_spec)$size > 500)
  stopifnot(file.exists(f_diff), file.info(f_diff)$size > 500)
  unlink(c(f_match, f_spec, f_diff))
}

many_lib <- stats::setNames(
  lapply(seq_len(8L), function(i) {
    df <- customer_base
    df$spend <- df$spend * (1 + 0.04 * i)
    df
  }),
  paste0("snapshot_v", seq_len(8L))
)
many_match <- dna_match(customer_mutated, many_lib, top_n = 8L)
many_png <- tempfile(fileext = ".png")
invisible(plot(many_match, file = many_png, width = 10, height = 6.5))
stopifnot(file.exists(many_png), file.info(many_png)$size > 1000)
unlink(many_png)

# S3 generics check
sum_dna <- summary(dna)
stopifnot(inherits(sum_dna, "summary.data_dna"))
stopifnot(is.data.frame(as.data.frame(dna)))
stopifnot(!is.list(as.data.frame(dna)$top_values))
stopifnot(is.data.frame(as.data.frame(changed)))
stopifnot(is.data.frame(as.data.frame(mutations)))
stopifnot(is.data.frame(as.data.frame(lineage)))
stopifnot(!is.list(as.data.frame(lineage)$top_mutations))

s3_diff_png <- tempfile(fileext = ".png")
invisible(plot(data_dna(demo$customers_old), data_dna(demo$customers_new), file = s3_diff_png))
stopifnot(file.exists(s3_diff_png), file.info(s3_diff_png)$size > 1000)
unlink(s3_diff_png)
