#' Create a data DNA profile
#'
#' Profiles an R data frame into a compact identity object that records schema,
#' shape, missingness, distributions, categories, uniqueness, time signals, and
#' stable fingerprints.
#'
#' @param df A data frame.
#' @param name Optional data set name shown on cards and print output.
#' @param sample_size Maximum number of rows used for profiling.
#'
#' @return A `data_dna` object.
#' @export
#'
#' @examples
#' demo <- dna_example_customers()
#' dna <- data_dna(demo$customers_new, name = "customers_new")
#' dna
data_dna <- function(df, name = NULL, sample_size = 10000L) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }
  stopifnot(length(sample_size) == 1L, sample_size >= 1L)

  n_rows <- nrow(df)
  n_cols <- ncol(df)
  idx <- sample_index(n_rows, sample_size)
  sample_df <- df[idx, , drop = FALSE]
  profiles <- lapply(names(sample_df), function(nm) column_profile(sample_df[[nm]], nm, n_rows = n_rows))
  columns <- profiles_to_frame(profiles)
  species <- dna_species_from_profile(columns, n_rows, n_cols)
  scores <- dna_scores(columns, n_rows, n_cols)
  fingerprints <- make_fingerprints(columns, n_rows, n_cols)
  dna_id <- compact_id(fingerprints$schema_hash, fingerprints$shape_hash, fingerprints$profile_hash)
  signals <- dna_signals(columns, n_rows, n_cols, species)
  warnings <- dna_warnings(columns)

  structure(
    list(
      name = if (is.null(name)) deparse(substitute(df)) else as.character(name),
      dna_id = dna_id,
      species = species,
      n_rows = n_rows,
      n_cols = n_cols,
      sample_size = length(idx),
      columns = columns,
      scores = scores,
      fingerprints = fingerprints,
      signals = signals,
      warnings = warnings,
      created = Sys.time()
    ),
    class = "data_dna"
  )
}

#' Guess the species of a data frame
#'
#' @param df A data frame.
#'
#' @return A character label such as `customer_table`, `event_stream`, or
#'   `wide_feature_matrix`.
#' @export
#'
#' @examples
#' dna_species(dna_example_customers()$customers_new)
dna_species <- function(df) {
  if (inherits(df, "data_dna")) {
    return(df$species)
  }
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame or data_dna object.", call. = FALSE)
  }
  profiles <- lapply(names(df), function(nm) column_profile(df[[nm]], nm, n_rows = nrow(df)))
  dna_species_from_profile(profiles_to_frame(profiles), nrow(df), ncol(df))
}

#' @export
print.data_dna <- function(x, ...) {
  cat("DataDNA Card\n")
  cat("  ID: ", x$dna_id, "\n", sep = "")
  cat("  Name: ", x$name, "\n", sep = "")
  cat("  Species: ", x$species, "\n", sep = "")
  cat("  Shape: ", format(x$n_rows, big.mark = ","), " rows x ", x$n_cols, " columns\n", sep = "")
  cat("  Identity strength: ", round(x$scores$identity_strength), " / 100\n", sep = "")
  cat("  Stability: ", x$scores$stability, "\n", sep = "")
  cat("  Drift risk: ", x$scores$drift_risk, "\n", sep = "")
  if (length(x$signals)) {
    cat("  Signals: ", paste(head(x$signals, 3L), collapse = " | "), "\n", sep = "")
  }
  if (length(x$warnings)) {
    cat("  Warnings: ", paste(head(x$warnings, 3L), collapse = " | "), "\n", sep = "")
  }
  invisible(x)
}

sample_index <- function(n_rows, sample_size) {
  if (n_rows == 0L) {
    return(integer())
  }
  size <- min(n_rows, as.integer(sample_size))
  unique(as.integer(round(seq(1, n_rows, length.out = size))))
}

column_profile <- function(x, name, n_rows) {
  type <- column_type(x)
  missing_rate <- mean(is.na(x))
  non_missing <- x[!is.na(x)]
  n_unique <- length(unique(non_missing))
  unique_rate <- if (length(non_missing)) n_unique / length(non_missing) else NA_real_
  role <- infer_role(name, type, missing_rate, unique_rate, n_unique, n_rows, non_missing)

  base <- list(
    name = name,
    type = type,
    role = role,
    missing_rate = missing_rate,
    unique_rate = unique_rate,
    n_unique = n_unique,
    mean = NA_real_,
    sd = NA_real_,
    q0 = NA_real_,
    q25 = NA_real_,
    q50 = NA_real_,
    q75 = NA_real_,
    q100 = NA_real_,
    zero_rate = NA_real_,
    outlier_rate = NA_real_,
    entropy = NA_real_,
    tail_rate = NA_real_,
    top_values = list(character()),
    date_min = NA_character_,
    date_max = NA_character_,
    interval_cv = NA_real_
  )

  if (type == "numeric") {
    vals <- as.numeric(non_missing)
    qs <- safe_quantile(vals)
    iqr <- qs[[4L]] - qs[[2L]]
    lower <- qs[[2L]] - 1.5 * iqr
    upper <- qs[[4L]] + 1.5 * iqr
    base$mean <- safe_mean(vals)
    base$sd <- safe_sd(vals)
    base$q0 <- qs[[1L]]
    base$q25 <- qs[[2L]]
    base$q50 <- qs[[3L]]
    base$q75 <- qs[[4L]]
    base$q100 <- qs[[5L]]
    base$zero_rate <- if (length(vals)) mean(vals == 0) else NA_real_
    base$outlier_rate <- if (length(vals) && iqr > 0) mean(vals < lower | vals > upper) else 0
  } else if (type %in% c("character", "factor", "logical")) {
    vals <- as.character(non_missing)
    base$entropy <- category_entropy(vals)
    base$tail_rate <- category_tail_rate(vals)
    base$top_values <- list(top_values(vals))
  } else if (type == "date") {
    vals <- sort(as.numeric(as.POSIXct(non_missing)))
    base$date_min <- if (length(vals)) as.character(min(non_missing)) else NA_character_
    base$date_max <- if (length(vals)) as.character(max(non_missing)) else NA_character_
    diffs <- diff(vals)
    base$interval_cv <- if (length(diffs) > 1L && mean(diffs) != 0) stats::sd(diffs) / abs(mean(diffs)) else 0
  }

  base
}

profiles_to_frame <- function(profiles) {
  if (!length(profiles)) {
    return(data.frame())
  }
  data.frame(
    name = vapply(profiles, `[[`, character(1), "name"),
    type = vapply(profiles, `[[`, character(1), "type"),
    role = vapply(profiles, `[[`, character(1), "role"),
    missing_rate = vapply(profiles, `[[`, numeric(1), "missing_rate"),
    unique_rate = vapply(profiles, `[[`, numeric(1), "unique_rate"),
    n_unique = vapply(profiles, `[[`, integer(1), "n_unique"),
    mean = vapply(profiles, `[[`, numeric(1), "mean"),
    sd = vapply(profiles, `[[`, numeric(1), "sd"),
    q0 = vapply(profiles, `[[`, numeric(1), "q0"),
    q25 = vapply(profiles, `[[`, numeric(1), "q25"),
    q50 = vapply(profiles, `[[`, numeric(1), "q50"),
    q75 = vapply(profiles, `[[`, numeric(1), "q75"),
    q100 = vapply(profiles, `[[`, numeric(1), "q100"),
    zero_rate = vapply(profiles, `[[`, numeric(1), "zero_rate"),
    outlier_rate = vapply(profiles, `[[`, numeric(1), "outlier_rate"),
    entropy = vapply(profiles, `[[`, numeric(1), "entropy"),
    tail_rate = vapply(profiles, `[[`, numeric(1), "tail_rate"),
    top_values = I(lapply(profiles, `[[`, "top_values")),
    date_min = vapply(profiles, `[[`, character(1), "date_min"),
    date_max = vapply(profiles, `[[`, character(1), "date_max"),
    interval_cv = vapply(profiles, `[[`, numeric(1), "interval_cv"),
    stringsAsFactors = FALSE
  )
}

column_type <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
    "date"
  } else if (is.logical(x)) {
    "logical"
  } else if (is.factor(x)) {
    "factor"
  } else if (is.numeric(x) || is.integer(x)) {
    "numeric"
  } else if (is.character(x)) {
    "character"
  } else {
    "other"
  }
}

infer_role <- function(name, type, missing_rate, unique_rate, n_unique, n_rows, values) {
  nm <- tolower(name)
  if (grepl("(^|_)(target|label|outcome|response|class|churn|status)($|_)", nm)) {
    return("target")
  }
  if (type == "date" || grepl("date|time|timestamp|created|updated|period", nm)) {
    return("time")
  }
  if (type == "logical" || grepl("(^is_|^has_|flag$|_flag$)", nm)) {
    return("flag")
  }
  id_like_name <- grepl("(^id$|_id$|uuid|guid|key$|code$|email)", nm)
  id_like_values <- type %in% c("character", "factor") &&
    is.finite(unique_rate) &&
    unique_rate > 0.95 &&
    n_unique >= min(20L, max(1L, floor(0.8 * n_rows)))
  if (missing_rate < 0.05 && (id_like_name || id_like_values)) {
    return("id")
  }
  if (type == "numeric") {
    return("measure")
  }
  if (type %in% c("character", "factor")) {
    lengths <- nchar(as.character(values), allowNA = FALSE)
    if (length(lengths) && stats::median(lengths) > 35 && unique_rate > 0.2) {
      return("text")
    }
    return("category")
  }
  "unknown"
}

category_entropy <- function(x) {
  if (!length(x)) {
    return(NA_real_)
  }
  p <- table(x) / length(x)
  -sum(p * log(p)) / log(length(p) + 1)
}

category_tail_rate <- function(x) {
  if (!length(x)) {
    return(NA_real_)
  }
  freq <- sort(table(x), decreasing = TRUE)
  if (length(freq) <= 5L) {
    return(0)
  }
  sum(freq[-seq_len(5L)]) / sum(freq)
}

top_values <- function(x, n = 5L) {
  if (!length(x)) {
    return(character())
  }
  names(sort(table(x), decreasing = TRUE))[seq_len(min(n, length(unique(x))))]
}

dna_species_from_profile <- function(columns, n_rows, n_cols) {
  if (!n_cols) {
    return("unknown_table")
  }
  names_lower <- tolower(columns$name)
  roles <- columns$role
  numeric_cols <- sum(columns$type == "numeric")
  category_cols <- sum(columns$role == "category")
  time_cols <- sum(columns$role == "time")
  id_cols <- sum(columns$role == "id")

  if (n_cols >= 50L && numeric_cols / n_cols >= 0.6) {
    "wide_feature_matrix"
  } else if (any(grepl("customer|client|user|account", names_lower)) && id_cols >= 1L) {
    "customer_table"
  } else if (any(grepl("amount|price|payment|order|invoice|transaction|qty|quantity", names_lower))) {
    "transaction_log"
  } else if (time_cols >= 1L && n_rows / max(n_cols, 1L) > 20 && (id_cols >= 1L || category_cols >= 1L)) {
    "event_stream"
  } else if (time_cols >= 1L && n_rows >= 10L && n_cols <= 8L && numeric_cols >= 1L) {
    "time_series"
  } else if (category_cols / max(n_cols, 1L) >= 0.6 && n_cols >= 5L) {
    "survey_data"
  } else if (id_cols >= 1L && n_rows <= 5000L && category_cols >= numeric_cols) {
    "lookup_table"
  } else {
    "unknown_table"
  }
}

dna_scores <- function(columns, n_rows, n_cols) {
  missing_penalty <- safe_mean(columns$missing_rate)
  unknown_penalty <- mean(columns$role == "unknown")
  id_bonus <- min(0.15, sum(columns$role == "id") * 0.05)
  identity_strength <- 100 * clip01(0.72 - 0.45 * missing_penalty - 0.2 * unknown_penalty + id_bonus + 0.13 * min(1, n_cols / 12))
  risk_value <- clip01(0.55 * missing_penalty + 0.25 * mean(columns$unique_rate < 0.02, na.rm = TRUE) + 0.2 * mean(columns$outlier_rate > 0.08, na.rm = TRUE))
  stability_value <- clip01(1 - risk_value)
  list(
    identity_strength = identity_strength,
    stability_value = stability_value,
    stability = if (stability_value >= 0.8) "high" else if (stability_value >= 0.55) "medium" else "low",
    drift_risk_value = risk_value,
    drift_risk = if (risk_value >= 0.65) "high" else if (risk_value >= 0.35) "medium" else "low"
  )
}

make_fingerprints <- function(columns, n_rows, n_cols) {
  schema <- paste(columns$name, columns$type, sep = ":")
  shape <- paste(floor(log10(max(n_rows, 1L))), n_cols, sep = "x")
  profile <- paste(
    columns$name,
    columns$role,
    round_num(columns$missing_rate, 2),
    round_num(columns$unique_rate, 2),
    round_num(columns$q50, 2),
    round_num(columns$entropy, 2),
    sep = ":"
  )
  list(
    schema_hash = stable_hash(schema),
    shape_hash = stable_hash(shape),
    profile_hash = stable_hash(profile)
  )
}

dna_signals <- function(columns, n_rows, n_cols, species) {
  role_counts <- sort(table(columns$role), decreasing = TRUE)
  signals <- c(
    paste0("species=", species),
    paste0(n_cols, " columns profiled"),
    paste0(sum(columns$role == "id"), " likely ID columns"),
    paste0(sum(columns$role == "measure"), " numeric measure columns"),
    paste0(sum(columns$role == "category"), " categorical columns")
  )
  if (length(role_counts)) {
    signals <- c(signals, paste0("dominant role=", names(role_counts)[[1L]]))
  }
  signals
}

dna_warnings <- function(columns) {
  warnings <- character()
  high_missing <- columns$name[columns$missing_rate >= 0.3]
  high_outliers <- columns$name[columns$outlier_rate >= 0.1 & !is.na(columns$outlier_rate)]
  high_tail <- columns$name[columns$role == "category" & columns$tail_rate >= 0.6 & !is.na(columns$tail_rate)]
  no_id <- !any(columns$role == "id")

  if (length(high_missing)) warnings <- c(warnings, paste0("high missingness: ", paste(head(high_missing, 3L), collapse = ", ")))
  if (length(high_outliers)) warnings <- c(warnings, paste0("outlier-heavy measures: ", paste(head(high_outliers, 3L), collapse = ", ")))
  if (length(high_tail)) warnings <- c(warnings, paste0("long-tail categories: ", paste(head(high_tail, 3L), collapse = ", ")))
  if (no_id) warnings <- c(warnings, "no strong ID column detected")
  warnings
}
