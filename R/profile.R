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
  if (!is.numeric(sample_size) || length(sample_size) != 1L || is.na(sample_size) || sample_size < 1L) {
    stop("`sample_size` must be a single positive integer.", call. = FALSE)
  }

  raw_name <- if (is.null(name)) paste(deparse(substitute(df)), collapse = " ") else as.character(name[[1L]])
  n_rows <- nrow(df)
  n_cols <- ncol(df)
  idx <- sample_index(n_rows, sample_size)
  sample_df <- df[idx, , drop = FALSE]
  sample_rows <- length(idx)
  profiles <- lapply(names(sample_df), function(nm) column_profile(sample_df[[nm]], nm, n_rows = sample_rows))
  columns <- profiles_to_frame(profiles)
  species <- dna_species_from_profile(columns, n_rows, n_cols)
  scores <- dna_scores(columns, n_rows, n_cols)
  fingerprints <- make_fingerprints(columns, n_rows, n_cols)
  dna_id <- compact_id(fingerprints$schema_hash, fingerprints$shape_hash, fingerprints$profile_hash)
  signals <- dna_signals(columns, n_rows, n_cols, species)
  warnings <- dna_warnings(columns, n_rows, n_cols)

  structure(
    list(
      name = raw_name,
      dna_id = dna_id,
      species = species,
      n_rows = n_rows,
      n_cols = n_cols,
      sample_size = sample_rows,
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
#' @param df A data frame or `data_dna` object.
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
  data_dna(df)$species
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
    cat("  Signals: ", paste(utils::head(x$signals, 3L), collapse = " | "), "\n", sep = "")
  }
  if (length(x$warnings)) {
    cat("  Warnings: ", paste(utils::head(x$warnings, 3L), collapse = " | "), "\n", sep = "")
  }
  invisible(x)
}

sample_index <- function(n_rows, sample_size) {
  if (n_rows <= 0L) {
    return(integer())
  }
  size <- min(n_rows, as.integer(sample_size))
  unique(as.integer(round(seq(1, n_rows, length.out = size))))
}

column_profile <- function(x, name, n_rows) {
  type <- column_type(x)
  missing_rate <- if (length(x)) mean(is.na(x)) else NA_real_
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
    n_unique = as.integer(n_unique),
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
    vals <- vals[is.finite(vals)]
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
    base$outlier_rate <- if (length(vals) && is.finite(iqr) && iqr > 0) mean(vals < lower | vals > upper) else if (length(vals)) 0 else NA_real_
  } else if (type %in% c("character", "factor", "logical")) {
    vals <- as.character(non_missing)
    base$entropy <- category_entropy(vals)
    base$tail_rate <- category_tail_rate(vals)
    base$top_values <- list(top_values(vals))
  } else if (type == "date") {
    if (length(non_missing)) {
      posix_vals <- as.POSIXct(non_missing, tz = "UTC")
      num_vals <- sort(as.numeric(posix_vals[is.finite(as.numeric(posix_vals))]))
      base$date_min <- as.character(min(non_missing))
      base$date_max <- as.character(max(non_missing))
      diffs <- diff(num_vals)
      base$interval_cv <- if (length(diffs) > 1L && mean(diffs) != 0) stats::sd(diffs) / abs(mean(diffs)) else 0
    }
  }

  base
}

empty_columns_frame <- function() {
  df <- data.frame(
    name = character(),
    type = character(),
    role = character(),
    missing_rate = numeric(),
    unique_rate = numeric(),
    n_unique = integer(),
    mean = numeric(),
    sd = numeric(),
    q0 = numeric(),
    q25 = numeric(),
    q50 = numeric(),
    q75 = numeric(),
    q100 = numeric(),
    zero_rate = numeric(),
    outlier_rate = numeric(),
    entropy = numeric(),
    tail_rate = numeric(),
    date_min = character(),
    date_max = character(),
    interval_cv = numeric(),
    stringsAsFactors = FALSE
  )
  df$top_values <- I(list())
  df[, c(
    "name", "type", "role", "missing_rate", "unique_rate", "n_unique",
    "mean", "sd", "q0", "q25", "q50", "q75", "q100", "zero_rate",
    "outlier_rate", "entropy", "tail_rate", "top_values",
    "date_min", "date_max", "interval_cv"
  )]
}

profiles_to_frame <- function(profiles) {
  if (!length(profiles)) {
    return(empty_columns_frame())
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

is_integer_surrogate_key <- function(values, nm) {
  if (!length(values) || length(values) < 20L) {
    return(FALSE)
  }
  if (grepl("amount|price|spend|cost|revenue|income|salary|age|score|rate|pct|ratio|temp|pressure|vibration|weight|height|visits|count|qty|quantity|balance", nm)) {
    return(FALSE)
  }
  num <- as.numeric(values)
  if (!all(is.finite(num)) || !all(num == floor(num)) || any(num < 0)) {
    return(FALSE)
  }
  diffs <- diff(num)
  all(diffs == 1) || all(diffs == -1)
}

infer_role <- function(name, type, missing_rate, unique_rate, n_unique, n_rows, values) {
  nm <- tolower(name)
  if (grepl("(^|_)(target|label|outcome|response)($|_)", nm)) {
    return("target")
  }
  if (grepl("(^|_)(class|churn|status)($|_)", nm) &&
      !grepl("(_rate|_score|_prob|_pct|_ratio|_index|_count)$", nm) &&
      (n_rows == 0L || n_unique <= 12L)) {
    return("target")
  }
  if (type == "date" || grepl("date|time|timestamp|created|updated|period", nm)) {
    return("time")
  }
  if (type == "logical" || grepl("(^is_|^has_|flag$|_flag$)", nm)) {
    return("flag")
  }

  strong_id_name <- grepl("(^id$|_id$|uuid|guid|^row_id$|^record_id$|email)", nm)
  weak_id_name <- grepl("(key$|code$|_no$|_num$|_number$|_seq$)", nm)
  measure_like_name <- grepl("amount|price|spend|cost|revenue|income|salary|age|score|rate|pct|ratio|weight|height|visits|count|qty|quantity", nm)

  if (n_rows == 0L) {
    if (strong_id_name) return("id")
    if (type == "numeric") return("measure")
    if (type %in% c("character", "factor")) return("category")
    return("unknown")
  }

  low_missing <- is.finite(missing_rate) && missing_rate < 0.05
  min_unique_floor <- min(20L, max(2L, floor(0.8 * n_rows)))
  if (n_rows == 1L) {
    min_unique_floor <- 1L
  }

  id_by_strong_name <- strong_id_name && !measure_like_name && low_missing &&
    is.finite(unique_rate) && (unique_rate >= 0.5 || n_rows <= 2L)
  id_by_weak_name <- weak_id_name && !measure_like_name && low_missing &&
    is.finite(unique_rate) && unique_rate >= 0.85 &&
    n_unique >= min(10L, max(1L, floor(0.7 * n_rows)))
  id_by_char_values <- type %in% c("character", "factor") && low_missing &&
    is.finite(unique_rate) && unique_rate > 0.95 &&
    n_unique >= min_unique_floor
  id_by_int_key <- type == "numeric" && low_missing &&
    is.finite(unique_rate) && unique_rate == 1 &&
    is_integer_surrogate_key(values, nm)

  if (id_by_strong_name || id_by_weak_name || id_by_char_values || id_by_int_key) {
    return("id")
  }
  if (type == "numeric") {
    return("measure")
  }
  if (type %in% c("character", "factor")) {
    lengths <- nchar(as.character(values), allowNA = FALSE)
    if (length(lengths) && stats::median(lengths) > 35 && is.finite(unique_rate) && unique_rate > 0.2) {
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
    return("empty_table")
  }
  names_lower <- tolower(columns$name)
  numeric_cols <- sum(columns$type == "numeric")
  category_cols <- sum(columns$role == "category")
  time_cols <- sum(columns$role == "time")
  id_cols <- sum(columns$role == "id")

  if (n_cols >= 50L && numeric_cols / n_cols >= 0.6) {
    "wide_feature_matrix"
  } else if (any(grepl("(^|_)(customer|cust|client|user|usr|account|acct|member|subscriber)(s)?($|_)", names_lower)) &&
             (id_cols >= 1L || any(grepl("(^id$|_id$)", names_lower)))) {
    "customer_table"
  } else if (any(grepl("amount|price|payment|order|invoice|transaction|qty|quantity", names_lower))) {
    "transaction_log"
  } else if (time_cols >= 1L && n_rows / max(n_cols, 1L) > 20 && (id_cols >= 1L || category_cols >= 1L)) {
    "event_stream"
  } else if (time_cols >= 1L && n_rows >= 10L && n_cols <= 8L && numeric_cols >= 1L) {
    "time_series"
  } else if (category_cols / max(n_cols, 1L) >= 0.6 && n_cols >= 5L) {
    "survey_data"
  } else if (id_cols >= 1L && n_rows > 0L && n_rows <= 5000L && category_cols >= numeric_cols) {
    "lookup_table"
  } else {
    "unknown_table"
  }
}

dna_scores <- function(columns, n_rows, n_cols) {
  if (n_cols == 0L) {
    return(list(
      identity_strength = 0,
      stability_value = 0,
      stability = "low",
      drift_risk_value = 1,
      drift_risk = "high"
    ))
  }

  if (n_rows == 0L) {
    id_bonus <- min(0.15, sum(columns$role == "id") * 0.05)
    identity_strength <- 100 * clip01(0.30 + id_bonus + 0.10 * min(1, n_cols / 12))
    risk_value <- 0.65
    stability_value <- 0.35
    return(list(
      identity_strength = identity_strength,
      stability_value = stability_value,
      stability = "low",
      drift_risk_value = risk_value,
      drift_risk = "high"
    ))
  }

  missing_penalty <- safe_mean(columns$missing_rate)
  if (!is.finite(missing_penalty)) {
    missing_penalty <- 1
  }
  unknown_penalty <- safe_prop(columns$role == "unknown")
  id_bonus <- min(0.15, sum(columns$role == "id") * 0.05)
  identity_strength <- 100 * clip01(
    0.72 - 0.45 * missing_penalty - 0.2 * unknown_penalty + id_bonus + 0.13 * min(1, n_cols / 12)
  )
  low_uniq_prop <- safe_prop(!is.finite(columns$unique_rate) | columns$unique_rate < 0.02)
  high_outlier_prop <- safe_prop(columns$outlier_rate > 0.08)
  risk_value <- clip01(0.55 * missing_penalty + 0.25 * low_uniq_prop + 0.2 * high_outlier_prop)
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
  schema <- if (nrow(columns)) paste(columns$name, columns$type, sep = ":") else character()
  shape <- paste(if (n_rows <= 0L) 0L else floor(log10(max(n_rows, 1L))), n_cols, sep = "x")
  profile <- if (nrow(columns)) {
    paste(
      columns$name,
      columns$role,
      round_num(columns$missing_rate, 2),
      round_num(columns$unique_rate, 2),
      round_num(columns$q50, 2),
      round_num(columns$entropy, 2),
      sep = ":"
    )
  } else {
    character()
  }
  list(
    schema_hash = stable_hash(schema),
    shape_hash = stable_hash(shape),
    profile_hash = stable_hash(profile)
  )
}

dna_signals <- function(columns, n_rows, n_cols, species) {
  signals <- c(
    paste0("species=", species),
    paste0(n_cols, " columns profiled"),
    paste0(sum(columns$role == "id"), " likely ID columns"),
    paste0(sum(columns$role == "measure"), " numeric measure columns"),
    paste0(sum(columns$role == "category"), " categorical columns")
  )
  if (nrow(columns)) {
    role_counts <- sort(table(columns$role), decreasing = TRUE)
    if (length(role_counts)) {
      signals <- c(signals, paste0("dominant role=", names(role_counts)[[1L]]))
    }
  }
  signals
}

dna_warnings <- function(columns, n_rows = NULL, n_cols = nrow(columns)) {
  warnings <- character()
  if (!n_cols) {
    return("table has 0 columns")
  }
  if (!is.null(n_rows) && n_rows == 0L) {
    warnings <- c(warnings, "table has 0 rows (schema-only profile)")
    return(warnings)
  }

  high_missing <- columns$name[!is.na(columns$missing_rate) & columns$missing_rate >= 0.3]
  all_missing <- columns$name[!is.na(columns$missing_rate) & columns$missing_rate == 1]
  high_outliers <- columns$name[!is.na(columns$outlier_rate) & columns$outlier_rate >= 0.1]
  high_tail <- columns$name[columns$role == "category" & !is.na(columns$tail_rate) & columns$tail_rate >= 0.6]
  constant_cols <- columns$name[!is.na(columns$n_unique) & columns$n_unique == 1L & !is.na(columns$missing_rate) & columns$missing_rate < 1]
  no_id <- !any(columns$role == "id")

  if (length(all_missing)) warnings <- c(warnings, paste0("100% missing columns: ", paste(utils::head(all_missing, 3L), collapse = ", ")))
  if (length(high_missing)) warnings <- c(warnings, paste0("high missingness: ", paste(utils::head(high_missing, 3L), collapse = ", ")))
  if (length(constant_cols)) warnings <- c(warnings, paste0("constant columns: ", paste(utils::head(constant_cols, 3L), collapse = ", ")))
  if (length(high_outliers)) warnings <- c(warnings, paste0("outlier-heavy measures: ", paste(utils::head(high_outliers, 3L), collapse = ", ")))
  if (length(high_tail)) warnings <- c(warnings, paste0("long-tail categories: ", paste(utils::head(high_tail, 3L), collapse = ", ")))
  if (no_id) warnings <- c(warnings, "no strong ID column detected")
  warnings
}
