#' Compare two data DNA profiles
#'
#' Computes a multi-signal fingerprint similarity between two data frames or
#' `data_dna` profiles, combining schema overlap (including likely column
#' renames), table shape, species, role structure, numeric distributions,
#' missingness, categories, identity uniqueness, and temporal coverage.
#'
#' @param x A data frame or `data_dna` object.
#' @param y A data frame or `data_dna` object.
#'
#' @return A `dna_comparison` object.
#' @export
#'
#' @examples
#' demo <- dna_example_customers()
#' dna_compare(demo$customers_old, demo$customers_new)
dna_compare <- function(x, y) {
  x_name <- paste(deparse(substitute(x)), collapse = " ")
  y_name <- paste(deparse(substitute(y)), collapse = " ")
  x <- as_data_dna(x, name = x_name)
  y <- as_data_dna(y, name = y_name)

  renames <- detect_renamed_columns(x, y)
  pairs <- aligned_column_pairs(x, y, renames = renames)

  scores <- c(
    schema = schema_similarity(x, y, renames = renames),
    shape = shape_similarity(x, y),
    species = species_similarity(x, y),
    role_structure = role_structure_similarity(x, y),
    distribution = distribution_similarity(x, y, pairs = pairs),
    missingness = missingness_similarity(x, y, pairs = pairs),
    category = category_similarity(x, y, pairs = pairs),
    identity = identity_similarity(x, y, pairs = pairs),
    temporal = temporal_similarity(x, y, pairs = pairs)
  )
  weights <- c(
    schema = 0.21,
    shape = 0.08,
    species = 0.10,
    role_structure = 0.14,
    distribution = 0.15,
    missingness = 0.11,
    category = 0.08,
    identity = 0.08,
    temporal = 0.05
  )
  overall <- weighted_score(scores, weights[names(scores)])
  if (!is.finite(overall)) {
    overall <- 0
  }
  relation <- if (overall >= 0.97) {
    "same dataset, near-identical version"
  } else if (overall >= 0.85) {
    "same dataset, changed version"
  } else if (overall >= 0.65) {
    "related dataset"
  } else {
    "different dataset"
  }

  structure(
    list(
      x_name = x$name,
      y_name = y$name,
      overall = overall,
      scores = scores,
      renames = renames,
      likely_relation = relation,
      top_mutations = top_mutation_messages(x, y, renames = renames)
    ),
    class = "dna_comparison"
  )
}

#' @export
print.dna_comparison <- function(x, ...) {
  cat("DataDNA comparison\n")
  cat("  ", x$x_name, " vs ", x$y_name, "\n", sep = "")
  cat("  Overall similarity: ", round(x$overall, 3), "\n", sep = "")
  cat("  Likely relation: ", x$likely_relation, "\n", sep = "")
  for (nm in names(x$scores)) {
    val <- x$scores[[nm]]
    if (is.finite(val)) {
      cat("  ", nm, ": ", round(val, 3), "\n", sep = "")
    }
  }
  if (!is.null(x$renames) && nrow(x$renames)) {
    rename_txt <- paste0(x$renames$old_name, " -> ", x$renames$new_name)
    cat("  Likely renames: ", paste(utils::head(rename_txt, 4L), collapse = ", "), "\n", sep = "")
  }
  if (length(x$top_mutations)) {
    cat("  Top mutations: ", paste(x$top_mutations, collapse = " | "), "\n", sep = "")
  }
  invisible(x)
}

aligned_column_pairs <- function(x, y, renames = NULL) {
  common <- intersect(x$columns$name, y$columns$name)
  old_nm <- common
  new_nm <- common
  if (!is.null(renames) && nrow(renames)) {
    old_nm <- c(old_nm, renames$old_name)
    new_nm <- c(new_nm, renames$new_name)
  }
  data.frame(old_name = old_nm, new_name = new_nm, stringsAsFactors = FALSE)
}

schema_similarity <- function(x, y, renames = NULL) {
  if (!x$n_cols && !y$n_cols) {
    return(1)
  }
  if (!x$n_cols || !y$n_cols) {
    return(0)
  }
  sx <- paste(x$columns$name, x$columns$type, sep = ":")
  sy <- paste(y$columns$name, y$columns$type, sep = ":")
  exact_inter <- length(intersect(sx, sy))
  rename_credit <- if (!is.null(renames) && nrow(renames)) 0.65 * sum(renames$similarity) else 0
  rename_count <- if (!is.null(renames)) nrow(renames) else 0L
  denom <- max(1L, length(union(sx, sy)) - rename_count)
  clip01((exact_inter + rename_credit) / denom)
}

shape_similarity <- function(x, y) {
  row_sim <- exp(-abs(log1p(x$n_rows) - log1p(y$n_rows)))
  col_sim <- exp(-abs(log1p(x$n_cols) - log1p(y$n_cols)))
  weighted_score(c(row_sim, col_sim), c(0.6, 0.4))
}

species_similarity <- function(x, y) {
  if (identical(x$species, y$species)) {
    return(1)
  }
  groups <- list(
    entity = c("customer_table", "lookup_table", "survey_data"),
    event = c("transaction_log", "event_stream", "time_series"),
    matrix = c("wide_feature_matrix"),
    empty = c("empty_table"),
    unknown = c("unknown_table")
  )
  same_group <- any(vapply(groups, function(g) x$species %in% g && y$species %in% g, logical(1)))
  if (same_group) 0.55 else 0
}

role_structure_similarity <- function(x, y) {
  if (!x$n_cols && !y$n_cols) {
    return(1)
  }
  if (!x$n_cols || !y$n_cols) {
    return(0)
  }
  role_sim <- distribution_overlap(x$columns$role, y$columns$role)
  type_sim <- distribution_overlap(x$columns$type, y$columns$type)
  weighted_score(c(role_sim, type_sim), c(0.7, 0.3))
}

missingness_similarity <- function(x, y, pairs = aligned_column_pairs(x, y)) {
  if (!nrow(pairs)) return(0)
  a <- x$columns[match(pairs$old_name, x$columns$name), , drop = FALSE]
  b <- y$columns[match(pairs$new_name, y$columns$name), , drop = FALSE]
  diffs <- abs(a$missing_rate - b$missing_rate)
  diffs <- diffs[is.finite(diffs)]
  if (!length(diffs)) return(NA_real_)
  clip01(1 - mean(diffs))
}

identity_similarity <- function(x, y, pairs = aligned_column_pairs(x, y)) {
  if (!nrow(pairs)) return(0)
  a <- x$columns[match(pairs$old_name, x$columns$name), , drop = FALSE]
  b <- y$columns[match(pairs$new_name, y$columns$name), , drop = FALSE]
  role_sim <- mean(a$role == b$role)
  uniq_diffs <- abs(a$unique_rate - b$unique_rate)
  uniq_diffs <- uniq_diffs[is.finite(uniq_diffs)]
  unique_sim <- if (length(uniq_diffs)) clip01(1 - mean(uniq_diffs)) else NA_real_
  weighted_score(c(role_sim, unique_sim), c(0.55, 0.45))
}

distribution_similarity <- function(x, y, pairs = aligned_column_pairs(x, y)) {
  if (!nrow(pairs)) return(0)
  a <- x$columns[match(pairs$old_name, x$columns$name), , drop = FALSE]
  b <- y$columns[match(pairs$new_name, y$columns$name), , drop = FALSE]
  keep <- a$type == "numeric" & b$type == "numeric"
  if (!any(keep)) return(NA_real_)
  shifts <- vapply(which(keep), function(i) numeric_shift(a[i, , drop = FALSE], b[i, , drop = FALSE]), numeric(1))
  shifts <- shifts[is.finite(shifts)]
  if (!length(shifts)) return(NA_real_)
  clip01(1 - mean(shifts))
}

category_similarity <- function(x, y, pairs = aligned_column_pairs(x, y)) {
  if (!nrow(pairs)) return(0)
  a <- x$columns[match(pairs$old_name, x$columns$name), , drop = FALSE]
  b <- y$columns[match(pairs$new_name, y$columns$name), , drop = FALSE]
  keep <- a$type %in% c("character", "factor", "logical") & b$type %in% c("character", "factor", "logical")
  if (!any(keep)) return(NA_real_)
  sims <- vapply(which(keep), function(i) jaccard(a$top_values[[i]], b$top_values[[i]]), numeric(1))
  mean(sims)
}

parse_dna_time <- function(val) {
  if (length(val) != 1L || is.na(val) || !nzchar(val)) {
    return(NA_real_)
  }
  out <- suppressWarnings(as.numeric(as.POSIXct(val, tz = "UTC")))
  if (length(out) == 1L && is.finite(out)) out else NA_real_
}

temporal_col_similarity <- function(a, b) {
  a_min <- parse_dna_time(a$date_min[[1L]])
  a_max <- parse_dna_time(a$date_max[[1L]])
  b_min <- parse_dna_time(b$date_min[[1L]])
  b_max <- parse_dna_time(b$date_max[[1L]])
  if (!all(is.finite(c(a_min, a_max, b_min, b_max)))) {
    return(NA_real_)
  }
  span <- max(a_max - a_min, b_max - b_min, 86400)
  shift <- (abs(b_min - a_min) + abs(b_max - a_max)) / (2 * span)
  window_sim <- exp(-shift)
  cv_sim <- if (is.finite(a$interval_cv[[1L]]) && is.finite(b$interval_cv[[1L]])) {
    exp(-abs(a$interval_cv[[1L]] - b$interval_cv[[1L]]))
  } else {
    1
  }
  0.75 * window_sim + 0.25 * cv_sim
}

temporal_similarity <- function(x, y, pairs = aligned_column_pairs(x, y)) {
  if (!nrow(pairs)) return(NA_real_)
  a <- x$columns[match(pairs$old_name, x$columns$name), , drop = FALSE]
  b <- y$columns[match(pairs$new_name, y$columns$name), , drop = FALSE]
  keep <- a$type == "date" & b$type == "date"
  if (!any(keep)) return(NA_real_)
  sims <- vapply(which(keep), function(i) temporal_col_similarity(a[i, , drop = FALSE], b[i, , drop = FALSE]), numeric(1))
  sims <- sims[is.finite(sims)]
  if (!length(sims)) return(NA_real_)
  clip01(mean(sims))
}

distribution_overlap <- function(x, y) {
  if (!length(x) && !length(y)) return(1)
  if (!length(x) || !length(y)) return(0)
  tx <- table(x) / length(x)
  ty <- table(y) / length(y)
  keys <- union(names(tx), names(ty))
  px <- as.numeric(tx[keys])
  py <- as.numeric(ty[keys])
  px[is.na(px)] <- 0
  py[is.na(py)] <- 0
  clip01(1 - sum(abs(px - py)) / 2)
}

top_mutation_messages <- function(x, y, n = 3L, renames = NULL) {
  changes <- collect_mutations(x, y, renames = renames)
  if (!nrow(changes)) {
    return(character())
  }
  priority <- match(changes$severity, c("high", "medium", "low"), nomatch = 4L)
  changes <- changes[order(priority), , drop = FALSE]
  utils::head(changes$message, n)
}
