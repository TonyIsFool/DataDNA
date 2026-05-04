#' Compare two data DNA profiles
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
  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))
  x <- as_data_dna(x, name = x_name)
  y <- as_data_dna(y, name = y_name)
  scores <- c(
    schema = schema_similarity(x, y),
    shape = shape_similarity(x, y),
    species = species_similarity(x, y),
    role_structure = role_structure_similarity(x, y),
    distribution = distribution_similarity(x, y),
    missingness = missingness_similarity(x, y),
    category = category_similarity(x, y),
    identity = identity_similarity(x, y)
  )
  weights <- c(
    schema = 0.22,
    shape = 0.08,
    species = 0.10,
    role_structure = 0.15,
    distribution = 0.15,
    missingness = 0.12,
    category = 0.08,
    identity = 0.10
  )
  overall <- weighted_score(scores, weights[names(scores)])
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
      likely_relation = relation,
      top_mutations = top_mutation_messages(x, y)
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
    cat("  ", nm, ": ", round(x$scores[[nm]], 3), "\n", sep = "")
  }
  if (length(x$top_mutations)) {
    cat("  Top mutations: ", paste(x$top_mutations, collapse = " | "), "\n", sep = "")
  }
  invisible(x)
}

schema_similarity <- function(x, y) {
  jaccard(paste(x$columns$name, x$columns$type, sep = ":"), paste(y$columns$name, y$columns$type, sep = ":"))
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
    unknown = c("unknown_table")
  )
  same_group <- any(vapply(groups, function(g) x$species %in% g && y$species %in% g, logical(1)))
  if (same_group) 0.55 else 0
}

role_structure_similarity <- function(x, y) {
  role_sim <- distribution_overlap(x$columns$role, y$columns$role)
  type_sim <- distribution_overlap(x$columns$type, y$columns$type)
  weighted_score(c(role_sim, type_sim), c(0.7, 0.3))
}

common_columns <- function(x, y) {
  intersect(x$columns$name, y$columns$name)
}

missingness_similarity <- function(x, y) {
  common <- common_columns(x, y)
  if (!length(common)) return(0)
  a <- x$columns[match(common, x$columns$name), ]
  b <- y$columns[match(common, y$columns$name), ]
  clip01(1 - mean(abs(a$missing_rate - b$missing_rate), na.rm = TRUE))
}

identity_similarity <- function(x, y) {
  common <- common_columns(x, y)
  if (!length(common)) return(0)
  a <- x$columns[match(common, x$columns$name), ]
  b <- y$columns[match(common, y$columns$name), ]
  role_sim <- mean(a$role == b$role)
  unique_sim <- clip01(1 - mean(abs(a$unique_rate - b$unique_rate), na.rm = TRUE))
  weighted_score(c(role_sim, unique_sim), c(0.55, 0.45))
}

distribution_similarity <- function(x, y) {
  common <- common_columns(x, y)
  if (!length(common)) return(0)
  a <- x$columns[match(common, x$columns$name), ]
  b <- y$columns[match(common, y$columns$name), ]
  keep <- a$type == "numeric" & b$type == "numeric"
  if (!any(keep)) return(NA_real_)
  qa <- as.matrix(a[keep, c("q25", "q50", "q75"), drop = FALSE])
  qb <- as.matrix(b[keep, c("q25", "q50", "q75"), drop = FALSE])
  scale <- pmax(abs(qa), abs(qb), 1, na.rm = TRUE)
  diff <- abs(qa - qb) / scale
  clip01(1 - mean(diff, na.rm = TRUE))
}

category_similarity <- function(x, y) {
  common <- common_columns(x, y)
  if (!length(common)) return(0)
  a <- x$columns[match(common, x$columns$name), ]
  b <- y$columns[match(common, y$columns$name), ]
  keep <- a$type %in% c("character", "factor", "logical") & b$type %in% c("character", "factor", "logical")
  if (!any(keep)) return(NA_real_)
  sims <- vapply(which(keep), function(i) jaccard(a$top_values[[i]], b$top_values[[i]]), numeric(1))
  mean(sims)
}

distribution_overlap <- function(x, y) {
  tx <- table(x) / length(x)
  ty <- table(y) / length(y)
  keys <- union(names(tx), names(ty))
  px <- as.numeric(tx[keys])
  py <- as.numeric(ty[keys])
  px[is.na(px)] <- 0
  py[is.na(py)] <- 0
  clip01(1 - sum(abs(px - py)) / 2)
}

top_mutation_messages <- function(x, y, n = 3L) {
  changes <- collect_mutations(x, y)
  if (!nrow(changes)) {
    return(character())
  }
  priority <- match(changes$severity, c("high", "medium", "low"), nomatch = 4L)
  changes <- changes[order(priority), , drop = FALSE]
  utils::head(changes$message, n)
}
