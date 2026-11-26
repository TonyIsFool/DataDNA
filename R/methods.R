#' S3 methods for DataDNA objects
#'
#' Standard R `summary()`, `plot()`, and `as.data.frame()` methods for
#' `data_dna`, `dna_comparison`, `dna_diff`, `dna_match`, and `dna_library`
#' objects.
#'
#' @param object A `data_dna` object.
#' @param x A DataDNA object (`data_dna`, `dna_comparison`, `dna_diff`,
#'   `dna_match`, or `dna_library`).
#' @param y Optional second data frame or `data_dna` object when plotting a
#'   `data_dna` specimen comparison.
#' @param row.names Optional row names passed to `as.data.frame()`.
#' @param optional Logical passed to `as.data.frame()`.
#' @param ... Additional arguments passed to underlying plotting or conversion
#'   functions.
#'
#' @return
#' - `summary.data_dna()` returns a `summary.data_dna` list.
#' - `plot.*` methods draw a paper-style figure and return their primary object
#'   invisibly.
#' - `as.data.frame.*` methods return flat, export-ready `data.frame` tables.
#'
#' @name datadna-methods
NULL

#' @rdname datadna-methods
#' @export
summary.data_dna <- function(object, ...) {
  cols <- object$columns
  role_counts <- if (nrow(cols)) sort(table(cols$role), decreasing = TRUE) else integer()
  type_counts <- if (nrow(cols)) sort(table(cols$type), decreasing = TRUE) else integer()
  out <- list(
    name = object$name,
    dna_id = object$dna_id,
    species = object$species,
    n_rows = object$n_rows,
    n_cols = object$n_cols,
    sample_size = object$sample_size,
    scores = object$scores,
    fingerprints = object$fingerprints,
    mean_missing_rate = safe_mean(cols$missing_rate),
    role_counts = as.list(role_counts),
    type_counts = as.list(type_counts),
    warnings = object$warnings
  )
  class(out) <- "summary.data_dna"
  out
}

#' @export
print.summary.data_dna <- function(x, ...) {
  cat("Summary of DataDNA Specimen: ", x$name, "\n", sep = "")
  cat("  DNA ID:    ", x$dna_id, "\n", sep = "")
  cat("  Species:   ", x$species, "\n", sep = "")
  cat("  Shape:     ", format(x$n_rows, big.mark = ","), " rows x ", x$n_cols, " columns (sampled: ", format(x$sample_size, big.mark = ","), ")\n", sep = "")
  cat("  Identity:  ", round(x$scores$identity_strength, 1), "/100 (stability: ", x$scores$stability, ", drift risk: ", x$scores$drift_risk, ")\n", sep = "")
  cat("  Hashes:    schema=", x$fingerprints$schema_hash, " shape=", x$fingerprints$shape_hash, " profile=", x$fingerprints$profile_hash, "\n", sep = "")
  cat("  Missing:   ", format_pct(x$mean_missing_rate, 1), " mean across columns\n", sep = "")
  if (length(x$role_counts)) {
    roles_txt <- paste0(names(x$role_counts), "=", unlist(x$role_counts, use.names = FALSE))
    cat("  Roles:     ", paste(roles_txt, collapse = ", "), "\n", sep = "")
  }
  if (length(x$warnings)) {
    cat("  Warnings:  ", paste(x$warnings, collapse = " | "), "\n", sep = "")
  }
  invisible(x)
}

#' @rdname datadna-methods
#' @export
plot.data_dna <- function(x, y = NULL, ...) {
  if (!is.null(y)) {
    return(dna_diff_plot(x, y, ...))
  }
  dna_plot(x, ...)
}

#' @rdname datadna-methods
#' @export
plot.dna_match <- function(x, y = NULL, ...) {
  dna_match_plot(x, ...)
}

#' @rdname datadna-methods
#' @export
plot.dna_comparison <- function(x, y = NULL, ...) {
  dna_diff_plot(x, ...)
}

#' @rdname datadna-methods
#' @export
plot.dna_diff <- function(x, y = NULL, ...) {
  dna_diff_plot(x, ...)
}

#' @rdname datadna-methods
#' @export
as.data.frame.data_dna <- function(x, row.names = NULL, optional = FALSE, ...) {
  out <- x$columns
  if ("top_values" %in% names(out)) {
    out$top_values <- vapply(
      out$top_values,
      function(vals) if (length(vals)) paste(vals, collapse = " | ") else "",
      character(1)
    )
  }
  if (!is.null(row.names)) {
    rownames(out) <- row.names
  }
  out
}

#' @rdname datadna-methods
#' @export
as.data.frame.dna_comparison <- function(x, row.names = NULL, optional = FALSE, ...) {
  out <- data.frame(
    x_name = rep(x$x_name, length(x$scores)),
    y_name = rep(x$y_name, length(x$scores)),
    dimension = names(x$scores),
    score = as.numeric(x$scores),
    overall = rep(x$overall, length(x$scores)),
    likely_relation = rep(x$likely_relation, length(x$scores)),
    stringsAsFactors = FALSE
  )
  if (!is.null(row.names)) {
    rownames(out) <- row.names
  }
  out
}

#' @rdname datadna-methods
#' @export
as.data.frame.dna_diff <- function(x, row.names = NULL, optional = FALSE, ...) {
  out <- x$changes
  if (!is.null(row.names)) {
    rownames(out) <- row.names
  }
  out
}

#' @rdname datadna-methods
#' @export
as.data.frame.dna_match <- function(x, row.names = NULL, optional = FALSE, ...) {
  out <- x$matches
  if ("top_mutations" %in% names(out)) {
    out$top_mutations <- vapply(
      out$top_mutations,
      function(vals) if (length(vals)) paste(vals, collapse = " | ") else "",
      character(1)
    )
  }
  if (!is.null(row.names)) {
    rownames(out) <- row.names
  }
  out
}

#' @rdname datadna-methods
#' @export
as.data.frame.dna_library <- function(x, row.names = NULL, optional = FALSE, ...) {
  if (!length(x)) {
    return(data.frame(
      name = character(),
      dna_id = character(),
      species = character(),
      n_rows = integer(),
      n_cols = integer(),
      identity_strength = numeric(),
      stability = character(),
      drift_risk = character(),
      stringsAsFactors = FALSE
    ))
  }
  rows <- lapply(seq_along(x), function(i) {
    item <- x[[i]]
    data.frame(
      name = item$name,
      dna_id = item$dna_id,
      species = item$species,
      n_rows = as.integer(item$n_rows),
      n_cols = as.integer(item$n_cols),
      identity_strength = item$scores$identity_strength,
      stability = item$scores$stability,
      drift_risk = item$scores$drift_risk,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  if (!is.null(row.names)) {
    rownames(out) <- row.names
  } else {
    rownames(out) <- NULL
  }
  out
}
