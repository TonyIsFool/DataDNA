#' Explain mutations between two data DNA profiles
#'
#' @param x A data frame or `data_dna` object.
#' @param y A data frame or `data_dna` object.
#'
#' @return A `dna_diff` object containing a mutation table.
#' @export
#'
#' @examples
#' demo <- dna_example_customers()
#' dna_diff(demo$customers_old, demo$customers_new)
dna_diff <- function(x, y) {
  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))
  x <- as_data_dna(x, name = x_name)
  y <- as_data_dna(y, name = y_name)
  changes <- collect_mutations(x, y)
  structure(
    list(
      x_name = x$name,
      y_name = y$name,
      changes = changes
    ),
    class = "dna_diff"
  )
}

#' @export
print.dna_diff <- function(x, ...) {
  cat("DataDNA mutation report\n")
  cat("  ", x$x_name, " -> ", x$y_name, "\n", sep = "")
  if (!nrow(x$changes)) {
    cat("  No notable mutations detected.\n")
    return(invisible(x))
  }
  for (i in seq_len(nrow(x$changes))) {
    cat("  [", x$changes$severity[[i]], "] ", x$changes$type[[i]], ": ", x$changes$message[[i]], "\n", sep = "")
  }
  invisible(x)
}

collect_mutations <- function(x, y) {
  old <- x$columns
  new <- y$columns
  old_names <- old$name
  new_names <- new$name
  changes <- list()

  added <- setdiff(new_names, old_names)
  removed <- setdiff(old_names, new_names)
  if (x$n_rows != y$n_rows || x$n_cols != y$n_cols) {
    changes <- append_change(
      changes,
      "shape",
      if (abs(log1p(x$n_rows) - log1p(y$n_rows)) > 0.35 || abs(x$n_cols - y$n_cols) > 3L) "medium" else "low",
      paste0(
        "shape changed from ",
        format(x$n_rows, big.mark = ","),
        "x",
        x$n_cols,
        " to ",
        format(y$n_rows, big.mark = ","),
        "x",
        y$n_cols
      )
    )
  }

  if (!length(intersect(old_names, new_names)) && (length(added) || length(removed))) {
    changes <- append_change(
      changes,
      "schema",
      "high",
      paste0("schema replaced: ", length(removed), " columns removed, ", length(added), " columns added")
    )
  }
  for (nm in utils::head(added, 6L)) changes <- append_change(changes, "schema", "medium", paste0("column added: ", nm))
  for (nm in utils::head(removed, 6L)) changes <- append_change(changes, "schema", "medium", paste0("column removed: ", nm))

  common <- intersect(old_names, new_names)
  for (nm in common) {
    a <- old[match(nm, old$name), ]
    b <- new[match(nm, new$name), ]
    if (!identical(a$type, b$type)) {
      changes <- append_change(changes, "type", "high", paste0(nm, " changed type from ", a$type, " to ", b$type))
    }
    miss_delta <- b$missing_rate - a$missing_rate
    if (is.finite(miss_delta) && abs(miss_delta) >= 0.1) {
      changes <- append_change(changes, "missingness", severity_from_delta(abs(miss_delta)), paste0(nm, " missingness changed by ", format_pct(miss_delta, 1)))
    }
    uniq_delta <- b$unique_rate - a$unique_rate
    if (is.finite(uniq_delta) && abs(uniq_delta) >= 0.15) {
      changes <- append_change(changes, "identity", severity_from_delta(abs(uniq_delta)), paste0(nm, " uniqueness changed by ", format_pct(uniq_delta, 1)))
    }
    if (a$type == "numeric" && b$type == "numeric") {
      shift <- numeric_shift(a, b)
      if (is.finite(shift) && shift >= 0.2) {
        changes <- append_change(changes, "distribution", severity_from_delta(shift), paste0(nm, " numeric distribution shifted"))
      }
    }
    if (a$type %in% c("character", "factor", "logical") && b$type %in% c("character", "factor", "logical")) {
      overlap <- jaccard(a$top_values[[1L]], b$top_values[[1L]])
      if (is.finite(overlap) && overlap < 0.6) {
        changes <- append_change(changes, "category", "medium", paste0(nm, " top categories changed"))
      }
    }
  }

  if (!length(changes)) {
    return(data.frame(type = character(), severity = character(), message = character(), stringsAsFactors = FALSE))
  }
  do.call(rbind, changes)
}

append_change <- function(changes, type, severity, message) {
  c(changes, list(data.frame(type = type, severity = severity, message = message, stringsAsFactors = FALSE)))
}

severity_from_delta <- function(delta) {
  if (delta >= 0.35) "high" else if (delta >= 0.18) "medium" else "low"
}

numeric_shift <- function(a, b) {
  qa <- as.numeric(a[c("q25", "q50", "q75")])
  qb <- as.numeric(b[c("q25", "q50", "q75")])
  scale <- pmax(abs(qa), abs(qb), 1)
  mean(abs(qa - qb) / scale, na.rm = TRUE)
}
