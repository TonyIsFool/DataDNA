#' Explain mutations between two data DNA profiles
#'
#' Detects schema changes, likely column renames, type and role transitions,
#' missingness shifts, uniqueness changes, numeric distribution drift, category
#' shifts, and temporal window drift between two data sets.
#'
#' @param x A data frame or `data_dna` object.
#' @param y A data frame or `data_dna` object.
#'
#' @return A `dna_diff` object containing `changes` and `renames` tables.
#' @export
#'
#' @examples
#' demo <- dna_example_customers()
#' dna_diff(demo$customers_old, demo$customers_new)
dna_diff <- function(x, y) {
  x_name <- paste(deparse(substitute(x)), collapse = " ")
  y_name <- paste(deparse(substitute(y)), collapse = " ")
  x <- as_data_dna(x, name = x_name)
  y <- as_data_dna(y, name = y_name)
  renames <- detect_renamed_columns(x, y)
  changes <- collect_mutations(x, y, renames = renames)
  structure(
    list(
      x_name = x$name,
      y_name = y$name,
      changes = changes,
      renames = renames
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

empty_renames_frame <- function() {
  data.frame(
    old_name = character(),
    new_name = character(),
    type = character(),
    similarity = numeric(),
    stringsAsFactors = FALSE
  )
}

column_profile_similarity <- function(a, b, has_rows = TRUE) {
  compatible_type <- identical(a$type, b$type) ||
    (a$type %in% c("character", "factor") && b$type %in% c("character", "factor"))
  if (!compatible_type) {
    return(0)
  }

  nm_sim <- name_similarity(a$name, b$name)
  if (!has_rows) {
    return(if (nm_sim >= 0.85) nm_sim else 0)
  }

  miss_sim <- if (is.finite(a$missing_rate) && is.finite(b$missing_rate)) {
    clip01(1 - abs(a$missing_rate - b$missing_rate))
  } else {
    0.5
  }
  uniq_sim <- if (is.finite(a$unique_rate) && is.finite(b$unique_rate)) {
    clip01(1 - abs(a$unique_rate - b$unique_rate))
  } else {
    0.5
  }
  role_sim <- if (identical(a$role, b$role)) 1 else 0.3

  val_sim <- if (a$type == "numeric" && b$type == "numeric") {
    clip01(1 - numeric_shift(a, b))
  } else if (a$type %in% c("character", "factor", "logical") && b$type %in% c("character", "factor", "logical")) {
    top_sim <- jaccard(a$top_values[[1L]], b$top_values[[1L]])
    ent_sim <- if (is.finite(a$entropy) && is.finite(b$entropy)) clip01(1 - abs(a$entropy - b$entropy)) else 0.5
    0.75 * top_sim + 0.25 * ent_sim
  } else if (a$type == "date" && b$type == "date") {
    ts <- temporal_col_similarity(a, b)
    if (is.finite(ts)) ts else 0.5
  } else {
    0.5
  }

  stat_sim <- weighted_score(c(val_sim, uniq_sim, miss_sim, role_sim), c(0.45, 0.25, 0.15, 0.15))
  if (!is.finite(stat_sim) || stat_sim < 0.85) {
    return(0)
  }
  if (nm_sim < 0.30 && stat_sim < 0.94) {
    return(0)
  }
  clip01(0.85 * stat_sim + 0.15 * nm_sim)
}

detect_renamed_columns <- function(x, y) {
  old <- x$columns
  new <- y$columns
  removed <- setdiff(old$name, new$name)
  added <- setdiff(new$name, old$name)
  if (!length(removed) || !length(added)) {
    return(empty_renames_frame())
  }

  has_rows <- x$n_rows > 0L && y$n_rows > 0L
  candidates <- list()
  for (old_nm in removed) {
    a <- old[match(old_nm, old$name), , drop = FALSE]
    for (new_nm in added) {
      b <- new[match(new_nm, new$name), , drop = FALSE]
      sim <- column_profile_similarity(a, b, has_rows = has_rows)
      if (is.finite(sim) && sim >= 0.82) {
        candidates[[length(candidates) + 1L]] <- data.frame(
          old_name = old_nm,
          new_name = new_nm,
          type = b$type[[1L]],
          similarity = sim,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (!length(candidates)) {
    return(empty_renames_frame())
  }

  cand_df <- do.call(rbind, candidates)
  cand_df <- cand_df[order(-cand_df$similarity), , drop = FALSE]

  used_old <- character()
  used_new <- character()
  picked <- list()
  for (i in seq_len(nrow(cand_df))) {
    row <- cand_df[i, , drop = FALSE]
    if (row$old_name %in% used_old || row$new_name %in% used_new) {
      next
    }
    used_old <- c(used_old, row$old_name)
    used_new <- c(used_new, row$new_name)
    picked[[length(picked) + 1L]] <- row
  }

  out <- do.call(rbind, picked)
  row.names(out) <- NULL
  out
}

collect_mutations <- function(x, y, renames = NULL) {
  old <- x$columns
  new <- y$columns
  old_names <- old$name
  new_names <- new$name
  if (is.null(renames)) {
    renames <- detect_renamed_columns(x, y)
  }

  changes <- list()

  raw_added <- setdiff(new_names, old_names)
  raw_removed <- setdiff(old_names, new_names)
  added <- setdiff(raw_added, renames$new_name)
  removed <- setdiff(raw_removed, renames$old_name)

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
      ),
      column = NA_character_
    )
  }

  if (!length(intersect(old_names, new_names)) && !nrow(renames) && (length(added) || length(removed))) {
    changes <- append_change(
      changes,
      "schema",
      "high",
      paste0("schema replaced: ", length(removed), " columns removed, ", length(added), " columns added"),
      column = NA_character_
    )
  }

  if (nrow(renames)) {
    for (i in seq_len(min(nrow(renames), 6L))) {
      changes <- append_change(
        changes,
        "rename",
        "low",
        paste0(
          "column likely renamed: ",
          renames$old_name[[i]],
          " -> ",
          renames$new_name[[i]],
          " (",
          format_pct(renames$similarity[[i]]),
          " match)"
        ),
        column = renames$new_name[[i]]
      )
    }
  }

  for (nm in utils::head(added, 6L)) {
    changes <- append_change(changes, "schema", "medium", paste0("column added: ", nm), column = nm)
  }
  for (nm in utils::head(removed, 6L)) {
    changes <- append_change(changes, "schema", "medium", paste0("column removed: ", nm), column = nm)
  }

  pairs <- aligned_column_pairs(x, y, renames = renames)
  for (idx in seq_len(nrow(pairs))) {
    old_nm <- pairs$old_name[[idx]]
    new_nm <- pairs$new_name[[idx]]
    label <- if (identical(old_nm, new_nm)) new_nm else paste0(old_nm, "->", new_nm)
    a <- old[match(old_nm, old$name), , drop = FALSE]
    b <- new[match(new_nm, new$name), , drop = FALSE]

    if (!identical(a$type, b$type)) {
      changes <- append_change(
        changes,
        "type",
        "high",
        paste0(label, " changed type from ", a$type, " to ", b$type),
        column = new_nm
      )
    } else if (!identical(a$role, b$role)) {
      changes <- append_change(
        changes,
        "role",
        "medium",
        paste0(label, " changed role from ", a$role, " to ", b$role),
        column = new_nm
      )
    }

    miss_delta <- b$missing_rate - a$missing_rate
    if (is.finite(miss_delta) && abs(miss_delta) >= 0.1) {
      changes <- append_change(
        changes,
        "missingness",
        severity_from_delta(abs(miss_delta)),
        paste0(label, " missingness changed by ", format_pct(miss_delta, 1)),
        column = new_nm
      )
    }

    uniq_delta <- b$unique_rate - a$unique_rate
    if (is.finite(uniq_delta) && abs(uniq_delta) >= 0.15) {
      changes <- append_change(
        changes,
        "identity",
        severity_from_delta(abs(uniq_delta)),
        paste0(label, " uniqueness changed by ", format_pct(uniq_delta, 1)),
        column = new_nm
      )
    }

    if (a$type == "numeric" && b$type == "numeric") {
      shift <- numeric_shift(a, b)
      if (is.finite(shift) && shift >= 0.2) {
        changes <- append_change(
          changes,
          "distribution",
          severity_from_delta(shift),
          paste0(label, " numeric distribution shifted"),
          column = new_nm
        )
      }
    }

    if (a$type %in% c("character", "factor", "logical") && b$type %in% c("character", "factor", "logical")) {
      overlap <- jaccard(a$top_values[[1L]], b$top_values[[1L]])
      if (is.finite(overlap) && overlap < 0.6) {
        changes <- append_change(
          changes,
          "category",
          "medium",
          paste0(label, " top categories changed"),
          column = new_nm
        )
      }
    }

    if (a$type == "date" && b$type == "date") {
      changes <- collect_temporal_mutations(changes, a, b, label, new_nm)
    }
  }

  if (!length(changes)) {
    return(data.frame(
      column = character(),
      type = character(),
      severity = character(),
      message = character(),
      stringsAsFactors = FALSE
    ))
  }
  out <- do.call(rbind, changes)
  row.names(out) <- NULL
  out
}

collect_temporal_mutations <- function(changes, a, b, label, col_name) {
  a_min <- parse_dna_time(a$date_min[[1L]])
  a_max <- parse_dna_time(a$date_max[[1L]])
  b_min <- parse_dna_time(b$date_min[[1L]])
  b_max <- parse_dna_time(b$date_max[[1L]])
  if (all(is.finite(c(a_min, a_max, b_min, b_max)))) {
    span <- max(a_max - a_min, b_max - b_min, 86400)
    boundary_shift <- max(abs(b_min - a_min), abs(b_max - a_max)) / span
    disjoint <- b_min > a_max || b_max < a_min
    if (disjoint || boundary_shift >= 0.25) {
      sev <- if (disjoint || boundary_shift >= 0.75) "medium" else "low"
      changes <- append_change(
        changes,
        "temporal",
        sev,
        paste0(
          label,
          " time window shifted (",
          a$date_min[[1L]], "..", a$date_max[[1L]],
          " -> ",
          b$date_min[[1L]], "..", b$date_max[[1L]],
          ")"
        ),
        column = col_name
      )
    }
  }
  if (is.finite(a$interval_cv[[1L]]) && is.finite(b$interval_cv[[1L]])) {
    cv_delta <- abs(b$interval_cv[[1L]] - a$interval_cv[[1L]])
    if (cv_delta >= 0.6) {
      changes <- append_change(
        changes,
        "temporal",
        if (cv_delta >= 1.2) "medium" else "low",
        paste0(label, " sampling regularity changed"),
        column = col_name
      )
    }
  }
  changes
}

append_change <- function(changes, type, severity, message, column = NA_character_) {
  c(
    changes,
    list(data.frame(
      column = as.character(column),
      type = type,
      severity = severity,
      message = message,
      stringsAsFactors = FALSE
    ))
  )
}

severity_from_delta <- function(delta) {
  if (delta >= 0.35) "high" else if (delta >= 0.18) "medium" else "low"
}

numeric_shift <- function(a, b) {
  qa <- as.numeric(a[c("q25", "q50", "q75")])
  qb <- as.numeric(b[c("q25", "q50", "q75")])
  if (all(!is.finite(qa)) && all(!is.finite(qb))) {
    return(0)
  }
  if (any(!is.finite(qa)) || any(!is.finite(qb))) {
    return(1)
  }
  spread <- suppressWarnings(max(a$q75 - a$q25, b$q75 - b$q25, a$sd, b$sd, 0, na.rm = TRUE))
  if (!is.finite(spread)) {
    spread <- 0
  }
  scale <- pmax(abs(qa), abs(qb), spread, 1e-4)
  mean(abs(qa - qb) / scale, na.rm = TRUE)
}
