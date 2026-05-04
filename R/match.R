#' Match a data set against a DNA library
#'
#' Finds the closest relatives of a query data set by comparing its data DNA
#' against a named library of data frames or `data_dna` objects.
#'
#' @param x A data frame or `data_dna` object to match.
#' @param library A list of data frames or `data_dna` objects.
#' @param top_n Maximum number of matches to return.
#' @param sample_size Maximum number of rows used when profiling raw data frames.
#'
#' @return A `dna_match` object.
#' @export
#'
#' @examples
#' demo <- dna_example_customers()
#' lib <- list(old = data_dna(demo$customers_old), new = data_dna(demo$customers_new))
#' dna_match(demo$customers_new, lib)
dna_match <- function(x, library, top_n = 5L, sample_size = 10000L) {
  if (!is.list(library)) {
    stop("`library` must be a list of data frames or data_dna objects.", call. = FALSE)
  }
  if (!length(library)) {
    stop("`library` must contain at least one candidate data set.", call. = FALSE)
  }
  stopifnot(length(top_n) == 1L, top_n >= 1L)

  query <- as_data_dna(x, name = deparse(substitute(x)), sample_size = sample_size)
  candidate_names <- match_library_names(library)
  comparisons <- vector("list", length(library))
  rows <- vector("list", length(library))

  for (i in seq_along(library)) {
    candidate <- as_data_dna(library[[i]], name = candidate_names[[i]], sample_size = sample_size)
    candidate$name <- candidate_names[[i]]
    comparison <- dna_compare(query, candidate)
    comparisons[[i]] <- comparison
    rows[[i]] <- match_row(candidate_names[[i]], candidate, comparison)
  }

  matches <- do.call(rbind, rows)
  matches$is_self <- matches$dna_id == query$dna_id
  order_idx <- order(matches$is_self, -matches$overall, na.last = TRUE)
  matches <- matches[order_idx, , drop = FALSE]
  comparisons <- comparisons[order_idx]

  actual_top_n <- min(as.integer(top_n), nrow(matches))
  matches <- matches[seq_len(actual_top_n), , drop = FALSE]
  comparisons <- comparisons[seq_len(actual_top_n)]
  row.names(matches) <- NULL

  structure(
    list(
      query = query,
      matches = matches,
      comparisons = comparisons,
      top_n = actual_top_n
    ),
    class = "dna_match"
  )
}

#' Render a DataDNA lineage match card
#'
#' Creates a static HTML/SVG lineage diagram for a `dna_match` object.
#'
#' @param match A `dna_match` object.
#' @param file Optional HTML file path. If supplied, the card is saved there.
#' @param open Logical. Open the saved file in a browser when `file` is supplied.
#'
#' @return An `htmltools` browsable object, invisibly when saved to `file`.
#' @export
#'
#' @examples
#' demo <- dna_example_customers()
#' lib <- list(old = data_dna(demo$customers_old), new = data_dna(demo$customers_new))
#' match <- dna_match(demo$customers_new, lib)
#' dna_match_card(match)
dna_match_card <- function(match, file = NULL, open = FALSE) {
  if (!inherits(match, "dna_match")) {
    stop("`match` must be a dna_match object.", call. = FALSE)
  }

  card <- build_match_card(match)
  if (!is.null(file)) {
    htmltools::save_html(card, file = file)
    if (isTRUE(open)) {
      utils::browseURL(normalizePath(file, winslash = "/", mustWork = FALSE))
    }
    return(invisible(card))
  }

  htmltools::browsable(card)
}

#' @export
print.dna_match <- function(x, ...) {
  cat("DataDNA lineage match\n")
  cat("  Query: ", x$query$name, " (", x$query$dna_id, ")\n", sep = "")
  if (!nrow(x$matches)) {
    cat("  No matches.\n")
    return(invisible(x))
  }

  best <- x$matches[1L, , drop = FALSE]
  cat("  Best match: ", best$name, " | ", round(best$overall, 3), " | ", best$relation, "\n", sep = "")
  cat("  Top candidates:\n")
  for (i in seq_len(nrow(x$matches))) {
    cat(
      "    ",
      i,
      ". ",
      x$matches$name[[i]],
      " (",
      round(x$matches$overall[[i]], 3),
      ", ",
      x$matches$relation[[i]],
      ")\n",
      sep = ""
    )
  }
  notes <- x$matches$top_mutations[[1L]]
  if (length(notes)) {
    cat("  Best-match mutations: ", paste(notes, collapse = " | "), "\n", sep = "")
  }
  invisible(x)
}

match_library_names <- function(library) {
  labels <- names(library)
  if (is.null(labels)) {
    labels <- rep("", length(library))
  }

  empty <- !nzchar(labels) | is.na(labels)
  if (any(empty)) {
    labels[empty] <- vapply(
      which(empty),
      function(i) {
        item <- library[[i]]
        if (inherits(item, "data_dna") && length(item$name) == 1L && nzchar(item$name)) {
          item$name
        } else {
          paste0("candidate_", i)
        }
      },
      character(1)
    )
  }

  make.unique(labels, sep = "_")
}

match_row <- function(name, candidate, comparison) {
  data.frame(
    name = name,
    dna_id = candidate$dna_id,
    species = candidate$species,
    overall = comparison$overall,
    relation = comparison$likely_relation,
    schema = unname(comparison$scores[["schema"]]),
    shape = unname(comparison$scores[["shape"]]),
    species_score = unname(comparison$scores[["species"]]),
    role_structure = unname(comparison$scores[["role_structure"]]),
    distribution = unname(comparison$scores[["distribution"]]),
    missingness = unname(comparison$scores[["missingness"]]),
    category = unname(comparison$scores[["category"]]),
    identity = unname(comparison$scores[["identity"]]),
    top_mutations = I(list(comparison$top_mutations)),
    stringsAsFactors = FALSE
  )
}

build_match_card <- function(match) {
  best <- match$matches[1L, , drop = FALSE]
  htmltools::tags$html(
    htmltools::tags$head(
      htmltools::tags$meta(charset = "utf-8"),
      htmltools::tags$title(paste("DataDNA lineage", match$query$name)),
      htmltools::tags$style(htmltools::HTML(match_card_css()))
    ),
    htmltools::tags$body(
      htmltools::tags$main(
        class = "dna-shell",
        htmltools::tags$section(
          class = "dna-card lineage-report",
          htmltools::tags$div(
            class = "report-header",
            htmltools::tags$div(
              class = "report-title",
              htmltools::tags$div(class = "kicker", "DataDNA lineage report"),
              htmltools::tags$h1(match$query$name),
              htmltools::tags$p(class = "dna-id", match$query$dna_id),
              htmltools::tags$div(
                class = "report-tags",
                htmltools::tags$span(paste("Species", match$query$species, sep = ": ")),
                htmltools::tags$span(paste("Rows", format(match$query$n_rows, big.mark = ","), sep = ": ")),
                htmltools::tags$span(paste("Columns", match$query$n_cols, sep = ": "))
              )
            ),
            htmltools::tags$div(
              class = "best-card",
              htmltools::tags$span("Closest relative"),
              htmltools::tags$strong(best$name),
              htmltools::tags$div(
                class = "best-score",
                htmltools::tags$em(format_pct(best$overall)),
                htmltools::tags$b(best$relation)
              )
            )
          ),
          htmltools::tags$div(
            class = "report-metrics",
            lineage_stat("Relation", best$relation, "relation"),
            lineage_stat("Similarity", format_pct(best$overall), "score"),
            lineage_stat("Best species", best$species, "species"),
            lineage_stat("Candidates scanned", nrow(match$matches), "count")
          ),
          htmltools::tags$div(
            class = "report-body",
            htmltools::tags$div(
              class = "lineage-panel lineage-graph-panel",
              htmltools::tags$div(
                class = "panel-heading",
                htmltools::tags$span("Lineage map"),
                htmltools::tags$h2("Similarity-weighted candidate graph")
              ),
              htmltools::HTML(lineage_svg(match))
            ),
            htmltools::tags$div(
              class = "lineage-panel lineage-rank",
              htmltools::tags$div(
                class = "panel-heading",
                htmltools::tags$span("Ranked relatives"),
                htmltools::tags$h2("Candidate ranking")
              ),
              match_table(match)
            )
          ),
          htmltools::tags$div(
            class = "lineage-summary",
            htmltools::tags$div(
              class = "panel-heading",
              htmltools::tags$span("Interpretation"),
              htmltools::tags$h2("Mutation summary")
            ),
            mutation_summary(match)
          )
        )
      )
    )
  )
}

lineage_stat <- function(label, value, kind) {
  htmltools::tags$div(
    class = paste("lineage-stat", kind),
    htmltools::tags$span(label),
    htmltools::tags$strong(value)
  )
}

short_label <- function(x, width = 26L) {
  x <- as.character(x)
  ifelse(nchar(x, type = "width") > width, paste0(substr(x, 1L, width - 1L), "..."), x)
}

lineage_svg <- function(match) {
  matches <- match$matches
  n <- nrow(matches)
  width <- 960
  height <- max(330, 130 + n * 92)
  query_x <- 72
  query_y <- height / 2
  query_w <- 250
  query_h <- 104
  cand_x <- 606
  cand_w <- 292
  cand_h <- 78
  cand_y <- if (n == 1L) height / 2 else seq(86, height - 86, length.out = n)

  lines <- character(n)
  nodes <- character(n)
  for (i in seq_len(n)) {
    score <- matches$overall[[i]]
    color <- relation_color(score)
    stroke <- 1.4 + 3.4 * clip01(score)
    label <- htmltools::htmlEscape(short_label(matches$name[[i]], 24L))
    relation <- htmltools::htmlEscape(matches$relation[[i]])
    pct <- format_pct(score)
    y_top <- cand_y[[i]] - cand_h / 2
    lines[[i]] <- sprintf(
      "<path d='M %s %s C %s %s, %s %s, %s %s' stroke='%s' stroke-width='%.1f' fill='none' opacity='.86'/>",
      query_x + query_w,
      query_y,
      query_x + query_w + 88,
      query_y,
      cand_x - 94,
      cand_y[[i]],
      cand_x,
      cand_y[[i]],
      color,
      stroke
    )
    nodes[[i]] <- sprintf(
      paste0(
        "<g>",
        "<rect x='%s' y='%s' width='%s' height='%s' rx='12' fill='#111b25' stroke='%s' stroke-width='1.2'/>",
        "<rect x='%s' y='%s' width='4' height='%s' rx='2' fill='%s'/>",
        "<text x='%s' y='%s' class='node-title'>%s</text>",
        "<text x='%s' y='%s' class='node-sub'>%s</text>",
        "<text x='%s' y='%s' text-anchor='end' class='node-score'>%s</text>",
        "</g>"
      ),
      cand_x,
      y_top,
      cand_w,
      cand_h,
      color,
      cand_x,
      y_top + 16,
      cand_h - 32,
      color,
      cand_x + 22,
      cand_y[[i]] - 8,
      label,
      cand_x + 22,
      cand_y[[i]] + 14,
      relation,
      cand_x + cand_w - 20,
      cand_y[[i]] + 2,
      pct
    )
  }

  query_label <- htmltools::htmlEscape(short_label(match$query$name, 22L))
  sprintf(
    paste0(
      "<svg class='lineage-svg' viewBox='0 0 %s %s' role='img' aria-label='DataDNA lineage graph'>",
      "<style>",
      ".node-title{font:600 14px Inter,Segoe UI,Helvetica Neue,Arial,sans-serif;fill:#eef3f6;}",
      ".node-score{font:700 20px Inter,Segoe UI,Helvetica Neue,Arial,sans-serif;fill:#eef3f6;}",
      ".node-sub{font:12px Inter,Segoe UI,Helvetica Neue,Arial,sans-serif;fill:#9aa8b5;}",
      ".query-title{font:700 16px Inter,Segoe UI,Helvetica Neue,Arial,sans-serif;fill:#eef3f6;}",
      ".query-sub{font:12px SFMono-Regular,Consolas,Liberation Mono,monospace;fill:#9aa8b5;}",
      ".section-label{font:600 11px Inter,Segoe UI,Helvetica Neue,Arial,sans-serif;fill:#6f7a86;}",
      "</style>",
      "<defs>",
      "<pattern id='grid' width='40' height='40' patternUnits='userSpaceOnUse'>",
      "<path d='M 40 0 L 0 0 0 40' fill='none' stroke='rgba(255,255,255,.035)' stroke-width='1'/>",
      "</pattern>",
      "</defs>",
      "<rect width='100%%' height='100%%' rx='18' fill='#0f1720'/>",
      "<rect width='100%%' height='100%%' rx='18' fill='url(#grid)'/>",
      "<text x='%s' y='42' class='section-label'>QUERY SPECIMEN</text>",
      "<text x='%s' y='42' class='section-label'>RELATIVE CANDIDATES</text>",
      "%s",
      "<g>",
      "<rect x='%s' y='%s' width='%s' height='%s' rx='14' fill='#121d28' stroke='#66d9c2' stroke-width='1.4'/>",
      "<rect x='%s' y='%s' width='4' height='%s' rx='2' fill='#66d9c2'/>",
      "<text x='%s' y='%s' class='query-title'>%s</text>",
      "<text x='%s' y='%s' class='query-sub'>%s</text>",
      "<text x='%s' y='%s' class='node-sub'>%s</text>",
      "</g>",
      "%s",
      "</svg>"
    ),
    width,
    height,
    query_x,
    cand_x,
    paste(lines, collapse = ""),
    query_x,
    query_y - query_h / 2,
    query_w,
    query_h,
    query_x,
    query_y - query_h / 2 + 18,
    query_h - 36,
    query_x + 24,
    query_y - 14,
    query_label,
    query_x + 24,
    query_y + 10,
    match$query$dna_id,
    query_x + 24,
    query_y + 34,
    match$query$species,
    paste(nodes, collapse = "")
  )
}

match_table <- function(match) {
  htmltools::tags$div(
    class = "rank-list",
    lapply(seq_len(nrow(match$matches)), function(i) {
      mutations <- match$matches$top_mutations[[i]]
      score <- match$matches$overall[[i]]
      htmltools::tags$article(
        class = "rank-card",
        htmltools::tags$div(
          class = "rank-topline",
          htmltools::tags$span(class = "rank-index", sprintf("%02d", i)),
          htmltools::tags$strong(match$matches$name[[i]]),
          htmltools::tags$em(format_pct(score))
        ),
        htmltools::tags$div(
          class = "rank-meter",
          htmltools::tags$i(style = paste0("width:", round(100 * clip01(score)), "%; background:", relation_color(score), ";"))
        ),
        htmltools::tags$p(match$matches$relation[[i]]),
        htmltools::tags$small(if (length(mutations)) paste(utils::head(mutations, 2L), collapse = " | ") else "No notable mutations")
      )
    })
  )
}

mutation_summary <- function(match) {
  best <- match$matches[1L, , drop = FALSE]
  notes <- best$top_mutations[[1L]]
  if (!length(notes)) {
    notes <- "No notable mutations detected for the closest relative."
  }

  htmltools::tags$div(
    class = "summary-grid",
    htmltools::tags$div(
      class = "summary-callout",
      htmltools::tags$span("Best lineage interpretation"),
      htmltools::tags$strong(best$relation),
      htmltools::tags$p(paste0(best$name, " is the nearest non-self relative with ", format_pct(best$overall), " similarity."))
    ),
    htmltools::tags$ul(
      class = "summary-notes",
      lapply(utils::head(notes, 4L), htmltools::tags$li)
    )
  )
}

relation_label_class <- function(score) {
  if (score >= 0.97) {
    "near"
  } else if (score >= 0.85) {
    "changed"
  } else if (score >= 0.65) {
    "related"
  } else {
    "different"
  }
}

relation_color <- function(score) {
  if (score >= 0.97) {
    "#66d9c2"
  } else if (score >= 0.85) {
    "#69a7d8"
  } else if (score >= 0.65) {
    "#d8b85f"
  } else {
    "#6f7a86"
  }
}

risk_from_similarity <- function(score) {
  if (score >= 0.85) "low" else if (score >= 0.65) "medium" else "high"
}

match_card_css <- function() {
  paste0(
    dna_card_css(),
    "
    body {
      color: #eef3f6;
      background:
        radial-gradient(circle at 18% 0%, rgba(102, 217, 194, .08), transparent 360px),
        linear-gradient(180deg, #0b1118 0%, #101923 100%);
      font-family: Inter, 'Segoe UI', 'Helvetica Neue', Arial, sans-serif;
    }
    .dna-shell {
      width: min(1180px, calc(100vw - 40px));
      margin: 28px auto;
    }
    .lineage-report {
      border: 1px solid rgba(255,255,255,.10);
      border-radius: 8px;
      background: #0f1720;
      box-shadow: 0 28px 90px rgba(0,0,0,.34);
    }
    .report-header {
      display: grid;
      grid-template-columns: minmax(0, 1fr) 320px;
      gap: 28px;
      padding: 34px 36px 30px;
      border-bottom: 1px solid rgba(255,255,255,.10);
      background: linear-gradient(180deg, rgba(255,255,255,.035), rgba(255,255,255,.01));
    }
    .report-title h1 {
      margin: 10px 0 8px;
      color: #eef3f6;
      font-size: 40px;
      line-height: 1.05;
      font-weight: 700;
      letter-spacing: 0;
    }
    .kicker {
      color: #66d9c2;
      font-size: 12px;
      font-weight: 700;
      text-transform: uppercase;
      letter-spacing: 0;
    }
    .dna-id {
      margin: 0;
      color: #9aa8b5;
      font-family: SFMono-Regular, Consolas, 'Liberation Mono', monospace;
      font-size: 14px;
    }
    .report-tags {
      display: flex;
      flex-wrap: wrap;
      gap: 8px;
      margin-top: 20px;
    }
    .report-tags span {
      padding: 7px 10px;
      border: 1px solid rgba(255,255,255,.10);
      border-radius: 6px;
      color: #cdd6de;
      background: rgba(255,255,255,.035);
      font-size: 12px;
    }
    .best-card {
      align-self: stretch;
      display: grid;
      align-content: center;
      gap: 12px;
      padding: 24px;
      border: 1px solid rgba(102,217,194,.28);
      border-radius: 8px;
      background: #101923;
    }
    .best-card > span {
      color: #9aa8b5;
      font-size: 12px;
      font-weight: 700;
      text-transform: uppercase;
    }
    .best-card > strong {
      color: #eef3f6;
      font-size: 28px;
      line-height: 1.08;
      font-weight: 700;
    }
    .best-score {
      display: flex;
      align-items: flex-end;
      gap: 12px;
    }
    .best-score em {
      color: #66d9c2;
      font-size: 40px;
      line-height: .95;
      font-style: normal;
      font-weight: 700;
    }
    .best-score b {
      color: #9aa8b5;
      font-size: 12px;
      font-weight: 600;
      line-height: 1.25;
      padding-bottom: 4px;
    }
    .report-metrics {
      display: grid;
      grid-template-columns: 1.35fr .7fr .9fr .65fr;
      gap: 1px;
      background: rgba(255,255,255,.10);
    }
    .lineage-stat {
      min-width: 0;
      padding: 18px 24px;
      background: #101923;
    }
    .lineage-stat span {
      display: block;
      color: #9aa8b5;
      font-size: 12px;
      font-weight: 650;
      text-transform: uppercase;
    }
    .lineage-stat strong {
      display: block;
      margin-top: 7px;
      color: #eef3f6;
      font-size: 16px;
      line-height: 1.25;
      font-weight: 650;
    }
    .report-body {
      display: grid;
      grid-template-columns: minmax(0, 1.45fr) minmax(320px, .75fr);
      gap: 20px;
      padding: 24px;
    }
    .lineage-panel,
    .lineage-summary {
      border: 1px solid rgba(255,255,255,.10);
      border-radius: 8px;
      background: rgba(255,255,255,.025);
    }
    .lineage-panel {
      padding: 20px;
    }
    .panel-heading {
      margin-bottom: 16px;
    }
    .panel-heading span {
      display: block;
      color: #66d9c2;
      font-size: 12px;
      font-weight: 700;
      text-transform: uppercase;
    }
    .panel-heading h2 {
      margin: 6px 0 0;
      color: #eef3f6;
      font-size: 20px;
      line-height: 1.2;
      font-weight: 650;
      letter-spacing: 0;
    }
    .lineage-svg {
      display: block;
      width: 100%;
      height: auto;
      min-height: 330px;
    }
    .rank-list {
      display: grid;
      gap: 12px;
    }
    .rank-card {
      padding: 14px;
      border: 1px solid rgba(255,255,255,.10);
      border-radius: 8px;
      background: #111b25;
    }
    .rank-topline {
      display: grid;
      grid-template-columns: 34px minmax(0, 1fr) auto;
      align-items: baseline;
      gap: 10px;
    }
    .rank-index {
      color: #6f7a86;
      font-family: SFMono-Regular, Consolas, 'Liberation Mono', monospace;
      font-size: 12px;
    }
    .rank-topline strong {
      min-width: 0;
      color: #eef3f6;
      font-size: 16px;
      font-weight: 650;
      overflow-wrap: anywhere;
    }
    .rank-topline em {
      color: #66d9c2;
      font-size: 16px;
      font-style: normal;
      font-weight: 700;
    }
    .rank-meter {
      height: 4px;
      margin: 11px 0 10px;
      border-radius: 999px;
      background: rgba(255,255,255,.08);
      overflow: hidden;
    }
    .rank-meter i {
      display: block;
      height: 100%;
      border-radius: inherit;
    }
    .rank-card p {
      margin: 0 0 7px;
      color: #cdd6de;
      font-size: 14px;
      line-height: 1.35;
    }
    .rank-card small {
      color: #9aa8b5;
      font-size: 12px;
      line-height: 1.35;
    }
    .lineage-summary {
      margin: 0 24px 24px;
      padding: 20px;
    }
    .summary-grid {
      display: grid;
      grid-template-columns: minmax(0, .9fr) minmax(0, 1.1fr);
      gap: 20px;
    }
    .summary-callout {
      padding: 16px;
      border-radius: 8px;
      background: #101923;
      border: 1px solid rgba(255,255,255,.10);
    }
    .summary-callout span {
      color: #9aa8b5;
      font-size: 12px;
      font-weight: 700;
      text-transform: uppercase;
    }
    .summary-callout strong {
      display: block;
      margin-top: 8px;
      color: #eef3f6;
      font-size: 20px;
      line-height: 1.2;
    }
    .summary-callout p {
      margin: 10px 0 0;
      color: #9aa8b5;
      font-size: 14px;
      line-height: 1.45;
    }
    .summary-notes {
      margin: 0;
      padding: 0;
      list-style: none;
      display: grid;
      gap: 8px;
    }
    .summary-notes li {
      padding: 11px 13px;
      border-radius: 6px;
      color: #cdd6de;
      background: rgba(255,255,255,.035);
      border: 1px solid rgba(255,255,255,.08);
      font-size: 14px;
      line-height: 1.35;
    }
    @media (max-width: 920px) {
      .report-header,
      .report-body,
      .summary-grid {
        grid-template-columns: 1fr;
      }
      .report-metrics {
        grid-template-columns: repeat(2, 1fr);
      }
    }
    "
  )
}
