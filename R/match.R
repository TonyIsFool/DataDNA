#' Match a data set against a DNA library
#'
#' Finds the closest relatives of a query data set by comparing its data DNA
#' against a named library of data frames or `data_dna` objects.
#'
#' @param x A data frame or `data_dna` object to match.
#' @param library A list (or `dna_library`) of data frames or `data_dna` objects.
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
  if (!is.numeric(top_n) || length(top_n) != 1L || is.na(top_n) || top_n < 1L) {
    stop("`top_n` must be a single positive integer.", call. = FALSE)
  }

  query <- as_data_dna(x, name = paste(deparse(substitute(x)), collapse = " "), sample_size = sample_size)
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

#' Render a precision-instrument interactive DataDNA lineage match card
#'
#' Creates a self-contained interactive HTML/SVG lineage instrument for a
#' `dna_match` object without external runtime dependencies.
#'
#' @param match A `dna_match` object.
#' @param file Optional HTML file path. If supplied, the card is saved there.
#' @param open Logical. Open the saved file in a browser when `file` is supplied.
#'
#' @return A `dna_html` document object, invisibly when saved to `file`.
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
    save_dna_html(card, file = file)
    if (isTRUE(open)) {
      utils::browseURL(normalizePath(file, winslash = "/", mustWork = FALSE))
    }
    return(invisible(card))
  }

  card
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
    temporal = unname(comparison$scores[["temporal"]]),
    top_mutations = I(list(comparison$top_mutations)),
    stringsAsFactors = FALSE
  )
}

best_relative_sentence <- function(best) {
  if (isTRUE(best$is_self[[1L]])) {
    paste0(best$name[[1L]], " matches the query specimen (", format_pct(best$overall[[1L]]), " similarity).")
  } else {
    paste0(best$name[[1L]], " is the nearest non-self relative with ", format_pct(best$overall[[1L]]), " similarity.")
  }
}

build_match_card <- function(match) {
  best <- match$matches[1L, , drop = FALSE]
  doc <- html_tag(
    "html",
    lang = "en",
    `data-theme` = "light",
    html_tag(
      "head",
      html_tag("meta", charset = "utf-8", .void = TRUE),
      html_tag("meta", name = "viewport", content = "width=device-width, initial-scale=1", .void = TRUE),
      html_tag("title", paste("DataDNA lineage", match$query$name)),
      html_tag("style", html_raw(match_card_css()))
    ),
    html_tag(
      "body",
      html_tag(
        "main",
        class = "dna-shell",
        html_tag(
          "section",
          class = "dna-card lineage-report",
          html_tag(
            "div",
            class = "top-rail",
            html_tag("span", class = "led"),
            html_tag("span", class = "kicker", "DataDNA lineage instrument // TE-SPEC 0.2"),
            html_tag(
              "button",
              type = "button",
              class = "theme-toggle",
              id = "theme-toggle",
              title = "Toggle light / dark instrument mode",
              "MODE: LIGHT"
            )
          ),
          html_tag(
            "div",
            class = "hero",
            html_tag(
              "div",
              class = "hero-main",
              html_tag("h1", match$query$name),
              html_tag(
                "div",
                class = "meta-row",
                html_tag(
                  "button",
                  type = "button",
                  class = "dna-id copyable",
                  `data-copy` = match$query$dna_id,
                  title = "Click to copy DNA ID",
                  match$query$dna_id
                ),
                html_tag("span", class = "meta-sep", "/"),
                html_tag("span", class = "meta-txt", toupper(gsub("_", " ", match$query$species, fixed = TRUE))),
                html_tag("span", class = "meta-sep", "/"),
                html_tag("span", class = "meta-txt", paste0(format(match$query$n_rows, big.mark = ","), " x ", match$query$n_cols))
              )
            ),
            html_tag(
              "div",
              class = "best-card",
              html_tag("span", "CLOSEST RELATIVE"),
              html_tag(
                "div",
                class = "best-score",
                html_tag("em", format_pct(best$overall)),
                html_tag("strong", best$name)
              )
            )
          ),
          html_tag(
            "div",
            class = "report-metrics",
            lineage_stat("Relation", best$relation, "relation"),
            lineage_stat("Similarity", format_pct(best$overall), "score"),
            lineage_stat("Best species", toupper(gsub("_", " ", best$species, fixed = TRUE)), "species"),
            lineage_stat("Candidates", nrow(match$matches), "count")
          ),
          html_tag(
            "div",
            class = "report-body",
            html_tag(
              "div",
              class = "lineage-panel lineage-graph-panel",
              html_tag(
                "div",
                class = "panel-head",
                html_tag("h2", "01  SIGNAL ROUTING & ELECTROPHORESIS"),
                html_tag("span", class = "panel-sub", "HOVER OR CLICK TO INSPECT")
              ),
              html_raw(lineage_svg(match))
            ),
            html_tag(
              "div",
              class = "lineage-panel lineage-rank",
              html_tag(
                "div",
                class = "panel-head",
                html_tag("h2", "02  CANDIDATE RANKING"),
                html_tag("span", class = "panel-sub", paste0(nrow(match$matches), " EVALUATED"))
              ),
              match_table(match)
            )
          ),
          html_tag(
            "div",
            class = "lineage-summary",
            html_tag(
              "div",
              class = "panel-head",
              html_tag("h2", "03  MUTATION TELEMETRY"),
              html_tag("span", class = "panel-sub", id = "summary-active-label", toupper(best$name))
            ),
            mutation_summary(match)
          )
        ),
        html_tag("div", class = "toast", id = "dna-toast", role = "status", `aria-live` = "polite")
      ),
      html_tag("script", type = "text/javascript", html_raw(match_card_js()))
    )
  )
  new_dna_html(doc, title = paste("DataDNA lineage", match$query$name))
}

lineage_stat <- function(label, value, kind) {
  html_tag(
    "div",
    class = paste("lineage-stat", kind),
    html_tag("span", label),
    html_tag("strong", as.character(value))
  )
}

short_label <- function(x, width = 26L) {
  x <- as.character(x)
  ifelse(nchar(x, type = "width") > width, paste0(substr(x, 1L, width - 1L), "..."), x)
}

svg_mini_gel <- function(x, y, w, h, seed_str, n_bars = 28L, bar_class = "gel-bar") {
  vals <- hash_to_unit_seq(seed_str, n_bars)
  slot <- w / n_bars
  rects <- character(n_bars)
  for (i in seq_len(n_bars)) {
    v <- vals[[i]]
    bh <- h * (0.25 + 0.75 * v)
    bw <- slot * (0.35 + 0.35 * v)
    bx <- x + (i - 0.5) * slot - bw / 2
    by <- y - bh / 2
    rects[[i]] <- sprintf(
      "<rect class='%s' x='%.1f' y='%.1f' width='%.1f' height='%.1f' opacity='%.2f'/>",
      bar_class, bx, by, bw, bh, 0.25 + 0.70 * v
    )
  }
  paste(rects, collapse = "")
}

lineage_svg <- function(match) {
  matches <- match$matches
  n <- nrow(matches)
  width <- 900
  height <- max(320, 100 + n * 86)
  query_x <- 34
  query_y <- height / 2
  query_w <- 224
  query_h <- 126
  cand_x <- 378
  cand_w <- 488
  cand_h <- 68
  cand_y <- if (n == 1L) height / 2 else seq(72, height - 72, length.out = n)

  lines <- character(n)
  nodes <- character(n)
  for (i in seq_len(n)) {
    score <- clip01(matches$overall[[i]])
    stroke <- if (i == 1L) 2.2 else (0.9 + 1.2 * score)
    label <- html_escape(short_label(matches$name[[i]], 20L), attr = TRUE)
    relation <- html_escape(matches$relation[[i]], attr = TRUE)
    pct <- format_pct(score)
    y_top <- cand_y[[i]] - cand_h / 2
    idx <- i - 1L
    cls_edge <- if (i == 1L) "edge-path is-best" else "edge-path"
    cls_node <- if (i == 1L) "cand-node is-best" else "cand-node"

    lines[[i]] <- sprintf(
      "<path class='%s' data-idx='%d' d='M %s %s C %s %s, %s %s, %s %s' stroke-width='%.1f' fill='none'/>",
      cls_edge, idx,
      query_x + query_w, query_y,
      query_x + query_w + 56, query_y,
      cand_x - 56, cand_y[[i]],
      cand_x, cand_y[[i]],
      stroke
    )

    gel_svg <- svg_mini_gel(
      cand_x + 216, cand_y[[i]], 182, 26,
      paste(matches$dna_id[[i]], matches$name[[i]]),
      n_bars = 32L,
      bar_class = if (i == 1L) "gel-bar best-bar" else "gel-bar"
    )

    nodes[[i]] <- sprintf(
      paste0(
        "<g class='%s' data-idx='%d' tabindex='0'>",
        "<rect class='node-box' x='%s' y='%s' width='%s' height='%s' rx='2'/>",
        "<rect class='node-accent' x='%s' y='%s' width='4' height='%s'/>",
        "<text x='%s' y='%s' class='node-title'>%s</text>",
        "<text x='%s' y='%s' class='node-sub'>%s</text>",
        "%s",
        "<text x='%s' y='%s' text-anchor='end' class='node-score'>%s</text>",
        "</g>"
      ),
      cls_node, idx,
      cand_x, y_top, cand_w, cand_h,
      cand_x, y_top, cand_h,
      cand_x + 18, cand_y[[i]] - 6, label,
      cand_x + 18, cand_y[[i]] + 14, relation,
      gel_svg,
      cand_x + cand_w - 16, cand_y[[i]] + 6, pct
    )
  }

  query_label <- html_escape(short_label(match$query$name, 18L), attr = TRUE)
  query_id <- html_escape(match$query$dna_id, attr = TRUE)
  query_gel <- svg_mini_gel(query_x + 16, query_y + 38, query_w - 32, 22, match$query$dna_id, n_bars = 28L, bar_class = "gel-bar query-bar")

  sprintf(
    paste0(
      "<svg class='lineage-svg' viewBox='0 0 %s %s' role='img' aria-label='DataDNA lineage graph'>",
      "<rect class='svg-bg' width='100%%' height='100%%' rx='2'/>",
      "<text x='%s' y='28' class='section-label'>QUERY MODULE</text>",
      "<text x='%s' y='28' class='section-label'>CANDIDATE ELECTROPHORESIS STRIPS</text>",
      "%s",
      "<g class='query-node'>",
      "<rect class='query-box' x='%s' y='%s' width='%s' height='%s' rx='2'/>",
      "<rect class='query-led' x='%s' y='%s' width='12' height='4'/>",
      "<text x='%s' y='%s' class='query-title'>%s</text>",
      "<text x='%s' y='%s' class='query-sub'>%s</text>",
      "%s",
      "</g>",
      "%s",
      "</svg>"
    ),
    width, height,
    query_x, cand_x,
    paste(lines, collapse = ""),
    query_x, query_y - query_h / 2, query_w, query_h,
    query_x + 16, query_y - query_h / 2 + 16,
    query_x + 16, query_y - 12, query_label,
    query_x + 16, query_y + 8, query_id,
    query_gel,
    paste(nodes, collapse = "")
  )
}

match_table <- function(match) {
  html_tag(
    "div",
    class = "rank-list",
    lapply(seq_len(nrow(match$matches)), function(i) {
      mutations <- match$matches$top_mutations[[i]]
      score <- clip01(match$matches$overall[[i]])
      html_tag(
        "article",
        class = if (i == 1L) "rank-card selected" else "rank-card",
        `data-idx` = as.character(i - 1L),
        `data-name` = toupper(match$matches$name[[i]]),
        `data-relation` = match$matches$relation[[i]],
        `data-sentence` = best_relative_sentence(match$matches[i, , drop = FALSE]),
        `data-mutations` = if (length(mutations)) paste(utils::head(mutations, 5L), collapse = "\n") else "No notable mutations detected.",
        html_tag(
          "div",
          class = "rank-topline",
          html_tag("span", class = "rank-index", sprintf("%02d", i)),
          html_tag("strong", match$matches$name[[i]]),
          html_tag("em", format_pct(score))
        ),
        html_tag(
          "div",
          class = "rank-meter",
          html_tag("i", style = paste0("width:", round(100 * score), "%;"))
        ),
        html_tag("p", match$matches$relation[[i]])
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

  html_tag(
    "div",
    class = "summary-grid",
    html_tag(
      "div",
      class = "summary-callout",
      html_tag("span", "INTERPRETATION"),
      html_tag("strong", id = "summary-relation", best$relation),
      html_tag("p", id = "summary-sentence", best_relative_sentence(best))
    ),
    html_tag(
      "ul",
      class = "summary-notes",
      id = "summary-notes",
      lapply(utils::head(notes, 4L), function(item) html_tag("li", item))
    )
  )
}

match_card_css <- function() {
  paste0(
    dna_card_css(),
    "
    .dna-shell { width: min(1160px, calc(100vw - 32px)); }
    .best-card { text-align: right; }
    .best-card > span { display: block; font-size: 10px; color: var(--ink-3); letter-spacing: .1em; }
    .best-score { display: flex; align-items: baseline; justify-content: flex-end; gap: 12px; margin-top: 4px; }
    .best-score em { color: var(--signal); font-size: 34px; line-height: 1; font-style: normal; font-weight: 700; }
    .best-score strong { font-size: 13px; color: var(--ink); font-weight: 700; }
    .report-metrics {
      display: grid;
      grid-template-columns: 1.4fr .7fr 1fr .65fr;
      gap: 1px;
      background: var(--border);
      border-bottom: 1px solid var(--border);
    }
    .lineage-stat { padding: 14px 28px; background: var(--surface); }
    .lineage-stat span { display: block; font-size: 10px; color: var(--ink-3); letter-spacing: .1em; text-transform: uppercase; }
    .lineage-stat strong { display: block; margin-top: 4px; font-size: 13px; color: var(--ink); font-weight: 700; }
    .report-body {
      display: grid;
      grid-template-columns: minmax(0, 1.55fr) minmax(260px, .65fr);
      gap: 24px;
      padding: 24px 28px;
    }
    .lineage-svg { display: block; width: 100%; height: auto; min-height: 300px; }
    .svg-bg { fill: var(--well); stroke: var(--border); stroke-width: 1; }
    .section-label { font: 700 10px 'SF Mono', ui-monospace, Consolas, monospace; fill: var(--ink-3); letter-spacing: .08em; }
    .query-box { fill: var(--ink); }
    .query-led { fill: var(--signal); }
    .query-title { font: 700 15px -apple-system, BlinkMacSystemFont, 'Inter', sans-serif; fill: var(--plate); }
    .query-sub { font: 11px 'SF Mono', ui-monospace, Consolas, monospace; fill: var(--ink-3); }
    .query-bar { fill: var(--plate); }
    .node-box { fill: var(--surface); stroke: var(--border); stroke-width: 1; transition: stroke .14s ease; }
    .node-accent { fill: var(--ink-3); transition: fill .14s ease; }
    .cand-node.is-best .node-accent, .cand-node.active .node-accent { fill: var(--signal); }
    .cand-node.active .node-box { stroke: var(--ink); stroke-width: 1.5; }
    .node-title { font: 700 13.5px -apple-system, BlinkMacSystemFont, 'Inter', sans-serif; fill: var(--ink); }
    .node-sub { font: 11px -apple-system, BlinkMacSystemFont, 'Inter', sans-serif; fill: var(--ink-2); }
    .node-score { font: 700 17px 'SF Mono', ui-monospace, Consolas, monospace; fill: var(--ink); }
    .cand-node.is-best .node-score, .cand-node.active .node-score { fill: var(--signal); }
    .gel-bar { fill: var(--ink); }
    .gel-bar.best-bar { fill: var(--signal); }
    .edge-path { stroke: var(--ink); opacity: .45; transition: opacity .14s ease, stroke .14s ease; }
    .edge-path.is-best, .edge-path.active { stroke: var(--signal); opacity: 1; }
    .edge-path.dimmed { opacity: .12; }
    .cand-node { cursor: pointer; transition: opacity .14s ease; }
    .cand-node.dimmed { opacity: .3; }
    .rank-list { display: grid; gap: 8px; }
    .rank-card {
      padding: 12px 14px;
      border: 1px solid var(--border);
      border-radius: 2px;
      background: var(--surface);
      cursor: pointer;
      transition: border-color .14s ease, opacity .14s ease;
    }
    .rank-card:hover { border-color: var(--ink); }
    .rank-card.selected { border-color: var(--ink); border-left: 3px solid var(--signal); }
    .rank-card.dimmed { opacity: .38; }
    .rank-topline { display: grid; grid-template-columns: 24px minmax(0, 1fr) auto; gap: 8px; align-items: baseline; }
    .rank-index { font-size: 11px; color: var(--ink-3); }
    .rank-topline strong { font-family: -apple-system, BlinkMacSystemFont, 'Inter', sans-serif; font-size: 13.5px; color: var(--ink); overflow-wrap: anywhere; }
    .rank-topline em { font-style: normal; font-size: 13px; font-weight: 700; color: var(--ink); }
    .rank-card.selected .rank-topline em { color: var(--signal); }
    .rank-meter { height: 2px; margin: 9px 0 7px; background: var(--border); }
    .rank-meter i { display: block; height: 100%; background: var(--ink); }
    .rank-card.selected .rank-meter i { background: var(--signal); }
    .rank-card p { margin: 0; font-size: 11px; color: var(--ink-2); }
    .lineage-summary { padding: 0 28px 28px; }
    .summary-grid { display: grid; grid-template-columns: minmax(0, .85fr) minmax(0, 1.15fr); gap: 20px; }
    .summary-callout { padding: 16px; border: 1px solid var(--border); background: var(--surface); }
    .summary-callout span { font-size: 10px; color: var(--ink-3); letter-spacing: .1em; }
    .summary-callout strong { display: block; margin-top: 6px; font-family: -apple-system, BlinkMacSystemFont, 'Inter', sans-serif; font-size: 16px; }
    .summary-callout p { margin: 8px 0 0; font-size: 12px; color: var(--ink-2); line-height: 1.5; }
    .summary-notes { margin: 0; padding: 0; list-style: none; display: grid; gap: 6px; }
    .summary-notes li { padding: 9px 12px; border-left: 2px solid var(--signal); background: var(--surface); font-size: 12px; color: var(--ink-2); }
    @media (max-width: 900px) {
      .report-body, .summary-grid { grid-template-columns: 1fr; }
      .report-metrics { grid-template-columns: repeat(2, 1fr); }
    }
    "
  )
}

match_card_js <- function() {
  "
  (function() {
    var root = document.documentElement;
    var toggle = document.getElementById('theme-toggle');
    if (toggle) {
      toggle.addEventListener('click', function() {
        var next = root.getAttribute('data-theme') === 'dark' ? 'light' : 'dark';
        root.setAttribute('data-theme', next);
        toggle.textContent = next === 'dark' ? 'MODE: DARK' : 'MODE: LIGHT';
      });
    }

    var toast = document.getElementById('dna-toast');
    var timer = null;
    function showToast(msg) {
      if (!toast) return;
      toast.textContent = msg;
      toast.classList.add('show');
      clearTimeout(timer);
      timer = setTimeout(function() { toast.classList.remove('show'); }, 1400);
    }

    document.querySelectorAll('.copyable').forEach(function(el) {
      el.addEventListener('click', function() {
        var val = el.getAttribute('data-copy') || '';
        if (navigator.clipboard && navigator.clipboard.writeText) {
          navigator.clipboard.writeText(val).then(function() { showToast('COPIED ' + val); });
        } else {
          showToast(val);
        }
      });
    });

    var nodes = document.querySelectorAll('.cand-node');
    var edges = document.querySelectorAll('.edge-path');
    var cards = document.querySelectorAll('.rank-card');
    var activeLabel = document.getElementById('summary-active-label');
    var sumRel = document.getElementById('summary-relation');
    var sumSent = document.getElementById('summary-sentence');
    var sumNotes = document.getElementById('summary-notes');
    var selectedIdx = '0';

    function setHover(idx) {
      var target = idx !== null ? String(idx) : null;
      nodes.forEach(function(n) {
        var m = n.getAttribute('data-idx') === target;
        n.classList.toggle('active', target !== null ? m : n.getAttribute('data-idx') === selectedIdx);
        n.classList.toggle('dimmed', target !== null && !m);
      });
      edges.forEach(function(e) {
        var m = e.getAttribute('data-idx') === target;
        e.classList.toggle('active', target !== null ? m : e.getAttribute('data-idx') === selectedIdx);
        e.classList.toggle('dimmed', target !== null && !m);
      });
      cards.forEach(function(c) {
        var m = c.getAttribute('data-idx') === target;
        c.classList.toggle('dimmed', target !== null && !m);
      });
    }

    function selectCandidate(idx) {
      selectedIdx = String(idx);
      cards.forEach(function(c) {
        var isSel = c.getAttribute('data-idx') === selectedIdx;
        c.classList.toggle('selected', isSel);
        if (isSel) {
          if (activeLabel) activeLabel.textContent = c.getAttribute('data-name') || '';
          if (sumRel) sumRel.textContent = c.getAttribute('data-relation') || '';
          if (sumSent) sumSent.textContent = c.getAttribute('data-sentence') || '';
          if (sumNotes) {
            var lines = (c.getAttribute('data-mutations') || '').split('\\n').filter(Boolean);
            sumNotes.innerHTML = '';
            lines.forEach(function(line) {
              var li = document.createElement('li');
              li.textContent = line;
              sumNotes.appendChild(li);
            });
          }
        }
      });
      setHover(null);
    }

    nodes.forEach(function(n) {
      var idx = n.getAttribute('data-idx');
      n.addEventListener('mouseenter', function() { setHover(idx); });
      n.addEventListener('mouseleave', function() { setHover(null); });
      n.addEventListener('click', function() { selectCandidate(idx); });
    });

    cards.forEach(function(c) {
      var idx = c.getAttribute('data-idx');
      c.addEventListener('mouseenter', function() { setHover(idx); });
      c.addEventListener('mouseleave', function() { setHover(null); });
      c.addEventListener('click', function() { selectCandidate(idx); });
    });

    setHover(null);
  })();
  "
}
