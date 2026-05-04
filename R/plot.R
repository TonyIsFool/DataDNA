#' Draw a paper-style lineage figure
#'
#' Creates a print-friendly, paper-style lineage figure for a `dna_match` object
#' using base R grid graphics. The figure can be drawn on the current graphics
#' device or saved directly to PNG or PDF.
#'
#' @param match A `dna_match` object.
#' @param file Optional output path. Supported extensions are `.png` and `.pdf`.
#' @param width Plot width in inches.
#' @param height Plot height in inches.
#' @param dpi Resolution used for PNG output.
#'
#' @return The input `dna_match` object, invisibly.
#' @export
#'
#' @examples
#' demo <- dna_example_customers()
#' lib <- list(old = data_dna(demo$customers_old), new = data_dna(demo$customers_new))
#' match <- dna_match(demo$customers_new, lib)
#' dna_match_plot(match)
dna_match_plot <- function(match, file = NULL, width = 11, height = 7, dpi = 144) {
  if (!inherits(match, "dna_match")) {
    stop("`match` must be a dna_match object.", call. = FALSE)
  }

  if (!is.null(file)) {
    ext <- tolower(tools::file_ext(file))
    if (!ext %in% c("png", "pdf")) {
      stop("`file` must end in .png or .pdf.", call. = FALSE)
    }
    if (ext == "png") {
      grDevices::png(file, width = width, height = height, units = "in", res = dpi, bg = "#ffffff")
    } else {
      grDevices::pdf(file, width = width, height = height, bg = "#ffffff", useDingbats = FALSE)
    }
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  draw_match_report(match)
  invisible(match)
}

draw_match_report <- function(match) {
  grid::grid.newpage()
  layout <- report_layout()
  palette <- report_palette()
  best <- match$matches[1L, , drop = FALSE]

  grid::grid.rect(gp = grid::gpar(fill = palette$background, col = NA))
  draw_compact_header(match, best, layout, palette)
  draw_lineage_figure(match, layout, palette)
  draw_compact_table(match, layout, palette)
  draw_numbered_notes(match, layout, palette)
}

report_layout <- function() {
  list(
    margin_left = 0.055,
    margin_right = 0.055,
    header_y = 0.91,
    meta_y = 0.855,
    graph = c(x = 0.055, y = 0.25, w = 0.60, h = 0.50),
    table = c(x = 0.69, y = 0.25, w = 0.255, h = 0.50),
    notes = c(x = 0.055, y = 0.06, w = 0.89, h = 0.13)
  )
}

draw_compact_header <- function(match, best, layout, palette) {
  grid::grid.text(
    "Figure 1. DataDNA lineage match",
    x = grid::unit(layout$margin_left, "npc"),
    y = grid::unit(layout$header_y, "npc"),
    just = c("left", "center"),
    gp = grid::gpar(col = palette$text, fontsize = 16, fontface = "bold", fontfamily = report_font())
  )

  grid::grid.text(
    paste0(
      "Query: ",
      match$query$name,
      "   |   ",
      match$query$species,
      "   |   ",
      format(match$query$n_rows, big.mark = ","),
      " x ",
      match$query$n_cols
    ),
    x = grid::unit(layout$margin_left, "npc"),
    y = grid::unit(layout$meta_y, "npc"),
    just = c("left", "center"),
    gp = grid::gpar(col = palette$muted, fontsize = 9.5, fontfamily = report_font())
  )
  grid::grid.text(
    paste0("DNA ID  ", match$query$dna_id),
    x = grid::unit(layout$margin_left, "npc"),
    y = grid::unit(layout$meta_y - 0.032, "npc"),
    just = c("left", "center"),
    gp = grid::gpar(col = palette$faint, fontsize = 8.6, fontfamily = report_mono_font())
  )

  draw_panel(0.70, 0.842, 0.245, 0.095, palette)
  grid::grid.text(
    "Best match",
    x = grid::unit(0.718, "npc"),
    y = grid::unit(0.908, "npc"),
    just = c("left", "center"),
    gp = grid::gpar(col = palette$muted, fontsize = 8.5, fontface = "bold", fontfamily = report_font())
  )
  grid::grid.text(
    plot_short_label(best$name, 20L),
    x = grid::unit(0.718, "npc"),
    y = grid::unit(0.882, "npc"),
    just = c("left", "center"),
    gp = grid::gpar(col = palette$text, fontsize = 12, fontface = "bold", fontfamily = report_font())
  )
  grid::grid.text(
    format_pct(best$overall),
    x = grid::unit(0.925, "npc"),
    y = grid::unit(0.884, "npc"),
    just = c("right", "center"),
    gp = grid::gpar(col = palette$accent, fontsize = 17, fontface = "bold", fontfamily = report_font())
  )
  grid::grid.text(
    plot_short_label(best$relation, 32L),
    x = grid::unit(0.718, "npc"),
    y = grid::unit(0.858, "npc"),
    just = c("left", "center"),
    gp = grid::gpar(col = palette$muted, fontsize = 8.5, fontfamily = report_font())
  )
}

draw_lineage_figure <- function(match, layout, palette) {
  g <- layout$graph
  draw_panel(g[["x"]], g[["y"]], g[["w"]], g[["h"]], palette)

  grid::grid.text(
    "A. Similarity-weighted lineage",
    x = grid::unit(g[["x"]] + 0.025, "npc"),
    y = grid::unit(g[["y"]] + g[["h"]] - 0.04, "npc"),
    just = c("left", "center"),
    gp = grid::gpar(col = palette$text, fontsize = 11.5, fontface = "bold", fontfamily = report_font())
  )

  matches <- match$matches
  n <- nrow(matches)
  qx <- g[["x"]] + 0.055
  qy <- g[["y"]] + g[["h"]] * 0.48
  qw <- 0.19
  qh <- 0.105
  cx <- g[["x"]] + 0.355
  cw <- 0.205
  ch <- 0.076
  cy <- if (n == 1L) qy else seq(g[["y"]] + g[["h"]] - 0.115, g[["y"]] + 0.10, length.out = n)

  for (i in seq_len(n)) {
    score <- matches$overall[[i]]
    col <- relation_color_static(score)
    grid::grid.xspline(
      x = grid::unit(c(qx + qw, qx + qw + 0.07, cx - 0.07, cx), "npc"),
      y = grid::unit(c(qy, qy, cy[[i]], cy[[i]]), "npc"),
      shape = 1,
      gp = grid::gpar(col = col, lwd = 0.8 + 2.2 * clip01(score), alpha = 0.82)
    )
  }

  draw_lineage_node(qx, qy, qw, qh, match$query$name, match$query$dna_id, match$query$species, palette$accent, palette, TRUE)
  for (i in seq_len(n)) {
    score <- matches$overall[[i]]
    draw_lineage_node(
      cx,
      cy[[i]],
      cw,
      ch,
      matches$name[[i]],
      plot_relation_label(matches$relation[[i]]),
      format_pct(score),
      relation_color_static(score),
      palette,
      FALSE
    )
  }
}

draw_compact_table <- function(match, layout, palette) {
  t <- layout$table
  draw_panel(t[["x"]], t[["y"]], t[["w"]], t[["h"]], palette)

  grid::grid.text(
    "B. Ranked candidates",
    x = grid::unit(t[["x"]] + 0.018, "npc"),
    y = grid::unit(t[["y"]] + t[["h"]] - 0.04, "npc"),
    just = c("left", "center"),
    gp = grid::gpar(col = palette$text, fontsize = 11.5, fontface = "bold", fontfamily = report_font())
  )

  header_y <- t[["y"]] + t[["h"]] - 0.085
  col_x <- c(t[["x"]] + 0.018, t[["x"]] + 0.07, t[["x"]] + t[["w"]] - 0.018)
  headers <- c("#", "candidate", "similarity")
  justs <- list(c("left", "center"), c("left", "center"), c("right", "center"))
  for (i in seq_along(headers)) {
    grid::grid.text(
      headers[[i]],
      x = grid::unit(col_x[[i]], "npc"),
      y = grid::unit(header_y, "npc"),
      just = justs[[i]],
      gp = grid::gpar(col = palette$muted, fontsize = 8, fontface = "bold", fontfamily = report_font())
    )
  }
  grid::grid.lines(
    x = grid::unit(c(t[["x"]] + 0.018, t[["x"]] + t[["w"]] - 0.018), "npc"),
    y = grid::unit(c(header_y - 0.025, header_y - 0.025), "npc"),
    gp = grid::gpar(col = palette$border, lwd = 1)
  )

  matches <- match$matches
  row_y <- header_y - 0.055
  row_gap <- min(0.084, (t[["h"]] - 0.15) / max(nrow(matches), 1L))
  for (i in seq_len(nrow(matches))) {
    score <- matches$overall[[i]]
    grid::grid.text(sprintf("%02d", i), x = grid::unit(col_x[[1L]], "npc"), y = grid::unit(row_y, "npc"), just = justs[[1L]], gp = grid::gpar(col = palette$faint, fontsize = 8.5, fontfamily = report_mono_font()))
    grid::grid.text(plot_short_label(matches$name[[i]], 19L), x = grid::unit(col_x[[2L]], "npc"), y = grid::unit(row_y + 0.01, "npc"), just = justs[[2L]], gp = grid::gpar(col = palette$text, fontsize = 9.2, fontface = "bold", fontfamily = report_font()))
    grid::grid.text(format_pct(score), x = grid::unit(col_x[[3L]], "npc"), y = grid::unit(row_y + 0.01, "npc"), just = justs[[3L]], gp = grid::gpar(col = relation_color_static(score), fontsize = 10, fontface = "bold", fontfamily = report_font()))
    grid::grid.text(plot_relation_label(matches$relation[[i]]), x = grid::unit(col_x[[2L]], "npc"), y = grid::unit(row_y - 0.015, "npc"), just = justs[[2L]], gp = grid::gpar(col = palette$muted, fontsize = 8.1, fontfamily = report_font()))

    meter_x <- t[["x"]] + 0.07
    meter_w <- t[["w"]] - 0.088
    grid::grid.rect(x = grid::unit(meter_x, "npc"), y = grid::unit(row_y - 0.04, "npc"), width = grid::unit(meter_w, "npc"), height = grid::unit(0.003, "npc"), just = c("left", "center"), gp = grid::gpar(fill = palette$panel, col = NA))
    grid::grid.rect(x = grid::unit(meter_x, "npc"), y = grid::unit(row_y - 0.04, "npc"), width = grid::unit(meter_w * clip01(score), "npc"), height = grid::unit(0.003, "npc"), just = c("left", "center"), gp = grid::gpar(fill = relation_color_static(score), col = NA))

    row_y <- row_y - row_gap
  }
}

draw_numbered_notes <- function(match, layout, palette) {
  n <- layout$notes
  best <- match$matches[1L, , drop = FALSE]
  notes <- best$top_mutations[[1L]]
  if (!length(notes)) {
    notes <- "No notable mutations detected for the closest relative."
  }
  notes <- utils::head(notes, 3L)

  draw_panel(n[["x"]], n[["y"]], n[["w"]], n[["h"]], palette)
  grid::grid.text(
    "C. Mutation notes",
    x = grid::unit(n[["x"]] + 0.025, "npc"),
    y = grid::unit(n[["y"]] + n[["h"]] - 0.035, "npc"),
    just = c("left", "center"),
    gp = grid::gpar(col = palette$text, fontsize = 11.5, fontface = "bold", fontfamily = report_font())
  )
  grid::grid.text(
    paste0(best$name, " is the nearest non-self relative (", format_pct(best$overall), " similarity)."),
    x = grid::unit(n[["x"]] + 0.025, "npc"),
    y = grid::unit(n[["y"]] + n[["h"]] - 0.07, "npc"),
    just = c("left", "center"),
    gp = grid::gpar(col = palette$muted, fontsize = 9, fontfamily = report_font())
  )

  y <- n[["y"]] + 0.035
  for (i in seq_along(notes)) {
    x <- n[["x"]] + 0.025 + (i - 1L) * (n[["w"]] - 0.05) / length(notes)
    grid::grid.text(
      paste0(i, ". ", plot_short_label(notes[[i]], 34L)),
      x = grid::unit(x, "npc"),
      y = grid::unit(y, "npc"),
      just = c("left", "center"),
      gp = grid::gpar(col = palette$text, fontsize = 8.8, fontfamily = report_font())
    )
  }
}

draw_lineage_node <- function(x, y, w, h, title, subtitle, meta, accent, palette, query = FALSE) {
  fill <- if (query) palette$panel_dark else palette$background
  text_col <- if (query) palette$text_reverse else palette$text
  sub_col <- if (query) "#c9d3dd" else palette$muted
  draw_round_rect(x, y - h / 2, w, h, fill = fill, col = accent)
  grid::grid.rect(x = grid::unit(x, "npc"), y = grid::unit(y, "npc"), width = grid::unit(0.0035, "npc"), height = grid::unit(h - 0.03, "npc"), just = c("left", "center"), gp = grid::gpar(fill = accent, col = NA))
  grid::grid.text(plot_short_label(title, 22L), x = grid::unit(x + 0.016, "npc"), y = grid::unit(y + h * 0.18, "npc"), just = c("left", "center"), gp = grid::gpar(col = text_col, fontsize = if (query) 10.5 else 9.2, fontface = "bold", fontfamily = report_font()))
  grid::grid.text(plot_short_label(subtitle, if (query) 27L else 30L), x = grid::unit(x + 0.016, "npc"), y = grid::unit(y - h * 0.03, "npc"), just = c("left", "center"), gp = grid::gpar(col = sub_col, fontsize = 7.6, fontfamily = if (query) report_mono_font() else report_font()))
  grid::grid.text(plot_short_label(meta, 26L), x = grid::unit(x + 0.016, "npc"), y = grid::unit(y - h * 0.25, "npc"), just = c("left", "center"), gp = grid::gpar(col = sub_col, fontsize = 7.6, fontfamily = report_font()))
}

draw_panel <- function(x, y, w, h, palette) {
  draw_round_rect(x, y, w, h, fill = palette$background, col = palette$border)
}

draw_round_rect <- function(x, y, w, h, fill, col) {
  grid::grid.roundrect(
    x = grid::unit(x, "npc"),
    y = grid::unit(y, "npc"),
    width = grid::unit(w, "npc"),
    height = grid::unit(h, "npc"),
    just = c("left", "bottom"),
    r = grid::unit(0.006, "npc"),
    gp = grid::gpar(fill = fill, col = col, lwd = 0.8)
  )
}

relation_color_static <- function(score) {
  if (score >= 0.97) {
    "#2f9e7e"
  } else if (score >= 0.85) {
    "#2b7bb9"
  } else if (score >= 0.65) {
    "#b08a1d"
  } else {
    "#8a949e"
  }
}

plot_short_label <- function(x, width = 26L) {
  x <- as.character(x)
  ifelse(nchar(x, type = "width") > width, paste0(substr(x, 1L, width - 1L), "..."), x)
}

plot_relation_label <- function(x) {
  x <- as.character(x)
  out <- x
  out[grepl("near-identical", x, fixed = TRUE)] <- "near-identical version"
  out[grepl("changed version", x, fixed = TRUE)] <- "changed version"
  out[grepl("related dataset", x, fixed = TRUE)] <- "related dataset"
  out[grepl("different dataset", x, fixed = TRUE)] <- "different dataset"
  out
}

report_palette <- function() {
  list(
    text = "#1f2933",
    text_reverse = "#f8fafc",
    muted = "#6b7280",
    faint = "#9aa8b5",
    accent = "#2b7bb9",
    border = "#d7dde3",
    background = "#ffffff",
    panel = "#f8fafc",
    panel_dark = "#111827"
  )
}

report_font <- function() {
  "sans"
}

report_mono_font <- function() {
  "mono"
}
