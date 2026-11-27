#' Draw a paper-style lineage figure
#'
#' Creates a minimalist precision-instrument lineage figure with DNA
#' electrophoresis bands for a `dna_match` object using base R grid graphics.
#' The figure can be drawn on the current graphics device or saved directly to
#' PNG, PDF, or SVG.
#'
#' @param match A `dna_match` object.
#' @param file Optional output path. Supported extensions are `.png`, `.pdf`,
#'   and `.svg`.
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
dna_match_plot <- function(match, file = NULL, width = 11, height = 6.8, dpi = 200) {
  if (!inherits(match, "dna_match")) {
    stop("`match` must be a dna_match object.", call. = FALSE)
  }

  if (!is.null(file)) {
    open_report_device(file = file, width = width, height = height, dpi = dpi)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  draw_match_report(match)
  invisible(match)
}

#' Draw a paper-style specimen profile figure
#'
#' Creates a precision-instrument static figure summarizing a single data frame
#' or `data_dna` profile using a high-density DNA electrophoresis band,
#' calibrated column signal tracks, and fingerprint telemetry.
#'
#' @param x A data frame or `data_dna` object.
#' @param file Optional output path (`.png`, `.pdf`, or `.svg`).
#' @param width Plot width in inches.
#' @param height Plot height in inches.
#' @param dpi Resolution used for PNG output.
#'
#' @return The `data_dna` object, invisibly.
#' @export
#'
#' @examples
#' demo <- dna_example_customers()
#' dna_plot(demo$customers_new)
dna_plot <- function(x, file = NULL, width = 10.8, height = 6.8, dpi = 200) {
  dna <- as_data_dna(x, name = paste(deparse(substitute(x)), collapse = " "))

  if (!is.null(file)) {
    open_report_device(file = file, width = width, height = height, dpi = dpi)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  draw_specimen_report(dna)
  invisible(dna)
}

#' Draw a paper-style drift and mutation figure
#'
#' Visualizes structural and distributional drift between two data sets (or an
#' existing `dna_comparison` / `dna_diff` object) using a dual-lane DNA
#' electrophoresis alignment spectrum and precision locus readouts.
#'
#' @param x A data frame, `data_dna`, `dna_comparison`, or `dna_diff` object.
#' @param y Optional second data frame or `data_dna` object when `x` is a data
#'   frame or `data_dna` profile.
#' @param file Optional output path (`.png`, `.pdf`, or `.svg`).
#' @param width Plot width in inches.
#' @param height Plot height in inches.
#' @param dpi Resolution used for PNG output.
#'
#' @return A `dna_comparison` object (or `x` when `x` is a `dna_diff`),
#'   invisibly.
#' @export
#'
#' @examples
#' demo <- dna_example_customers()
#' dna_diff_plot(demo$customers_old, demo$customers_new)
dna_diff_plot <- function(x, y = NULL, file = NULL, width = 10.8, height = 6.8, dpi = 200) {
  if (!is.null(y)) {
    x_dna <- as_data_dna(x, name = paste(deparse(substitute(x)), collapse = " "))
    y_dna <- as_data_dna(y, name = paste(deparse(substitute(y)), collapse = " "))
    comp <- dna_compare(x_dna, y_dna)
    comp$x_name <- x_dna$name
    comp$y_name <- y_dna$name
    diff_obj <- dna_diff(x_dna, y_dna)
    diff_obj$x_name <- x_dna$name
    diff_obj$y_name <- y_dna$name
    ret <- comp
  } else if (inherits(x, "dna_comparison")) {
    comp <- x
    changes <- if (length(x$top_mutations)) {
      data.frame(
        column = rep(NA_character_, length(x$top_mutations)),
        type = rep("mutation", length(x$top_mutations)),
        severity = rep("medium", length(x$top_mutations)),
        message = x$top_mutations,
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(column = character(), type = character(), severity = character(), message = character(), stringsAsFactors = FALSE)
    }
    diff_obj <- structure(
      list(x_name = x$x_name, y_name = x$y_name, changes = changes, renames = x$renames),
      class = "dna_diff"
    )
    ret <- comp
  } else if (inherits(x, "dna_diff")) {
    comp <- NULL
    diff_obj <- x
    ret <- diff_obj
  } else {
    stop("Supply two data sets (`x`, `y`) or a `dna_comparison` / `dna_diff` object.", call. = FALSE)
  }

  if (!is.null(file)) {
    open_report_device(file = file, width = width, height = height, dpi = dpi)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  draw_diff_report(comp, diff_obj)
  invisible(ret)
}

open_report_device <- function(file, width, height, dpi) {
  dir_path <- dirname(file)
  if (!nzchar(dir_path) || !dir.exists(dir_path)) {
    stop("Directory does not exist: ", dir_path, call. = FALSE)
  }
  ext <- tolower(tools::file_ext(file))
  if (!ext %in% c("png", "pdf", "svg")) {
    stop("`file` must end in .png, .pdf, or .svg.", call. = FALSE)
  }
  bg <- report_palette()$canvas
  if (ext == "png") {
    grDevices::png(file, width = width, height = height, units = "in", res = dpi, bg = bg)
  } else if (ext == "pdf") {
    grDevices::pdf(file, width = width, height = height, bg = bg, useDingbats = FALSE)
  } else {
    if (!isTRUE(suppressWarnings(capabilities("cairo")))) {
      stop("SVG output requires Cairo graphics support (e.g., XQuartz on macOS).", call. = FALSE)
    }
    grDevices::svg(file, width = width, height = height, bg = bg)
  }
}

# ==============================================================================
# 1. DRIFT & MUTATION INSTRUMENT (Dual-Lane Electrophoresis Alignment)
# ==============================================================================

draw_diff_report <- function(comp, diff_obj) {
  grid::grid.newpage()
  pal <- report_palette()
  draw_instrument_frame(pal)

  n_changes <- nrow(diff_obj$changes)
  n_renames <- if (!is.null(diff_obj$renames)) nrow(diff_obj$renames) else 0L

  kpi_val <- if (!is.null(comp)) format_pct(comp$overall) else as.character(n_changes)
  kpi_lbl <- if (!is.null(comp)) "ALIGNMENT" else "MUTATIONS"
  kpi_sub <- if (!is.null(comp)) toupper(plot_relation_label(comp$likely_relation)) else paste0(n_renames, " RENAMES")
  kpi_col <- if (!is.null(comp) && is.finite(comp$overall) && comp$overall < 0.85) pal$signal else pal$ink

  draw_instrument_header(
    code = "DATADNA // SEQ-DIFF",
    title = paste0(plot_short_label(diff_obj$x_name, 20L), "  ->  ", plot_short_label(diff_obj$y_name, 20L)),
    meta = paste0(n_changes, " MUTATIONS   /   ", n_renames, " LIKELY RENAMES"),
    kpi_label = kpi_lbl,
    kpi_value = kpi_val,
    kpi_sub = kpi_sub,
    kpi_col = kpi_col,
    pal = pal
  )

  # --- HERO: Dual-Lane DNA Electrophoresis Spectrum (y = 0.43 to 0.77) ---
  ml <- 0.055; mr <- 0.945; fw <- mr - ml
  draw_section_tag("01", "ELECTROPHORESIS ALIGNMENT SPECTRUM", ml, 0.775, pal)

  # Extract 9-dimension scores or fallback uniform profile
  dim_names <- c("schema", "shape", "species", "role_structure", "distribution", "missingness", "category", "identity", "temporal")
  dim_codes <- c("SCH", "SHP", "SPC", "ROL", "DST", "MIS", "CAT", "IDN", "TMP")
  if (!is.null(comp) && length(comp$scores)) {
    sc_vec <- vapply(dim_names, function(nm) {
      val <- comp$scores[nm]
      if (length(val) == 1L && is.finite(val)) clip01(as.numeric(val)) else NA_real_
    }, numeric(1))
  } else {
    sc_vec <- rep(0.8, length(dim_names))
  }

  # Calibrated gel well background
  well_y0 <- 0.455; well_y1 <- 0.742
  grid::grid.rect(
    x = grid::unit(ml, "npc"),
    y = grid::unit(well_y0, "npc"),
    width = grid::unit(fw, "npc"),
    height = grid::unit(well_y1 - well_y0, "npc"),
    just = c("left", "bottom"),
    gp = grid::gpar(fill = pal$well, col = pal$border, lwd = 0.6)
  )

  # Lane labels on left margin inside well
  lane_x0 <- ml + 0.075
  lane_x1 <- mr - 0.018
  lane_w <- lane_x1 - lane_x0
  mid_y <- 0.608

  grid::grid.text("BASE", x = grid::unit(ml + 0.014, "npc"), y = grid::unit(mid_y + 0.068, "npc"), just = c("left", "center"), gp = grid::gpar(col = pal$muted, fontsize = 7.2, fontface = "bold", fontfamily = report_mono_font()))
  grid::grid.text("CAND", x = grid::unit(ml + 0.014, "npc"), y = grid::unit(mid_y - 0.068, "npc"), just = c("left", "center"), gp = grid::gpar(col = pal$muted, fontsize = 7.2, fontface = "bold", fontfamily = report_mono_font()))

  # Center calibration ruler
  draw_ruler_axis(lane_x0, lane_x1, mid_y, n_major = 9L, n_minor = 90L, pal = pal)

  # High-density deterministic electrophoresis bands (9 loci x 10 sub-bands = 90 bands)
  n_loci <- length(dim_names)
  sub_per_locus <- 10L
  n_bands <- n_loci * sub_per_locus
  seed_base <- hash_to_unit_seq(paste(diff_obj$x_name, paste(dim_names, collapse = "")), n_bands)
  seed_cand <- hash_to_unit_seq(paste(diff_obj$y_name, n_changes, n_renames), n_bands)

  slot_w <- lane_w / n_bands
  band_w <- slot_w * 0.48
  max_h <- 0.090

  for (b in seq_len(n_bands)) {
    locus <- ((b - 1L) %/% sub_per_locus) + 1L
    align <- sc_vec[[locus]]
    if (!is.finite(align)) align <- 0.5
    drift <- 1 - align

    bx <- lane_x0 + (b - 0.5) * slot_w
    # Baseline band (extends upward from mid_y + 0.012)
    h_base <- (0.18 + 0.82 * seed_base[[b]]) * max_h
    a_base <- 0.22 + 0.72 * seed_base[[b]]
    grid::grid.rect(
      x = grid::unit(bx, "npc"),
      y = grid::unit(mid_y + 0.012, "npc"),
      width = grid::unit(band_w, "npc"),
      height = grid::unit(h_base, "npc"),
      just = c("center", "bottom"),
      gp = grid::gpar(fill = pal$ink, col = NA, alpha = a_base)
    )

    # Candidate band (extends downward from mid_y - 0.012)
    # High alignment = faithful mirror; low alignment = fractured height + Signal Orange flare
    perturbed <- (1 - drift) * seed_base[[b]] + drift * seed_cand[[b]]
    h_cand <- (0.18 + 0.82 * perturbed) * max_h
    is_drifted_band <- drift > 0.15 && (abs(seed_base[[b]] - seed_cand[[b]]) > (0.55 - drift * 0.45))
    col_cand <- if (is_drifted_band) pal$signal else pal$ink
    a_cand <- if (is_drifted_band) min(0.95, 0.55 + drift * 0.5) else (0.22 + 0.72 * perturbed)

    grid::grid.rect(
      x = grid::unit(bx, "npc"),
      y = grid::unit(mid_y - 0.012, "npc"),
      width = grid::unit(band_w, "npc"),
      height = grid::unit(h_cand, "npc"),
      just = c("center", "top"),
      gp = grid::gpar(fill = col_cand, col = NA, alpha = a_cand)
    )
  }

  # Locus labels & exact numeric alignment ticks at bottom of gel well
  locus_w <- lane_w / n_loci
  for (i in seq_len(n_loci)) {
    lx <- lane_x0 + (i - 0.5) * locus_w
    val <- sc_vec[[i]]
    is_alert <- is.finite(val) && val < 0.80
    # Vertical hairline separator between loci
    if (i > 1L) {
      sx <- lane_x0 + (i - 1L) * locus_w
      grid::grid.lines(
        x = grid::unit(c(sx, sx), "npc"),
        y = grid::unit(c(well_y0 + 0.008, well_y1 - 0.008), "npc"),
        gp = grid::gpar(col = pal$border, lwd = 0.45, lty = "dotted")
      )
    }
    grid::grid.text(
      dim_codes[[i]],
      x = grid::unit(lx, "npc"),
      y = grid::unit(well_y1 - 0.014, "npc"),
      just = c("center", "center"),
      gp = grid::gpar(col = if (is_alert) pal$signal else pal$muted, fontsize = 7.0, fontface = "bold", fontfamily = report_mono_font())
    )
    grid::grid.text(
      if (is.finite(val)) format_pct(val) else "--",
      x = grid::unit(lx, "npc"),
      y = grid::unit(well_y0 + 0.015, "npc"),
      just = c("center", "center"),
      gp = grid::gpar(col = if (is_alert) pal$signal else pal$ink, fontsize = 7.4, fontface = "bold", fontfamily = report_mono_font())
    )
  }

  # --- LOWER TELEMETRY: Precision Mutation Log (2 clean hairline columns, no boxes) ---
  draw_section_tag("02", "MUTATION TELEMETRY", ml, 0.405, pal)

  sev_counts <- c(
    high = sum(diff_obj$changes$severity == "high"),
    medium = sum(diff_obj$changes$severity == "medium"),
    low = sum(diff_obj$changes$severity == "low")
  )
  # Right-aligned minimal telemetry counter
  grid::grid.text(
    paste0(sev_counts[["high"]], " CRIT  /  ", sev_counts[["medium"]], " WARN  /  ", sev_counts[["low"]], " INFO"),
    x = grid::unit(mr, "npc"),
    y = grid::unit(0.405, "npc"),
    just = c("right", "center"),
    gp = grid::gpar(col = pal$muted, fontsize = 7.4, fontfamily = report_mono_font())
  )
  draw_hairline(ml, mr, 0.386, pal$ink, lwd = 0.8)

  if (n_changes) {
    prio <- match(diff_obj$changes$severity, c("high", "medium", "low"), nomatch = 4L)
    ordered <- diff_obj$changes[order(prio), , drop = FALSE]
    show <- utils::head(ordered, 8L)
    col_x <- c(ml, ml + fw * 0.515)
    col_w <- fw * 0.485
    row_top <- 0.348
    row_step <- 0.056

    for (i in seq_len(nrow(show))) {
      c_idx <- if (i <= 4L) 1L else 2L
      r_idx <- if (i <= 4L) i else (i - 4L)
      rx <- col_x[[c_idx]]
      ry <- row_top - (r_idx - 1L) * row_step
      sev <- show$severity[[i]]
      scol <- instrument_sev_color(sev, pal)

      # Precision vertical signal bar (no text badge overlap!)
      grid::grid.rect(
        x = grid::unit(rx, "npc"),
        y = grid::unit(ry, "npc"),
        width = grid::unit(0.0038, "npc"),
        height = grid::unit(0.022, "npc"),
        just = c("left", "center"),
        gp = grid::gpar(fill = scol, col = NA)
      )
      grid::grid.text(
        sprintf("%02d", i),
        x = grid::unit(rx + 0.012, "npc"),
        y = grid::unit(ry, "npc"),
        just = c("left", "center"),
        gp = grid::gpar(col = pal$tick, fontsize = 7.4, fontfamily = report_mono_font())
      )
      grid::grid.text(
        plot_short_label(show$message[[i]], 46L),
        x = grid::unit(rx + 0.038, "npc"),
        y = grid::unit(ry, "npc"),
        just = c("left", "center"),
        gp = grid::gpar(col = pal$ink, fontsize = 8.4, fontfamily = report_font())
      )
      draw_hairline(rx, rx + col_w - 0.01, ry - row_step * 0.5, pal$border, lwd = 0.45)
    }
  } else {
    grid::grid.text("NO MUTATIONS DETECTED BETWEEN SEQUENCES.", x = grid::unit(ml, "npc"), y = grid::unit(0.30, "npc"), just = c("left", "center"), gp = grid::gpar(col = pal$muted, fontsize = 8.4, fontfamily = report_mono_font()))
  }

  # Footer telemetry bar
  draw_hairline(ml, mr, 0.115, pal$border, lwd = 0.65)
  rename_note <- if (n_renames) {
    paste0("RENAMES  ", paste(utils::head(paste0(diff_obj$renames$old_name, " -> ", diff_obj$renames$new_name), 4L), collapse = "   |   "))
  } else {
    "RENAMES  NONE INFERRED"
  }
  grid::grid.text(plot_short_label(rename_note, 92L), x = grid::unit(ml, "npc"), y = grid::unit(0.082, "npc"), just = c("left", "center"), gp = grid::gpar(col = pal$muted, fontsize = 7.8, fontfamily = report_mono_font()))
  grid::grid.text("TE-SPEC // 0.2.0", x = grid::unit(mr, "npc"), y = grid::unit(0.082, "npc"), just = c("right", "center"), gp = grid::gpar(col = pal$tick, fontsize = 7.2, fontfamily = report_mono_font()))
}

# ==============================================================================
# 2. SPECIMEN PROFILE INSTRUMENT (Electrophoresis Barcode + Column Spectrum)
# ==============================================================================

draw_specimen_report <- function(dna) {
  grid::grid.newpage()
  pal <- report_palette()
  draw_instrument_frame(pal)

  risk_col <- if (identical(dna$scores$drift_risk, "high")) pal$signal else pal$ink
  draw_instrument_header(
    code = "DATADNA // SPECIMEN",
    title = plot_short_label(dna$name, 34L),
    meta = paste0(
      toupper(gsub("_", " ", dna$species, fixed = TRUE)), "   /   ",
      format(dna$n_rows, big.mark = ","), " x ", dna$n_cols, "   /   ",
      dna$dna_id
    ),
    kpi_label = "IDENTITY",
    kpi_value = paste0(round(dna$scores$identity_strength)),
    kpi_sub = paste0("STABILITY ", toupper(dna$scores$stability), " / RISK ", toupper(dna$scores$drift_risk)),
    kpi_col = risk_col,
    pal = pal
  )

  ml <- 0.055; mr <- 0.945; fw <- mr - ml

  # --- HERO: Full-Width DNA Electrophoresis Gel Strip (y = 0.61 to 0.75) ---
  draw_section_tag("01", "GENOMIC ELECTROPHORESIS SIGNATURE", ml, 0.775, pal)
  grid::grid.text(
    paste0("SCH ", dna$fingerprints$schema_hash, "   SHP ", dna$fingerprints$shape_hash, "   PRF ", dna$fingerprints$profile_hash),
    x = grid::unit(mr, "npc"),
    y = grid::unit(0.775, "npc"),
    just = c("right", "center"),
    gp = grid::gpar(col = pal$muted, fontsize = 7.3, fontfamily = report_mono_font())
  )

  gel_y0 <- 0.618; gel_y1 <- 0.745
  grid::grid.rect(
    x = grid::unit(ml, "npc"),
    y = grid::unit(gel_y0, "npc"),
    width = grid::unit(fw, "npc"),
    height = grid::unit(gel_y1 - gel_y0, "npc"),
    just = c("left", "bottom"),
    gp = grid::gpar(fill = pal$well, col = pal$border, lwd = 0.6)
  )

  # Top & bottom precision ruler ticks inside gel well
  draw_ruler_ticks(ml + 0.015, mr - 0.015, gel_y1 - 0.002, tick_dir = -1, n_major = 8L, n_minor = 80L, pal = pal)
  draw_ruler_ticks(ml + 0.015, mr - 0.015, gel_y0 + 0.002, tick_dir = 1, n_major = 8L, n_minor = 80L, pal = pal)

  # 88 high-density spectral bands derived from fingerprints
  n_bands <- 88L
  sig_vals <- hash_to_unit_seq(
    paste(dna$fingerprints$schema_hash, dna$fingerprints$shape_hash, dna$fingerprints$profile_hash, dna$dna_id),
    n_bands
  )
  gx0 <- ml + 0.015; gw <- (mr - 0.015) - gx0
  g_slot <- gw / n_bands
  g_mid <- (gel_y0 + gel_y1) / 2
  for (i in seq_len(n_bands)) {
    v <- sig_vals[[i]]
    bh <- (0.14 + 0.72 * v) * (gel_y1 - gel_y0 - 0.032)
    is_peak <- v > 0.90
    grid::grid.rect(
      x = grid::unit(gx0 + (i - 0.5) * g_slot, "npc"),
      y = grid::unit(g_mid, "npc"),
      width = grid::unit(g_slot * (0.32 + 0.36 * v), "npc"),
      height = grid::unit(bh, "npc"),
      just = c("center", "center"),
      gp = grid::gpar(fill = if (is_peak) pal$signal else pal$ink, col = NA, alpha = if (is_peak) 0.92 else (0.18 + 0.76 * v))
    )
  }

  # --- SECTION 02: Calibrated Column Signal Matrix (y = 0.14 to 0.56) ---
  draw_section_tag("02", "COLUMN SIGNAL OSCILLOSCOPE", ml, 0.572, pal)
  cols <- dna$columns
  if (nrow(cols)) {
    ord <- order(cols$missing_rate, -cols$unique_rate, decreasing = TRUE, na.last = TRUE)
    show <- cols[utils::head(ord, 8L), , drop = FALSE]

    hy <- 0.532
    cx_col <- ml
    cx_role <- ml + 0.195
    cx_miss <- ml + 0.285
    cx_bar0 <- ml + 0.365
    cx_bar1 <- ml + 0.685
    cx_uniq <- ml + 0.755
    cx_anch <- mr

    grid::grid.text("COLUMN", x = grid::unit(cx_col, "npc"), y = grid::unit(hy, "npc"), just = c("left", "center"), gp = grid::gpar(col = pal$muted, fontsize = 7.2, fontface = "bold", fontfamily = report_mono_font()))
    grid::grid.text("ROLE", x = grid::unit(cx_role, "npc"), y = grid::unit(hy, "npc"), just = c("left", "center"), gp = grid::gpar(col = pal$muted, fontsize = 7.2, fontface = "bold", fontfamily = report_mono_font()))
    grid::grid.text("MISS", x = grid::unit(cx_miss, "npc"), y = grid::unit(hy, "npc"), just = c("right", "center"), gp = grid::gpar(col = pal$muted, fontsize = 7.2, fontface = "bold", fontfamily = report_mono_font()))
    grid::grid.text("UNIQUENESS  0% - 100%", x = grid::unit(cx_bar0, "npc"), y = grid::unit(hy, "npc"), just = c("left", "center"), gp = grid::gpar(col = pal$muted, fontsize = 7.2, fontface = "bold", fontfamily = report_mono_font()))
    grid::grid.text("UNIQ", x = grid::unit(cx_uniq, "npc"), y = grid::unit(hy, "npc"), just = c("right", "center"), gp = grid::gpar(col = pal$muted, fontsize = 7.2, fontface = "bold", fontfamily = report_mono_font()))
    grid::grid.text("ANCHOR", x = grid::unit(cx_anch, "npc"), y = grid::unit(hy, "npc"), just = c("right", "center"), gp = grid::gpar(col = pal$muted, fontsize = 7.2, fontface = "bold", fontfamily = report_mono_font()))
    draw_hairline(ml, mr, hy - 0.018, pal$ink, lwd = 0.8)

    ry <- hy - 0.048
    rgap <- min(0.044, 0.35 / max(nrow(show), 1L))
    bw <- cx_bar1 - cx_bar0

    for (i in seq_len(nrow(show))) {
      row <- show[i, , drop = FALSE]
      miss <- if (is.finite(row$missing_rate[[1L]])) clip01(row$missing_rate[[1L]]) else 0
      uniq <- if (is.finite(row$unique_rate[[1L]])) clip01(row$unique_rate[[1L]]) else 0
      has_miss <- miss > 0.005

      grid::grid.text(plot_short_label(row$name[[1L]], 20L), x = grid::unit(cx_col, "npc"), y = grid::unit(ry, "npc"), just = c("left", "center"), gp = grid::gpar(col = pal$ink, fontsize = 8.6, fontface = "bold", fontfamily = report_font()))
      grid::grid.text(role_short_code(row$role[[1L]]), x = grid::unit(cx_role, "npc"), y = grid::unit(ry, "npc"), just = c("left", "center"), gp = grid::gpar(col = pal$muted, fontsize = 7.6, fontfamily = report_mono_font()))
      grid::grid.text(format_pct(miss), x = grid::unit(cx_miss, "npc"), y = grid::unit(ry, "npc"), just = c("right", "center"), gp = grid::gpar(col = if (has_miss) pal$signal else pal$tick, fontsize = 8.0, fontface = if (has_miss) "bold" else "plain", fontfamily = report_mono_font()))

      # Precision stem-and-needle track for uniqueness (+ orange tick for missingness)
      draw_hairline(cx_bar0, cx_bar1, ry, pal$border, lwd = 0.9)
      if (uniq > 0) {
        grid::grid.lines(
          x = grid::unit(c(cx_bar0, cx_bar0 + bw * uniq), "npc"),
          y = grid::unit(c(ry, ry), "npc"),
          gp = grid::gpar(col = pal$ink, lwd = 2.0)
        )
        # Crisp terminal needle
        grid::grid.rect(
          x = grid::unit(cx_bar0 + bw * uniq, "npc"),
          y = grid::unit(ry, "npc"),
          width = grid::unit(0.0032, "npc"),
          height = grid::unit(0.015, "npc"),
          just = c("center", "center"),
          gp = grid::gpar(fill = pal$ink, col = NA)
        )
      }
      if (has_miss) {
        grid::grid.rect(
          x = grid::unit(cx_bar0 + bw * miss, "npc"),
          y = grid::unit(ry, "npc"),
          width = grid::unit(0.0038, "npc"),
          height = grid::unit(0.018, "npc"),
          just = c("center", "center"),
          gp = grid::gpar(fill = pal$signal, col = NA)
        )
      }

      grid::grid.text(format_pct(uniq), x = grid::unit(cx_uniq, "npc"), y = grid::unit(ry, "npc"), just = c("right", "center"), gp = grid::gpar(col = pal$ink, fontsize = 8.0, fontfamily = report_mono_font()))
      grid::grid.text(plot_short_label(column_anchor_text(row), 16L), x = grid::unit(cx_anch, "npc"), y = grid::unit(ry, "npc"), just = c("right", "center"), gp = grid::gpar(col = pal$muted, fontsize = 7.9, fontfamily = report_mono_font()))
      draw_hairline(ml, mr, ry - rgap * 0.5, pal$border, lwd = 0.4)
      ry <- ry - rgap
    }
  } else {
    grid::grid.text("NO COLUMNS AVAILABLE IN SPECIMEN.", x = grid::unit(ml, "npc"), y = grid::unit(0.36, "npc"), just = c("left", "center"), gp = grid::gpar(col = pal$muted, fontsize = 8.4, fontfamily = report_mono_font()))
  }

  # Footer diagnostics telemetry
  draw_hairline(ml, mr, 0.115, pal$border, lwd = 0.65)
  diag_txt <- if (length(dna$warnings)) paste0("ALERT  ", paste(utils::head(dna$warnings, 2L), collapse = "  |  ")) else paste0("STATUS  OK  /  ", paste(utils::head(dna$signals, 3L), collapse = "  |  "))
  grid::grid.text(plot_short_label(diag_txt, 96L), x = grid::unit(ml, "npc"), y = grid::unit(0.082, "npc"), just = c("left", "center"), gp = grid::gpar(col = if (length(dna$warnings)) pal$signal else pal$muted, fontsize = 7.7, fontfamily = report_mono_font()))
  grid::grid.text("TE-SPEC // 0.2.0", x = grid::unit(mr, "npc"), y = grid::unit(0.082, "npc"), just = c("right", "center"), gp = grid::gpar(col = pal$tick, fontsize = 7.2, fontfamily = report_mono_font()))
}

column_anchor_text <- function(row) {
  tp <- row$type[[1L]]
  if (tp == "numeric" && is.finite(row$q50[[1L]])) {
    paste0("med ", signif(row$q50[[1L]], 4))
  } else if (tp %in% c("character", "factor", "logical")) {
    tv <- row$top_values[[1L]]
    if (length(tv)) paste0("top ", tv[[1L]]) else "-"
  } else if (tp == "date" && !is.na(row$date_min[[1L]])) {
    row$date_min[[1L]]
  } else {
    "-"
  }
}

# ==============================================================================
# 3. LINEAGE MATCH INSTRUMENT (Signal Router + Electrophoresis Comparison)
# ==============================================================================

draw_match_report <- function(match) {
  grid::grid.newpage()
  pal <- report_palette()
  best <- match$matches[1L, , drop = FALSE]
  draw_instrument_frame(pal)

  draw_instrument_header(
    code = "DATADNA // LINEAGE",
    title = plot_short_label(match$query$name, 34L),
    meta = paste0(
      toupper(gsub("_", " ", match$query$species, fixed = TRUE)), "   /   ",
      format(match$query$n_rows, big.mark = ","), " x ", match$query$n_cols, "   /   ",
      match$query$dna_id
    ),
    kpi_label = "CLOSEST",
    kpi_value = format_pct(best$overall),
    kpi_sub = paste0(toupper(plot_short_label(best$name, 16L)), " / ", toupper(plot_relation_label(best$relation))),
    kpi_col = pal$signal,
    pal = pal
  )

  ml <- 0.055; mr <- 0.945; fw <- mr - ml
  draw_section_tag("01", "SIGNAL ROUTING & ELECTROPHORESIS COMPARISON", ml, 0.775, pal)

  well_y0 <- 0.165; well_y1 <- 0.745
  grid::grid.rect(
    x = grid::unit(ml, "npc"),
    y = grid::unit(well_y0, "npc"),
    width = grid::unit(fw, "npc"),
    height = grid::unit(well_y1 - well_y0, "npc"),
    just = c("left", "bottom"),
    gp = grid::gpar(fill = pal$well, col = pal$border, lwd = 0.6)
  )

  matches <- match$matches
  n <- nrow(matches)
  qx <- ml + 0.025
  qw <- 0.215
  qh <- 0.165
  qy <- (well_y0 + well_y1) / 2

  cx <- ml + 0.345
  cw <- (mr - 0.025) - cx
  usable_h <- (well_y1 - well_y0) - 0.075
  ch <- min(0.098, (usable_h / max(n, 1L)) * 0.80)
  top_c <- well_y1 - 0.042 - ch / 2
  bot_c <- well_y0 + 0.042 + ch / 2
  cy <- if (n == 1L) qy else seq(top_c, bot_c, length.out = n)

  # Signal routing Bezier traces
  for (i in seq_len(n)) {
    sc <- clip01(matches$overall[[i]])
    is_best <- (i == 1L)
    tcol <- if (is_best) pal$signal else pal$ink
    grid::grid.xspline(
      x = grid::unit(c(qx + qw, qx + qw + 0.052, cx - 0.052, cx), "npc"),
      y = grid::unit(c(qy, qy, cy[[i]], cy[[i]]), "npc"),
      shape = 1,
      gp = grid::gpar(col = tcol, lwd = if (is_best) 2.2 else (0.55 + 1.1 * sc), alpha = if (is_best) 0.95 else (0.25 + 0.55 * sc))
    )
  }

  # Query hardware module (matte carbon black + signal LED + mini electrophoresis strip)
  grid::grid.rect(
    x = grid::unit(qx, "npc"),
    y = grid::unit(qy - qh / 2, "npc"),
    width = grid::unit(qw, "npc"),
    height = grid::unit(qh, "npc"),
    just = c("left", "bottom"),
    gp = grid::gpar(fill = pal$ink, col = pal$ink, lwd = 0.8)
  )
  # Signal LED dot
  grid::grid.rect(
    x = grid::unit(qx + 0.014, "npc"),
    y = grid::unit(qy + qh / 2 - 0.024, "npc"),
    width = grid::unit(0.006, "npc"),
    height = grid::unit(0.0095, "npc"),
    just = c("left", "center"),
    gp = grid::gpar(fill = pal$signal, col = NA)
  )
  grid::grid.text("QUERY", x = grid::unit(qx + 0.028, "npc"), y = grid::unit(qy + qh / 2 - 0.024, "npc"), just = c("left", "center"), gp = grid::gpar(col = "#999790", fontsize = 6.8, fontface = "bold", fontfamily = report_mono_font()))
  grid::grid.text(plot_short_label(match$query$name, 19L), x = grid::unit(qx + 0.014, "npc"), y = grid::unit(qy + 0.018, "npc"), just = c("left", "center"), gp = grid::gpar(col = "#F4F3EF", fontsize = 10.5, fontface = "bold", fontfamily = report_font()))
  grid::grid.text(match$query$dna_id, x = grid::unit(qx + 0.014, "npc"), y = grid::unit(qy - 0.012, "npc"), just = c("left", "center"), gp = grid::gpar(col = "#999790", fontsize = 7.2, fontfamily = report_mono_font()))
  draw_mini_gel(qx + 0.014, qy - qh / 2 + 0.024, qw - 0.028, 0.022, match$query$dna_id, col_hi = "#F4F3EF", col_lo = "#666660", n_bars = 32L)

  # Candidate strips with embedded electrophoresis barcode
  compact <- n > 5L
  for (i in seq_len(n)) {
    sc <- clip01(matches$overall[[i]])
    is_best <- (i == 1L)
    y0 <- cy[[i]] - ch / 2
    grid::grid.rect(
      x = grid::unit(cx, "npc"),
      y = grid::unit(y0, "npc"),
      width = grid::unit(cw, "npc"),
      height = grid::unit(ch, "npc"),
      just = c("left", "bottom"),
      gp = grid::gpar(fill = pal$plate, col = if (is_best) pal$ink else pal$border, lwd = if (is_best) 1.0 else 0.6)
    )
    # Left signal bar
    grid::grid.rect(
      x = grid::unit(cx, "npc"),
      y = grid::unit(cy[[i]], "npc"),
      width = grid::unit(0.004, "npc"),
      height = grid::unit(ch, "npc"),
      just = c("left", "center"),
      gp = grid::gpar(fill = if (is_best) pal$signal else pal$tick, col = NA)
    )

    grid::grid.text(sprintf("%02d", i), x = grid::unit(cx + 0.014, "npc"), y = grid::unit(cy[[i]] + if (compact) 0 else ch * 0.16, "npc"), just = c("left", "center"), gp = grid::gpar(col = pal$tick, fontsize = 7.4, fontfamily = report_mono_font()))
    grid::grid.text(plot_short_label(matches$name[[i]], 18L), x = grid::unit(cx + 0.042, "npc"), y = grid::unit(cy[[i]] + if (compact) 0 else ch * 0.16, "npc"), just = c("left", "center"), gp = grid::gpar(col = pal$ink, fontsize = if (compact) 8.4 else 9.2, fontface = "bold", fontfamily = report_font()))
    if (!compact) {
      grid::grid.text(plot_relation_label(matches$relation[[i]]), x = grid::unit(cx + 0.042, "npc"), y = grid::unit(cy[[i]] - ch * 0.20, "npc"), just = c("left", "center"), gp = grid::gpar(col = pal$muted, fontsize = 7.6, fontfamily = report_font()))
    }

    # Candidate mini electrophoresis strip in middle-right
    gel_x <- cx + 0.22
    gel_w <- cw - 0.305
    if (gel_w > 0.08) {
      draw_mini_gel(
        gel_x, cy[[i]], gel_w, min(0.026, ch * 0.45),
        paste(matches$dna_id[[i]], matches$name[[i]]),
        col_hi = if (is_best) pal$signal else pal$ink,
        col_lo = pal$tick,
        n_bars = 36L
      )
    }

    # Right-aligned tabular similarity score
    grid::grid.text(
      format_pct(sc),
      x = grid::unit(cx + cw - 0.016, "npc"),
      y = grid::unit(cy[[i]], "npc"),
      just = c("right", "center"),
      gp = grid::gpar(col = if (is_best) pal$signal else pal$ink, fontsize = if (compact) 9.0 else 11.0, fontface = "bold", fontfamily = report_mono_font())
    )
  }

  # Footer telemetry bar
  draw_hairline(ml, mr, 0.115, pal$border, lwd = 0.65)
  notes <- best$top_mutations[[1L]]
  foot_txt <- paste0(best_relative_sentence(best), if (length(notes)) paste0("   //   ", notes[[1L]]) else "")
  grid::grid.text(plot_short_label(foot_txt, 98L), x = grid::unit(ml, "npc"), y = grid::unit(0.082, "npc"), just = c("left", "center"), gp = grid::gpar(col = pal$muted, fontsize = 7.8, fontfamily = report_mono_font()))
  grid::grid.text("TE-SPEC // 0.2.0", x = grid::unit(mr, "npc"), y = grid::unit(0.082, "npc"), just = c("right", "center"), gp = grid::gpar(col = pal$tick, fontsize = 7.2, fontfamily = report_mono_font()))
}

# ==============================================================================
# Precision Instrument Primitives
# ==============================================================================

draw_instrument_frame <- function(pal) {
  grid::grid.rect(gp = grid::gpar(fill = pal$canvas, col = NA))
  # Precision top hairline + small Signal Orange corner index
  grid::grid.rect(
    x = grid::unit(0.055, "npc"),
    y = grid::unit(0.968, "npc"),
    width = grid::unit(0.028, "npc"),
    height = grid::unit(0.0055, "npc"),
    just = c("left", "center"),
    gp = grid::gpar(fill = pal$signal, col = NA)
  )
  draw_hairline(0.090, 0.945, 0.968, pal$border, lwd = 0.7)
}

draw_instrument_header <- function(code, title, meta, kpi_label, kpi_value, kpi_sub, kpi_col, pal) {
  ml <- 0.055; mr <- 0.945
  grid::grid.text(code, x = grid::unit(ml, "npc"), y = grid::unit(0.935, "npc"), just = c("left", "center"), gp = grid::gpar(col = pal$muted, fontsize = 7.5, fontface = "bold", fontfamily = report_mono_font()))
  grid::grid.text(title, x = grid::unit(ml, "npc"), y = grid::unit(0.890, "npc"), just = c("left", "center"), gp = grid::gpar(col = pal$ink, fontsize = 16.5, fontface = "bold", fontfamily = report_font()))
  grid::grid.text(meta, x = grid::unit(ml, "npc"), y = grid::unit(0.848, "npc"), just = c("left", "center"), gp = grid::gpar(col = pal$muted, fontsize = 7.8, fontfamily = report_mono_font()))

  # Right telemetry block
  grid::grid.text(kpi_label, x = grid::unit(mr, "npc"), y = grid::unit(0.935, "npc"), just = c("right", "center"), gp = grid::gpar(col = pal$muted, fontsize = 7.3, fontface = "bold", fontfamily = report_mono_font()))
  grid::grid.text(kpi_value, x = grid::unit(mr, "npc"), y = grid::unit(0.888, "npc"), just = c("right", "center"), gp = grid::gpar(col = kpi_col, fontsize = 22, fontface = "bold", fontfamily = report_mono_font()))
  grid::grid.text(plot_short_label(kpi_sub, 34L), x = grid::unit(mr, "npc"), y = grid::unit(0.848, "npc"), just = c("right", "center"), gp = grid::gpar(col = pal$muted, fontsize = 7.4, fontfamily = report_mono_font()))

  draw_hairline(ml, mr, 0.815, pal$ink, lwd = 0.9)
}

draw_section_tag <- function(num, label, x, y, pal) {
  grid::grid.text(num, x = grid::unit(x, "npc"), y = grid::unit(y, "npc"), just = c("left", "center"), gp = grid::gpar(col = pal$signal, fontsize = 7.6, fontface = "bold", fontfamily = report_mono_font()))
  grid::grid.text(label, x = grid::unit(x + 0.026, "npc"), y = grid::unit(y, "npc"), just = c("left", "center"), gp = grid::gpar(col = pal$ink, fontsize = 7.8, fontface = "bold", fontfamily = report_mono_font()))
}

draw_ruler_axis <- function(x0, x1, y, n_major = 9L, n_minor = 90L, pal) {
  draw_hairline(x0, x1, y, pal$ink, lwd = 0.65)
  xs_minor <- seq(x0, x1, length.out = n_minor + 1L)
  for (i in seq_along(xs_minor)) {
    is_maj <- ((i - 1L) %% (n_minor / n_major)) == 0
    th <- if (is_maj) 0.0075 else 0.0038
    grid::grid.lines(
      x = grid::unit(c(xs_minor[[i]], xs_minor[[i]]), "npc"),
      y = grid::unit(c(y - th, y + th), "npc"),
      gp = grid::gpar(col = if (is_maj) pal$ink else pal$tick, lwd = if (is_maj) 0.65 else 0.45)
    )
  }
}

draw_ruler_ticks <- function(x0, x1, y, tick_dir = -1, n_major = 8L, n_minor = 80L, pal) {
  xs <- seq(x0, x1, length.out = n_minor + 1L)
  step <- max(1L, as.integer(n_minor / n_major))
  for (i in seq_along(xs)) {
    is_maj <- ((i - 1L) %% step) == 0
    th <- (if (is_maj) 0.008 else 0.004) * tick_dir
    grid::grid.lines(
      x = grid::unit(c(xs[[i]], xs[[i]]), "npc"),
      y = grid::unit(c(y, y + th), "npc"),
      gp = grid::gpar(col = if (is_maj) pal$muted else pal$tick, lwd = if (is_maj) 0.6 else 0.4)
    )
  }
}

draw_mini_gel <- function(x, y, w, h, seed_str, col_hi, col_lo, n_bars = 32L) {
  vals <- hash_to_unit_seq(seed_str, n_bars)
  slot <- w / n_bars
  for (i in seq_len(n_bars)) {
    v <- vals[[i]]
    grid::grid.rect(
      x = grid::unit(x + (i - 0.5) * slot, "npc"),
      y = grid::unit(y, "npc"),
      width = grid::unit(slot * (0.35 + 0.35 * v), "npc"),
      height = grid::unit(h * (0.28 + 0.72 * v), "npc"),
      just = c("center", "center"),
      gp = grid::gpar(fill = if (v > 0.72) col_hi else col_lo, col = NA, alpha = 0.28 + 0.68 * v)
    )
  }
}

hash_to_unit_seq <- function(seed_str, n) {
  out <- numeric(n)
  cur <- as.character(seed_str)
  idx <- 1L
  while (idx <= n) {
    h <- stable_hash(paste0(cur, "#", idx))
    bytes <- as.integer(charToRaw(h))
    for (b in bytes) {
      if (idx > n) break
      out[[idx]] <- ((b * 37L + idx * 17L) %% 100L) / 100
      idx <- idx + 1L
    }
    cur <- h
  }
  out
}

draw_hairline <- function(x0, x1, y, col, lwd = 0.6) {
  grid::grid.lines(
    x = grid::unit(c(x0, x1), "npc"),
    y = grid::unit(c(y, y), "npc"),
    gp = grid::gpar(col = col, lwd = lwd)
  )
}

instrument_sev_color <- function(level, pal) {
  switch(
    as.character(level),
    high = pal$signal,
    medium = pal$ink,
    low = pal$tick,
    pal$tick
  )
}

role_short_code <- function(role) {
  map <- c(
    id = "ID",
    time = "TIME",
    measure = "MEAS",
    category = "CAT",
    flag = "FLAG",
    text = "TEXT",
    target = "TRGT",
    unknown = "UNK"
  )
  unname(map[match(as.character(role), names(map), nomatch = match("unknown", names(map)))])
}

plot_short_label <- function(x, width = 26L) {
  x <- as.character(x)
  vapply(x, function(s) {
    if (is.na(s)) {
      return("")
    }
    if (nchar(s, type = "width") > width) {
      trimmed <- sub("[. ]+$", "", substr(s, 1L, max(1L, width - 3L)))
      paste0(trimmed, "...")
    } else {
      s
    }
  }, character(1), USE.NAMES = FALSE)
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
    canvas = "#F4F3EF",
    plate = "#FAF9F6",
    well = "#ECEAE4",
    ink = "#141414",
    muted = "#666660",
    tick = "#A8A69F",
    border = "#D6D4CC",
    signal = "#FF4F00"
  )
}

report_font <- function() {
  "sans"
}

report_mono_font <- function() {
  "mono"
}
