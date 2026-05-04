#' Render a laboratory-style data DNA card
#'
#' @param x A data frame or `data_dna` object.
#' @param file Optional HTML file path. If supplied, the card is saved there.
#' @param open Logical. Open the saved file in a browser when `file` is supplied.
#'
#' @return An `htmltools` browsable object, invisibly when saved to `file`.
#' @export
#'
#' @examples
#' demo <- dna_example_customers()
#' card <- dna_card(demo$customers_new)
dna_card <- function(x, file = NULL, open = FALSE) {
  dna <- as_data_dna(x)
  card <- build_dna_card(dna)

  if (!is.null(file)) {
    htmltools::save_html(card, file = file)
    if (isTRUE(open)) {
      utils::browseURL(normalizePath(file, winslash = "/", mustWork = FALSE))
    }
    return(invisible(card))
  }

  htmltools::browsable(card)
}

build_dna_card <- function(dna) {
  htmltools::tags$html(
    htmltools::tags$head(
      htmltools::tags$meta(charset = "utf-8"),
      htmltools::tags$title(paste("DataDNA", dna$name)),
      htmltools::tags$style(htmltools::HTML(dna_card_css()))
    ),
    htmltools::tags$body(
      htmltools::tags$main(
        class = "dna-shell",
        htmltools::tags$section(
          class = "dna-card",
          htmltools::tags$div(
            class = "hero",
            htmltools::tags$div(
              htmltools::tags$div(class = "kicker", "DataDNA specimen card"),
              htmltools::tags$h1(dna$name),
              htmltools::tags$p(class = "dna-id", dna$dna_id)
            ),
            htmltools::tags$div(
              class = paste("risk", dna$scores$drift_risk),
              htmltools::tags$span("Drift risk"),
              htmltools::tags$strong(dna$scores$drift_risk)
            )
          ),
          htmltools::tags$div(
            class = "barcode",
            lapply(dna_barcode_values(dna), function(v) htmltools::tags$i(style = paste0("--h:", v, "%;")))
          ),
          fingerprint_stack(dna),
          htmltools::tags$div(
            class = "metrics",
            metric_tile("Species", dna$species),
            metric_tile("Shape", paste0(format(dna$n_rows, big.mark = ","), " x ", dna$n_cols)),
            metric_tile("Identity", paste0(round(dna$scores$identity_strength), "/100")),
            metric_tile("Stability", dna$scores$stability)
          ),
          htmltools::tags$div(
            class = "grid",
            card_panel("Role map", role_map(dna)),
            card_panel("Column signals", column_signal_table(dna)),
            card_panel("Lab notes", lab_notes(dna))
          )
        )
      )
    )
  )
}

metric_tile <- function(label, value) {
  htmltools::tags$div(
    class = "metric",
    htmltools::tags$span(label),
    htmltools::tags$strong(value)
  )
}

card_panel <- function(title, content) {
  htmltools::tags$div(
    class = "panel",
    htmltools::tags$h2(title),
    content
  )
}

dna_barcode_values <- function(dna) {
  text <- paste(dna$fingerprints$schema_hash, dna$fingerprints$profile_hash, dna$dna_id)
  bytes <- as.integer(charToRaw(text))
  vals <- 25 + (bytes %% 70)
  vals[seq_len(min(48L, length(vals)))]
}

role_map <- function(dna) {
  cols <- dna$columns
  htmltools::tags$div(
    htmltools::tags$div(
      class = "role-map",
      lapply(seq_len(nrow(cols)), function(i) {
        htmltools::tags$div(
          class = "gene",
          title = paste(cols$name[[i]], cols$role[[i]], sep = " - "),
          style = paste0("background:", role_palette(cols$role[[i]]), ";")
        )
      })
    ),
    role_legend(cols)
  )
}

fingerprint_stack <- function(dna) {
  htmltools::tags$div(
    class = "fingerprints",
    fingerprint_chip("schema", dna$fingerprints$schema_hash),
    fingerprint_chip("shape", dna$fingerprints$shape_hash),
    fingerprint_chip("profile", dna$fingerprints$profile_hash)
  )
}

fingerprint_chip <- function(label, value) {
  htmltools::tags$div(
    class = "fingerprint-chip",
    htmltools::tags$span(label),
    htmltools::tags$strong(value)
  )
}

role_legend <- function(cols) {
  counts <- sort(table(cols$role), decreasing = TRUE)
  htmltools::tags$div(
    class = "legend",
    lapply(names(counts), function(role) {
      htmltools::tags$span(
        htmltools::tags$i(style = paste0("background:", role_palette(role), ";")),
        paste0(role, " ", counts[[role]])
      )
    })
  )
}

column_signal_table <- function(dna) {
  cols <- dna$columns
  order_idx <- order(cols$missing_rate, decreasing = TRUE, na.last = TRUE)
  show <- cols[head(order_idx, 8L), , drop = FALSE]
  htmltools::tags$table(
    class = "signals-table",
    htmltools::tags$thead(
      htmltools::tags$tr(
        htmltools::tags$th("Column"),
        htmltools::tags$th("Role"),
        htmltools::tags$th("Missing"),
        htmltools::tags$th("Unique")
      )
    ),
    htmltools::tags$tbody(
      lapply(seq_len(nrow(show)), function(i) {
        htmltools::tags$tr(
          htmltools::tags$td(show$name[[i]]),
          htmltools::tags$td(show$role[[i]]),
          htmltools::tags$td(format_pct(show$missing_rate[[i]])),
          htmltools::tags$td(format_pct(show$unique_rate[[i]]))
        )
      })
    )
  )
}

lab_notes <- function(dna) {
  notes <- c(dna$signals, dna$warnings)
  if (!length(notes)) {
    notes <- "No notable mutations detected."
  }
  htmltools::tags$ul(
    class = "notes",
    lapply(head(notes, 8L), htmltools::tags$li)
  )
}

dna_card_css <- function() {
  "
  * { box-sizing: border-box; }
  body {
    margin: 0;
    min-height: 100vh;
    color: #edf7f4;
    background:
      radial-gradient(circle at top left, rgba(77, 212, 172, .16), transparent 32rem),
      linear-gradient(135deg, #111827 0%, #172033 48%, #0f1720 100%);
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
  }
  .dna-shell { width: min(1120px, calc(100vw - 32px)); margin: 32px auto; }
  .dna-card {
    border: 1px solid rgba(166, 244, 214, .28);
    background: rgba(9, 18, 28, .78);
    box-shadow: 0 24px 80px rgba(0, 0, 0, .34);
    border-radius: 8px;
    overflow: hidden;
  }
  .hero {
    display: flex;
    justify-content: space-between;
    gap: 24px;
    padding: 34px;
    border-bottom: 1px solid rgba(166, 244, 214, .16);
  }
  .kicker { color: #7ee7c1; text-transform: uppercase; font-size: 12px; letter-spacing: 0; }
  h1 { margin: 8px 0; font-size: 34px; line-height: 1.1; letter-spacing: 0; }
  .dna-id { margin: 0; color: #b9c7d6; font-family: Consolas, monospace; }
  .risk {
    align-self: flex-start;
    min-width: 130px;
    padding: 14px 16px;
    border-radius: 8px;
    background: rgba(255, 255, 255, .06);
    border: 1px solid rgba(255, 255, 255, .12);
  }
  .risk span { display: block; color: #b9c7d6; font-size: 12px; }
  .risk strong { display: block; margin-top: 4px; text-transform: uppercase; }
  .risk.low strong { color: #7ee7c1; }
  .risk.medium strong { color: #ffd166; }
  .risk.high strong { color: #ff8a65; }
  .barcode {
    display: flex;
    align-items: end;
    gap: 4px;
    height: 98px;
    padding: 20px 34px;
    background: rgba(255, 255, 255, .035);
  }
  .barcode i {
    display: block;
    width: 10px;
    height: var(--h);
    border-radius: 3px 3px 0 0;
    background: linear-gradient(180deg, #7ee7c1, #63b3ff);
  }
  .fingerprints {
    display: grid;
    grid-template-columns: repeat(3, 1fr);
    gap: 1px;
    background: rgba(166, 244, 214, .14);
    border-top: 1px solid rgba(166, 244, 214, .12);
  }
  .fingerprint-chip {
    padding: 12px 24px;
    background: rgba(12, 25, 39, .94);
    font-family: Consolas, monospace;
  }
  .fingerprint-chip span {
    display: block;
    color: #9eb1c3;
    font-size: 11px;
    text-transform: uppercase;
  }
  .fingerprint-chip strong {
    display: block;
    margin-top: 5px;
    color: #d9fff2;
    font-size: 14px;
    letter-spacing: 0;
  }
  .metrics {
    display: grid;
    grid-template-columns: repeat(4, 1fr);
    gap: 1px;
    background: rgba(166, 244, 214, .14);
  }
  .metric { padding: 20px 24px; background: rgba(12, 25, 39, .96); }
  .metric span { display: block; color: #9eb1c3; font-size: 12px; }
  .metric strong { display: block; margin-top: 6px; font-size: 19px; }
  .grid { display: grid; grid-template-columns: 1fr 1.35fr 1fr; gap: 18px; padding: 24px; }
  .panel {
    min-width: 0;
    padding: 18px;
    border: 1px solid rgba(255, 255, 255, .1);
    border-radius: 8px;
    background: rgba(255, 255, 255, .045);
  }
  h2 { margin: 0 0 14px; font-size: 15px; letter-spacing: 0; }
  .role-map { display: grid; grid-template-columns: repeat(auto-fit, minmax(18px, 1fr)); gap: 7px; }
  .gene { height: 40px; border-radius: 5px; box-shadow: inset 0 -8px 14px rgba(0, 0, 0, .18); }
  .legend {
    display: flex;
    flex-wrap: wrap;
    gap: 8px;
    margin-top: 14px;
    color: #cbd7e2;
    font-size: 12px;
  }
  .legend span {
    display: inline-flex;
    align-items: center;
    gap: 6px;
    padding: 5px 8px;
    border-radius: 999px;
    background: rgba(255, 255, 255, .06);
  }
  .legend i {
    display: inline-block;
    width: 9px;
    height: 9px;
    border-radius: 50%;
  }
  .signals-table { width: 100%; border-collapse: collapse; font-size: 13px; }
  .signals-table th { color: #9eb1c3; font-weight: 600; text-align: left; }
  .signals-table th, .signals-table td { padding: 8px 6px; border-bottom: 1px solid rgba(255, 255, 255, .08); }
  .notes { margin: 0; padding-left: 18px; color: #dbe8e4; }
  .notes li { margin: 0 0 9px; }
  @media (max-width: 820px) {
    .hero, .grid { grid-template-columns: 1fr; display: grid; }
    .fingerprints { grid-template-columns: 1fr; }
    .metrics { grid-template-columns: repeat(2, 1fr); }
    h1 { font-size: 27px; }
  }
  "
}
