#' Render a precision-instrument interactive data DNA card
#'
#' Builds a self-contained HTML specimen instrument for a data frame or
#' `data_dna` object without external runtime dependencies. Features a
#' high-density interactive DNA electrophoresis spectrum, calibrated column
#' oscilloscope, click-to-copy hashes, and light/dark instrument modes.
#'
#' @param x A data frame or `data_dna` object.
#' @param file Optional HTML file path. If supplied, the card is saved there.
#' @param open Logical. Open the saved file in a browser when `file` is supplied.
#'
#' @return A `dna_html` document object, invisibly when saved to `file`.
#' @export
#'
#' @examples
#' demo <- dna_example_customers()
#' card <- dna_card(demo$customers_new)
dna_card <- function(x, file = NULL, open = FALSE) {
  dna <- as_data_dna(x, name = paste(deparse(substitute(x)), collapse = " "))
  card <- build_dna_card(dna)

  if (!is.null(file)) {
    save_dna_html(card, file = file)
    if (isTRUE(open)) {
      utils::browseURL(normalizePath(file, winslash = "/", mustWork = FALSE))
    }
    return(invisible(card))
  }

  card
}

build_dna_card <- function(dna) {
  doc <- html_tag(
    "html",
    lang = "en",
    `data-theme` = "light",
    html_tag(
      "head",
      html_tag("meta", charset = "utf-8", .void = TRUE),
      html_tag("meta", name = "viewport", content = "width=device-width, initial-scale=1", .void = TRUE),
      html_tag("title", paste("DataDNA", dna$name)),
      html_tag("style", html_raw(dna_card_css()))
    ),
    html_tag(
      "body",
      html_tag(
        "main",
        class = "dna-shell",
        html_tag(
          "section",
          class = "dna-card",
          html_tag(
            "div",
            class = "top-rail",
            html_tag("span", class = "led"),
            html_tag("span", class = "kicker", "DataDNA specimen card // TE-SPEC 0.2"),
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
              html_tag("h1", dna$name),
              html_tag(
                "div",
                class = "meta-row",
                html_tag(
                  "button",
                  type = "button",
                  class = "dna-id copyable",
                  `data-copy` = dna$dna_id,
                  title = "Click to copy DNA ID",
                  dna$dna_id
                ),
                html_tag("span", class = "meta-sep", "/"),
                html_tag("span", class = "meta-txt", toupper(gsub("_", " ", dna$species, fixed = TRUE))),
                html_tag("span", class = "meta-sep", "/"),
                html_tag("span", class = "meta-txt", paste0(format(dna$n_rows, big.mark = ","), " x ", dna$n_cols))
              )
            ),
            html_tag(
              "div",
              class = paste("risk", dna$scores$drift_risk),
              html_tag("span", "IDENTITY / RISK"),
              html_tag(
                "div",
                class = "risk-readout",
                html_tag("strong", paste0(round(dna$scores$identity_strength))),
                html_tag("em", paste0("RISK ", toupper(dna$scores$drift_risk)))
              )
            )
          ),
          html_tag(
            "div",
            class = "gel-section",
            html_tag(
              "div",
              class = "gel-head",
              html_tag("span", "01  ELECTROPHORESIS SIGNATURE"),
              html_tag("span", class = "lcd-readout", id = "lcd-readout", "HOVER BAND TO INSPECT LOCUS")
            ),
            html_tag(
              "div",
              class = "barcode",
              lapply(seq_along(dna_barcode_values(dna)), function(i) {
                vals <- dna_barcode_values(dna)
                v <- vals[[i]]
                is_peak <- v >= 84
                html_tag(
                  "i",
                  class = if (is_peak) "peak" else NULL,
                  `data-band` = sprintf("%02d", i),
                  `data-amp` = paste0(round(v), "%"),
                  style = paste0("--h:", v, "%;")
                )
              })
            )
          ),
          fingerprint_stack(dna),
          html_tag(
            "div",
            class = "metrics",
            metric_tile("SPECIES", toupper(gsub("_", " ", dna$species, fixed = TRUE))),
            metric_tile("SHAPE", paste0(format(dna$n_rows, big.mark = ","), " x ", dna$n_cols)),
            metric_tile("IDENTITY", paste0(round(dna$scores$identity_strength), "/100")),
            metric_tile("STABILITY", toupper(dna$scores$stability))
          ),
          html_tag(
            "div",
            class = "body-stack",
            html_tag(
              "div",
              class = "panel",
              html_tag(
                "div",
                class = "panel-head",
                html_tag("h2", "02  ROLE FILTER"),
                html_tag("span", class = "panel-sub", "CLICK TO ISOLATE COLUMNS")
              ),
              role_map(dna)
            ),
            html_tag(
              "div",
              class = "panel",
              html_tag(
                "div",
                class = "panel-head",
                html_tag("h2", "03  COLUMN SIGNAL OSCILLOSCOPE"),
                html_tag("span", class = "panel-sub", "CLICK HEADER TO SORT")
              ),
              column_signal_table(dna)
            ),
            html_tag(
              "div",
              class = "panel",
              html_tag(
                "div",
                class = "panel-head",
                html_tag("h2", "04  DIAGNOSTIC LOG"),
                html_tag("span", class = "panel-sub", "SIGNALS & ALERTS")
              ),
              lab_notes(dna)
            )
          )
        ),
        html_tag("div", class = "toast", id = "dna-toast", role = "status", `aria-live` = "polite")
      ),
      html_tag("script", type = "text/javascript", html_raw(dna_card_js()))
    )
  )
  new_dna_html(doc, title = paste("DataDNA", dna$name))
}

metric_tile <- function(label, value) {
  html_tag(
    "div",
    class = "metric",
    html_tag("span", label),
    html_tag("strong", value)
  )
}

dna_barcode_values <- function(dna) {
  text <- paste(dna$fingerprints$schema_hash, dna$fingerprints$shape_hash, dna$fingerprints$profile_hash, dna$dna_id)
  bytes <- as.integer(charToRaw(text))
  n <- 72L
  vals <- numeric(n)
  for (i in seq_len(n)) {
    b <- bytes[[((i - 1L) %% length(bytes)) + 1L]]
    vals[[i]] <- 18 + ((b * 31L + i * 19L) %% 78L)
  }
  vals
}

role_map <- function(dna) {
  cols <- dna$columns
  if (!nrow(cols)) {
    return(html_tag("p", class = "empty-note", "NO COLUMNS AVAILABLE."))
  }
  role_legend(cols)
}

fingerprint_stack <- function(dna) {
  html_tag(
    "div",
    class = "fingerprints",
    fingerprint_chip("SCHEMA", dna$fingerprints$schema_hash),
    fingerprint_chip("SHAPE", dna$fingerprints$shape_hash),
    fingerprint_chip("PROFILE", dna$fingerprints$profile_hash)
  )
}

fingerprint_chip <- function(label, value) {
  html_tag(
    "button",
    type = "button",
    class = "fingerprint-chip copyable",
    `data-copy` = value,
    title = paste("Click to copy", label, "hash"),
    html_tag("span", label),
    html_tag("strong", value)
  )
}

role_legend <- function(cols) {
  counts <- sort(table(cols$role), decreasing = TRUE)
  html_tag(
    "div",
    class = "legend",
    lapply(names(counts), function(role) {
      html_tag(
        "button",
        type = "button",
        class = "legend-item",
        `data-role` = role,
        html_tag("span", toupper(role)),
        html_tag("em", sprintf("%02d", as.integer(counts[[role]])))
      )
    })
  )
}

column_signal_table <- function(dna) {
  cols <- dna$columns
  if (!nrow(cols)) {
    return(html_tag("p", class = "empty-note", "NO COLUMNS TO DISPLAY."))
  }
  order_idx <- order(cols$missing_rate, -cols$unique_rate, decreasing = TRUE, na.last = TRUE)
  show <- cols[utils::head(order_idx, 20L), , drop = FALSE]
  html_tag(
    "table",
    class = "signals-table",
    id = "signals-table",
    html_tag(
      "thead",
      html_tag(
        "tr",
        html_tag("th", `data-sort` = "0", `data-type` = "str", "COLUMN"),
        html_tag("th", `data-sort` = "1", `data-type` = "str", "ROLE"),
        html_tag("th", class = "num", `data-sort` = "2", `data-type` = "num", "MISS"),
        html_tag("th", class = "scope-col", "SPECTRUM  0 - 100%"),
        html_tag("th", class = "num", `data-sort` = "4", `data-type` = "num", "UNIQ")
      )
    ),
    html_tag(
      "tbody",
      lapply(seq_len(nrow(show)), function(i) {
        miss <- if (is.finite(show$missing_rate[[i]])) clip01(show$missing_rate[[i]]) else 0
        uniq <- if (is.finite(show$unique_rate[[i]])) clip01(show$unique_rate[[i]]) else 0
        has_miss <- miss > 0.005
        html_tag(
          "tr",
          `data-role` = show$role[[i]],
          html_tag("td", class = "col-name", `data-val` = show$name[[i]], show$name[[i]]),
          html_tag("td", class = "col-role", `data-val` = show$role[[i]], toupper(show$role[[i]])),
          html_tag("td", class = if (has_miss) "num alert" else "num muted", `data-val` = sprintf("%.6f", miss), format_pct(miss)),
          html_tag(
            "td",
            class = "scope-cell",
            html_tag(
              "div",
              class = "scope-track",
              html_tag("span", class = "scope-fill", style = paste0("width:", round(100 * uniq), "%;")),
              if (has_miss) html_tag("span", class = "scope-miss", style = paste0("left:", round(100 * miss), "%;"))
            )
          ),
          html_tag("td", class = "num", `data-val` = sprintf("%.6f", uniq), format_pct(uniq))
        )
      })
    )
  )
}

lab_notes <- function(dna) {
  notes <- c(dna$signals, dna$warnings)
  if (!length(notes)) {
    notes <- "No structural or quality warnings flagged."
  }
  html_tag(
    "ul",
    class = "notes",
    lapply(utils::head(notes, 8L), function(item) html_tag("li", item))
  )
}

dna_card_css <- function() {
  "
  :root, [data-theme='light'] {
    --bg: #EAE8E1;
    --plate: #F4F3EF;
    --well: #E8E6DF;
    --surface: #FAF9F6;
    --ink: #141414;
    --ink-2: #5A5A54;
    --ink-3: #96948D;
    --border: #D2D0C8;
    --signal: #FF4F00;
  }
  [data-theme='dark'] {
    --bg: #080808;
    --plate: #111111;
    --well: #181817;
    --surface: #151514;
    --ink: #F0EFEA;
    --ink-2: #9A9992;
    --ink-3: #5C5C57;
    --border: #262624;
    --signal: #FF5A1F;
  }
  * { box-sizing: border-box; }
  body {
    margin: 0;
    min-height: 100vh;
    color: var(--ink);
    background: var(--bg);
    font-family: 'SF Mono', 'JetBrains Mono', ui-monospace, Consolas, monospace;
    -webkit-font-smoothing: antialiased;
    font-variant-numeric: tabular-nums;
    transition: background .18s ease, color .18s ease;
  }
  .dna-shell { width: min(1080px, calc(100vw - 32px)); margin: 32px auto 56px; }
  .dna-card {
    border: 1px solid var(--border);
    background: var(--plate);
    border-radius: 4px;
    overflow: hidden;
  }
  .top-rail {
    display: flex;
    align-items: center;
    gap: 10px;
    padding: 12px 28px;
    border-bottom: 1px solid var(--border);
    background: var(--surface);
  }
  .led { width: 18px; height: 5px; background: var(--signal); display: inline-block; }
  .kicker { font-size: 11px; font-weight: 700; letter-spacing: .1em; color: var(--ink-2); text-transform: uppercase; }
  .theme-toggle {
    margin-left: auto;
    padding: 4px 10px;
    border: 1px solid var(--border);
    border-radius: 2px;
    background: transparent;
    color: var(--ink-2);
    font: 600 10px inherit;
    letter-spacing: .08em;
    cursor: pointer;
  }
  .theme-toggle:hover { border-color: var(--ink); color: var(--ink); }
  .hero {
    display: flex;
    justify-content: space-between;
    align-items: flex-end;
    gap: 24px;
    padding: 28px 28px 24px;
    border-bottom: 1px solid var(--ink);
  }
  h1 {
    margin: 0 0 10px;
    font-family: -apple-system, BlinkMacSystemFont, 'Inter', 'Segoe UI', sans-serif;
    font-size: 30px;
    font-weight: 700;
    letter-spacing: -.03em;
    line-height: 1.05;
  }
  .meta-row { display: flex; flex-wrap: wrap; align-items: center; gap: 10px; font-size: 11.5px; color: var(--ink-2); }
  .meta-sep { color: var(--border); }
  .dna-id {
    padding: 3px 8px;
    border: 1px solid var(--border);
    border-radius: 2px;
    background: var(--surface);
    color: var(--ink);
    font: 600 11.5px inherit;
    cursor: pointer;
  }
  .dna-id:hover { border-color: var(--signal); color: var(--signal); }
  .risk { text-align: right; }
  .risk span { display: block; font-size: 10px; color: var(--ink-3); letter-spacing: .1em; }
  .risk-readout { display: flex; align-items: baseline; justify-content: flex-end; gap: 10px; margin-top: 4px; }
  .risk-readout strong { font-size: 34px; line-height: 1; font-weight: 700; }
  .risk.high .risk-readout strong, .risk.high .risk-readout em { color: var(--signal); }
  .risk-readout em { font-style: normal; font-size: 11px; color: var(--ink-2); font-weight: 600; }
  .gel-section { padding: 20px 28px 22px; border-bottom: 1px solid var(--border); }
  .gel-head { display: flex; justify-content: space-between; align-items: center; margin-bottom: 12px; font-size: 10.5px; font-weight: 700; letter-spacing: .08em; }
  .lcd-readout { color: var(--signal); font-weight: 600; }
  .barcode {
    display: flex;
    align-items: center;
    gap: 3px;
    height: 92px;
    padding: 12px 14px;
    background: var(--well);
    border: 1px solid var(--border);
    border-radius: 2px;
  }
  .barcode i {
    flex: 1 1 0;
    display: block;
    height: var(--h);
    background: var(--ink);
    opacity: .62;
    cursor: crosshair;
    transition: opacity .1s ease, background .1s ease, transform .1s ease;
  }
  .barcode i.peak { background: var(--signal); opacity: .92; }
  .barcode i:hover { opacity: 1; background: var(--signal); transform: scaleY(1.12); }
  .fingerprints {
    display: grid;
    grid-template-columns: repeat(3, 1fr);
    gap: 1px;
    background: var(--border);
    border-bottom: 1px solid var(--border);
  }
  .fingerprint-chip {
    border: 0;
    text-align: left;
    padding: 12px 28px;
    background: var(--surface);
    font-family: inherit;
    cursor: pointer;
  }
  .fingerprint-chip:hover { background: var(--well); }
  .fingerprint-chip span { display: block; font-size: 10px; color: var(--ink-3); letter-spacing: .1em; }
  .fingerprint-chip strong { display: block; margin-top: 4px; font-size: 13px; color: var(--ink); }
  .metrics {
    display: grid;
    grid-template-columns: repeat(4, 1fr);
    gap: 1px;
    background: var(--border);
    border-bottom: 1px solid var(--border);
  }
  .metric { padding: 14px 28px; background: var(--plate); }
  .metric span { display: block; font-size: 10px; color: var(--ink-3); letter-spacing: .1em; }
  .metric strong { display: block; margin-top: 4px; font-size: 14px; font-weight: 700; }
  .body-stack { display: grid; gap: 24px; padding: 24px 28px 30px; }
  .panel-head {
    display: flex;
    justify-content: space-between;
    align-items: baseline;
    margin-bottom: 12px;
    padding-bottom: 8px;
    border-bottom: 1px solid var(--ink);
  }
  h2 { margin: 0; font-size: 11px; font-weight: 700; letter-spacing: .1em; }
  .panel-sub { font-size: 10px; color: var(--ink-3); letter-spacing: .06em; }
  .legend { display: flex; flex-wrap: wrap; gap: 8px; }
  .legend-item {
    display: inline-flex;
    align-items: center;
    gap: 8px;
    padding: 6px 12px;
    border: 1px solid var(--border);
    border-radius: 2px;
    background: var(--surface);
    color: var(--ink);
    font: 600 11px inherit;
    letter-spacing: .06em;
    cursor: pointer;
  }
  .legend-item em { font-style: normal; color: var(--ink-3); }
  .legend-item:hover { border-color: var(--ink); }
  .legend-item.active { background: var(--ink); color: var(--plate); border-color: var(--ink); }
  .legend-item.active em { color: var(--signal); }
  .signals-table { width: 100%; border-collapse: collapse; font-size: 12px; }
  .signals-table th {
    padding: 0 8px 8px;
    border-bottom: 1px solid var(--border);
    color: var(--ink-3);
    font-size: 10px;
    font-weight: 700;
    letter-spacing: .08em;
    text-align: left;
    cursor: pointer;
    user-select: none;
  }
  .signals-table th:hover { color: var(--ink); }
  .signals-table th.num, .signals-table td.num { text-align: right; white-space: nowrap; }
  .signals-table td { padding: 9px 8px; border-bottom: 1px solid var(--border); transition: opacity .15s ease; }
  .signals-table tr:hover td { background: var(--well); }
  .signals-table tr.dimmed td { opacity: .22; }
  .col-name { font-family: -apple-system, BlinkMacSystemFont, 'Inter', sans-serif; font-weight: 600; color: var(--ink); }
  .col-role { color: var(--ink-2); font-size: 11px; }
  .muted { color: var(--ink-3); }
  .alert { color: var(--signal); font-weight: 700; }
  .scope-col { width: 34%; }
  .scope-track {
    position: relative;
    height: 1px;
    background: var(--border);
  }
  .scope-fill {
    position: absolute;
    left: 0;
    top: -1px;
    height: 3px;
    background: var(--ink);
  }
  .scope-fill::after {
    content: '';
    position: absolute;
    right: -1px;
    top: -4px;
    width: 2px;
    height: 11px;
    background: var(--ink);
  }
  .scope-miss {
    position: absolute;
    top: -6px;
    width: 3px;
    height: 13px;
    background: var(--signal);
  }
  .notes { margin: 0; padding: 0; list-style: none; display: grid; gap: 6px; }
  .notes li {
    padding: 8px 12px;
    border-left: 2px solid var(--ink);
    background: var(--surface);
    color: var(--ink-2);
    font-size: 12px;
  }
  .empty-note { margin: 0; color: var(--ink-3); font-size: 12px; }
  .toast {
    position: fixed;
    right: 24px;
    bottom: 24px;
    padding: 8px 14px;
    background: var(--ink);
    color: var(--plate);
    font-size: 11px;
    letter-spacing: .06em;
    opacity: 0;
    pointer-events: none;
    transition: opacity .15s ease;
  }
  .toast.show { opacity: 1; }
  @media (max-width: 820px) {
    .hero { flex-direction: column; align-items: flex-start; }
    .risk { text-align: left; }
    .risk-readout { justify-content: flex-start; }
    .fingerprints { grid-template-columns: 1fr; }
    .metrics { grid-template-columns: repeat(2, 1fr); }
    .scope-col, .scope-cell { display: none; }
  }
  "
}

dna_card_js <- function() {
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

    var lcd = document.getElementById('lcd-readout');
    document.querySelectorAll('.barcode i').forEach(function(bar) {
      bar.addEventListener('mouseenter', function() {
        if (lcd) lcd.textContent = 'LOCUS ' + bar.getAttribute('data-band') + '  //  AMPLITUDE ' + bar.getAttribute('data-amp');
      });
      bar.addEventListener('mouseleave', function() {
        if (lcd) lcd.textContent = 'HOVER BAND TO INSPECT LOCUS';
      });
    });

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

    var activeRole = null;
    var btns = document.querySelectorAll('.legend-item');
    var rows = document.querySelectorAll('#signals-table tbody tr');
    btns.forEach(function(b) {
      b.addEventListener('click', function() {
        var role = b.getAttribute('data-role');
        activeRole = (activeRole === role) ? null : role;
        btns.forEach(function(x) { x.classList.toggle('active', x.getAttribute('data-role') === activeRole); });
        rows.forEach(function(r) { r.classList.toggle('dimmed', !!activeRole && r.getAttribute('data-role') !== activeRole); });
      });
    });

    var table = document.getElementById('signals-table');
    if (table) {
      var st = { idx: -1, asc: true };
      table.querySelectorAll('thead th[data-sort]').forEach(function(th) {
        th.addEventListener('click', function() {
          var idx = parseInt(th.getAttribute('data-sort'), 10);
          var type = th.getAttribute('data-type');
          var tbody = table.querySelector('tbody');
          var trs = Array.prototype.slice.call(tbody.querySelectorAll('tr'));
          st.asc = (st.idx === idx) ? !st.asc : (type === 'str');
          st.idx = idx;
          trs.sort(function(a, b) {
            var va = a.children[idx].getAttribute('data-val') || a.children[idx].textContent;
            var vb = b.children[idx].getAttribute('data-val') || b.children[idx].textContent;
            var cmp = type === 'num' ? (parseFloat(va) - parseFloat(vb)) : va.localeCompare(vb);
            return st.asc ? cmp : -cmp;
          });
          trs.forEach(function(tr) { tbody.appendChild(tr); });
        });
      });
    }
  })();
  "
}
