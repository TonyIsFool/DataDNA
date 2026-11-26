as_data_dna <- function(x, name = NULL, sample_size = 10000L) {
  if (inherits(x, "data_dna")) {
    return(x)
  }
  data_dna(x, name = name, sample_size = sample_size)
}

stable_hash <- function(x) {
  text <- paste(as.character(x), collapse = "\r")
  bytes <- as.integer(charToRaw(enc2utf8(text)))
  hash <- 2166136261
  modulus <- 4294967291

  if (!length(bytes)) {
    return("00000000")
  }

  for (byte in bytes) {
    hash <- (hash + byte) %% modulus
    hash <- (hash * 16777619) %% modulus
  }

  sprintf("%08X", as.integer(hash %% 2147483647))
}

compact_id <- function(...) {
  raw <- stable_hash(unlist(list(...), use.names = FALSE))
  paste0("DNA-", substr(raw, 1, 4), "-", substr(raw, 5, 8), "-", substr(stable_hash(raw), 1, 4))
}

safe_mean <- function(x) {
  if (is.null(x) || !length(x)) {
    return(NA_real_)
  }
  x <- x[is.finite(x)]
  if (!length(x)) NA_real_ else mean(x)
}

safe_prop <- function(cond) {
  if (is.null(cond) || !length(cond)) {
    return(0)
  }
  cond <- cond[!is.na(cond)]
  if (!length(cond)) 0 else mean(as.logical(cond))
}

safe_sd <- function(x) {
  if (is.null(x) || !length(x)) {
    return(NA_real_)
  }
  x <- x[is.finite(x)]
  if (length(x) < 2L) NA_real_ else stats::sd(x)
}

safe_quantile <- function(x) {
  if (is.null(x) || !length(x)) {
    return(rep(NA_real_, 5L))
  }
  x <- x[is.finite(x)]
  if (!length(x)) {
    return(rep(NA_real_, 5L))
  }
  as.numeric(stats::quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1), names = FALSE, type = 7))
}

clip01 <- function(x) {
  pmax(0, pmin(1, x))
}

jaccard <- function(x, y) {
  x <- unique(x[!is.na(x)])
  y <- unique(y[!is.na(y)])
  denom <- length(union(x, y))
  if (denom == 0L) {
    return(1)
  }
  length(intersect(x, y)) / denom
}

name_similarity <- function(a, b) {
  a_clean <- tolower(gsub("[^a-zA-Z0-9]+", "_", as.character(a)))
  b_clean <- tolower(gsub("[^a-zA-Z0-9]+", "_", as.character(b)))
  if (identical(a_clean, b_clean)) {
    return(1)
  }
  tok_a <- setdiff(strsplit(a_clean, "_", fixed = TRUE)[[1L]], "")
  tok_b <- setdiff(strsplit(b_clean, "_", fixed = TRUE)[[1L]], "")
  tok_sim <- if (length(tok_a) && length(tok_b)) jaccard(tok_a, tok_b) else 0
  max_len <- max(nchar(a_clean), nchar(b_clean), 1L)
  edit_dist <- as.numeric(utils::adist(a_clean, b_clean))
  char_sim <- clip01(1 - edit_dist / max_len)
  max(tok_sim, char_sim)
}

weighted_score <- function(values, weights) {
  ok <- is.finite(values) & is.finite(weights) & weights > 0
  if (!any(ok)) {
    return(NA_real_)
  }
  sum(values[ok] * weights[ok]) / sum(weights[ok])
}

format_pct <- function(x, digits = 0L) {
  if (!length(x) || !is.finite(x[[1L]])) {
    return("NA")
  }
  paste0(round(100 * x[[1L]], digits), "%")
}

round_num <- function(x, digits = 3L) {
  if (!length(x)) {
    return(numeric())
  }
  ifelse(is.finite(x), round(x, digits), NA_real_)
}

role_palette <- function(role) {
  palette <- c(
    id = "#16795A",
    time = "#1E4E79",
    measure = "#B4691E",
    category = "#8F4B6C",
    flag = "#4D7A3B",
    text = "#5B528C",
    target = "#B83B3B",
    unknown = "#8C939E"
  )
  unname(palette[match(role, names(palette), nomatch = match("unknown", names(palette)))])
}

html_escape <- function(x, attr = FALSE) {
  if (inherits(x, "dna_html_raw")) {
    return(as.character(x))
  }
  out <- enc2utf8(as.character(x))
  out <- gsub("&", "&amp;", out, fixed = TRUE)
  out <- gsub("<", "&lt;", out, fixed = TRUE)
  out <- gsub(">", "&gt;", out, fixed = TRUE)
  if (isTRUE(attr)) {
    out <- gsub("\"", "&quot;", out, fixed = TRUE)
    out <- gsub("'", "&#39;", out, fixed = TRUE)
  }
  out
}

html_raw <- function(x) {
  structure(paste(as.character(x), collapse = ""), class = c("dna_html_raw", "character"))
}

flatten_html_children <- function(items) {
  out <- character()
  for (item in items) {
    if (is.null(item)) {
      next
    }
    if (inherits(item, c("dna_html_raw", "dna_html"))) {
      out <- c(out, as.character(item))
    } else if (is.list(item)) {
      out <- c(out, flatten_html_children(item))
    } else if (length(item)) {
      out <- c(out, html_escape(item, attr = FALSE))
    }
  }
  out
}

html_tag <- function(tag, ..., .void = FALSE) {
  args <- list(...)
  nms <- names(args)
  if (is.null(nms)) {
    nms <- rep("", length(args))
  }
  is_attr <- nzchar(nms)
  attrs <- args[is_attr]
  children <- args[!is_attr]

  attr_str <- ""
  if (length(attrs)) {
    rendered <- character()
    for (i in seq_along(attrs)) {
      val <- attrs[[i]]
      if (is.null(val) || identical(val, FALSE) || (length(val) == 1L && is.na(val))) {
        next
      }
      key <- nms[[i]]
      if (identical(val, TRUE)) {
        rendered <- c(rendered, key)
      } else {
        rendered <- c(rendered, paste0(key, "=\"", html_escape(paste(val, collapse = " "), attr = TRUE), "\""))
      }
    }
    if (length(rendered)) {
      attr_str <- paste0(" ", paste(rendered, collapse = " "))
    }
  }

  if (isTRUE(.void)) {
    return(html_raw(paste0("<", tag, attr_str, ">")))
  }

  inner <- paste(flatten_html_children(children), collapse = "")
  html_raw(paste0("<", tag, attr_str, ">", inner, "</", tag, ">"))
}

new_dna_html <- function(tag_obj, title = "DataDNA card") {
  doc <- paste0("<!DOCTYPE html>\n", as.character(tag_obj))
  structure(
    doc,
    title = title,
    class = c("dna_html", "character")
  )
}

save_dna_html <- function(card, file) {
  dir_path <- dirname(file)
  if (!nzchar(dir_path) || !dir.exists(dir_path)) {
    stop("Directory does not exist: ", dir_path, call. = FALSE)
  }
  con <- file(file, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(enc2utf8(as.character(card))), con)
  invisible(file)
}

#' @export
as.character.dna_html <- function(x, ...) {
  unclass(x)[[1L]]
}

#' @export
print.dna_html <- function(x, browse = interactive(), ...) {
  if (isTRUE(browse)) {
    viewer <- getOption("viewer")
    tmp <- tempfile(pattern = "datadna-", fileext = ".html")
    save_dna_html(x, tmp)
    if (is.function(viewer)) {
      viewer(tmp)
      return(invisible(x))
    }
    utils::browseURL(normalizePath(tmp, winslash = "/", mustWork = FALSE))
    return(invisible(x))
  }
  title <- attr(x, "title", exact = TRUE)
  cat("DataDNA HTML document", if (!is.null(title)) paste0(": ", title) else "", "\n", sep = "")
  cat("  Size: ", format(nchar(as.character(x), type = "bytes"), big.mark = ","), " bytes\n", sep = "")
  invisible(x)
}
