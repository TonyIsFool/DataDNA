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
  x <- x[is.finite(x)]
  if (!length(x)) NA_real_ else mean(x)
}

safe_sd <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2L) NA_real_ else stats::sd(x)
}

safe_quantile <- function(x) {
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

weighted_score <- function(values, weights) {
  ok <- is.finite(values) & is.finite(weights) & weights > 0
  if (!any(ok)) {
    return(NA_real_)
  }
  sum(values[ok] * weights[ok]) / sum(weights[ok])
}

format_pct <- function(x, digits = 0L) {
  if (!is.finite(x)) {
    return("NA")
  }
  paste0(round(100 * x, digits), "%")
}

round_num <- function(x, digits = 3L) {
  ifelse(is.finite(x), round(x, digits), NA_real_)
}

role_palette <- function(role) {
  palette <- c(
    id = "#4dd4ac",
    time = "#63b3ff",
    measure = "#ffd166",
    category = "#f78fb3",
    flag = "#c7f464",
    text = "#b39ddb",
    target = "#ff8a65",
    unknown = "#b9c0c8"
  )
  unname(palette[match(role, names(palette), nomatch = match("unknown", names(palette)))])
}
