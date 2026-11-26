#' Build or load a compact DataDNA library
#'
#' Profiles a list of data frames/`data_dna` objects, or scans tabular/R data
#' files (`.csv`, `.tsv`, `.rds`, `.rda`, `.RData`) from disk one by one so
#' raw tables do not stay resident in memory.
#'
#' @param x A list of data frames/`data_dna` objects, a directory path, or a
#'   character vector of file paths. Passing a single `.rds` path that stores a
#'   previously saved `dna_library` loads it directly.
#' @param sample_size Maximum number of rows sampled per table when profiling.
#' @param recursive Logical. Search subdirectories when `x` is a directory.
#' @param file Optional `.rds` path where the resulting `dna_library` is saved.
#'
#' @return A `dna_library` object (a named list of `data_dna` objects).
#' @export
#'
#' @examples
#' demo <- dna_example_customers()
#' lib <- dna_library(demo)
#' lib
dna_library <- function(x, sample_size = 10000L, recursive = FALSE, file = NULL) {
  entries <- if (is.list(x)) {
    library_from_list(x, sample_size = sample_size)
  } else if (is.character(x) && length(x) >= 1L) {
    library_from_paths(x, sample_size = sample_size, recursive = recursive)
  } else {
    stop("`x` must be a list, a directory path, or a vector of file paths.", call. = FALSE)
  }

  lib <- structure(entries, class = c("dna_library", "list"))
  if (!is.null(file)) {
    dir_path <- dirname(file)
    if (!nzchar(dir_path) || !dir.exists(dir_path)) {
      stop("Directory does not exist: ", dir_path, call. = FALSE)
    }
    saveRDS(lib, file = file)
    return(invisible(lib))
  }
  lib
}

library_from_list <- function(x, sample_size) {
  if (!length(x)) {
    return(stats::setNames(list(), character()))
  }
  nms <- match_library_names(x)
  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    dna <- as_data_dna(x[[i]], name = nms[[i]], sample_size = sample_size)
    dna$name <- nms[[i]]
    out[[i]] <- dna
  }
  names(out) <- nms
  out
}

library_from_paths <- function(paths, sample_size, recursive) {
  if (length(paths) == 1L && file.exists(paths) && !dir.exists(paths) &&
      tolower(tools::file_ext(paths)) == "rds") {
    obj <- readRDS(paths)
    if (inherits(obj, "dna_library")) {
      return(unclass(obj))
    }
  }

  files <- character()
  for (p in paths) {
    if (dir.exists(p)) {
      found <- list.files(
        p,
        pattern = "\\.(csv|tsv|rds|rda|RData)$",
        ignore.case = TRUE,
        full.names = TRUE,
        recursive = isTRUE(recursive)
      )
      files <- c(files, found)
    } else if (file.exists(p)) {
      files <- c(files, p)
    } else {
      stop("Path does not exist: ", p, call. = FALSE)
    }
  }

  if (!length(files)) {
    stop("No supported data files (.csv, .tsv, .rds, .rda, .RData) found.", call. = FALSE)
  }

  out <- list()
  labels <- character()
  for (f in files) {
    ext <- tolower(tools::file_ext(f))
    stem <- tools::file_path_sans_ext(basename(f))
    if (ext == "csv") {
      df <- utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
      out[[length(out) + 1L]] <- data_dna(df, name = stem, sample_size = sample_size)
      labels <- c(labels, stem)
    } else if (ext == "tsv") {
      df <- utils::read.delim(f, stringsAsFactors = FALSE, check.names = FALSE)
      out[[length(out) + 1L]] <- data_dna(df, name = stem, sample_size = sample_size)
      labels <- c(labels, stem)
    } else if (ext == "rds") {
      obj <- readRDS(f)
      if (inherits(obj, "data_dna")) {
        obj$name <- stem
        out[[length(out) + 1L]] <- obj
        labels <- c(labels, stem)
      } else if (is.data.frame(obj)) {
        out[[length(out) + 1L]] <- data_dna(obj, name = stem, sample_size = sample_size)
        labels <- c(labels, stem)
      }
    } else if (ext %in% c("rda", "rdata")) {
      env <- new.env(parent = emptyenv())
      loaded_names <- load(f, envir = env)
      for (obj_nm in loaded_names) {
        obj <- get(obj_nm, envir = env, inherits = FALSE)
        if (is.data.frame(obj)) {
          entry_nm <- if (length(loaded_names) == 1L) stem else paste0(stem, "$", obj_nm)
          out[[length(out) + 1L]] <- data_dna(obj, name = entry_nm, sample_size = sample_size)
          labels <- c(labels, entry_nm)
        }
      }
    }
  }

  if (!length(out)) {
    stop("No data frames or data_dna objects could be loaded from the supplied paths.", call. = FALSE)
  }

  labels <- make.unique(labels, sep = "_")
  for (i in seq_along(out)) {
    out[[i]]$name <- labels[[i]]
  }
  names(out) <- labels
  out
}

#' @export
print.dna_library <- function(x, ...) {
  cat("DataDNA library (", length(x), " specimen", if (length(x) == 1L) "" else "s", ")\n", sep = "")
  if (!length(x)) {
    return(invisible(x))
  }
  show_n <- min(length(x), 10L)
  for (i in seq_len(show_n)) {
    item <- x[[i]]
    cat(
      "  ", i, ". ", item$name,
      " [", item$species, ", ",
      format(item$n_rows, big.mark = ","), "x", item$n_cols,
      ", ", item$dna_id, "]\n",
      sep = ""
    )
  }
  if (length(x) > show_n) {
    cat("  ... and ", length(x) - show_n, " more\n", sep = "")
  }
  invisible(x)
}
