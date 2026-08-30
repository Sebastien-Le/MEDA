# Internal helpers shared by the factorial analyses.
#
# FactoMineR::dimdesc() already computes and orders the relevant statistics.
# These helpers only normalise its output for typed jamovi tables; they do not
# recompute, filter, or reorder the results.

.meda_empty_dimdesc <- function() {
  list(
    continuous = data.frame(
      dimension = character(), variable = character(),
      correlation = numeric(), p = numeric(), n = integer(),
      stringsAsFactors = FALSE
    ),
    categorical = data.frame(
      dimension = character(), variable = character(),
      r2 = numeric(), p = numeric(),
      stringsAsFactors = FALSE
    ),
    categories = data.frame(
      dimension = character(), category = character(),
      estimate = numeric(), p = numeric(),
      stringsAsFactors = FALSE
    )
  )
}

.meda_dim_label <- function(x, fallback) {
  x <- if (length(x) == 0 || is.na(x) || !nzchar(x)) fallback else x
  number <- sub(".*?([0-9]+)$", "\\1", x)
  if (!identical(number, x) && grepl("^[0-9]+$", number))
    return(paste("Dimension", number))
  x
}

.meda_dimdesc_frame <- function(x) {
  if (is.null(x))
    return(NULL)

  ans <- tryCatch(as.data.frame(x, stringsAsFactors = FALSE),
                  error = function(e) NULL)
  if (is.null(ans) || nrow(ans) == 0)
    return(NULL)

  if (is.null(rownames(ans)))
    rownames(ans) <- as.character(seq_len(nrow(ans)))
  ans
}

.meda_dimdesc_column <- function(x, candidates, fallback = NULL) {
  if (is.null(x) || ncol(x) == 0)
    return(numeric())

  normalised <- tolower(gsub("[^[:alnum:]]", "", names(x)))
  wanted <- tolower(gsub("[^[:alnum:]]", "", candidates))
  hit <- match(wanted, normalised, nomatch = 0L)
  hit <- hit[hit > 0L]

  if (length(hit) > 0)
    return(suppressWarnings(as.numeric(x[[hit[[1]]]])))
  if (!is.null(fallback) && fallback >= 1 && fallback <= ncol(x))
    return(suppressWarnings(as.numeric(x[[fallback]])))
  rep(NA_real_, nrow(x))
}

.meda_dimdesc_n <- function(dimdesc, variables) {
  x <- tryCatch(dimdesc$call$X, error = function(e) NULL)
  if (is.null(x))
    return(rep(NA_integer_, length(variables)))

  vapply(variables, function(variable) {
    if (!variable %in% names(x))
      return(NA_integer_)
    as.integer(sum(!is.na(x[[variable]])))
  }, integer(1))
}

.meda_category_label <- function(x) {
  # Only the first separator is changed: a modality may itself contain '='.
  sub("\\s*=\\s*", " = ", x, perl = TRUE)
}

.meda_tidy_dimdesc <- function(dimdesc) {
  out <- .meda_empty_dimdesc()
  if (is.null(dimdesc) || !is.list(dimdesc))
    return(out)

  dim_names <- names(dimdesc)
  if (is.null(dim_names))
    dim_names <- rep("", length(dimdesc))

  for (i in seq_along(dimdesc)) {
    if (tolower(dim_names[[i]]) == "call")
      next

    dimension <- .meda_dim_label(dim_names[[i]], paste("Dimension", i))
    description <- dimdesc[[i]]
    if (is.null(description) || !is.list(description))
      next

    block_names <- names(description)
    if (is.null(block_names))
      block_names <- rep("", length(description))

    for (j in seq_along(description)) {
      block <- .meda_dimdesc_frame(description[[j]])
      if (is.null(block))
        next

      kind <- tolower(gsub("[^[:alnum:]]", "", block_names[[j]]))
      labels <- rownames(block)

      if (startsWith(kind, "quanti")) {
        out$continuous <- rbind(out$continuous, data.frame(
          dimension = rep(dimension, nrow(block)),
          variable = labels,
          correlation = .meda_dimdesc_column(block, "correlation", 1L),
          p = .meda_dimdesc_column(block, c("p.value", "pvalue"), 2L),
          n = .meda_dimdesc_n(dimdesc, labels),
          stringsAsFactors = FALSE
        ))
      } else if (startsWith(kind, "quali")) {
        out$categorical <- rbind(out$categorical, data.frame(
          dimension = rep(dimension, nrow(block)),
          variable = labels,
          r2 = .meda_dimdesc_column(block, c("R2", "R.square", "Rsquared"), 1L),
          p = .meda_dimdesc_column(block, c("p.value", "pvalue"), 2L),
          stringsAsFactors = FALSE
        ))
      } else if (startsWith(kind, "category")) {
        out$categories <- rbind(out$categories, data.frame(
          dimension = rep(dimension, nrow(block)),
          category = .meda_category_label(labels),
          estimate = .meda_dimdesc_column(block, "Estimate", 1L),
          p = .meda_dimdesc_column(block, c("p.value", "pvalue"), 2L),
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  rownames(out$continuous) <- NULL
  rownames(out$categorical) <- NULL
  rownames(out$categories) <- NULL
  out
}

.meda_fill_dimdesc_table <- function(table, data) {
  has_rows <- !is.null(data) && nrow(data) > 0
  table$setVisible(visible = has_rows)
  if (!has_rows)
    return(invisible(NULL))

  for (i in seq_len(nrow(data))) {
    table$addRow(rowKey = i)
    table$setRow(rowNo = i, values = as.list(data[i, , drop = FALSE]))
  }
  invisible(NULL)
}

.meda_fill_dimdesc_group <- function(group, tidy) {
  if (is.null(tidy))
    tidy <- .meda_empty_dimdesc()

  .meda_fill_dimdesc_table(group$continuous, tidy$continuous)
  .meda_fill_dimdesc_table(group$categorical, tidy$categorical)
  .meda_fill_dimdesc_table(group$categories, tidy$categories)

  group$setVisible(visible = any(vapply(tidy, nrow, integer(1)) > 0L))
  invisible(NULL)
}

# CA has a different dimdesc() contract: each dimension contains one block for
# rows and one for columns.  Keep this adapter separate from the PCA/MCA/MFA
# adapter while sharing the defensive traversal and table-filling rules.
.meda_tidy_ca_dimdesc <- function(dimdesc) {
  out <- data.frame(
    dim = character(), rowcol = character(), name = character(),
    coord = numeric(), stringsAsFactors = FALSE
  )

  if (is.null(dimdesc) || !is.list(dimdesc))
    return(out)

  dim_names <- names(dimdesc)
  if (is.null(dim_names))
    dim_names <- rep("", length(dimdesc))

  for (i in seq_along(dimdesc)) {
    if (tolower(dim_names[[i]]) == "call")
      next

    # Preserve the CA presentation already used by MEDA ("Dim 1", "Dim 2", …).
    dimension <- dim_names[[i]]
    if (is.na(dimension) || !nzchar(dimension))
      dimension <- paste("Dim", i)
    description <- dimdesc[[i]]
    if (is.null(description) || !is.list(description))
      next

    block_names <- names(description)
    if (is.null(block_names))
      block_names <- rep("", length(description))

    for (j in seq_along(description)) {
      block <- .meda_dimdesc_frame(description[[j]])
      if (is.null(block))
        next

      out <- rbind(out, data.frame(
        dim = rep(dimension, nrow(block)),
        rowcol = rep(block_names[[j]], nrow(block)),
        name = rownames(block),
        coord = suppressWarnings(as.numeric(block[[1L]])),
        stringsAsFactors = FALSE
      ))
    }
  }

  rownames(out) <- NULL
  out
}

# Generic guards shared by PCA, CA, MCA and MFA. They deliberately stay
# independent from the user interface so the same statistical contract is
# applied by the analyses, the image renderers and the saved outputs.
.meda_integer_scalar <- function(value, minimum = 1L, maximum = Inf,
                                 alternatives = numeric()) {
  value <- suppressWarnings(as.numeric(value))
  length(value) == 1L && is.finite(value) && value %% 1 == 0 &&
    (value %in% alternatives || (value >= minimum && value <= maximum))
}

.meda_valid_axes <- function(x, y, n_axes) {
  axes <- suppressWarnings(as.numeric(c(x, y)))
  n_axes <- suppressWarnings(as.numeric(n_axes))

  if (length(axes) != 2L || length(n_axes) != 1L ||
      !all(is.finite(axes)) || !is.finite(n_axes) ||
      any(axes %% 1 != 0) || any(axes < 1) ||
      axes[[1L]] == axes[[2L]] || any(axes > n_axes))
    return(NULL)

  as.integer(axes)
}

.meda_hcpc_coordinates <- function(coordinates, ncp, nbclust,
                                   label = "Clustering") {
  coordinates <- tryCatch(
    as.data.frame(coordinates, check.names = FALSE),
    error = function(e) NULL
  )
  if (is.null(coordinates) || ncol(coordinates) < 1L)
    jmvcore::reject(paste0(label, " failed: no factor coordinates are available"))

  if (!.meda_integer_scalar(ncp, minimum = 1L))
    jmvcore::reject("The number of components used for clustering must be a positive integer")
  ncp <- min(as.integer(ncp), ncol(coordinates))
  coordinates <- coordinates[, seq_len(ncp), drop = FALSE]

  if (!all(is.finite(as.matrix(coordinates))))
    jmvcore::reject(paste0(label, " failed: non-finite factor coordinates were detected"))
  if (nrow(coordinates) < 3L)
    jmvcore::reject(paste0(label, " requires at least three observations"))

  if (!.meda_integer_scalar(
    nbclust,
    minimum = 2L,
    maximum = nrow(coordinates) - 1L,
    alternatives = -1
  )) {
    jmvcore::reject("The number of clusters must be -1 or an integer between 2 and n - 1")
  }
  nbclust <- as.integer(nbclust)

  n_distinct <- nrow(unique(coordinates))
  required_distinct <- if (nbclust == -1L) 3L else nbclust
  if (n_distinct < required_distinct) {
    jmvcore::reject(paste0(
      label, " requires at least ", required_distinct,
      " distinct factor profiles"
    ))
  }
  if (nbclust == -1L && nrow(coordinates) < 4L)
    jmvcore::reject("Automatic clustering requires at least four observations")

  result <- tryCatch(
    FactoMineR::HCPC(
      coordinates,
      nb.clust = nbclust,
      graph = FALSE,
      description = FALSE
    ),
    error = function(e) {
      jmvcore::reject(paste0(label, " failed: ", conditionMessage(e)))
      NULL
    }
  )
  if (!is.null(result))
    attr(result, "MEDA.ncp.classified") <- ncp
  result
}
