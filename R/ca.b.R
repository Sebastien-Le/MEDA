# This file is a generated template, your changes will not be overwritten
CAClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "CAClass",
  inherit = CABase,
  active = list(
    dataProcessed = function() {
      # dataProcessed is an in-run cache only. Data changes are handled by
      # jamovi through clearWith: donnees on persistent result states.
      if (is.null(private$.dataProcessed))
        private$.dataProcessed <- private$.buildData()
      private$.dataProcessed
    },

    nbclust = function() {
      private$.computeNbclust()
    },

    CAResult = function() {
      key <- private$.makeCAKey()
      required_ncp <- private$.requiredNcp()
      cached <- private$.readCAFromCache(key, required_ncp)
      if (!is.null(cached))
        return(cached)

      value <- private$.CA(self$dataProcessed)
      if (!is.null(value)) {
        attr(value, "MEDA.cache.key") <- key
        self$results$caCache$setState(value)
      }
      value
    },

    classifResult = function() {
      key <- private$.makeClassifKey()
      cached <- self$results$classifCache$state
      if (!is.null(cached) && identical(
        attr(cached, "MEDA.cache.key", exact = TRUE), key
      ))
        return(cached)

      value <- private$.classif(self$CAResult)
      if (!is.null(value)) {
        attr(value, "MEDA.cache.key") <- key
        self$results$classifCache$setState(value)
      }
      value
    }
  ),
  
  private = list(
    .dataProcessed = NULL,
    
    #---------------------------------------------
    #### Init + run functions ----
    
    .init = function() {
      if (is.null(self$options$activecol)) {
        if (isTRUE(self$options$tuto))
          self$results$instructions$setVisible(visible = TRUE)
      }
      self$results$instructions$setContent(
        "
  <div style='
      font-family: inherit;
      margin: 8px 0;
      padding: 14px 18px;
      background-color: #F4F7FB;
      border: 1px solid #CBD8E8;
      border-left: 5px solid #6B9DE8;
      border-radius: 6px;
      color: #333333;
      line-height: 1.45;
  '>

    <p style='
        margin: 0 0 10px 0;
        color: #355F98;
        font-size: 1.08em;
    '>
      <b>What you should know before running a CA in jamovi</b>
    </p>

    <div style='
        border-top: 1px solid #CBD8E8;
        margin-bottom: 12px;
    '></div>

    <p style='margin: 0 0 9px 0;'>
      <b>Purpose.</b>
      Correspondence Analysis (CA) is a multivariate statistical method used
      to analyze the relationships between the rows and columns of a
      contingency table. It provides a graphical representation of departures
      from independence between the two categorical variables.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Interpretation.</b>
      Row points located close to one another have similar column profiles,
      while column points located close to one another have similar row
      profiles. Points far from the origin generally have more distinctive
      profiles and contribute more strongly to the dimensions.
    </p>

    <p style='margin: 0 0 9px 0;'>
      Associations between row and column categories should be interpreted
      from their positions relative to the origin and their contributions to
      the dimensions, rather than from the distance between a row point and a
      column point alone.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Columns.</b>
      While the <i>Active Columns</i> field is <b>mandatory</b>, the
      <i>Supplementary Columns</i> field is optional. Supplementary columns do
      not determine the dimensions, but they may provide valuable information
      for interpreting the structure revealed by the active columns.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Clustering.</b>
      Clustering is based on the number of components saved. By default,
      clustering uses the first five components; that is, the distance between
      rows is calculated from their coordinates on these five components.
    </p>

    <p style='margin: 0;'>
      By default, the <i>Number of clusters</i> field is set to -1, which means
      that the number of clusters is selected automatically by the clustering
      procedure.
    </p>

  </div>
  "
      )
    },
    
    .run = function() {
      # Private R6 caches are valid only within the current run/redraw cycle.
      # Persistent freshness across runs is governed by jamovi result states.
      private$.resetRunCaches()

      if (is.null(self$options$activecol))
        return()
      
      private$.errorCheck()
      
      data      <- self$dataProcessed
      res.ca    <- self$CAResult
      
      if (is.null(res.ca) || !inherits(res.ca, "CA")) {
        jmvcore::reject("CA failed. Please check your data.")
        return()
      }
      
      res.classif <- NULL
      need_classif <- isTRUE(self$options$graphclassif) ||
        (isTRUE(self$options$newvar2) &&
         self$results$newvar2$isNotFilled())
      if (need_classif)
        res.classif <- self$classifResult
      
      res.xsq <- private$.chisq(data)
      private$.chideux(res.xsq)
      
      tab  <- private$.dimdesc(res.ca)
      if (isTRUE(self$options$showCode))
        self$results$code$setContent(private$.code(res.ca))
      
      if (!is.null(tab))
        private$.dodTable(tab)
      
      private$.printeigenTable(res.ca)
      private$.printTables(res.ca, "coord")
      private$.printTables(res.ca, "contrib")
      private$.printTables(res.ca, "cos2")
      
      marker <- list(ready = TRUE)
      if (is.null(self$results$ploticol$state))
        self$results$ploticol$setState(marker)
      if (is.null(self$results$plotirow$state))
        self$results$plotirow$setState(marker)
      if (is.null(self$results$plotell$state))
        self$results$plotell$setState(marker)
      
      if (isTRUE(self$options$graphclassif) && !is.null(res.classif))
        self$results$plotclassif$setState(marker)
      
      if (!is.null(res.classif))
        private$.output2(res.classif, data)
      
      private$.output(res.ca, data)
    },
    
    #### Compute results ----

    .dataSignature = function() {
      paste(
        c(
          "activecol", self$options$activecol,
          "illustrativecol", self$options$illustrativecol,
          "indiv", self$options$indiv
        ),
        collapse = "\r"
      )
    },

    .requiredNcp = function() {
      candidates <- suppressWarnings(as.numeric(c(
        self$options$ncp,
        self$options$nbfact,
        self$options$abs,
        self$options$ord
      )))
      candidates <- candidates[is.finite(candidates) & candidates > 0]
      as.integer(max(c(2, candidates)))
    },

    .makeCAKey = function() {
      # The cache key identifies the statistical CA model only. The number
      # of computed dimensions is tracked separately in MEDA.ncp.requested.
      private$.dataSignature()
    },

    .makeClassifKey = function() {
      paste(
        private$.makeCAKey(),
        self$options$ncp,
        self$options$nbclust,
        sep = "\n"
      )
    },

    .readCAFromCache = function(key, required_ncp) {
      cached <- self$results$caCache$state
      if (is.null(cached) || !inherits(cached, "CA"))
        return(NULL)

      cached_key <- attr(cached, "MEDA.cache.key", exact = TRUE)
      cached_ncp <- suppressWarnings(as.integer(
        attr(cached, "MEDA.ncp.requested", exact = TRUE)
      ))
      if (length(cached_ncp) == 0L || is.na(cached_ncp))
        cached_ncp <- if (!is.null(cached$row$coord)) ncol(cached$row$coord) else 0L

      if (identical(cached_key, key) && cached_ncp >= required_ncp)
        return(cached)

      NULL
    },

    .getSharedCA = function() {
      private$.readCAFromCache(
        private$.makeCAKey(),
        private$.requiredNcp()
      )
    },

    .resetRunCaches = function() {
      private$.dataProcessed <- NULL
    },
    
    .computeNbclust = function() {
      return(self$options$nbclust)
    },
    
    .CA = function(data) {
      
      ncp_use <- private$.requiredNcp()
      
      actcol_gui  <- self$options$activecol
      illucol_gui <- self$options$illustrativecol
      
      col_sup_index <- NULL
      if (!is.null(illucol_gui) && length(illucol_gui) > 0) {
        for (col in illucol_gui) {
          if (!col %in% colnames(data)) next
          if (all(is.na(data[[col]]))) {
            data[[col]] <- NULL
            next
          }
          if (!is.numeric(data[[col]]))
            suppressWarnings(data[[col]] <- as.numeric(as.character(data[[col]])))
        }
        col_sup_index <- match(illucol_gui, colnames(data))
        col_sup_index <- col_sup_index[!is.na(col_sup_index)]
        if (length(col_sup_index) == 0)
          col_sup_index <- NULL
      }
      
      res <- tryCatch(
        FactoMineR::CA(data, ncp = ncp_use, col.sup = col_sup_index, graph = FALSE),
        error = function(e) {
          jmvcore::reject(paste("CA failed:", e$message))
          return(NULL)
        }
      )
      if (!is.null(res))
        attr(res, "MEDA.ncp.requested") <- as.integer(ncp_use)
      res
    },
    
    .code = function(table) {
      if (is.null(table))
        return("# The CA could not be computed.")

      r_literal <- function(value) {
        if (is.null(value))
          return("NULL")
        paste(deparse(value, width.cutoff = 500L), collapse = "\n")
      }

      add_call <- function(code, assignment, fun, arguments) {
        prefix <- if (is.null(assignment)) "" else paste0(assignment, " <- ")
        suffix <- if (length(arguments) > 1L) {
          c(rep(",", length(arguments) - 1L), "")
        } else {
          ""
        }
        c(
          code,
          paste0(prefix, fun, "("),
          paste0("  ", arguments, suffix),
          ")"
        )
      }

      option_names <- function(value) {
        if (is.null(value) || length(value) == 0L)
          return(character(0))
        value <- as.character(value)
        value[!is.na(value) & nzchar(value)]
      }

      active_cols <- option_names(self$options$activecol)
      supplementary_cols <- option_names(self$options$illustrativecol)
      variable_names <- c(active_cols, supplementary_cols)

      if (length(active_cols) < 2L)
        return("# Select at least two active columns to generate the CA code.")

      ncp_use <- ncol(table$row$coord)
      if (is.null(ncp_use) || !is.finite(ncp_use) || ncp_use < 1L)
        return("# The CA did not retain any usable dimension.")
      ncp_use <- as.integer(ncp_use)

      n_desc <- suppressWarnings(as.integer(self$options$nbfact))
      if (length(n_desc) == 0L || is.na(n_desc) || n_desc < 1L)
        n_desc <- 1L
      n_desc <- min(n_desc, ncp_use)

      proba <- suppressWarnings(as.numeric(self$options$proba)) / 100
      if (length(proba) == 0L || !is.finite(proba))
        proba <- 0.05

      axes_candidate <- suppressWarnings(as.integer(c(
        self$options$abs, self$options$ord
      )))
      axes_ok <- NULL
      if (length(axes_candidate) == 2L &&
          all(is.finite(axes_candidate)) &&
          all(axes_candidate >= 1L) &&
          all(axes_candidate <= ncp_use) &&
          axes_candidate[1] != axes_candidate[2]) {
        axes_ok <- axes_candidate
      } else if (ncp_use >= 2L) {
        axes_ok <- c(1L, 2L)
      }

      supplementary_indices <- if (length(supplementary_cols) > 0L) {
        length(active_cols) + seq_along(supplementary_cols)
      } else {
        NULL
      }

      code <- c(
        "library(FactoMineR)",
        "",
        "# This script can be pasted directly into the jamovi Rj Editor.",
        "# The dataset open in jamovi is available as data.",
        "",
        "# Keep active columns first, then supplementary columns.",
        paste0(
          "data_CA <- data[, ", r_literal(variable_names),
          ", drop = FALSE]"
        )
      )

      indiv <- option_names(self$options$indiv)
      if (length(indiv) > 0L) {
        code <- c(
          code,
          "",
          "# Use the selected identifier as row names.",
          paste0(
            "id_CA <- as.character(data[[",
            r_literal(indiv[1]), "]])"
          ),
          "missing_id_CA <- is.na(id_CA) | id_CA == \"\"",
          "id_CA[missing_id_CA] <- as.character(which(missing_id_CA))",
          "rownames(data_CA) <- make.unique(id_CA)"
        )
      }

      code <- c(
        code,
        "",
        "# Active contingency table",
        paste0(
          "active_CA <- data_CA[, ",
          r_literal(as.integer(seq_along(active_cols))),
          ", drop = FALSE]"
        ),
        "",
        "# Pearson chi-squared test",
        "stats::chisq.test(active_CA)",
        "",
        "# Correspondence Analysis",
        "# col.sup identifies supplementary columns.",
        "# ncp is the number of dimensions retained in the result."
      )

      ca_arguments <- "data_CA"
      if (!is.null(supplementary_indices)) {
        ca_arguments <- c(
          ca_arguments,
          paste0(
            "col.sup = ",
            r_literal(as.integer(supplementary_indices))
          )
        )
      }
      ca_arguments <- c(
        ca_arguments,
        paste0("ncp = ", r_literal(ncp_use)),
        "graph = FALSE"
      )
      code <- add_call(
        code, "res_ca", "FactoMineR::CA", ca_arguments
      )

      code <- c(
        code,
        "",
        "# Eigenvalues and percentages of explained variance",
        "res_ca$eig",
        "",
        "# Automatic description uses the active table only.",
        "# axes selects the dimensions; proba is the significance threshold."
      )
      code <- add_call(
        code,
        "res_ca_active",
        "FactoMineR::CA",
        c(
          "active_CA",
          paste0("ncp = ", r_literal(ncp_use)),
          "graph = FALSE"
        )
      )
      code <- c(
        code,
        paste0(
          "dimensions_ca <- ",
          r_literal(as.integer(seq_len(n_desc)))
        )
      )
      code <- add_call(
        code,
        "desc_ca",
        "FactoMineR::dimdesc",
        c(
          "res_ca_active",
          "axes = dimensions_ca",
          paste0("proba = ", r_literal(proba))
        )
      )
      code <- c(code, "desc_ca")

      if (isTRUE(self$options$coordrow)) {
        code <- c(
          code, "", "# Row coordinates",
          "res_ca$row$coord[, dimensions_ca, drop = FALSE]"
        )
      }
      if (isTRUE(self$options$contribrow)) {
        code <- c(
          code, "", "# Row contributions",
          "res_ca$row$contrib[, dimensions_ca, drop = FALSE]"
        )
      }
      if (isTRUE(self$options$cosrow)) {
        code <- c(
          code, "", "# Row squared cosines",
          "res_ca$row$cos2[, dimensions_ca, drop = FALSE]"
        )
      }
      if (isTRUE(self$options$coordcol)) {
        code <- c(
          code, "", "# Active-column coordinates",
          "res_ca$col$coord[, dimensions_ca, drop = FALSE]"
        )
      }
      if (isTRUE(self$options$contribcol)) {
        code <- c(
          code, "", "# Active-column contributions",
          "res_ca$col$contrib[, dimensions_ca, drop = FALSE]"
        )
      }
      if (isTRUE(self$options$coscol)) {
        code <- c(
          code, "", "# Active-column squared cosines",
          "res_ca$col$cos2[, dimensions_ca, drop = FALSE]"
        )
      }

      if (isTRUE(self$options$newvar)) {
        n_saved <- min(
          suppressWarnings(as.integer(self$options$ncp)), ncp_use
        )
        if (is.finite(n_saved) && n_saved >= 1L) {
          code <- c(
            code,
            "",
            "# Coordinates saved by MEDA",
            paste0(
              "coordinates_ca <- res_ca$row$coord[, ",
              r_literal(as.integer(seq_len(n_saved))),
              ", drop = FALSE]"
            )
          )
        }
      }

      if (!is.null(axes_ok)) {
        select_col <- paste("cos2", self$options$limcoscol)
        select_row <- paste("cos2", self$options$limcosrow)
        column_invisible <- if (isTRUE(self$options$addillucol)) {
          "row"
        } else {
          c("row", "col.sup")
        }
        superimposed_invisible <- if (isTRUE(self$options$addillucol)) {
          NULL
        } else {
          "col.sup"
        }

        code <- c(
          code,
          "",
          "# Dimensions used in the following maps",
          paste0("axes_ca <- ", r_literal(as.integer(axes_ok))),
          "",
          "# graph.type = \"classic\" is the safest choice in the Rj Editor.",
          "# In RStudio, it can be replaced with graph.type = \"ggplot\".",
          "# selectRow and selectCol let plot.CA filter elements by cos2.",
          "# autoLab = \"yes\" reduces label overlap but may be slow.",
          "",
          "# Rows"
        )
        code <- add_call(
          code, NULL, "FactoMineR::plot.CA",
          c(
            "res_ca",
            "axes = axes_ca",
            paste0("selectCol = ", r_literal(select_col)),
            paste0("selectRow = ", r_literal(select_row)),
            "invisible = c(\"col\", \"col.sup\")",
            "title = \"Representation of the Rows\"",
            "graph.type = \"classic\"",
            "autoLab = \"no\""
          )
        )

        code <- c(code, "", "# Active and supplementary columns")
        code <- add_call(
          code, NULL, "FactoMineR::plot.CA",
          c(
            "res_ca",
            "axes = axes_ca",
            paste0("selectCol = ", r_literal(select_col)),
            paste0("selectRow = ", r_literal(select_row)),
            paste0(
              "invisible = ", r_literal(column_invisible)
            ),
            "title = \"Representation of the Columns\"",
            "graph.type = \"classic\"",
            "autoLab = \"no\""
          )
        )

        ellipse_col <- isTRUE(self$options$ellipsecol)
        ellipse_row <- isTRUE(self$options$ellipserow)
        if (ellipse_col || ellipse_row) {
          ellipse_choice <- if (ellipse_col && ellipse_row) {
            c("col", "row")
          } else if (ellipse_col) {
            "col"
          } else {
            "row"
          }
          ellipse_title <- if (ellipse_col && ellipse_row) {
            "Representation of the Ellipses for the Rows and the Columns"
          } else if (ellipse_col) {
            "Representation of the Ellipses for the Columns"
          } else {
            "Representation of the Ellipses for the Rows"
          }
          code <- c(code, "", "# Confidence ellipses")
          code <- add_call(
            code, NULL, "FactoMineR::ellipseCA",
            c(
              "res_ca",
              "axes = axes_ca",
              paste0("selectCol = ", r_literal(select_col)),
              paste0("selectRow = ", r_literal(select_row)),
              paste0("ellipse = ", r_literal(ellipse_choice)),
              "col.row = \"blue\"",
              "col.col = \"red\"",
              paste0(
                "invisible = ", r_literal(superimposed_invisible)
              ),
              paste0("title = ", r_literal(ellipse_title)),
              "graph.type = \"classic\"",
              "autoLab = \"no\""
            )
          )
        } else {
          code <- c(code, "", "# Superimposed map of rows and columns")
          code <- add_call(
            code, NULL, "FactoMineR::plot.CA",
            c(
              "res_ca",
              "axes = axes_ca",
              paste0("selectCol = ", r_literal(select_col)),
              paste0("selectRow = ", r_literal(select_row)),
              paste0(
                "invisible = ", r_literal(superimposed_invisible)
              ),
              "title = \"Superimposed Representation of the Rows and the Columns\"",
              "graph.type = \"classic\"",
              "autoLab = \"no\""
            )
          )
        }
      }

      need_classif <- isTRUE(self$options$graphclassif) ||
        isTRUE(self$options$newvar2)
      if (need_classif) {
        n_classif <- min(
          suppressWarnings(as.integer(self$options$ncp)), ncp_use
        )
        nbclust <- suppressWarnings(as.integer(self$options$nbclust))
        if (length(nbclust) == 0L || is.na(nbclust))
          nbclust <- -1L
        code <- c(
          code,
          "",
          "# Hierarchical clustering on the retained CA coordinates",
          "# nb.clust = -1 lets HCPC choose the number of clusters.",
          paste0(
            "coord_hcpc_ca <- as.data.frame(res_ca$row$coord[, ",
            r_literal(as.integer(seq_len(n_classif))),
            ", drop = FALSE])"
          )
        )
        code <- add_call(
          code,
          "res_hcpc",
          "FactoMineR::HCPC",
          c(
            "coord_hcpc_ca",
            paste0("nb.clust = ", r_literal(nbclust)),
            "graph = FALSE",
            "description = FALSE"
          )
        )
        if (isTRUE(self$options$newvar2)) {
          code <- c(
            code,
            "cluster_ca <- as.factor(res_hcpc$data.clust[, \"clust\"])"
          )
        }
        if (isTRUE(self$options$graphclassif) && !is.null(axes_ok) &&
            max(axes_ok) <= n_classif) {
          code <- c(code, "", "# Cluster map")
          code <- add_call(
            code, NULL, "FactoMineR::plot.HCPC",
            c(
              "res_hcpc",
              "axes = axes_ca",
              "choice = \"map\"",
              "draw.tree = FALSE",
              "new.plot = FALSE"
            )
          )
        }
      }

      paste(code, collapse = "\n")
    },
    
    .classif = function(res) {
      if (is.null(res) || is.null(res$row$coord))
        return(NULL)
      .meda_hcpc_coordinates(
        res$row$coord,
        self$options$ncp,
        self$nbclust,
        "CA clustering"
      )
    },
    
    .chisq = function(data) {
      # Protection si pas de colonnes actives
      if (is.null(self$options$activecol)) return(NULL)
      dataactcol <- data[, self$options$activecol, drop = FALSE]
      tryCatch(stats::chisq.test(dataactcol), error = function(e) NULL)
    },
    
    .chideux = function(res.xsq) {
      if (is.null(res.xsq)) return()
      self$results$xsqgroup$xsq$setRow(rowNo = 1, values = list(
        xsquared = res.xsq$statistic,
        df       = res.xsq$parameter,
        pvxsq    = res.xsq$p.value
      ))
    },
    
    .dimdesc = function(table) {
      proba <- self$options$proba / 100
      
      dataactcol <- data.frame(self$data[, self$options$activecol, drop = FALSE])
      colnames(dataactcol) <- self$options$activecol
      
      if (!is.null(self$options$indiv)) {
        ids <- as.character(self$data[[self$options$indiv]])
        missing <- is.na(ids) | ids == ""
        ids[missing] <- as.character(which(missing))
        rownames(dataactcol) <- make.unique(ids)
      }
      
      res_ca_active <- tryCatch(
        FactoMineR::CA(
          dataactcol,
          ncp = private$.requiredNcp(),
          graph = FALSE
        ),
        error = function(e) return(NULL)
      )
      
      if (is.null(res_ca_active) || is.null(res_ca_active$row$coord))
        return(NULL)
      
      max_dim   <- ncol(res_ca_active$row$coord)
      nbfact_gui <- min(self$options$nbfact, max_dim)
      
      ddca <- tryCatch(
        FactoMineR::dimdesc(res_ca_active, axes = 1:nbfact_gui, proba = proba),
        error = function(e) return(NULL)
      )
      
      if (is.null(ddca) || length(ddca) == 0)
        return(NULL)
      
      .meda_tidy_ca_dimdesc(ddca)
    },
    
    .getValidAxes = function(res.ca) {
      if (is.null(res.ca) || is.null(res.ca$eig))
        return(NULL)
      .meda_valid_axes(self$options$abs, self$options$ord, nrow(res.ca$eig))
    },
    
    .dodTable = function(tab) {
      for (i in 1:nrow(tab))
        self$results$descofdimgroup$descofdim$addRow(rowKey = i,
                                                     values = list(dim = as.character(tab[, 1])[i]))
      for (i in seq_along(tab[, 1])) {
        row <- list(
          rowcol = as.character(tab[, 2])[i],
          cat    = as.character(tab[, 3])[i],
          coord  = tab[, 4][i]
        )
        self$results$descofdimgroup$descofdim$setRow(rowNo = i, values = row)
      }
    },
    
    .printTables = function(table, quoi) {
      
      # Ne calculer que si au moins un des deux tableaux est demandé
      show_row <- switch(quoi,
                         "coord"  = isTRUE(self$options$coordrow),
                         "contrib"= isTRUE(self$options$contribrow),
                         "cos2"   = isTRUE(self$options$cosrow),
                         FALSE
      )
      show_col <- switch(quoi,
                         "coord"  = isTRUE(self$options$coordcol),
                         "contrib"= isTRUE(self$options$contribcol),
                         "cos2"   = isTRUE(self$options$coscol),
                         FALSE
      )
      
      if (!show_row && !show_col) return()
      
      col_gui    <- self$options$activecol
      max_dim    <- ncol(table$row$coord)
      nbfact_gui <- min(self$options$nbfact, max_dim)
      
      if (quoi == "coord") {
        quoivar  <- table$col$coord
        quoiind  <- table$row$coord
        tablevar <- self$results$colgroup$coordonnees
        tableind <- self$results$rowgroup$coordonnees
      } else if (quoi == "contrib") {
        quoivar  <- table$col$contrib
        quoiind  <- table$row$contrib
        tablevar <- self$results$colgroup$contribution
        tableind <- self$results$rowgroup$contribution
      } else if (quoi == "cos2") {
        quoivar  <- table$col$cos2
        quoiind  <- table$row$cos2
        tablevar <- self$results$colgroup$cosinus
        tableind <- self$results$rowgroup$cosinus
      } else {
        return()
      }
      
      if (show_col) {
        tablevar$addColumn(name = "column", title = "", type = "text")
        for (i in seq_len(nrow(quoivar)))
          tablevar$addRow(rowKey = i, value = NULL)
        for (i in seq_len(nbfact_gui))
          tablevar$addColumn(name = paste0("dim", i), title = paste0("Dim.", i), type = "number")
        for (var in seq_len(nrow(quoivar))) {
          row <- list(column = rownames(quoivar)[var])
          for (i in seq_len(nbfact_gui))
            row[[paste0("dim", i)]] <- quoivar[var, i]
          tablevar$setRow(rowNo = var, values = row)
        }
      }
      
      if (show_row) {
        tableind$addColumn(name = "row", title = "", type = "text")
        for (i in seq_len(nrow(quoiind)))
          tableind$addRow(rowKey = i, value = NULL)
        for (i in seq_len(nbfact_gui))
          tableind$addColumn(name = paste0("dim", i), title = paste0("Dim.", i), type = "number")
        for (ind in seq_len(nrow(quoiind))) {
          row <- list(row = rownames(quoiind)[ind])
          for (i in seq_len(nbfact_gui))
            row[[paste0("dim", i)]] <- quoiind[ind, i]
          tableind$setRow(rowNo = ind, values = row)
        }
      }
    },
    
    .printeigenTable = function(table) {
      eigen      <- table$eig[, 1]
      purcent    <- table$eig[, 2]
      purcentcum <- table$eig[, 3]
      for (i in seq_along(eigen)) {
        self$results$eigengroup$eigen$addRow(rowKey = i, values = list(
          component  = paste("Dim.", i),
          eigenvalue = eigen[i],
          purcent    = purcent[i],
          purcentcum = purcentcum[i]
        ))
      }
    },
    
    .plotcol = function(image, ...) {
      if (is.null(self$options$activecol))
        return(FALSE)
      
      res.ca <- private$.getSharedCA()
      if (is.null(res.ca) || !inherits(res.ca, "CA"))
        return(FALSE)
      
      axes_ok <- private$.getValidAxes(res.ca)
      if (is.null(axes_ok))
        return(FALSE)
      
      fcol       <- paste("cos2", self$options$limcoscol)
      frow       <- paste("cos2", self$options$limcosrow)
      addillucol <- self$options$addillucol
      
      ok <- tryCatch({
        if (isTRUE(addillucol)) {
          p <- FactoMineR::plot.CA(
            res.ca,
            axes = axes_ok,
            selectCol = fcol,
            selectRow = frow,
            invisible = "row",
            title = "Representation of the Columns"
          )
        } else {
          p <- FactoMineR::plot.CA(
            res.ca,
            axes = axes_ok,
            selectCol = fcol,
            selectRow = frow,
            invisible = c("row", "col.sup"),
            title = "Representation of the Columns"
          )
        }
        print(p)
        TRUE
      }, error = function(e) {
        jmvcore::reject(paste("Column plot failed:", e$message))
        FALSE
      })
      
      ok
    },
    
    .plotrow = function(image, ...) {
      if (is.null(self$options$activecol))
        return(FALSE)
      
      res.ca <- private$.getSharedCA()
      if (is.null(res.ca) || !inherits(res.ca, "CA"))
        return(FALSE)
      
      axes_ok <- private$.getValidAxes(res.ca)
      if (is.null(axes_ok))
        return(FALSE)
      
      fcol <- paste("cos2", self$options$limcoscol)
      frow <- paste("cos2", self$options$limcosrow)
      
      ok <- tryCatch({
        p <- FactoMineR::plot.CA(
          res.ca,
          axes = axes_ok,
          selectCol = fcol,
          selectRow = frow,
          invisible = c("col", "col.sup"),
          title = "Representation of the Rows"
        )
        print(p)
        TRUE
      }, error = function(e) {
        jmvcore::reject(paste("Row plot failed:", e$message))
        FALSE
      })
      
      ok
    },
    
    .plotell = function(image, ...) {
      if (is.null(self$options$activecol))
        return(FALSE)
      
      res.ca <- private$.getSharedCA()
      if (is.null(res.ca) || !inherits(res.ca, "CA"))
        return(FALSE)
      
      axes_ok <- private$.getValidAxes(res.ca)
      if (is.null(axes_ok))
        return(FALSE)
      
      fcol           <- paste("cos2", self$options$limcoscol)
      frow           <- paste("cos2", self$options$limcosrow)
      ellipsecol_gui <- self$options$ellipsecol
      ellipserow_gui <- self$options$ellipserow
      addillucol     <- self$options$addillucol
      adc            <- if (isTRUE(addillucol)) NULL else "col.sup"
      
      ok <- tryCatch({
        if (ellipsecol_gui && ellipserow_gui) {
          p <- ellipseCA(
            res.ca, axes = axes_ok, selectCol = fcol, selectRow = frow,
            ellipse = c("col", "row"), col.row = "blue", col.col = "red",
            invisible = adc,
            title = "Representation of the Ellipses for the Rows and the Columns"
          )
        } else if (ellipsecol_gui) {
          p <- ellipseCA(
            res.ca, axes = axes_ok, selectCol = fcol, selectRow = frow,
            ellipse = "col", col.row = "blue", col.col = "red",
            invisible = adc,
            title = "Representation of the Ellipses for the Columns"
          )
        } else if (ellipserow_gui) {
          p <- ellipseCA(
            res.ca, axes = axes_ok, selectCol = fcol, selectRow = frow,
            ellipse = "row", col.row = "blue", col.col = "red",
            invisible = adc,
            title = "Representation of the Ellipses for the Rows"
          )
        } else {
          p <- FactoMineR::plot.CA(
            res.ca, axes = axes_ok, selectCol = fcol, selectRow = frow,
            invisible = adc,
            title = "Superimposed Representation of the Rows and the Columns"
          )
        }
        print(p)
        TRUE
      }, error = function(e) {
        jmvcore::reject(paste("Ellipse plot failed:", e$message))
        FALSE
      })
      
      ok
    },
    
    .plotclassif = function(image, ...) {
      if (is.null(self$options$activecol))
        return(FALSE)
      
      res.classif <- self$results$classifCache$state
      if (is.null(res.classif) || !identical(
        attr(res.classif, "MEDA.cache.key", exact = TRUE),
        private$.makeClassifKey()
      ))
        return(FALSE)
      
      classified_ncp <- suppressWarnings(as.integer(
        attr(res.classif, "MEDA.ncp.classified", exact = TRUE)
      ))
      axes_ok <- .meda_valid_axes(
        self$options$abs, self$options$ord, classified_ncp
      )
      if (is.null(axes_ok))
        return(FALSE)
      
      ok <- tryCatch({
        FactoMineR::plot.HCPC(
          res.classif,
          axes = axes_ok,
          choice = "map",
          draw.tree = FALSE,
          new.plot = FALSE,
          title = "Representation of the Rows According to Clusters"
        )
        TRUE
      }, error = function(e) {
        jmvcore::reject(paste("Cluster plot failed:", e$message))
        FALSE
      })
      
      ok
    },
    
    ### Helper functions ----
    
    .errorCheck = function() {
      if (is.null(self$options$activecol) || length(self$options$activecol) < 2)
        jmvcore::reject("At least two active columns are required")
      if (!.meda_integer_scalar(self$options$nbfact, minimum = 1L))
        jmvcore::reject("The number of displayed dimensions must be a positive integer")
      if (!.meda_integer_scalar(self$options$ncp, minimum = 1L))
        jmvcore::reject("The number of saved dimensions must be a positive integer")
      if (!.meda_integer_scalar(self$options$abs, minimum = 1L) ||
          !.meda_integer_scalar(self$options$ord, minimum = 1L) ||
          self$options$abs == self$options$ord)
        jmvcore::reject("The two plotted dimensions must be distinct positive integers")
      if (isTRUE(self$options$graphclassif) &&
          max(self$options$abs, self$options$ord) > self$options$ncp)
        jmvcore::reject("The cluster-map axes must not exceed the number of dimensions used for clustering")
      proba <- suppressWarnings(as.numeric(self$options$proba))
      if (length(proba) != 1L || !is.finite(proba) ||
          proba < 0 || proba > 100)
        jmvcore::reject("The significance threshold must be between 0 and 100")

      active <- self$data[, self$options$activecol, drop = FALSE]
      if (nrow(active) < 3L)
        jmvcore::reject("CA requires at least three rows")
      if (!all(vapply(active, is.numeric, logical(1))))
        jmvcore::reject("All active columns must be numeric counts")
      active_matrix <- as.matrix(active)
      if (any(!is.finite(active_matrix)))
        jmvcore::reject("Active columns must not contain missing or infinite values")
      if (any(active_matrix < 0))
        jmvcore::reject("Active columns must contain non-negative counts")
      if (sum(active_matrix) <= 0 || any(rowSums(active_matrix) <= 0) ||
          any(colSums(active_matrix) <= 0))
        jmvcore::reject("The active contingency table must have positive row and column margins")

      if (!is.null(self$options$illustrativecol) &&
          length(self$options$illustrativecol) > 0L) {
        supplementary <- self$data[, self$options$illustrativecol, drop = FALSE]
        if (!all(vapply(supplementary, is.numeric, logical(1))))
          jmvcore::reject("All supplementary columns must be numeric counts")
        supplementary_matrix <- as.matrix(supplementary)
        if (any(!is.finite(supplementary_matrix)) || any(supplementary_matrix < 0))
          jmvcore::reject("Supplementary columns must contain finite non-negative counts")
      }

      total <- sum(active_matrix)
      expected <- outer(rowSums(active_matrix), colSums(active_matrix)) / total
      standardized <- (active_matrix - expected) / sqrt(expected)
      max_axes <- min(
        nrow(active_matrix) - 1L,
        ncol(active_matrix) - 1L,
        qr(standardized)$rank
      )
      if (max_axes < 2L)
        jmvcore::reject("The active contingency table must provide at least two CA dimensions")
      if (is.null(.meda_valid_axes(self$options$abs, self$options$ord, max_axes)))
        jmvcore::reject(paste0("The plotted dimensions must be between 1 and ", max_axes))
    },
    
    .output = function(res.ca, data) {
      output <- self$results$newvar
      if (!isTRUE(self$options$newvar) || !output$isNotFilled())
        return()
      nFactors_out <- min(self$options$ncp, ncol(res.ca$row$coord))
      if (nFactors_out < 1L)
        return()
      output$set(
        keys         = seq_len(nFactors_out),
        titles       = paste("Dim.", seq_len(nFactors_out)),
        descriptions = rep("CA component", nFactors_out),
        measureTypes = rep("continuous", nFactors_out)
      )
      
      for (i in seq_len(nFactors_out))
        output$setValues(index = i, as.numeric(res.ca$row$coord[, i]))
      row_nums <- attr(data, "jamovi_row_nums")
      if (is.null(row_nums))
        row_nums <- rownames(data)
      output$setRowNums(row_nums)
    },
    
    .output2 = function(res.classif, data) {
      if (is.null(res.classif) || is.null(res.classif$data.clust))
        return()
      
      output <- self$results$newvar2
      if (!isTRUE(self$options$newvar2) || !output$isNotFilled())
        return()
      output$set(
        keys         = 1,
        titles       = "Cluster",
        descriptions = "Cluster variable",
        measureTypes = "nominal"
      )
      
      output$setValues(index = 1, as.factor(res.classif$data.clust[, ncol(res.classif$data.clust)]))
      row_nums <- attr(data, "jamovi_row_nums")
      if (is.null(row_nums))
        row_nums <- rownames(data)
      output$setRowNums(row_nums)
    },
    
    .buildData = function() {
      data_list <- list()
      
      if (!is.null(self$options$activecol) && length(self$options$activecol) > 0) {
        dataactcol <- data.frame(self$data[, self$options$activecol, drop = FALSE])
        colnames(dataactcol) <- self$options$activecol
        data_list <- c(data_list, list(dataactcol))
      }
      
      if (!is.null(self$options$illustrativecol) && length(self$options$illustrativecol) > 0) {
        datacolsup <- data.frame(self$data[, self$options$illustrativecol, drop = FALSE])
        colnames(datacolsup) <- self$options$illustrativecol
        data_list <- c(data_list, list(datacolsup))
      }
      
      if (length(data_list) == 0)
        return(NULL)
      
      data <- as.data.frame(do.call(cbind, data_list))
      jamovi_row_nums <- rownames(data)
      
      if (!is.null(self$options$indiv)) {
        ids <- as.character(self$data[[self$options$indiv]])
        missing <- is.na(ids) | ids == ""
        ids[missing] <- as.character(which(missing))
        rownames(data) <- make.unique(ids)
      } else {
        rownames(data) <- jamovi_row_nums
      }
      attr(data, "jamovi_row_nums") <- jamovi_row_nums
      data
    }
  )
)
