# This file is a generated template, your changes will not be overwritten
PCAClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "PCAClass",
  inherit = PCABase,
  active = list(
    dataProcessed = function() {
      # dataProcessed is an in-run cache only. Data changes are handled by
      # jamovi through clearWith: data on persistent result states.
      if (is.null(private$.dataProcessed))
        private$.dataProcessed <- private$.buildData()
      private$.dataProcessed
    },

    nVaract = function() {
      if (is.null(private$.nVaract))
        private$.nVaract <- private$.computeNVaract()
      return(private$.nVaract)
    },

    nQualsup = function() {
      if (is.null(private$.nQualsup))
        private$.nQualsup <- private$.computeNQualsup()
      return(private$.nQualsup)
    },

    nQuantsup = function() {
      if (is.null(private$.nQuantsup))
        private$.nQuantsup <- private$.computeNQuantsup()
      return(private$.nQuantsup)
    },

    nbclust = function() {
      private$.computeNbclust()
    },

    classifResult = function() {
      key <- private$.makeClassifKey()
      cached <- self$results$classifCache$state
      if (!is.null(cached) && identical(
        attr(cached, "MEDA.cache.key", exact = TRUE), key
      ))
        return(cached)

      value <- private$.getclassifResult()
      if (!is.null(value)) {
        attr(value, "MEDA.cache.key") <- key
        self$results$classifCache$setState(value)
      }
      value
    },

    PCAResult = function() {
      key <- private$.makePCAKey()
      required_ncp <- private$.requiredNcp()
      cached <- private$.readPCAFromCache(key, required_ncp)
      if (!is.null(cached))
        return(cached)

      value <- private$.getPCAResult()
      if (!is.null(value)) {
        attr(value, "MEDA.cache.key") <- key
        self$results$pcaCache$setState(value)
      }
      value
    }
  ),

  private = list(

    .dataProcessed = NULL,
    .nVaract       = NULL,
    .nQuantsup     = NULL,
    .nQualsup      = NULL,

    #---------------------------------------------
    #### Init + run functions ----

    .init = function() {
      if (is.null(self$options$actvars) || self$nVaract < 2) {
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
      <b>What you should know before running a PCA in jamovi</b>
    </p>

    <div style='
        border-top: 1px solid #CBD8E8;
        margin-bottom: 12px;
    '></div>

    <p style='margin: 0 0 9px 0;'>
      <b>Purpose.</b>
      The main aim of Principal Component Analysis (PCA) is to show how
      individuals are structured according to their description. Therefore,
      the choice of active variables is of paramount importance, as it defines
      how individuals are described.
    </p>

    <p style='margin: 0 0 9px 0;'>
      The choice depends on the problem you are trying to address and,
      therefore, on the perspective from which you want to answer it.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Variables.</b>
      While the <i>Active Variables</i> field is <b>mandatory</b>, the
      <i>Supplementary Variables</i> fields are optional. However, supplementary
      variables may be essential for interpreting the structure of the
      individuals.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Standardization.</b>
      Once you have selected the active variables, you can choose whether or
      not to standardize them. By default, active variables are standardized.
      This choice is particularly important when variables are expressed in
      different units of measurement.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Clustering.</b>
      Clustering is based on the number of components saved. By default,
      clustering uses the first five components; that is, the distance between
      individuals is calculated from these five components.
    </p>

    <p style='margin: 0;'>
      By default, the <i>Number of clusters</i> field is set to −1, which means
      that the number of clusters is selected automatically.
    </p>

  </div>
  "
      )
    },

    .run = function() {

      # Private R6 caches are valid only within the current run/redraw cycle.
      # Persistent freshness across runs is governed by jamovi result states.
      private$.resetRunCaches()

      if (is.null(self$options$actvars) || self$nVaract < 2)
        return()

      private$.errorCheck()

      private$.updateMissingNotice()

      res.pca <- self$PCAResult
      if (is.null(res.pca))
        return()

      res.classif <- NULL
      need_classif <- isTRUE(self$options$graphclassif) ||
        (isTRUE(self$options$newvar2) &&
         self$results$newvar2$isNotFilled())

      if (need_classif)
        res.classif <- self$classifResult

      .meda_fill_dimdesc_group(self$results$dimdesc, private$.dimdesc())
      if (isTRUE(self$options$showCode))
        self$results$code$setContent(private$.code())

      private$.printeigenTable()
      private$.printTables("coord")
      private$.printTables("contrib")
      private$.printTables("cos2")

      # The complete PCA object is kept once in pcaCache. Images receive a
      # lightweight marker only, which avoids serializing the same object for
      # every plot and prevents stale image states after an option change.
      marker <- list(ready = TRUE)
      if (is.null(self$results$plotind$state))
        self$results$plotind$setState(marker)
      if (is.null(self$results$plotvar$state))
        self$results$plotvar$setState(marker)

      # Graphes supplémentaires optionnels
      if (isTRUE(self$options$graphind))
        self$results$plotseulind$setState(marker)

      if (isTRUE(self$options$graphmod) && !is.null(self$options$qualisup))
        self$results$plotseulmod$setState(marker)

      if (self$options$habillage > 0)
        self$results$plothabillage$setState(marker)

      if (isTRUE(self$options$graphvaract))
        self$results$plotseulvaract$setState(marker)

      if (isTRUE(self$options$graphvarillu) && !is.null(self$options$quantisup))
        self$results$plotseulvarillu$setState(marker)

      if (isTRUE(self$options$graphclassif) && !is.null(res.classif))
        self$results$plotclassif$setState(marker)

      if (!is.null(res.classif))
        private$.output2(res.classif)


      private$.output()
    },

    #### Compute results ----

    .dataSignature = function() {
      paste(
        c(
          "actvars", self$options$actvars,
          "quantisup", self$options$quantisup,
          "qualisup", self$options$qualisup,
          "individus", self$options$individus
        ),
        collapse = "\r"
      )
    },

    .requiredNcp = function() {
      candidates <- suppressWarnings(as.numeric(c(
        self$options$ncp,
        self$options$nFactors,
        self$options$abs,
        self$options$ord
      )))
      candidates <- candidates[is.finite(candidates) & candidates > 0]
      as.integer(max(c(2, candidates)))
    },

    .makePCAKey = function() {
      # The cache key identifies the statistical PCA model only. The number
      # of computed dimensions is tracked separately in MEDA.ncp.requested.
      paste(
        private$.dataSignature(),
        isTRUE(self$options$norme),
        sep = "\n"
      )
    },
    .makeClassifKey = function() {
      paste(
        private$.makePCAKey(),
        self$options$ncp,
        self$options$nbclust,
        sep = "\n"
      )
    },
    .readPCAFromCache = function(key, required_ncp) {
      cached <- self$results$pcaCache$state
      if (is.null(cached) || !inherits(cached, "PCA"))
        return(NULL)

      cached_key <- attr(cached, "MEDA.cache.key", exact = TRUE)
      cached_ncp <- suppressWarnings(as.integer(
        attr(cached, "MEDA.ncp.requested", exact = TRUE)
      ))
      if (length(cached_ncp) == 0L || is.na(cached_ncp))
        cached_ncp <- if (!is.null(cached$ind$coord)) ncol(cached$ind$coord) else 0L

      if (identical(cached_key, key) && cached_ncp >= required_ncp)
        return(cached)

      NULL
    },
    .getSharedPCA = function() {
      private$.readPCAFromCache(
        private$.makePCAKey(),
        private$.requiredNcp()
      )
    },

    .resetRunCaches = function() {
      private$.dataProcessed <- NULL
      private$.nVaract <- NULL
      private$.nQuantsup <- NULL
      private$.nQualsup <- NULL
    },

    .updateMissingNotice = function() {
      notice <- self$results$missingNotice

      summarize_missing <- function(vars) {
        if (is.null(vars) || length(vars) == 0L)
          return(c(values = 0L, rows = 0L))

        selected <- self$data[, vars, drop = FALSE]
        missing <- is.na(selected)
        c(
          values = sum(missing),
          rows = sum(rowSums(missing) > 0L)
        )
      }

      plural <- function(n, singular, plural_form = paste0(singular, "s")) {
        if (n == 1L) singular else plural_form
      }

      active_missing <- summarize_missing(self$options$actvars)
      quanti_missing <- summarize_missing(self$options$quantisup)
      quali_missing <- summarize_missing(self$options$qualisup)

      if (sum(c(
        active_missing[["values"]],
        quanti_missing[["values"]],
        quali_missing[["values"]]
      )) == 0L) {
        notice$setVisible(FALSE)
        return(invisible(NULL))
      }

      messages <- character(0)

      if (active_missing[["values"]] > 0L) {
        messages <- c(
          messages,
          paste0(
            active_missing[["values"]], " missing ",
            plural(active_missing[["values"]], "value"),
            " across ", active_missing[["rows"]], " ",
            plural(active_missing[["rows"]], "individual"),
            " were detected in the active variables. ",
            "Following FactoMineR::PCA(), missing numeric values are replaced ",
            "by the corresponding variable mean."
          )
        )
      }

      if (quanti_missing[["values"]] > 0L) {
        messages <- c(
          messages,
          paste0(
            quanti_missing[["values"]], " missing ",
            plural(quanti_missing[["values"]], "value"),
            " across ", quanti_missing[["rows"]], " ",
            plural(quanti_missing[["rows"]], "individual"),
            " were detected in the supplementary quantitative variables. ",
            "FactoMineR applies the same variable-mean replacement to these ",
            "missing numeric values."
          )
        )
      }

      if (quali_missing[["values"]] > 0L) {
        messages <- c(
          messages,
          paste0(
            quali_missing[["values"]], " missing ",
            plural(quali_missing[["values"]], "value"),
            " across ", quali_missing[["rows"]], " ",
            plural(quali_missing[["rows"]], "individual"),
            " were detected in the supplementary categorical variables. ",
            "FactoMineR excludes missing entries from calculations involving ",
            "the corresponding supplementary categorical variable."
          )
        )
      }

      notice$setContent(paste0(
        "<div style='",
        "margin: 6px 0; padding: 10px 14px; ",
        "background-color: #F4F7FB; border: 1px solid #CBD8E8; ",
        "border-left: 4px solid #6B9DE8; border-radius: 5px; ",
        "line-height: 1.4;'>",
        "<b>Missing values.</b> ",
        paste(messages, collapse = " "),
        "</div>"
      ))
      notice$setVisible(TRUE)
      invisible(NULL)
    },

    .computeNbclust = function() {
      return(self$options$nbclust)
    },

    .computeNQuantsup = function() {
      if (is.null(self$options$quantisup)) return(0)
      length(self$options$quantisup)
    },

    .computeNQualsup = function() {
      if (is.null(self$options$qualisup)) return(0)
      length(self$options$qualisup)
    },

    .computeNVaract = function() {
      if (is.null(self$options$actvars)) return(0)
      length(self$options$actvars)
    },

    .getclassifResult = function() {
      if (is.null(self$options$actvars) || self$nVaract < 2)
        return(NULL)

      .meda_hcpc_coordinates(
        self$PCAResult$ind$coord,
        self$options$ncp,
        self$nbclust,
        "PCA clustering"
      )
    },

    .getPCAResult = function() {

      data <- self$dataProcessed
      if (is.null(data)) return(NULL)

      has_quanti <- !is.null(self$options$quantisup) && length(self$options$quantisup) > 0
      has_quali  <- !is.null(self$options$qualisup)  && length(self$options$qualisup)  > 0

      ncp_target <- private$.requiredNcp()
      ncp_upper  <- min(nrow(data) - 1, self$nVaract)

      if (is.na(ncp_upper) || ncp_upper < 1) {
        jmvcore::reject("PCA failed: not enough rows or active variables to compute at least one component")
        return(NULL)
      }

      ncp_use <- min(ncp_target, ncp_upper)

      r <- tryCatch({
        if (has_quanti && !has_quali) {
          FactoMineR::PCA(
            data,
            quanti.sup = (self$nVaract + 1):(self$nVaract + self$nQuantsup),
            ncp        = ncp_use,
            scale.unit = isTRUE(self$options$norme),
            graph      = FALSE
          )
        } else if (!has_quanti && has_quali) {
          FactoMineR::PCA(
            data,
            quali.sup  = (self$nVaract + 1):(self$nVaract + self$nQualsup),
            ncp        = ncp_use,
            scale.unit = isTRUE(self$options$norme),
            graph      = FALSE
          )
        } else if (has_quanti && has_quali) {
          FactoMineR::PCA(
            data,
            quanti.sup = (self$nVaract + 1):(self$nVaract + self$nQuantsup),
            quali.sup  = (self$nVaract + self$nQuantsup + 1):(self$nVaract + self$nQuantsup + self$nQualsup),
            ncp        = ncp_use,
            scale.unit = isTRUE(self$options$norme),
            graph      = FALSE
          )
        } else {
          FactoMineR::PCA(
            data,
            ncp        = ncp_use,
            scale.unit = isTRUE(self$options$norme),
            graph      = FALSE
          )
        }
      }, error = function(e) {
        jmvcore::reject(paste("PCA failed:", e$message))
        return(NULL)
      })

      if (!is.null(r))
        attr(r, "MEDA.ncp.requested") <- as.integer(ncp_use)
      r
    },

    .getValidAxes = function(res.pca) {
      if (is.null(res.pca) || is.null(res.pca$eig) ||
          is.null(res.pca$ind$coord))
        return(NULL)
      .meda_valid_axes(
        self$options$abs,
        self$options$ord,
        min(nrow(res.pca$eig), ncol(res.pca$ind$coord))
      )
    },

    .dimdesc = function() {
      table <- self$PCAResult
      if (is.null(table))
        return(.meda_empty_dimdesc())

      nFactors_out <- min(self$options$nFactors, ncol(table$ind$coord))
      if (is.null(nFactors_out) || nFactors_out < 1)
        return(.meda_empty_dimdesc())

      res <- tryCatch(
        FactoMineR::dimdesc(
          table,
          axes = seq_len(nFactors_out),
          proba = self$options$proba / 100
        ),
        error = function(e) NULL
      )
      if (is.null(res))
        return(.meda_empty_dimdesc())
      .meda_tidy_dimdesc(res)
    },

    .code = function() {
      res.pca <- self$PCAResult
      if (is.null(res.pca))
        return("# The PCA could not be computed.")

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

      active_vars <- option_names(self$options$actvars)
      quanti_sup_vars <- option_names(self$options$quantisup)
      quali_sup_vars <- option_names(self$options$qualisup)
      variable_names <- c(active_vars, quanti_sup_vars, quali_sup_vars)

      if (length(active_vars) < 2L)
        return("# Select at least two active variables to generate the PCA code.")

      ncp_use <- ncol(res.pca$ind$coord)
      if (is.null(ncp_use) || !is.finite(ncp_use) || ncp_use < 1L)
        return("# The PCA did not retain any usable dimension.")
      ncp_use <- as.integer(ncp_use)

      n_desc <- suppressWarnings(as.integer(self$options$nFactors))
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

      quanti_sup_indices <- if (length(quanti_sup_vars) > 0L) {
        length(active_vars) + seq_along(quanti_sup_vars)
      } else {
        NULL
      }
      quali_sup_indices <- if (length(quali_sup_vars) > 0L) {
        length(active_vars) + length(quanti_sup_vars) +
          seq_along(quali_sup_vars)
      } else {
        NULL
      }

      code <- c(
        "library(FactoMineR)",
        "",
        "# This script can be pasted directly into the jamovi Rj Editor.",
        "# The dataset open in jamovi is available as data.",
        "",
        "# Keep active variables first, then supplementary variables.",
        paste0(
          "data_PCA <- data[, ", r_literal(variable_names),
          ", drop = FALSE]"
        )
      )

      individus <- option_names(self$options$individus)
      if (length(individus) > 0L) {
        code <- c(
          code,
          "",
          "# Use the selected identifier as row names.",
          paste0(
            "id_PCA <- as.character(data[[",
            r_literal(individus[1]), "]])"
          ),
          "missing_id_PCA <- is.na(id_PCA) | id_PCA == \"\"",
          "id_PCA[missing_id_PCA] <- as.character(which(missing_id_PCA))",
          "rownames(data_PCA) <- make.unique(id_PCA)"
        )
      }

      code <- c(
        code,
        "",
        "# Principal Component Analysis",
        "# scale.unit = TRUE standardizes the active variables.",
        "# quanti.sup and quali.sup identify supplementary columns.",
        "# ncp is the number of dimensions retained in the result."
      )

      pca_arguments <- c(
        "data_PCA",
        paste0("scale.unit = ", r_literal(isTRUE(self$options$norme)))
      )
      if (!is.null(quanti_sup_indices)) {
        pca_arguments <- c(
          pca_arguments,
          paste0(
            "quanti.sup = ",
            r_literal(as.integer(quanti_sup_indices))
          )
        )
      }
      if (!is.null(quali_sup_indices)) {
        pca_arguments <- c(
          pca_arguments,
          paste0(
            "quali.sup = ",
            r_literal(as.integer(quali_sup_indices))
          )
        )
      }
      pca_arguments <- c(
        pca_arguments,
        paste0("ncp = ", r_literal(ncp_use)),
        "graph = FALSE"
      )
      code <- add_call(
        code, "res_pca", "FactoMineR::PCA", pca_arguments
      )

      code <- c(
        code,
        "",
        "# Eigenvalues and percentages of explained variance",
        "res_pca$eig",
        "",
        "# Automatic description of the dimensions",
        "# axes selects the dimensions; proba is the significance threshold.",
        paste0(
          "dimensions_pca <- ",
          r_literal(as.integer(seq_len(n_desc)))
        )
      )
      code <- add_call(
        code,
        "desc_pca",
        "FactoMineR::dimdesc",
        c(
          "res_pca",
          "axes = dimensions_pca",
          paste0("proba = ", r_literal(proba))
        )
      )
      code <- c(code, "desc_pca")

      if (isTRUE(self$options$coordind)) {
        code <- c(
          code, "", "# Individual coordinates",
          "res_pca$ind$coord[, dimensions_pca, drop = FALSE]"
        )
      }
      if (isTRUE(self$options$contribind)) {
        code <- c(
          code, "", "# Individual contributions",
          "res_pca$ind$contrib[, dimensions_pca, drop = FALSE]"
        )
      }
      if (isTRUE(self$options$cosind)) {
        code <- c(
          code, "", "# Individual squared cosines",
          "res_pca$ind$cos2[, dimensions_pca, drop = FALSE]"
        )
      }
      if (isTRUE(self$options$coordvar)) {
        code <- c(
          code, "", "# Active-variable coordinates",
          "res_pca$var$coord[, dimensions_pca, drop = FALSE]"
        )
      }
      if (isTRUE(self$options$contribvar)) {
        code <- c(
          code, "", "# Active-variable contributions",
          "res_pca$var$contrib[, dimensions_pca, drop = FALSE]"
        )
      }
      if (isTRUE(self$options$cosvar)) {
        code <- c(
          code, "", "# Active-variable squared cosines",
          "res_pca$var$cos2[, dimensions_pca, drop = FALSE]"
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
              "coordinates_pca <- res_pca$ind$coord[, ",
              r_literal(as.integer(seq_len(n_saved))),
              ", drop = FALSE]"
            )
          )
        }
      }

      if (!is.null(axes_ok)) {
        code <- c(
          code,
          "",
          "# Dimensions used in the following maps",
          paste0("axes_pca <- ", r_literal(as.integer(axes_ok))),
          "",
          "# graph.type = \"classic\" is the safest choice in the Rj Editor.",
          "# In RStudio, it can be replaced with graph.type = \"ggplot\".",
          "# choix = \"ind\" draws individuals and qualitative categories.",
          "# choix = \"var\" draws the correlation circle.",
          "# autoLab = \"yes\" reduces label overlap but may be slow.",
          "",
          "# Individuals and supplementary qualitative categories"
        )
        individual_title <- if (length(quali_sup_vars) > 0L) {
          "Representation of the Individuals and the Categories"
        } else {
          "Representation of the Individuals"
        }
        code <- add_call(
          code, NULL, "FactoMineR::plot.PCA",
          c(
            "res_pca",
            "choix = \"ind\"",
            "axes = axes_pca",
            paste0("title = ", r_literal(individual_title)),
            "graph.type = \"classic\"",
            "autoLab = \"no\""
          )
        )

        variable_title <- if (length(quanti_sup_vars) > 0L) {
          "Representation of the Variables (Active and Supplementary)"
        } else {
          "Correlation Circle"
        }
        code <- c(code, "", "# Active and supplementary quantitative variables")
        code <- add_call(
          code, NULL, "FactoMineR::plot.PCA",
          c(
            "res_pca",
            "choix = \"var\"",
            "axes = axes_pca",
            paste0("title = ", r_literal(variable_title)),
            "graph.type = \"classic\"",
            "autoLab = \"no\""
          )
        )

        if (isTRUE(self$options$graphind)) {
          individual_only_arguments <- c(
            "res_pca",
            "choix = \"ind\"",
            "axes = axes_pca",
            "habillage = \"none\""
          )
          if (length(quali_sup_vars) > 0L)
            individual_only_arguments <- c(
              individual_only_arguments, "invisible = \"quali\""
            )
          individual_only_arguments <- c(
            individual_only_arguments,
            "title = \"Representation of the Individuals\"",
            "graph.type = \"classic\"",
            "autoLab = \"no\""
          )
          code <- c(code, "", "# Individuals only")
          code <- add_call(
            code, NULL, "FactoMineR::plot.PCA",
            individual_only_arguments
          )
        }

        if (isTRUE(self$options$graphmod) && length(quali_sup_vars) > 0L) {
          code <- c(code, "", "# Supplementary qualitative categories only")
          code <- add_call(
            code, NULL, "FactoMineR::plot.PCA",
            c(
              "res_pca",
              "choix = \"ind\"",
              "axes = axes_pca",
              "invisible = \"ind\"",
              "title = \"Representation of the Categories\"",
              "graph.type = \"classic\"",
              "autoLab = \"no\""
            )
          )
        }

        habillage <- suppressWarnings(as.integer(self$options$habillage))
        if (length(habillage) == 1L && is.finite(habillage) &&
            habillage >= 1L && habillage <= length(quali_sup_vars)) {
          code <- c(
            code,
            "",
            "# Individuals colored by a supplementary categorical variable"
          )
          code <- add_call(
            code, NULL, "FactoMineR::plot.PCA",
            c(
              "res_pca",
              "choix = \"ind\"",
              "axes = axes_pca",
              paste0(
                "habillage = ",
                r_literal(quali_sup_vars[habillage])
              ),
              "invisible = \"quali\"",
              "title = \"Representation of the Individuals (Colored by Variable)\"",
              "graph.type = \"classic\"",
              "autoLab = \"no\""
            )
          )
        }

        if (isTRUE(self$options$graphvaract)) {
          active_variable_arguments <- c(
            "res_pca",
            "choix = \"var\"",
            "axes = axes_pca"
          )
          if (length(quanti_sup_vars) > 0L)
            active_variable_arguments <- c(
              active_variable_arguments,
              "invisible = \"quanti.sup\""
            )
          active_variable_arguments <- c(
            active_variable_arguments,
            "title = \"Representation of the Active Variables\"",
            "graph.type = \"classic\"",
            "autoLab = \"no\""
          )
          code <- c(code, "", "# Active variables only")
          code <- add_call(
            code, NULL, "FactoMineR::plot.PCA",
            active_variable_arguments
          )
        }

        if (isTRUE(self$options$graphvarillu) &&
            length(quanti_sup_vars) > 0L) {
          code <- c(code, "", "# Supplementary quantitative variables only")
          code <- add_call(
            code, NULL, "FactoMineR::plot.PCA",
            c(
              "res_pca",
              "choix = \"var\"",
              "axes = axes_pca",
              "invisible = \"var\"",
              "title = \"Representation of the Supplementary Variables\"",
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
        code <- c(
          code,
          "",
          "# Hierarchical clustering on the retained PCA coordinates",
          "# nb.clust = -1 lets HCPC choose the number of clusters.",
          paste0(
            "coord_hcpc_pca <- as.data.frame(res_pca$ind$coord[, ",
            r_literal(as.integer(seq_len(n_classif))),
            ", drop = FALSE])"
          )
        )
        code <- add_call(
          code,
          "res_hcpc",
          "FactoMineR::HCPC",
          c(
            "coord_hcpc_pca",
            paste0("nb.clust = ", r_literal(nbclust)),
            "graph = FALSE",
            "description = FALSE"
          )
        )
        if (isTRUE(self$options$newvar2)) {
          code <- c(
            code,
            "cluster_pca <- as.factor(res_hcpc$data.clust[, \"clust\"])"
          )
        }
        if (isTRUE(self$options$graphclassif) && !is.null(axes_ok) &&
            max(axes_ok) <= n_classif) {
          code <- c(code, "", "# Cluster map")
          code <- add_call(
            code, NULL, "FactoMineR::plot.HCPC",
            c(
              "res_hcpc",
              "axes = axes_pca",
              "choice = \"map\"",
              "draw.tree = FALSE",
              "new.plot = FALSE"
            )
          )
        }
      }

      paste(code, collapse = "\n")
    },

    .printeigenTable = function() {
      table      <- self$PCAResult$eig
      eigen      <- table[, 1]
      purcent    <- table[, 2]
      purcentcum <- table[, 3]

      for (i in seq_along(eigen)) {
        self$results$eigengroup$eigen$addRow(rowKey = i, values = list(
          component  = paste("Dim.", i),
          eigenvalue = eigen[i],
          purcent    = purcent[i],
          purcentcum = purcentcum[i]
        ))
      }
    },

    .printTables = function(quoi) {

      # Ne calculer que si au moins un des deux tableaux est demandé
      show_ind <- switch(quoi,
                         "coord"  = isTRUE(self$options$coordind),
                         "contrib"= isTRUE(self$options$contribind),
                         "cos2"   = isTRUE(self$options$cosind),
                         FALSE
      )
      show_var <- switch(quoi,
                         "coord"  = isTRUE(self$options$coordvar),
                         "contrib"= isTRUE(self$options$contribvar),
                         "cos2"   = isTRUE(self$options$cosvar),
                         FALSE
      )

      if (!show_ind && !show_var) return()

      table <- self$PCAResult
      if (quoi == "coord") {
        quoivar  <- table$var$coord
        quoiind  <- table$ind$coord
        tablevar <- self$results$variables$coordonnees
        tableind <- self$results$individus$coordonnees
      } else if (quoi == "contrib") {
        quoivar  <- table$var$contrib
        quoiind  <- table$ind$contrib
        tablevar <- self$results$variables$contribution
        tableind <- self$results$individus$contribution
      } else if (quoi == "cos2") {
        quoivar  <- table$var$cos2
        quoiind  <- table$ind$cos2
        tablevar <- self$results$variables$cosinus
        tableind <- self$results$individus$cosinus
      } else {
        return()
      }

      nFactors_out <- min(self$options$nFactors, ncol(quoivar), ncol(quoiind))

      if (show_var) {
        tablevar$addColumn(name = "variables", title = "", type = "text")
        for (i in seq_len(nrow(quoivar)))
          tablevar$addRow(rowKey = i, value = NULL)
        for (i in seq_len(nFactors_out))
          tablevar$addColumn(name = paste0("dim", i), title = paste0("Dim.", i), type = "number")
        for (var in seq_len(nrow(quoivar))) {
          row <- list(variables = rownames(quoivar)[var])
          for (i in seq_len(nFactors_out))
            row[[paste0("dim", i)]] <- quoivar[var, i]
          tablevar$setRow(rowNo = var, values = row)
        }
      }

      if (show_ind) {
        tableind$addColumn(name = "individus", title = "", type = "text")
        for (i in seq_len(nrow(quoiind)))
          tableind$addRow(rowKey = i, value = NULL)
        for (i in seq_len(nFactors_out))
          tableind$addColumn(name = paste0("dim", i), title = paste0("Dim.", i), type = "number")
        for (ind in seq_len(nrow(quoiind))) {
          row <- list(individus = rownames(quoiind)[ind])
          for (i in seq_len(nFactors_out))
            row[[paste0("dim", i)]] <- quoiind[ind, i]
          tableind$setRow(rowNo = ind, values = row)
        }
      }
    },

    .renderPCAPlot = function(arguments, label) {
      res.pca <- private$.getSharedPCA()
      axes_ok <- private$.getValidAxes(res.pca)
      if (is.null(res.pca) || is.null(axes_ok))
        return(FALSE)

      ok <- tryCatch({
        graph <- do.call(
          FactoMineR::plot.PCA,
          c(list(res.pca, axes = axes_ok), arguments)
        )
        if (!is.null(graph))
          print(graph)
        TRUE
      }, error = function(e) {
        jmvcore::reject(paste0(label, " failed: ", conditionMessage(e)))
        FALSE
      })
      ok
    },

    .plotindividus = function(image, ...) {
      if (self$nVaract < 2) return(FALSE)
      title <- if (!is.null(self$options$qualisup) &&
                   length(self$options$qualisup) > 0) {
        "Representation of the Individuals and the Categories"
      } else {
        "Representation of the Individuals"
      }
      private$.renderPCAPlot(list(title = title), "Individuals plot")
    },

    .plothabillage = function(image, ...) {
      if (self$nVaract < 2) return(FALSE)
      habillage_value <- self$nVaract + self$nQuantsup + self$options$habillage

      args <- list(
        habillage = habillage_value,
        title     = "Representation of the Individuals (Colored by Variable)"
      )

      if (!is.null(self$options$qualisup) && length(self$options$qualisup) > 0)
        args$invisible <- "quali"

      private$.renderPCAPlot(args, "Colored-individuals plot")
    },

    .plotseulind = function(image, ...) {
      if (self$nVaract < 2) return(FALSE)
      args <- list(
        habillage = "none",
        title     = "Representation of the Individuals"
      )

      if (!is.null(self$options$qualisup) && length(self$options$qualisup) > 0)
        args$invisible <- "quali"

      private$.renderPCAPlot(args, "Individuals-only plot")

    },

    .plotseulmod = function(image, ...) {
      if (self$nVaract < 2) return(FALSE)
      if (is.null(self$options$qualisup) || length(self$options$qualisup) == 0)
        return(FALSE)
      private$.renderPCAPlot(
        list(invisible = "ind", title = "Representation of the Categories"),
        "Categories plot"
      )
    },

    .plotvariables = function(image, ...) {
      if (self$nVaract < 2) return(FALSE)
      title <- if (!is.null(self$options$quantisup) &&
                   length(self$options$quantisup) > 0) {
        "Representation of the Variables (Active and Supplementary)"
      } else {
        "Correlation Circle"
      }
      private$.renderPCAPlot(
        list(choix = "var", title = title),
        "Variables plot"
      )
    },

    .plotseulvaract = function(image, ...) {
      if (self$nVaract < 2) return(FALSE)
      args <- list(
        choix = "var",
        title = "Representation of the Active Variables"
      )
      if (!is.null(self$options$quantisup) && length(self$options$quantisup) > 0)
        args$invisible <- "quanti.sup"

      private$.renderPCAPlot(args, "Active-variables plot")
    },

    .plotseulvarillu = function(image, ...) {
      if (self$nVaract < 2) return(FALSE)
      if (is.null(self$options$quantisup) || length(self$options$quantisup) == 0)
        return(FALSE)
      private$.renderPCAPlot(
        list(
          choix = "var",
          invisible = "var",
          title = "Representation of the Supplementary Variables"
        ),
        "Supplementary-variables plot"
      )
    },

    .plotclassif = function(image, ...) {
      if (is.null(self$options$actvars) || self$nVaract < 2)
        return(FALSE)
      res.classif <- self$results$classifCache$state
      if (is.null(res.classif) || !identical(
        attr(res.classif, "MEDA.cache.key", exact = TRUE),
        private$.makeClassifKey()
      ))
        return(FALSE)
      n_axes <- suppressWarnings(as.integer(
        attr(res.classif, "MEDA.ncp.classified", exact = TRUE)
      ))
      axes_ok <- .meda_valid_axes(self$options$abs, self$options$ord, n_axes)
      if (is.null(axes_ok))
        return(FALSE)
      tryCatch({
        FactoMineR::plot.HCPC(
          res.classif,
          axes = axes_ok,
          choice = "map",
          draw.tree = FALSE,
          new.plot = FALSE,
          title = "Representation of the Individuals According to Clusters"
        )
        TRUE
      }, error = function(e) {
        jmvcore::reject(paste0("Cluster plot failed: ", conditionMessage(e)))
        FALSE
      })
    },

    #---------------------------------------------
    ### Helper functions ----

    .errorCheck = function() {
      if (!.meda_integer_scalar(self$options$nFactors, minimum = 1L))
        jmvcore::reject("The number of displayed components must be a positive integer")
      if (!.meda_integer_scalar(self$options$ncp, minimum = 1L))
        jmvcore::reject("The number of saved components must be a positive integer")
      if (!.meda_integer_scalar(self$options$abs, minimum = 1L) ||
          !.meda_integer_scalar(self$options$ord, minimum = 1L) ||
          self$options$abs == self$options$ord)
        jmvcore::reject("The two plotted dimensions must be distinct positive integers")
      if (isTRUE(self$options$graphclassif) &&
          max(self$options$abs, self$options$ord) > self$options$ncp)
        jmvcore::reject("The cluster-map axes must not exceed the number of components used for clustering")
      if (!is.numeric(self$options$proba) || length(self$options$proba) != 1L ||
          !is.finite(self$options$proba) || self$options$proba < 0 ||
          self$options$proba > 100)
        jmvcore::reject("The significance threshold must be between 0 and 100")

      if (!.meda_integer_scalar(self$options$habillage, minimum = 0L))
        jmvcore::reject("The grouping variable index must be a non-negative integer")
      if (self$options$habillage > self$nQualsup) {
        if (self$nQualsup == 0L)
          jmvcore::reject(
            "A grouping variable can only be selected when at least one supplementary categorical variable is available"
          )
        jmvcore::reject(
          paste0(
            "The grouping variable index must be between 0 and ",
            self$nQualsup
          )
        )
      }

      data <- self$dataProcessed
      upper <- min(nrow(data) - 1L, self$nVaract)
      if (upper < 2L)
        jmvcore::reject("PCA requires enough observations to compute at least two dimensions")
      if (is.null(.meda_valid_axes(self$options$abs, self$options$ord, upper)))
        jmvcore::reject(paste0("The plotted dimensions must be between 1 and ", upper))

      active <- self$data[, self$options$actvars, drop = FALSE]
      for (variable in self$options$actvars) {
        values <- active[[variable]]
        if (!is.numeric(values))
          jmvcore::reject(paste0("Active variable '", variable, "' must be numeric"))
        if (any(is.infinite(values), na.rm = TRUE))
          jmvcore::reject(paste0("Active variable '", variable, "' contains infinite values"))
        observed <- values[is.finite(values)]
        if (length(observed) < 2L || length(unique(observed)) < 2L)
          jmvcore::reject(paste0("Active variable '", variable, "' has no usable variance"))
      }
    },

    .output = function() {
      output <- self$results$newvar
      if (!isTRUE(self$options$newvar) || !output$isNotFilled())
        return()
      nFactors_out <- min(self$options$ncp, ncol(self$PCAResult$ind$coord))
      if (nFactors_out < 1L)
        return()
      output$set(
        keys         = seq_len(nFactors_out),
        titles       = paste("Dim.", seq_len(nFactors_out)),
        descriptions = rep("PCA component", nFactors_out),
        measureTypes = rep("continuous", nFactors_out)
      )

      for (i in seq_len(nFactors_out))
        output$setValues(index = i, as.numeric(self$PCAResult$ind$coord[, i]))
      row_nums <- attr(self$dataProcessed, "jamovi_row_nums")
      if (is.null(row_nums))
        row_nums <- rownames(self$dataProcessed)
      output$setRowNums(row_nums)
    },

    .output2 = function(res.classif) {
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
      row_nums <- attr(self$dataProcessed, "jamovi_row_nums")
      if (is.null(row_nums))
        row_nums <- rownames(self$dataProcessed)
      output$setRowNums(row_nums)
    },

    .buildData = function() {

      data_list <- list()

      if (!is.null(self$options$actvars) && length(self$options$actvars) > 0) {
        dataactvars <- data.frame(self$data[, self$options$actvars, drop = FALSE])
        colnames(dataactvars) <- self$options$actvars
        data_list <- c(data_list, list(dataactvars))
      }

      if (!is.null(self$options$quantisup) && length(self$options$quantisup) > 0) {
        dataquantisup <- data.frame(self$data[, self$options$quantisup, drop = FALSE])
        colnames(dataquantisup) <- self$options$quantisup
        data_list <- c(data_list, list(dataquantisup))
      }

      if (!is.null(self$options$qualisup) && length(self$options$qualisup) > 0) {
        dataqualisup <- data.frame(self$data[, self$options$qualisup, drop = FALSE])
        colnames(dataqualisup) <- self$options$qualisup
        data_list <- c(data_list, list(dataqualisup))
      }

      if (length(data_list) == 0)
        return(NULL)

      data <- as.data.frame(do.call(cbind, data_list))
      jamovi_row_nums <- rownames(data)

      if (!is.null(self$options$individus)) {
        ids <- as.character(self$data[[self$options$individus]])
        missing <- is.na(ids) | ids == ""
        ids[missing] <- as.character(which(missing))
        rownames(data) <- make.unique(ids)
      } else {
        rownames(data) <- jamovi_row_nums
      }
      attr(data, "jamovi_row_nums") <- jamovi_row_nums
      return(data)
    }
  )
)
