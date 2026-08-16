MCAClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "MCAClass",
  inherit = MCABase,
  
  active = list(
    
    nVaract = function() {
      private$.computeNVaract()
    },
    
    nbclust = function() {
      private$.computeNbclust()
    },
    
    dataProcessed = function() {
      key <- private$.makeDataProcessingKey()
      if (is.null(private$.dataProcessed) ||
          !identical(private$.dataProcessedKey, key)) {
        private$.dataProcessed <- private$.buildData()
        private$.dataProcessedKey <- key
      }
      private$.dataProcessed
    },

    classifResult = function() {
      key <- private$.makeClassifKey()
      cached <- self$results$classifCache$state
      if (!is.null(cached) && identical(
        attr(cached, "MEDA.cache.key", exact = TRUE), key
      ))
        return(cached)

      value <- private$.getClassifResult()
      if (!is.null(value)) {
        attr(value, "MEDA.cache.key") <- key
        self$results$classifCache$setState(value)
      }
      value
    },
    
    MCAResult = function() {
      cached <- self$results$mcaCache$state
      required_ncp <- private$.requiredNcp()
      key <- private$.makeMCAKey()
      data_key <- private$.dataValueSignature()

      if (!is.null(cached) && inherits(cached, "MCA") &&
          identical(attr(cached, "MEDA.cache.key", exact = TRUE), key) &&
          identical(attr(cached, "MEDA.data.key", exact = TRUE), data_key)) {
        cached_ncp <- suppressWarnings(as.integer(attr(cached, "MEDA.ncp.requested")))
        if (length(cached_ncp) == 0 || is.na(cached_ncp))
          cached_ncp <- if (!is.null(cached$ind$coord)) ncol(cached$ind$coord) else 0L
        if (cached_ncp >= required_ncp)
          return(cached)
      }

      value <- private$.getMCAResult()
      if (!is.null(value)) {
        attr(value, "MEDA.cache.key") <- key
        attr(value, "MEDA.data.key") <- data_key
        self$results$mcaCache$setState(value)
      }
      value
    }
  ),
  
  private = list(
    
    .dataProcessed = NULL,
    .dataProcessedKey = NULL,
    
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
      <b>What you should know before running an MCA in jamovi</b>
    </p>

    <div style='
        border-top: 1px solid #CBD8E8;
        margin-bottom: 12px;
    '></div>

    <p style='margin: 0 0 9px 0;'>
      <b>Purpose.</b>
      Multiple Correspondence Analysis (MCA) is an extension of Correspondence
      Analysis (CA) used to analyze the relationships among several categorical
      variables simultaneously. It can be regarded as the categorical
      counterpart of Principal Component Analysis.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Interpretation.</b>
      Individuals located close to one another have similar response profiles.
      Categories positioned in the same direction from the origin tend to
      characterize the same individuals. Contributions and squared cosines
      should be used to identify the individuals and categories that are most
      important for interpreting each dimension.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Variables.</b>
      While the <i>Active Variables</i> field is <b>mandatory</b>, the
      <i>Supplementary Variables</i> fields are optional. Supplementary
      variables do not determine the dimensions, but they may provide valuable
      information for interpreting the structure of the individuals.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Rare categories.</b>
      The ventilation threshold determines how infrequent categories are
      handled. By default, categories selected by fewer than 5% of the
      individuals are ventilated. These rare categories are removed, and the
      individuals concerned are randomly reassigned to one of the remaining
      categories of the same variable.
    </p>

    <p style='margin: 0 0 9px 0;'>
      Because ventilation modifies the data used by the MCA and involves a
      random reassignment, results may vary slightly between runs. Set the
      ventilation threshold to 0% if rare categories should remain unchanged.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Clustering.</b>
      Clustering is based on the number of components saved. By default,
      clustering uses the first five components; that is, the distance between
      individuals is calculated from their coordinates on these five
      components.
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
      
      if (is.null(self$options$actvars) || self$nVaract < 2)
        return()
      
      private$.errorCheck()
      
      res.mca <- self$MCAResult
      if (is.null(res.mca))
        return()
      
      res.classif <- NULL
      need_classif <- isTRUE(self$options$graphclassif) ||
        (isTRUE(self$options$newvar2) &&
         self$results$newvar2$isNotFilled())
      
      if (need_classif)
        res.classif <- self$classifResult
      
      desc <- self$results$dimdescCache$state
      desc_key <- private$.makeDimdescKey()
      if (is.null(desc) || !identical(
        attr(desc, "MEDA.cache.key", exact = TRUE), desc_key
      )) {
        desc <- private$.dimdesc(res.mca)
        attr(desc, "MEDA.cache.key") <- desc_key
        self$results$dimdescCache$setState(desc)
      }
      .meda_fill_dimdesc_group(self$results$dimdesc, desc)
      if (isTRUE(self$options$showCode))
        self$results$code$setContent(private$.code(res.mca))
      
      private$.printeigenTable(res.mca)
      private$.printTables(res.mca, "coord")
      private$.printTables(res.mca, "contrib")
      private$.printTables(res.mca, "cos2")
      
      # The complete FactoMineR object is cached once in mcaCache. Image
      # states only contain a small marker, so the same object is not
      # serialized once per graph.
      marker <- list(ready = TRUE)
      if (is.null(self$results$plotindiv$state))
        self$results$plotindiv$setState(marker)
      if (is.null(self$results$plotvar$state))
        self$results$plotvar$setState(marker)
      if (is.null(self$results$plotitemvar$state))
        self$results$plotitemvar$setState(marker)

      has_quanti <- !is.null(self$options$quantisup) &&
        length(self$options$quantisup) > 0
      if (isTRUE(self$options$quantimod) && has_quanti &&
          is.null(self$results$plotquantisup$state))
        self$results$plotquantisup$setState(marker)
      
      # Graphe clustering optionnel
      if (isTRUE(self$options$graphclassif) && !is.null(res.classif) &&
          is.null(self$results$plotclassif$state))
        self$results$plotclassif$setState(marker)
      
      if (!is.null(res.classif))
        private$.output2(res.classif)
      
      private$.output(res.mca)
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

    .dataValueSignature = function() {
      .meda_selected_data_signature(
        self$data,
        c(
          self$options$actvars,
          self$options$quantisup,
          self$options$qualisup,
          self$options$individus
        )
      )
    },

    .makeDataProcessingKey = function() {
      paste(private$.dataSignature(), private$.dataValueSignature(), sep = "\n")
    },

    .makeMCAKey = function() {
      paste(
        private$.dataSignature(),
        self$options$ventil,
        private$.requiredNcp(),
        sep = "\n"
      )
    },

    .makeClassifKey = function() {
      data_key <- private$.dataValueSignature()
      if (is.null(data_key)) {
        cached <- self$results$mcaCache$state
        if (!is.null(cached) && inherits(cached, "MCA") &&
            identical(
              attr(cached, "MEDA.cache.key", exact = TRUE),
              private$.makeMCAKey()
            )) {
          data_key <- attr(cached, "MEDA.data.key", exact = TRUE)
        }
      }
      if (is.null(data_key))
        data_key <- "unavailable"

      paste(
        private$.makeMCAKey(),
        "data", data_key,
        self$options$ncp,
        self$options$nbclust,
        sep = "\n"
      )
    },

    .makeDimdescKey = function() {
      paste(
        private$.makeMCAKey(),
        self$options$nFactors,
        self$options$proba,
        sep = "\n"
      )
    },
    
    .computeNbclust = function() {
      self$options$nbclust
    },
    
    .computeNVaract = function() {
      if (is.null(self$options$actvars)) return(0)
      length(self$options$actvars)
    },

    .requiredNcp = function() {
      candidates <- c(
        self$options$ncp,
        self$options$nFactors,
        self$options$abs,
        self$options$ord
      )
      candidates <- suppressWarnings(as.numeric(candidates))
      candidates <- candidates[is.finite(candidates) & candidates > 0]
      max(c(3, candidates))
    },
    
    .getMCAResult = function() {
      
      data          <- self$dataProcessed
      if (is.null(data)) return(NULL)
      
      quantisup_gui <- self$options$quantisup
      qualisup_gui  <- self$options$qualisup
      ventil        <- self$options$ventil / 100
      nVaract       <- self$nVaract
      nQuantsup     <- if (!is.null(quantisup_gui)) length(quantisup_gui) else 0
      nQualsup      <- if (!is.null(qualisup_gui))  length(qualisup_gui)  else 0
      
      has_quanti <- nQuantsup > 0
      has_quali  <- nQualsup  > 0
      
      ncp_use <- private$.requiredNcp()
      
      r <- tryCatch({
        if (has_quanti && !has_quali) {
          FactoMineR::MCA(
            data,
            quanti.sup   = (nVaract + 1):(nVaract + nQuantsup),
            ncp          = ncp_use,
            level.ventil = ventil,
            graph        = FALSE
          )
        } else if (!has_quanti && has_quali) {
          FactoMineR::MCA(
            data,
            quali.sup    = (nVaract + 1):(nVaract + nQualsup),
            ncp          = ncp_use,
            level.ventil = ventil,
            graph        = FALSE
          )
        } else if (has_quanti && has_quali) {
          FactoMineR::MCA(
            data,
            quanti.sup   = (nVaract + 1):(nVaract + nQuantsup),
            quali.sup    = (nVaract + nQuantsup + 1):(nVaract + nQuantsup + nQualsup),
            ncp          = ncp_use,
            level.ventil = ventil,
            graph        = FALSE
          )
        } else {
          FactoMineR::MCA(
            data,
            ncp          = ncp_use,
            level.ventil = ventil,
            graph        = FALSE
          )
        }
      }, error = function(e) {
        jmvcore::reject(paste("MCA failed:", e$message))
        return(NULL)
      })
      
      if (!is.null(r))
        attr(r, "MEDA.ncp.requested") <- as.integer(ncp_use)
      r
    },
    
    .getValidAxes = function(res) {
      if (is.null(res) || is.null(res$eig))
        return(NULL)
      .meda_valid_axes(self$options$abs, self$options$ord, nrow(res$eig))
    },
    
    .getSharedMCA = function() {
      cached <- self$results$mcaCache$state
      key <- private$.makeMCAKey()
      required_ncp <- private$.requiredNcp()
      if (is.null(cached) || !inherits(cached, "MCA") ||
          !identical(attr(cached, "MEDA.cache.key", exact = TRUE), key))
        return(NULL)
      cached_ncp <- suppressWarnings(as.integer(
        attr(cached, "MEDA.ncp.requested", exact = TRUE)
      ))
      if (length(cached_ncp) != 1L || !is.finite(cached_ncp) ||
          cached_ncp < required_ncp)
        return(NULL)
      cached
    },

    .getClassifResult = function() {
      res.mca <- self$MCAResult
      if (is.null(res.mca) || is.null(res.mca$ind$coord))
        return(NULL)

      .meda_hcpc_coordinates(
        res.mca$ind$coord,
        self$options$ncp,
        self$nbclust,
        "MCA clustering"
      )
    },
    
    .dimdesc = function(table) {
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
    
    .code = function(table) {
      if (is.null(table))
        return("# The MCA could not be computed.")

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
        return("# Select at least two active variables to generate the MCA code.")

      ncp_use <- ncol(table$ind$coord)
      if (is.null(ncp_use) || !is.finite(ncp_use) || ncp_use < 1L)
        return("# The MCA did not retain any usable dimension.")
      ncp_use <- as.integer(ncp_use)

      n_desc <- suppressWarnings(as.integer(self$options$nFactors))
      if (length(n_desc) == 0L || is.na(n_desc) || n_desc < 1L)
        n_desc <- 1L
      n_desc <- min(n_desc, ncp_use)

      proba <- suppressWarnings(as.numeric(self$options$proba)) / 100
      if (length(proba) == 0L || !is.finite(proba))
        proba <- 0.05

      ventil <- suppressWarnings(as.numeric(self$options$ventil)) / 100
      if (length(ventil) == 0L || !is.finite(ventil))
        ventil <- 0.05

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
        "# Active variables must be factors.",
        paste0(
          "data_MCA <- data[, ", r_literal(variable_names),
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
            "id_MCA <- as.character(data[[",
            r_literal(individus[1]), "]])"
          ),
          "missing_id_MCA <- is.na(id_MCA) | id_MCA == \"\"",
          "id_MCA[missing_id_MCA] <- as.character(seq_len(sum(missing_id_MCA)))",
          "rownames(data_MCA) <- make.unique(id_MCA)"
        )
      }

      code <- c(
        code,
        "",
        "# Multiple Correspondence Analysis",
        "# quanti.sup and quali.sup identify supplementary columns.",
        "# level.ventil is the minimum frequency for rare categories.",
        "# ncp is the number of dimensions retained in the result."
      )

      mca_arguments <- "data_MCA"
      if (!is.null(quanti_sup_indices)) {
        mca_arguments <- c(
          mca_arguments,
          paste0(
            "quanti.sup = ",
            r_literal(as.integer(quanti_sup_indices))
          )
        )
      }
      if (!is.null(quali_sup_indices)) {
        mca_arguments <- c(
          mca_arguments,
          paste0(
            "quali.sup = ",
            r_literal(as.integer(quali_sup_indices))
          )
        )
      }
      mca_arguments <- c(
        mca_arguments,
        paste0("level.ventil = ", r_literal(ventil)),
        paste0("ncp = ", r_literal(ncp_use)),
        "graph = FALSE"
      )
      code <- add_call(
        code, "res_mca", "FactoMineR::MCA", mca_arguments
      )

      code <- c(
        code,
        "",
        "# Eigenvalues and percentages of explained variance",
        "res_mca$eig",
        "",
        "# Automatic description of the dimensions",
        "# axes selects the dimensions; proba is the significance threshold.",
        paste0(
          "dimensions_mca <- ",
          r_literal(as.integer(seq_len(n_desc)))
        )
      )
      code <- add_call(
        code,
        "desc_mca",
        "FactoMineR::dimdesc",
        c(
          "res_mca",
          "axes = dimensions_mca",
          paste0("proba = ", r_literal(proba))
        )
      )
      code <- c(code, "desc_mca")

      if (isTRUE(self$options$indcoord)) {
        code <- c(
          code, "", "# Individual coordinates",
          "res_mca$ind$coord[, dimensions_mca, drop = FALSE]"
        )
      }
      if (isTRUE(self$options$indcontrib)) {
        code <- c(
          code, "", "# Individual contributions",
          "res_mca$ind$contrib[, dimensions_mca, drop = FALSE]"
        )
      }
      if (isTRUE(self$options$indcos)) {
        code <- c(
          code, "", "# Individual squared cosines",
          "res_mca$ind$cos2[, dimensions_mca, drop = FALSE]"
        )
      }
      if (isTRUE(self$options$varcoord)) {
        code <- c(
          code, "", "# Category coordinates",
          "res_mca$var$coord[, dimensions_mca, drop = FALSE]"
        )
      }
      if (isTRUE(self$options$varcontrib)) {
        code <- c(
          code, "", "# Category contributions",
          "res_mca$var$contrib[, dimensions_mca, drop = FALSE]"
        )
      }
      if (isTRUE(self$options$varcos)) {
        code <- c(
          code, "", "# Category squared cosines",
          "res_mca$var$cos2[, dimensions_mca, drop = FALSE]"
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
              "coordinates_mca <- res_mca$ind$coord[, ",
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
          paste0("axes_mca <- ", r_literal(as.integer(axes_ok))),
          "",
          "# graph.type = \"classic\" is the safest choice in the Rj Editor.",
          "# In RStudio, it can be replaced with graph.type = \"ggplot\".",
          "# selectMod lets plot.MCA select categories by cos2, contribution or v.test.",
          "# autoLab = \"yes\" reduces label overlap but may be slow.",
          "",
          "# Individuals"
        )
        code <- add_call(
          code, NULL, "FactoMineR::plot.MCA",
          c(
            "res_mca",
            "choix = \"ind\"",
            "axes = axes_mca",
            "invisible = c(\"var\", \"quali.sup\", \"quanti.sup\")",
            "title = \"Representation of the Individuals\"",
            "graph.type = \"classic\"",
            "autoLab = \"no\""
          )
        )

        code <- c(code, "", "# Variables")
        code <- add_call(
          code, NULL, "FactoMineR::plot.MCA",
          c(
            "res_mca",
            "choix = \"var\"",
            "axes = axes_mca",
            "title = \"Representation of the Variables\"",
            "graph.type = \"classic\"",
            "autoLab = \"no\""
          )
        )

        invisible_categories <- "ind"
        if (!isTRUE(self$options$varmodvar))
          invisible_categories <- c(invisible_categories, "var")
        if (!isTRUE(self$options$varmodqualisup))
          invisible_categories <- c(invisible_categories, "quali.sup")
        category_arguments <- c(
          "res_mca",
          "choix = \"ind\"",
          "axes = axes_mca",
          paste0(
            "invisible = ", r_literal(invisible_categories)
          )
        )
        modality <- as.character(self$options$modality)
        if (length(modality) == 1L && !is.na(modality) &&
            nzchar(trimws(modality))) {
          category_arguments <- c(
            category_arguments,
            paste0("selectMod = ", r_literal(modality))
          )
        }
        category_arguments <- c(
          category_arguments,
          "title = \"Representation of the Categories\"",
          "graph.type = \"classic\"",
          "autoLab = \"no\""
        )
        code <- c(code, "", "# Active and supplementary categories")
        code <- add_call(
          code, NULL, "FactoMineR::plot.MCA", category_arguments
        )

        if (isTRUE(self$options$quantimod) &&
            length(quanti_sup_vars) > 0L) {
          code <- c(
            code, "", "# Supplementary quantitative variables"
          )
          code <- add_call(
            code, NULL, "FactoMineR::plot.MCA",
            c(
              "res_mca",
              "choix = \"quanti.sup\"",
              "axes = axes_mca",
              "label = \"quanti.sup\"",
              "graph.type = \"classic\"",
              "autoLab = \"no\""
            )
          )
        }
      }

      need_classif <- isTRUE(self$options$graphclassif) ||
        isTRUE(self$options$newvar2)
      if (need_classif) {
        n_classif <- suppressWarnings(as.integer(self$options$ncp))
        if (length(n_classif) == 0L || is.na(n_classif) || n_classif < 1L)
          n_classif <- min(5L, ncp_use)
        n_classif <- min(n_classif, ncp_use)
        nbclust <- suppressWarnings(as.integer(self$options$nbclust))
        if (length(nbclust) == 0L || is.na(nbclust))
          nbclust <- -1L

        code <- c(
          code,
          "",
          "# Hierarchical clustering on the retained MCA coordinates",
          "# nb.clust = -1 lets HCPC choose the number of clusters.",
          paste0(
            "coord_hcpc_mca <- as.data.frame(res_mca$ind$coord[, ",
            r_literal(as.integer(seq_len(n_classif))),
            ", drop = FALSE])"
          )
        )
        code <- add_call(
          code,
          "res_hcpc",
          "FactoMineR::HCPC",
          c(
            "coord_hcpc_mca",
            paste0("nb.clust = ", r_literal(nbclust)),
            "graph = FALSE",
            "description = FALSE"
          )
        )
        if (isTRUE(self$options$newvar2)) {
          code <- c(
            code,
            "cluster_mca <- as.factor(res_hcpc$data.clust[, \"clust\"])"
          )
        }
        if (isTRUE(self$options$graphclassif) && !is.null(axes_ok) &&
            max(axes_ok) <= n_classif) {
          code <- c(code, "", "# Cluster map")
          code <- add_call(
            code, NULL, "FactoMineR::plot.HCPC",
            c(
              "res_hcpc",
              "axes = axes_mca",
              "choice = \"map\"",
              "draw.tree = FALSE",
              "new.plot = FALSE"
            )
          )
        }
      }

      paste(code, collapse = "\n")
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
    
    .printTables = function(table, quoi) {
      
      # Ne calculer que si au moins un des deux tableaux est demandé
      show_ind <- switch(quoi,
                         "coord"  = isTRUE(self$options$indcoord),
                         "contrib"= isTRUE(self$options$indcontrib),
                         "cos2"   = isTRUE(self$options$indcos),
                         FALSE
      )
      show_var <- switch(quoi,
                         "coord"  = isTRUE(self$options$varcoord),
                         "contrib"= isTRUE(self$options$varcontrib),
                         "cos2"   = isTRUE(self$options$varcos),
                         FALSE
      )
      
      if (!show_ind && !show_var) return()
      
      nFactors_out <- min(self$options$nFactors, ncol(table$ind$coord))
      
      individus_gui <- if (!is.null(self$options$individus))
        as.character(self$data[[self$options$individus]])
      else
        as.character(seq_len(nrow(self$data)))
      
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
          row <- list(individus = individus_gui[ind])
          for (i in seq_len(nFactors_out))
            row[[paste0("dim", i)]] <- quoiind[ind, i]
          tableind$setRow(rowNo = ind, values = row)
        }
      }
    },
    
    .plotindiv = function(image, ...) {
      if (is.null(self$options$actvars)) return()
      res.mca <- private$.getSharedMCA()
      axes_ok <- private$.getValidAxes(res.mca)
      if (is.null(axes_ok)) return(FALSE)

      tryCatch({
        plot <- FactoMineR::plot.MCA(
          res.mca,
          axes      = axes_ok,
          choix     = "ind",
          invisible = c("var", "quali.sup", "quanti.sup"),
          title     = "Representation of the Individuals",
          autoLab   = "no"
        )
        print(plot)
        TRUE
      }, error = function(e) {
        jmvcore::reject(paste("Plot of individuals failed:", e$message))
        FALSE
      })
    },
    
    .plotvar = function(image, ...) {
      if (is.null(self$options$actvars)) return()
      res.mca <- private$.getSharedMCA()
      axes_ok <- private$.getValidAxes(res.mca)
      if (is.null(axes_ok)) return(FALSE)

      tryCatch({
        plot <- FactoMineR::plot.MCA(
          res.mca,
          axes    = axes_ok,
          choix   = "var",
          title   = "Representation of the Variables",
          autoLab = "no"
        )
        print(plot)
        TRUE
      }, error = function(e) {
        jmvcore::reject(paste("Plot of variables failed:", e$message))
        FALSE
      })
    },
    
    .plotitemvar = function(image, ...) {
      if (is.null(self$options$actvars)) return()
      res.mca <- private$.getSharedMCA()
      axes_ok <- private$.getValidAxes(res.mca)
      if (is.null(axes_ok)) return(FALSE)
      
      invisible_vec <- "ind"
      if (!isTRUE(self$options$varmodvar))
        invisible_vec <- c(invisible_vec, "var")
      if (!isTRUE(self$options$varmodqualisup))
        invisible_vec <- c(invisible_vec, "quali.sup")
      
      # [CORRECTION 3] Protection contre modality mal formé ou vide
      use_selectMod <- !is.null(self$options$modality) &&
        nchar(trimws(self$options$modality)) > 0
      
      plot <- tryCatch({
        if (use_selectMod) {
          FactoMineR::plot.MCA(res.mca,
                               axes      = axes_ok,
                               choix     = "ind",
                               invisible = invisible_vec,
                               selectMod = self$options$modality,
                               title     = "Representation of the Categories",
                               autoLab   = "no"
          )
        } else {
          FactoMineR::plot.MCA(res.mca,
                               axes      = axes_ok,
                               choix     = "ind",
                               invisible = invisible_vec,
                               title     = "Representation of the Categories",
                               autoLab   = "no"
          )
        }
      }, error = function(e) {
        jmvcore::reject(paste("Plot of categories failed:", e$message))
        NULL
      })
      
      if (is.null(plot)) return()
      print(plot)
      TRUE
    },
    
    .plotquantisup = function(image, ...) {
      if (is.null(self$options$actvars)) return()
      if (is.null(self$options$quantisup) || length(self$options$quantisup) == 0) return()

      res.mca <- private$.getSharedMCA()
      axes_ok <- private$.getValidAxes(res.mca)
      if (is.null(axes_ok)) return(FALSE)

      tryCatch({
        plot <- FactoMineR::plot.MCA(
          res.mca,
          axes    = axes_ok,
          choix   = "quanti.sup",
          autoLab = "no"
        )
        print(plot)
        TRUE
      }, error = function(e) {
        jmvcore::reject(paste("Plot of supplementary quantitative variables failed:", e$message))
        FALSE
      })
    },
    
    .plotclassif = function(image, ...) {
      if (is.null(self$options$actvars))
        return(FALSE)
      
      res.classif <- self$results$classifCache$state
      if (is.null(res.classif) || !identical(
        attr(res.classif, "MEDA.cache.key", exact = TRUE),
        private$.makeClassifKey()
      ))
        return(FALSE)
      
      classified_ncp <- suppressWarnings(as.integer(
        attr(res.classif, "MEDA.ncp.classified")
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
          title = "Representation of the Individuals According to Clusters"
        )
        TRUE
      }, error = function(e) {
        jmvcore::reject(paste("Cluster plot failed:", e$message))
        FALSE
      })
      
      ok
    },
    
    #---------------------------------------------
    ### Helper functions ----
    
    .errorCheck = function() {
      if (self$nVaract < 2)
        jmvcore::reject("At least two active variables are required")
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

      ventil <- suppressWarnings(as.numeric(self$options$ventil))
      if (length(ventil) != 1L || !is.finite(ventil) ||
          ventil < 0 || ventil >= 100)
        jmvcore::reject("The rare-category threshold must be between 0 (included) and 100 (excluded)")
      proba <- suppressWarnings(as.numeric(self$options$proba))
      if (length(proba) != 1L || !is.finite(proba) ||
          proba < 0 || proba > 100)
        jmvcore::reject("The significance threshold must be between 0 and 100")

      for (variable in self$options$actvars) {
        values <- self$data[[variable]]
        observed <- values[!is.na(values)]
        if (length(unique(as.character(observed))) < 2L)
          jmvcore::reject(paste0("Active variable '", variable, "' must contain at least two observed categories"))
      }
    },
    
    .output = function(res.mca) {
      output <- self$results$newvar
      if (!isTRUE(self$options$newvar) || !output$isNotFilled())
        return()
      nFactors_out <- min(self$options$ncp, ncol(res.mca$ind$coord))
      if (nFactors_out < 1L)
        return()
      output$set(
        keys         = seq_len(nFactors_out),
        titles       = paste("Dim.", seq_len(nFactors_out)),
        descriptions = rep("MCA component", nFactors_out),
        measureTypes = rep("continuous", nFactors_out)
      )
      
      for (i in seq_len(nFactors_out))
        output$setValues(index = i, as.numeric(res.mca$ind$coord[, i]))
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
        ids[is.na(ids) | ids == ""] <- as.character(seq_len(sum(is.na(ids) | ids == "")))
        rownames(data) <- make.unique(ids)
      } else {
        rownames(data) <- jamovi_row_nums
      }
      attr(data, "jamovi_row_nums") <- jamovi_row_nums
      return(data)
    }
  )
)
