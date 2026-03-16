MCAClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "MCAClass",
  inherit = MCABase,
  
  active = list(
    
    nVaract = function() {
      if (is.null(private$.nVaract))
        private$.nVaract <- private$.computeNVaract()
      private$.nVaract
    },
    
    nbclust = function() {
      if (is.null(private$.nbclust))
        private$.nbclust <- private$.computeNbclust()
      private$.nbclust
    },
    
    dataProcessed = function() {
      if (is.null(private$.dataProcessed))
        private$.dataProcessed <- private$.buildData()
      private$.dataProcessed
    },
    
    MCAResult = function() {
      if (is.null(private$.MCAResult))
        private$.MCAResult <- private$.getMCAResult()
      private$.MCAResult
    }
  ),
  
  private = list(
    
    .nbclust       = NULL,
    .nVaract       = NULL,
    .dataProcessed = NULL,
    .MCAResult     = NULL,
    
    #---------------------------------------------
    #### Init + run functions ----
    
    .init = function() {
      if (is.null(self$options$actvars) || self$nVaract < 2) {
        if (isTRUE(self$options$tuto))
          self$results$instructions$setVisible(visible = TRUE)
      }
      
      self$results$instructions$setContent(
        "<html>
          <head>
          </head>
          <body>
          <div class='justified-text'>
          <p><b>What you should know before running an MCA in jamovi</b></p>
          <p>______________________________________________________________________________</p>
          <p> Multiple Correspondence Analysis (MCA) is an extension of Correspondence Analysis (CA) that allows
          for the analysis of the relationships among more than two categorical variables simultaneously.
          This method can also be seen as a PCA on categorical variables.</p>
          <p> While the <I>Active Variables</I> field is mandatory, the <I>Supplementary Variables</I> fields are optional.
          However, if you have supplementary variables,
          they may be essential for interpreting the structure on the individuals.</p>
          <p> Once you have selected the active variables, you can choose to get rid of the categories that were rarely
          chosen to describe/measure your individuals. By default, categories that are used by less than 5% of the individuals
          are removed: new categories are then randomly assigned to those individuals.</p>
          <p> Clustering is based on the number of components saved.
          By default, clustering is based on the first 5 components.</p>
          <p> By default, the <I>Number of clusters</I> field is set to -1 which means that the number of
          clusters is automatically chosen by the computer.</p>
          <p>______________________________________________________________________________</p>
          </div>
          </body>
          </html>"
      )
    },
    
    .run = function() {
      
      if (is.null(self$options$actvars) || self$nVaract < 2)
        return()
      
      private$.errorCheck()
      
      res.mca <- self$MCAResult
      if (is.null(res.mca))
        return()
      
      # [CORRECTION 1] La classification est calculée si le graphe est demandé
      # OU si la variable cluster doit être sauvegardée
      res.classif <- NULL
      need_classif <- isTRUE(self$options$graphclassif) || !self$results$newvar2$isNotFilled()
      
      if (need_classif)
        res.classif <- private$.classif(res.mca)
      
      self$results$dimdesc$setContent(private$.dimdesc(res.mca))
      self$results$code$setContent(private$.code(res.mca))
      
      private$.printeigenTable(res.mca)
      private$.printTables(res.mca, "coord")
      private$.printTables(res.mca, "contrib")
      private$.printTables(res.mca, "cos2")
      
      # Graphes toujours affichés
      self$results$plotindiv$setState(res.mca)
      self$results$plotvar$setState(res.mca)
      self$results$plotitemvar$setState(res.mca)
      self$results$plotquantisup$setState(res.mca)
      
      # Graphe clustering optionnel
      if (isTRUE(self$options$graphclassif) && !is.null(res.classif))
        self$results$plotclassif$setState(res.classif)
      
      if (!is.null(res.classif))
        private$.output2(res.classif)
      
      private$.output(res.mca)
    },
    
    #### Compute results ----
    
    .computeNbclust = function() {
      self$options$nbclust
    },
    
    .computeNVaract = function() {
      if (is.null(self$options$actvars)) return(0)
      length(self$options$actvars)
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
      
      ncp_candidates <- c(self$options$ncp, self$options$nFactors)
      ncp_candidates <- suppressWarnings(as.numeric(ncp_candidates))
      ncp_candidates <- ncp_candidates[!is.na(ncp_candidates) & ncp_candidates > 0]
      ncp_target <- if (length(ncp_candidates) == 0) 2 else max(ncp_candidates)
      
      # Plancher à 3 pour éviter l'erreur plot.MCA "non convenient data"
      ncp_target <- max(ncp_target, 3)
      
      # Pour l'ACM, le nombre max de dimensions est lié aux modalités
      # FactoMineR plafonne silencieusement si ncp_target est trop grand
      ncp_use <- ncp_target
      
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
      
      private$.MCAResult <- r
      return(private$.MCAResult)
    },
    
    .classif = function(res.mca) {
      tryCatch(
        FactoMineR::HCPC(res.mca, nb.clust = self$nbclust, graph = FALSE),
        error = function(e) NULL
      )
    },
    
    .dimdesc = function(table) {
      if (is.null(table))
        return("No result available")
      
      nFactors_out <- min(self$options$nFactors, ncol(table$ind$coord))
      if (is.null(nFactors_out) || nFactors_out < 1)
        return("No dimension available")
      
      res <- FactoMineR::dimdesc(table, axes = 1:nFactors_out, proba = self$options$proba / 100)
      paste(capture.output(print(res[-length(res)])), collapse = "\n")
    },
    
    .code = function(table) {
      
      quantisup_gui <- self$options$quantisup
      qualisup_gui  <- self$options$qualisup
      ventil        <- self$options$ventil / 100
      nVaract       <- self$nVaract
      nQuantsup     <- if (!is.null(quantisup_gui)) length(quantisup_gui) else 0
      nQualsup      <- if (!is.null(qualisup_gui))  length(qualisup_gui)  else 0
      
      has_quanti <- nQuantsup > 0
      has_quali  <- nQualsup  > 0
      
      names_var <- paste(names(table$call$X), collapse = ", ")
      data_str  <- paste0("data_MCA <- data[ ,c(", names_var, ")]")
      ncp_str   <- self$options$ncp
      
      code_str <- if (has_quanti && !has_quali) {
        paste0("MCA(data_MCA, quanti.sup=", nVaract + 1, ":", nVaract + nQuantsup,
               ", level.ventil=", ventil, ", ncp=", ncp_str, ")")
      } else if (!has_quanti && has_quali) {
        paste0("MCA(data_MCA, quali.sup=", nVaract + 1, ":", nVaract + nQualsup,
               ", level.ventil=", ventil, ", ncp=", ncp_str, ")")
      } else if (has_quanti && has_quali) {
        paste0("MCA(data_MCA, quanti.sup=", nVaract + 1, ":", nVaract + nQuantsup,
               ", quali.sup=", nVaract + nQuantsup + 1, ":", nVaract + nQuantsup + nQualsup,
               ", level.ventil=", ventil, ", ncp=", ncp_str, ")")
      } else {
        paste0("MCA(data_MCA, level.ventil=", ventil, ", ncp=", ncp_str, ")")
      }
      
      print(list("dataset" = data_str, "R code" = code_str))
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
        self$data[[self$options$individus]]
      else
        seq_len(nrow(self$data))
      
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
        for (ind in seq_along(individus_gui)) {
          row <- list(individus = if (is.null(self$options$individus))
            individus_gui[ind] else rownames(quoiind)[ind])
          for (i in seq_len(nFactors_out))
            row[[paste0("dim", i)]] <- quoiind[ind, i]
          tableind$setRow(rowNo = ind, values = row)
        }
      }
    },
    
    .plotindiv = function(image, ...) {
      if (is.null(self$options$actvars)) return()
      res.mca <- image$state
      if (is.null(res.mca)) return()
      
      plot <- FactoMineR::plot.MCA(res.mca,
                                   axes      = c(self$options$abs, self$options$ord),
                                   choix     = "ind",
                                   invisible = c("var", "quali.sup", "quanti.sup"),
                                   title     = "Representation of the Individuals"
      )
      print(plot)
      TRUE
    },
    
    .plotvar = function(image, ...) {
      if (is.null(self$options$actvars)) return()
      res.mca <- image$state
      if (is.null(res.mca)) return()
      
      plot <- FactoMineR::plot.MCA(res.mca,
                                   axes  = c(self$options$abs, self$options$ord),
                                   choix = "var",
                                   title = "Representation of the Variables"
      )
      print(plot)
      TRUE
    },
    
    .plotitemvar = function(image, ...) {
      if (is.null(self$options$actvars)) return()
      res.mca <- image$state
      if (is.null(res.mca)) return()
      
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
                               axes      = c(self$options$abs, self$options$ord),
                               choix     = "ind",
                               invisible = invisible_vec,
                               selectMod = self$options$modality,
                               title     = "Representation of the Categories"
          )
        } else {
          FactoMineR::plot.MCA(res.mca,
                               axes      = c(self$options$abs, self$options$ord),
                               choix     = "ind",
                               invisible = invisible_vec,
                               title     = "Representation of the Categories"
          )
        }
      }, error = function(e) NULL)
      
      if (is.null(plot)) return()
      print(plot)
      TRUE
    },
    
    .plotquantisup = function(image, ...) {
      if (is.null(self$options$actvars)) return()
      res.mca <- image$state
      if (is.null(res.mca)) return()
      if (is.null(self$options$quantisup) || length(self$options$quantisup) == 0) return()
      
      plot <- FactoMineR::plot.MCA(res.mca,
                                   axes  = c(self$options$abs, self$options$ord),
                                   choix = "quanti.sup"
      )
      print(plot)
      TRUE
    },
    
    .plotclassif = function(image, ...) {
      if (is.null(self$options$actvars)) return()
      res.classif <- image$state
      if (is.null(res.classif)) return()
      
      plot <- FactoMineR::plot.HCPC(res.classif,
                                    axes      = c(self$options$abs, self$options$ord),
                                    choice    = "map",
                                    draw.tree = FALSE,
                                    title     = "Representation of the Individuals According to Clusters"
      )
      print(plot)
      TRUE
    },
    
    #---------------------------------------------
    ### Helper functions ----
    
    .errorCheck = function() {
      if (self$nVaract < self$options$nFactors)
        jmvcore::reject('Number of components cannot be bigger than number of variables')
    },
    
    .output = function(res.mca) {
      nFactors_out <- min(self$options$ncp, ncol(res.mca$ind$coord))
      
      if (self$results$newvar$isNotFilled()) {
        self$results$newvar$set(
          keys         = 1:nFactors_out,
          titles       = paste("Dim.", 1:nFactors_out),
          descriptions = rep("MCA component", nFactors_out),
          measureTypes = rep("continuous", nFactors_out)
        )
      }
      
      for (i in seq_len(nFactors_out))
        self$results$newvar$setValues(index = i, as.numeric(res.mca$ind$coord[, i]))
      
      self$results$newvar$setRowNums(rownames(self$dataProcessed))
    },
    
    .output2 = function(res.classif) {
      if (is.null(res.classif) || is.null(res.classif$data.clust))
        return()
      
      output <- self$results$newvar2
      
      if (output$isNotFilled()) {
        output$set(
          keys         = 1,
          titles       = "Cluster",
          descriptions = "Cluster variable",
          measureTypes = "nominal"
        )
      }
      
      output$setValues(index = 1, as.factor(res.classif$data.clust[, ncol(res.classif$data.clust)]))
      output$setRowNums(rownames(self$dataProcessed))
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
      
      rownames(data) <- if (!is.null(self$options$individus))
        self$data[[self$options$individus]]
      else
        seq_len(nrow(data))
      
      return(data)
    }
  )
)