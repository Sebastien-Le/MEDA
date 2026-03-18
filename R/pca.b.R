# This file is a generated template, your changes will not be overwritten
PCAClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "PCAClass",
  inherit = PCABase,
  active = list(
    dataProcessed = function() {
      if (is.null(private$.dataProcessed))
        private$.dataProcessed <- private$.buildData()
      return(private$.dataProcessed)
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
      if (is.null(private$.nbclust))
        private$.nbclust <- private$.computeNbclust()
      return(private$.nbclust)
    },
    
    classifResult = function() {
      if (is.null(private$.classifResult))
        private$.classifResult <- private$.getclassifResult()
      return(private$.classifResult)
    },
    
    PCAResult = function() {
      if (is.null(private$.PCAResult))
        private$.PCAResult <- private$.getPCAResult()
      return(private$.PCAResult)
    }
  ),
  
  private = list(
    
    .dataProcessed = NULL,
    .nVaract       = NULL,
    .nQuantsup     = NULL,
    .nQualsup      = NULL,
    .nbclust       = NULL,
    .classifResult = NULL,
    .PCAResult     = NULL,
    
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
            <p><b>What you should know before running a PCA in jamovi</b></p>
            <p>______________________________________________________________________________</p>
            <p> The main aim of Principal Component Analysis (PCA) is to show how individuals are structured according to their description.
            Therefore, the choice of active variables is of paramount importance as it defines how individuals are described.</p>
            <p> The choice depends on the problem you are trying to address and therefore the perspective from which you want to answer it.</p>
            <p> While the <I>Active Variables</I> field is <B>mandatory</B>, the <I>Supplementary Variables</I> fields are optional.
            However, if you have supplementary variables they may be essential for interpreting the structure on the individuals.</p>
            <p> Once you have selected the active variables, you can choose whether or not to standardize them. By default,
            the active variables are standardized. This choice is essential when variables are measured in relation to different units of measurement.</p>
            <p> Clustering is based on the number of components saved.
            By default, clustering is based on the first 5 components,
            <I>i.e.</I> the distance between individuals is calculated on these 5 components.</p>
            <p> By default, the <I>Number of clusters</I> field is set to -1 which means that the number of clusters
            is automatically chosen by the computer.</p>
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
      
      res.pca <- self$PCAResult
      if (is.null(res.pca))
        return()
      
      res.classif <- NULL
      need_classif <- isTRUE(self$options$graphclassif) || !self$results$newvar2$isNotFilled()
      
      if (need_classif)
        res.classif <- private$.getclassifResult()
      
      self$results$descdesdim$setContent(private$.dimdesc())
      self$results$code$setContent(private$.code())
      
      private$.printeigenTable()
      private$.printTables("coord")
      private$.printTables("contrib")
      private$.printTables("cos2")
      
      # Graphes toujours affichés
      self$results$plotind$setState(self$PCAResult)
      self$results$plotvar$setState(self$PCAResult)
      
      # Graphes supplémentaires optionnels
      if (isTRUE(self$options$graphind))
        self$results$plotseulind$setState(self$PCAResult)
      
      if (isTRUE(self$options$graphmod) && !is.null(self$options$qualisup))
        self$results$plotseulmod$setState(self$PCAResult)
      
      if (self$options$habillage > 0)
        self$results$plothabillage$setState(self$PCAResult)
      
      if (isTRUE(self$options$graphvaract))
        self$results$plotseulvaract$setState(self$PCAResult)
      
      if (isTRUE(self$options$graphvarillu) && !is.null(self$options$quantisup))
        self$results$plotseulvarillu$setState(self$PCAResult)
      
      if (isTRUE(self$options$graphclassif) && !is.null(res.classif))
        self$results$plotclassif$setState(res.classif)
      
      if (!is.null(res.classif))
        private$.output2(res.classif)
      
      
      private$.output()
    },

    #### Compute results ----
    
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
      
      reshcpc <- tryCatch(
        FactoMineR::HCPC(self$PCAResult, nb.clust = self$nbclust, graph = FALSE),
        error = function(e) NULL
      )
      private$.classifResult <- reshcpc
      return(private$.classifResult)
    },
    
    .getPCAResult = function() {
      
      data <- self$dataProcessed
      if (is.null(data)) return(NULL)
      
      has_quanti <- !is.null(self$options$quantisup) && length(self$options$quantisup) > 0
      has_quali  <- !is.null(self$options$qualisup)  && length(self$options$qualisup)  > 0
      
      ncp_candidates <- c(self$options$ncp, self$options$nFactors)
      ncp_candidates <- suppressWarnings(as.numeric(ncp_candidates))
      ncp_candidates <- ncp_candidates[!is.na(ncp_candidates) & ncp_candidates > 0]
      
      ncp_target <- if (length(ncp_candidates) == 0) 2 else max(ncp_candidates)
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
      
      private$.PCAResult <- r
      return(private$.PCAResult)
    },
    
    .dimdesc = function() {
      table <- self$PCAResult
      if (is.null(table))
        return("No result available")
      
      nFactors_out <- min(self$options$nFactors, ncol(table$ind$coord))
      if (is.null(nFactors_out) || nFactors_out < 1)
        return("No dimension available")
      
      res <- FactoMineR::dimdesc(table, axes = 1:nFactors_out, proba = self$options$proba / 100)
      paste(capture.output(print(res[-length(res)])), collapse = "\n")
    },
    
    .code = function() {
      
      has_quanti <- !is.null(self$options$quantisup) && length(self$options$quantisup) > 0
      has_quali  <- !is.null(self$options$qualisup)  && length(self$options$qualisup)  > 0
      
      names_var <- paste0("'", names(self$PCAResult$call$X), "'", collapse = ", ")
      data_str  <- paste0("data_PCA <- data[, c(", names_var, ")]")
      
      norme_str <- if (isTRUE(self$options$norme)) "TRUE" else "FALSE"
      ncp_str   <- self$options$ncp
      
      code_str <- if (has_quanti && !has_quali) {
        paste0(
          "PCA(data_PCA, quanti.sup=", self$nVaract + 1, ":", self$nVaract + self$nQuantsup,
          ", scale.unit=", norme_str,
          ", ncp=", ncp_str,
          ", graph=FALSE)"
        )
      } else if (!has_quanti && has_quali) {
        paste0(
          "PCA(data_PCA, quali.sup=", self$nVaract + 1, ":", self$nVaract + self$nQualsup,
          ", scale.unit=", norme_str,
          ", ncp=", ncp_str,
          ", graph=FALSE)"
        )
      } else if (has_quanti && has_quali) {
        q1 <- self$nVaract + self$nQuantsup
        paste0(
          "PCA(data_PCA, quanti.sup=", self$nVaract + 1, ":", q1,
          ", quali.sup=", q1 + 1, ":", q1 + self$nQualsup,
          ", scale.unit=", norme_str,
          ", ncp=", ncp_str,
          ", graph=FALSE)"
        )
      } else {
        paste0(
          "PCA(data_PCA, scale.unit=", norme_str,
          ", ncp=", ncp_str,
          ", graph=FALSE)"
        )
      }
      
      out <- list(
        "dataset" = data_str,
        "R code"  = code_str
      )
      
      paste(capture.output(print(out)), collapse = "\n")
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
    
    # .printTables = function(quoi) {
    #   
    #   table <- self$PCAResult
    #   individus_gui <- if (!is.null(self$options$individus))
    #     self$data[[self$options$individus]]
    #   else
    #     seq_len(nrow(self$data))
    #   
    #   if (quoi == "coord") {
    #     quoivar  <- table$var$coord
    #     quoiind  <- table$ind$coord
    #     tablevar <- self$results$variables$coordonnees
    #     tableind <- self$results$individus$coordonnees
    #   } else if (quoi == "contrib") {
    #     quoivar  <- table$var$contrib
    #     quoiind  <- table$ind$contrib
    #     tablevar <- self$results$variables$contribution
    #     tableind <- self$results$individus$contribution
    #   } else if (quoi == "cos2") {
    #     quoivar  <- table$var$cos2
    #     quoiind  <- table$ind$cos2
    #     tablevar <- self$results$variables$cosinus
    #     tableind <- self$results$individus$cosinus
    #   } else {
    #     return()
    #   }
    #   
    #   nFactors_out <- min(self$options$nFactors, ncol(quoivar), ncol(quoiind))
    #   
    #   tableind$addColumn(name = "individus", title = "", type = "text")
    #   for (i in seq_len(nrow(quoiind)))
    #     tableind$addRow(rowKey = i, value = NULL)
    #   
    #   tablevar$addColumn(name = "variables", title = "", type = "text")
    #   for (i in seq_len(nrow(quoivar)))
    #     tablevar$addRow(rowKey = i, value = NULL)
    #   
    #   for (i in seq_len(nFactors_out)) {
    #     tablevar$addColumn(name = paste0("dim", i), title = paste0("Dim.", i), type = "number")
    #     tableind$addColumn(name = paste0("dim", i), title = paste0("Dim.", i), type = "number")
    #   }
    #   
    #   for (var in seq_len(nrow(quoivar))) {
    #     row <- list(variables = rownames(quoivar)[var])
    #     for (i in seq_len(nFactors_out))
    #       row[[paste0("dim", i)]] <- quoivar[var, i]
    #     tablevar$setRow(rowNo = var, values = row)
    #   }
    #   
    #   for (ind in seq_along(individus_gui)) {
    #     row <- list(individus = if (is.null(self$options$individus))
    #       individus_gui[ind] else rownames(quoiind)[ind])
    #     for (i in seq_len(nFactors_out))
    #       row[[paste0("dim", i)]] <- quoiind[ind, i]
    #     tableind$setRow(rowNo = ind, values = row)
    #   }
    # },
    
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
          row <- list(individus = individus_gui[ind])
          for (i in seq_len(nFactors_out))
            row[[paste0("dim", i)]] <- quoiind[ind, i]
          tableind$setRow(rowNo = ind, values = row)
        }
      }
    },
    
    .plotindividus = function(image, ...) {
      if (self$nVaract < 2) return()
      
      res.pca <- image$state
      
      if (!is.null(self$options$qualisup) && length(self$options$qualisup) > 0) {
        plot <- FactoMineR::plot.PCA(res.pca,
                                     axes  = c(self$options$abs, self$options$ord),
                                     title = "Representation of the Individuals and the Categories"
        )
      } else {
        plot <- FactoMineR::plot.PCA(res.pca,
                                     axes  = c(self$options$abs, self$options$ord),
                                     title = "Representation of the Individuals"
        )
      }
      print(plot)
      TRUE
    },
    
    .plothabillage = function(image, ...) {
      if (self$nVaract < 2) return()
      res.pca <- image$state
      habillage_value <- self$nVaract + self$nQuantsup + self$options$habillage
      
      args <- list(
        res.pca,
        axes      = c(self$options$abs, self$options$ord),
        habillage = habillage_value,
        title     = "Representation of the Individuals (Colored by Variable)"
      )
      
      if (!is.null(self$options$qualisup) && length(self$options$qualisup) > 0)
        args$invisible <- "quali"
      
      plot <- do.call(FactoMineR::plot.PCA, args)
      print(plot)
      TRUE
    },
    
    .plotseulind = function(image, ...) {
      if (self$nVaract < 2) return()
      res.pca <- image$state
      args <- list(
        res.pca,
        axes      = c(self$options$abs, self$options$ord),
        habillage = "none",
        title     = "Representation of the Individuals"
      )
      
      if (!is.null(self$options$qualisup) && length(self$options$qualisup) > 0)
        args$invisible <- "quali"
      
      plot <- do.call(FactoMineR::plot.PCA, args)
      print(plot)
      TRUE
      
    },
    
    .plotseulmod = function(image, ...) {
      if (self$nVaract < 2) return()
      if (is.null(self$options$qualisup) || length(self$options$qualisup) == 0) return()
      res.pca <- image$state
      plot <- FactoMineR::plot.PCA(res.pca,
                                   axes      = c(self$options$abs, self$options$ord),
                                   invisible = "ind",
                                   title     = "Representation of the Categories"
      )
      print(plot)
      TRUE
    },
    
    .plotvariables = function(image, ...) {
      if (self$nVaract < 2) return()
      
      res.pca <- image$state
      abs_gui <- self$options$abs
      ord_gui <- self$options$ord
      
      # Graphe principal : variables actives + illustratives si quantisup présent
      if (!is.null(self$options$quantisup) && length(self$options$quantisup) > 0) {
        plot <- FactoMineR::plot.PCA(res.pca,
                                     choix = "var",
                                     axes  = c(abs_gui, ord_gui),
                                     title = "Representation of the Variables (Active and Supplementary)"
        )
      } else {
        plot <- FactoMineR::plot.PCA(res.pca,
                                     choix = "var",
                                     axes  = c(abs_gui, ord_gui),
                                     title = "Correlation Circle"
        )
      }
      print(plot)
      TRUE
    },
    
    .plotseulvaract = function(image, ...) {
      if (self$nVaract < 2) return()
      res.pca <- image$state
      args <- list(
        res.pca,
        choix = "var",
        axes  = c(self$options$abs, self$options$ord),
        title = "Representation of the Active Variables"
      )
      if (!is.null(self$options$quantisup) && length(self$options$quantisup) > 0)
        args$invisible <- "quanti.sup"
      
      plot <- do.call(FactoMineR::plot.PCA, args)
      print(plot)
      TRUE
    },
    
    .plotseulvarillu = function(image, ...) {
      if (self$nVaract < 2) return()
      if (is.null(self$options$quantisup) || length(self$options$quantisup) == 0) return()
      res.pca <- image$state
      plot <- FactoMineR::plot.PCA(res.pca,
                                   choix     = "var",
                                   axes      = c(self$options$abs, self$options$ord),
                                   invisible = "var",
                                   title     = "Representation of the Supplementary Variables"
      )
      print(plot)
      TRUE
    },
    
    .plotclassif = function(image, ...) {
      if (is.null(self$options$actvars) || self$nVaract < 2) return()
      
      res.classif <- image$state
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
      if (self$options$nFactors > self$nVaract)
        jmvcore::reject('Number of components cannot be bigger than number of variables')
    },
    
    .output = function() {
      nFactors_out <- min(self$options$ncp, ncol(self$PCAResult$ind$coord))
      
      if (self$results$newvar$isNotFilled()) {
        self$results$newvar$set(
          keys         = 1:nFactors_out,
          titles       = paste("Dim.", 1:nFactors_out),
          descriptions = rep("PCA component", nFactors_out),
          measureTypes = rep("continuous", nFactors_out)
        )
      }
      
      for (i in seq_len(nFactors_out))
        self$results$newvar$setValues(index = i, as.numeric(self$PCAResult$ind$coord[, i]))
      
      self$results$newvar$setRowNums(seq_len(nrow(self$dataProcessed)))
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
      output$setRowNums(seq_len(nrow(self$dataProcessed)))
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
      
      if (!is.null(self$options$individus)) {
        ids <- as.character(self$data[[self$options$individus]])
        ids[is.na(ids) | ids == ""] <- as.character(seq_len(sum(is.na(ids) | ids == "")))
        rownames(data) <- make.unique(ids)
      } else {
        rownames(data) <- as.character(seq_len(nrow(data)))
      }
      
      return(data)
    }
  )
)