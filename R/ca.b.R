# This file is a generated template, your changes will not be overwritten
CAClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "CAClass",
  inherit = CABase,
  active = list(
    nbclust = function() {
      if (is.null(private$.nbclust))
        private$.nbclust <- private$.computeNbclust()
      return(private$.nbclust)
    }
  ),
  
  private = list(
    .nbclust = NULL,
    
    #---------------------------------------------
    #### Init + run functions ----
    
    .init = function() {
      if (is.null(self$options$activecol)) {
        if (isTRUE(self$options$tuto))
          self$results$instructions$setVisible(visible = TRUE)
      }
      self$results$instructions$setContent(
        "<html>
        <head></head>
        <body>
        <div class='justified-text'>
        <p><b>What you should know before running a CA in jamovi</b></p>
        <p>______________________________________________________________________________</p>
        <p> Correspondence Analysis (CA) is a multivariate statistical technique used to analyze
        the associations between two categorical variables. It is often applied to explore and visualize
        the relationships between the rows and columns of a contingency table, revealing a structure of association and disassociation.</p>
        <p> The interpretation of the CA plot allows to identify which categories of variables
        tend to co-occur or are associated with each other and which ones are relatively
        independent or disassociated. This knowledge can provide valuable insights into the underlying
        relationships between the two categorical variables of interest.</p>
        <p> While the <I>Active Columns</I> field is <B>mandatory</B>, the <I>Supplementary Columns</I> field is <B>optional</B>.
        However, if you have supplementary columns, they may be essential for interpreting the structure of association.</p>
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
      if (is.null(self$options$activecol))
        return()
      
      private$.errorCheck()
      
      data      <- private$.buildData()
      res.ca    <- private$.CA(data)
      
      if (is.null(res.ca) || !inherits(res.ca, "CA")) {
        jmvcore::reject("CA failed. Please check your data.")
        return()
      }
      
      res.classif <- NULL
      need_classif <- isTRUE(self$options$graphclassif) || !self$results$newvar2$isNotFilled()
      if (need_classif)
        res.classif <- private$.classif(res.ca)
      
      res.xsq <- private$.chisq(data)
      private$.chideux(res.xsq)
      
      tab  <- private$.dimdesc(res.ca)
      code <- private$.code(res.ca)
      self$results$code$setContent(code)
      
      if (!is.null(tab))
        private$.dodTable(tab)
      
      private$.printeigenTable(res.ca)
      private$.printTables(res.ca, "coord")
      private$.printTables(res.ca, "contrib")
      private$.printTables(res.ca, "cos2")
      
      self$results$ploticol$setState(res.ca)
      self$results$plotirow$setState(res.ca)
      self$results$plotell$setState(res.ca)
      
      if (isTRUE(self$options$graphclassif) && !is.null(res.classif))
        self$results$plotclassif$setState(res.classif)
      
      if (!is.null(res.classif))
        private$.output2(res.classif, data)
      
      private$.output(res.ca, data)
    },
    
    #### Compute results ----
    
    .computeNbclust = function() {
      return(self$options$nbclust)
    },
    
    .CA = function(data) {
      
      # Logique ncp défensive alignée sur PCA/MCA
      ncp_candidates <- c(self$options$ncp, self$options$nbfact)
      ncp_candidates <- suppressWarnings(as.numeric(ncp_candidates))
      ncp_candidates <- ncp_candidates[!is.na(ncp_candidates) & ncp_candidates > 0]
      ncp_target     <- if (length(ncp_candidates) == 0) 2 else max(ncp_candidates)
      ncp_target     <- max(ncp_target, 3)  # plancher à 3 pour plot.CA
      ncp_use        <- ncp_target
      
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
      return(res)
    },
    
    .code = function(table) {
      illucol_gui <- self$options$illustrativecol
      
      names_var <- paste0("'", names(table$call$Xtot), "'", collapse = ", ")
      data_str  <- paste0("data_CA <- data[, c(", names_var, ")]")
      
      if (!is.null(illucol_gui) && length(illucol_gui) > 0) {
        illucol_index <- match(illucol_gui, names(table$call$Xtot))
        illucol_index <- illucol_index[!is.na(illucol_index)]
        
        code_str <- paste0(
          "CA(data_CA, col.sup=c(",
          paste(illucol_index, collapse = ", "),
          "), ncp=", self$options$ncp,
          ", graph=FALSE)"
        )
      } else {
        code_str <- paste0(
          "CA(data_CA, ncp=", self$options$ncp,
          ", graph=FALSE)"
        )
      }
      
      out <- list(
        "dataset" = data_str,
        "R code"  = code_str
      )
      
      paste(capture.output(print(out)), collapse = "\n")
    },
    
    .classif = function(res) {
      tryCatch(
        FactoMineR::HCPC(res, nb.clust = self$nbclust, graph = FALSE),
        error = function(e) NULL
      )
    },
    
    .chisq = function(data) {
      # Protection si pas de colonnes actives
      if (is.null(self$options$activecol)) return(NULL)
      dataactcol <- data.frame(self$data[, self$options$activecol, drop = FALSE])
      colnames(dataactcol) <- self$options$activecol
      tryCatch(chisq.test(dataactcol), error = function(e) NULL)
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
        ids[is.na(ids) | ids == ""] <- as.character(seq_len(sum(is.na(ids) | ids == "")))
        rownames(dataactcol) <- make.unique(ids)
      }
      
      res_ca_active <- tryCatch(
        FactoMineR::CA(dataactcol, ncp = self$options$ncp, graph = FALSE),
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
      
      tab <- cbind(names(ddca)[1], names(ddca[[1]][1]),
                   rownames(as.data.frame(ddca[[1]][1])), as.data.frame(ddca[[1]][1])[[1]])
      tab <- as.data.frame(tab)
      
      pretab <- cbind(names(ddca)[1], names(ddca[[1]][2]),
                      rownames(as.data.frame(ddca[[1]][2])), as.data.frame(ddca[[1]][2])[[1]])
      tab <- rbind(tab, pretab)
      colnames(tab) <- c("dim", "rowcol", "name", "coord")
      
      for (i in 2:length(ddca)) {
        for (k in 1:2) {
          temp <- as.data.frame(ddca[[i]][[k]])
          if (nrow(temp) > 0) {
            pretab <- cbind(names(ddca)[i], names(ddca[[i]])[k], rownames(temp), temp[[1]])
            pretab <- as.data.frame(pretab)
            colnames(pretab) <- c("dim", "rowcol", "name", "coord")
            tab <- rbind(tab, pretab)
          }
        }
      }
      
      tab[, 4] <- as.numeric(as.character(tab[, 4]))
      as.data.frame(tab)
    },
    
    .getValidAxes = function(res.ca) {
      abs_gui <- suppressWarnings(as.numeric(self$options$abs))
      ord_gui <- suppressWarnings(as.numeric(self$options$ord))
      
      if (is.null(res.ca) || is.null(res.ca$eig))
        return(NULL)
      
      n_axes <- nrow(res.ca$eig)
      
      if (is.na(abs_gui) || is.na(ord_gui) || abs_gui < 1 || ord_gui < 1)
        return(NULL)
      
      if (abs_gui > n_axes || ord_gui > n_axes)
        return(NULL)
      
      c(abs_gui, ord_gui)
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
      
      row_gui <- if (!is.null(self$options$indiv))
        as.character(self$data[[self$options$indiv]])
      else
        as.character(seq_len(nrow(self$data)))
      
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
          row <- list(row = row_gui[ind])
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
      
      res.ca <- image$state
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
      
      res.ca <- image$state
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
      
      res.ca <- image$state
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
      
      res.classif <- image$state
      if (is.null(res.classif))
        return(FALSE)
      
      abs_gui <- suppressWarnings(as.numeric(self$options$abs))
      ord_gui <- suppressWarnings(as.numeric(self$options$ord))
      
      if (is.na(abs_gui) || is.na(ord_gui))
        return(FALSE)
      
      ok <- tryCatch({
        p <- FactoMineR::plot.HCPC(
          res.classif,
          axes = c(abs_gui, ord_gui),
          choice = "map",
          draw.tree = FALSE,
          title = "Representation of the Rows According to Clusters"
        )
        print(p)
        TRUE
      }, error = function(e) {
        jmvcore::reject(paste("Cluster plot failed:", e$message))
        FALSE
      })
      
      ok
    },
    
    ### Helper functions ----
    
    .errorCheck = function() {
      nbfact_gui <- suppressWarnings(as.numeric(self$options$nbfact))
      
      if (is.na(nbfact_gui) || nbfact_gui < 1)
        jmvcore::reject("Number of dimensions must be at least 1")
      
      if (is.null(self$options$activecol) || length(self$options$activecol) < 2)
        jmvcore::reject("At least two active columns are required")
      
      if (length(self$options$activecol) < nbfact_gui)
        jmvcore::reject("Number of dimensions cannot exceed the number of active columns")
    },
    
    .output = function(res.ca, data) {
      nFactors_out <- min(self$options$ncp, ncol(res.ca$row$coord))
      
      if (self$results$newvar$isNotFilled()) {
        self$results$newvar$set(
          keys         = 1:nFactors_out,
          titles       = paste("Dim.", 1:nFactors_out),
          descriptions = rep("CA component", nFactors_out),
          measureTypes = rep("continuous", nFactors_out)
        )
      }
      
      for (i in seq_len(nFactors_out))
        self$results$newvar$setValues(index = i, as.numeric(res.ca$row$coord[, i]))
      
      self$results$newvar$setRowNums(seq_len(nrow(data)))
    },
    
    .output2 = function(res.classif, data) {
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
      output$setRowNums(seq_len(nrow(data)))
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
      
      if (!is.null(self$options$indiv)) {
        ids <- as.character(self$data[[self$options$indiv]])
        ids[is.na(ids) | ids == ""] <- as.character(seq_len(sum(is.na(ids) | ids == "")))
        rownames(data) <- make.unique(ids)
      } else {
        rownames(data) <- as.character(seq_len(nrow(data)))
      }
      
      data
    }
  )
)