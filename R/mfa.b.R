MFAClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "MFAClass",
  inherit = MFABase,
  active = list(
    dataProcessed = function() {
      if (is.null(private$.dataProcessed))
        private$.dataProcessed <- private$.buildData()
      return(private$.dataProcessed)
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
    
    MFAResult = function() {
      if (is.null(private$.MFAResult))
        private$.MFAResult <- private$.getMFAResult()
      return(private$.MFAResult)
    }
  ),
  
  private = list(
    .dataProcessed = NULL,
    .nbclust = NULL,
    .classifResult = NULL,
    .MFAResult = NULL,
    
    #---------------------------------------------  
    #### Init + run functions ----
    
    .init = function() {
      if (is.null(self$data) || (is.null(self$options$quantivar) && is.null(self$options$qualivar))) {
        if (self$options$tuto == TRUE) {
          self$results$instructions$setVisible(visible = TRUE)
        }
      }
      
      self$results$instructions$setContent(
        "<html>
        <head>
        </head>
        <body>
        <div class='justified-text'>
        <p><b>What you should know before running an MFA in jamovi</b></p>
        <p>______________________________________________________________________________</p>
        <p> The main objective of MFA is to analyse datasets structured according to groups of variables. 
        Therefore, the definition of the groups of variables is of utmost importance.</p>

        <p> In order to define the groups properly, you have to consider the order of the variables in the dataset analysed by MFA.
        Moving variables to the right-hand blocks creates the dataset analysed by MFA. 
        The order in which the variables are displayed corresponds to the order of the variables in the dataset analysed by MFA: continuous variables first, then categorical variables.</p>

        <p> 0. Open the <b>wine</b> dataset. Choose the Ident variable as <I>Individual Labels</I>. Put all the continuous (<I>resp.</I> categorical) variables in the proper 
        block. The two first fields <I>\"Groups definition\"</I> and <I>\"Groups type\"</I> are <B>mandatory</B>, while the two other
        fields <I>\"Groups name\"</I> and <I>\"Supplementary fields\"</I> are <B>optional</B>.</p>

        <p> 1. To define the groups, get rid of the characters <I>\"Ex: \"</I>. In this example, 6 groups of variables are constituted
        with respectively 5,3,10,9,2, and 2 variables.</p>

        <p> 2. The first five groups are scaled to unit variance (<I>\"s\"</I>), the last
        group is considered as categorical (<I>\"n\"</I>).</p>

        <p> 3. Giving a name to the groups is important when interpreting the results but is not mandatory. Considering 
        supplementary groups is also important when interpreting the results but not mandatory. In this example, 
        the two last groups will not change the representation of the individuals for instance, as they are considered
        as supplementary.</p>

        <p> Clustering is based on the number of components saved. 
        By default, clustering is based on the first 5 components, <I>i.e.</I> the distance between individuals is calculated on these 5 components.</p>
      
        <p> By default, the <I>Number of clusters</I> field is set to -1 which means that the number of clusters is automatically chosen by the computer.</p>

        <p>______________________________________________________________________________</p>
        
        </div>
        </body>
        </html>"
      )
    },
    
    .run = function() {
      if (is.null(self$options$quantivar) && is.null(self$options$qualivar))
        return()
      
      private$.errorCheck()
      
      res.mfa <- self$MFAResult
      if (is.null(res.mfa))
        return()
      
      res.classif <- NULL
      need_classif <- isTRUE(self$options$graphclassif) || !self$results$newvar2$isNotFilled()
      if (need_classif)
        res.classif <- private$.getclassifResult()
      
      dimdesc <- private$.dimdesc()
      self$results$descdesdim$setContent(dimdesc)
      
      private$.printeigenTable()
      
      self$results$plotgroup$setState(self$MFAResult)
      self$results$plotaxe$setState(self$MFAResult)
      self$results$plotind$setState(self$MFAResult)
      
      if (!is.null(self$MFAResult$summary.quali) && nrow(self$MFAResult$summary.quali) > 0) {
        self$results$plotcat$setVisible(visible = TRUE)
        self$results$plotcat$setState(self$MFAResult)
      }
      
      if (any(grepl("quanti", names(self$MFAResult)))) {
        self$results$plotvar$setVisible(visible = TRUE)
        self$results$plotvar$setState(self$MFAResult)
      }
      
      if (isTRUE(self$options$graphclassif) && !is.null(res.classif))
        self$results$plotclassif$setState(res.classif)
      
      if (!is.null(res.classif))
        private$.output2(res.classif)
      
      private$.output()
      self$results$code$setContent(private$.code())
    },
    
    #---------------------------------------------
    #### Compute results ----
    
    .computeNbclust = function() {
      nbclust <- self$options$nbclust
      return(nbclust)
    },
    
    .getclassifResult = function() {
      groupdef_gui  <- self$options$groupdef
      grouptype_gui <- self$options$grouptype
      if (groupdef_gui == "Ex: 5,3,10,9,2,2" || grouptype_gui == "Ex: s,s,s,s,s,n")
        return(NULL)
      
      reshcpc <- tryCatch(
        FactoMineR::HCPC(self$MFAResult, nb.clust = self$nbclust, graph = FALSE),
        error = function(e) NULL
      )
      private$.classifResult <- reshcpc
      return(private$.classifResult)
    },
    
    .getMFAResult = function() {
      data <- self$dataProcessed
      if (is.null(data)) return(NULL)
      
      groupdef_gui  <- self$options$groupdef
      groupill_gui  <- self$options$groupill
      grouptype_gui <- self$options$grouptype
      groupname_gui <- self$options$groupname
      
      if (groupdef_gui == "Ex: 5,3,10,9,2,2" || grouptype_gui == "Ex: s,s,s,s,s,n")
        return(NULL)
      
      ncp_candidates <- c(self$options$ncp, self$options$nFactors)
      ncp_candidates <- suppressWarnings(as.numeric(ncp_candidates))
      ncp_candidates <- ncp_candidates[!is.na(ncp_candidates) & ncp_candidates > 0]
      ncp_target     <- if (length(ncp_candidates) == 0) 2 else max(ncp_candidates)
      ncp_target     <- max(ncp_target, 3)
      ncp_use        <- ncp_target
      
      group <- trimws(strsplit(groupdef_gui, ",")[[1]])
      group <- as.numeric(group)
      
      type <- trimws(unlist(strsplit(grouptype_gui, ",")))
      has_ill  <- !(groupill_gui  %in% c("Ex: 5,6", "", "0"))
      has_name <- !(groupname_gui %in% c("Ex: olf,vis,olfag,gust,ens,orig", "", "0"))
      num_sup  <- if (has_ill)  as.numeric(trimws(strsplit(groupill_gui, ",")[[1]])) else NULL
      name_grp <- if (has_name) trimws(unlist(strsplit(groupname_gui, ",")))          else NULL
      
      r <- tryCatch({
        FactoMineR::MFA(
          data,
          group         = group,
          type          = type,
          ncp           = ncp_use,
          num.group.sup = num_sup,
          name.group    = name_grp,
          graph         = FALSE
        )
      }, error = function(e) {
        jmvcore::reject(paste("MFA failed:", e$message))
        return(NULL)
      })
      
      private$.MFAResult <- r
      return(private$.MFAResult)
    },
    
    .code = function() {
      
      groupdef_gui  <- self$options$groupdef
      groupill_gui  <- self$options$groupill
      grouptype_gui <- self$options$grouptype
      groupname_gui <- self$options$groupname
      
      names_var <- paste0("'", names(self$MFAResult$call$X), "'", collapse = ", ")
      data_str  <- paste0("data_MFA <- data[, c(", names_var, ")]")
      
      bad_groupdef <- is.null(groupdef_gui) ||
        !nzchar(trimws(as.character(groupdef_gui))) ||
        groupdef_gui == "Ex: 5,3,10,9,2,2"
      
      bad_grouptype <- is.null(grouptype_gui) ||
        !nzchar(trimws(as.character(grouptype_gui))) ||
        grouptype_gui == "Ex: s,s,s,s,s,n"
      
      if (bad_groupdef || bad_grouptype) {
        out <- list(
          "dataset" = data_str,
          "R code"  = "MFA(data_MFA, group=..., type=..., ncp=..., graph=FALSE)"
        )
        return(paste(capture.output(print(out)), collapse = "\n"))
      }
      
      has_ill <- !is.null(groupill_gui) &&
        nzchar(trimws(as.character(groupill_gui))) &&
        !groupill_gui %in% c("Ex: 5,6", "0")
      
      has_name <- !is.null(groupname_gui) &&
        nzchar(trimws(as.character(groupname_gui))) &&
        !groupname_gui %in% c("Ex: olf,vis,olfag,gust,ens,orig", "0")
      
      group_vec <- trimws(unlist(strsplit(as.character(groupdef_gui), ",")))
      type_vec  <- trimws(unlist(strsplit(as.character(grouptype_gui), ",")))
      
      group_vec <- group_vec[nzchar(group_vec)]
      type_vec  <- type_vec[nzchar(type_vec)]
      
      group_num <- suppressWarnings(as.numeric(group_vec))
      
      if (length(group_num) == 0 || any(is.na(group_num))) {
        out <- list(
          "dataset" = data_str,
          "R code"  = "MFA(data_MFA, group=..., type=..., ncp=..., graph=FALSE)"
        )
        return(paste(capture.output(print(out)), collapse = "\n"))
      }
      
      group_str <- paste0("c(", paste(group_num, collapse = ", "), ")")
      type_str  <- paste0("c('", paste(type_vec, collapse = "', '"), "')")
      
      code_parts <- c(
        paste0("group=", group_str),
        paste0("type=", type_str)
      )
      
      if (has_ill) {
        ill_vec <- trimws(unlist(strsplit(as.character(groupill_gui), ",")))
        ill_vec <- ill_vec[nzchar(ill_vec)]
        ill_num <- suppressWarnings(as.numeric(ill_vec))
        
        if (length(ill_num) > 0 && !any(is.na(ill_num))) {
          ill_str <- paste0("c(", paste(ill_num, collapse = ", "), ")")
          code_parts <- c(code_parts, paste0("num.group.sup=", ill_str))
        }
      }
      
      if (has_name) {
        name_vec <- trimws(unlist(strsplit(as.character(groupname_gui), ",")))
        name_vec <- name_vec[nzchar(name_vec)]
        
        if (length(name_vec) > 0) {
          name_str <- paste0("c('", paste(name_vec, collapse = "', '"), "')")
          code_parts <- c(code_parts, paste0("name.group=", name_str))
        }
      }
      
      code_parts <- c(
        code_parts,
        paste0("ncp=", self$options$ncp),
        "graph=FALSE"
      )
      
      code_str <- paste0(
        "MFA(data_MFA, ",
        paste(code_parts, collapse = ", "),
        ")"
      )
      
      out <- list(
        "dataset" = data_str,
        "R code"  = code_str
      )
      
      paste(capture.output(print(out)), collapse = "\n")
    },
    
    .dimdesc = function() {
      table <- self$MFAResult
      proba <- self$options$proba / 100
      groupdef_gui  <- self$options$groupdef
      grouptype_gui <- self$options$grouptype
      
      if (groupdef_gui == "Ex: 5,3,10,9,2,2" || grouptype_gui == "Ex: s,s,s,s,s,n")
        return("No result available")
      
      if (is.null(table) || is.null(table$eig))
        return("No result available")
      
      nFactors_out <- min(self$options$nFactors, nrow(table$eig))
      if (is.null(nFactors_out) || nFactors_out < 1)
        return("No dimension available")
      
      res <- FactoMineR::dimdesc(table, axes = 1:nFactors_out, proba = proba)
      paste(capture.output(print(res[-length(res)])), collapse = "\n")
    },
    
    .printeigenTable = function() {
      table <- self$MFAResult
      groupdef_gui  <- self$options$groupdef
      grouptype_gui <- self$options$grouptype
      
      if (groupdef_gui == "Ex: 5,3,10,9,2,2" || grouptype_gui == "Ex: s,s,s,s,s,n")
        return()
      
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
    
    .getValidAxes = function(res.mfa) {
      abs_gui <- suppressWarnings(as.numeric(self$options$abs))
      ord_gui <- suppressWarnings(as.numeric(self$options$ord))
      
      if (is.null(res.mfa) || is.null(res.mfa$eig))
        return(NULL)
      
      n_axes <- nrow(res.mfa$eig)
      
      if (is.na(abs_gui) || is.na(ord_gui) || n_axes < 2)
        return(NULL)
      
      if (abs_gui < 1 || ord_gui < 1 || abs_gui > n_axes || ord_gui > n_axes)
        return(NULL)
      
      c(abs_gui, ord_gui)
    },
    
    .getInvisibleMFA = function(res.mfa, hide = c("quali", "quali.sup")) {
      hide <- unique(hide)
      available <- character(0)
      
      if (!is.null(res.mfa$quali.var))
        available <- c(available, "quali")
      
      if (!is.null(res.mfa$quali.var.sup))
        available <- c(available, "quali.sup")
      
      if (!is.null(res.mfa$ind))
        available <- c(available, "ind")
      
      intersect(hide, available)
    },
    
    .plotindividus = function(image, ...) {
      groupdef_gui  <- self$options$groupdef
      grouptype_gui <- self$options$grouptype
      
      if (groupdef_gui == "Ex: 5,3,10,9,2,2" || grouptype_gui == "Ex: s,s,s,s,s,n")
        return(FALSE)
      
      res.mfa <- image$state
      if (is.null(res.mfa))
        return(FALSE)
      
      axes_ok <- private$.getValidAxes(res.mfa)
      if (is.null(axes_ok))
        return(FALSE)
      
      invisible_vec <- private$.getInvisibleMFA(res.mfa, c("quali", "quali.sup"))
      
      ok <- tryCatch({
        if (length(invisible_vec) > 0) {
          p <- FactoMineR::plot.MFA(
            res.mfa,
            axes = axes_ok,
            invisible = invisible_vec,
            title = "Representation of the Individuals"
          )
        } else {
          p <- FactoMineR::plot.MFA(
            res.mfa,
            axes = axes_ok,
            title = "Representation of the Individuals"
          )
        }
        print(p)
        TRUE
      }, error = function(e) {
        jmvcore::reject(paste("Plot of individuals failed:", e$message))
        FALSE
      })
      
      ok
    },
    
    .plotcategory = function(image, ...) {
      res.mfa <- image$state
      if (is.null(res.mfa))
        return(FALSE)
      
      if (is.null(res.mfa$summary.quali) || nrow(res.mfa$summary.quali) == 0)
        return(FALSE)
      
      axes_ok <- private$.getValidAxes(res.mfa)
      if (is.null(axes_ok))
        return(FALSE)
      
      invisible_vec <- private$.getInvisibleMFA(res.mfa, "ind")
      
      ok <- tryCatch({
        if (length(invisible_vec) > 0) {
          p <- FactoMineR::plot.MFA(
            res.mfa,
            axes = axes_ok,
            invisible = invisible_vec,
            title = "Representation of the Categories"
          )
        } else {
          p <- FactoMineR::plot.MFA(
            res.mfa,
            axes = axes_ok,
            title = "Representation of the Categories"
          )
        }
        print(p)
        TRUE
      }, error = function(e) {
        jmvcore::reject(paste("Plot of categories failed:", e$message))
        FALSE
      })
      
      ok
    },
    
    .plotvariables = function(image, ...) {
      groupdef_gui  <- self$options$groupdef
      grouptype_gui <- self$options$grouptype
      
      if (groupdef_gui == "Ex: 5,3,10,9,2,2" || grouptype_gui == "Ex: s,s,s,s,s,n")
        return(FALSE)
      
      res.mfa <- image$state
      if (is.null(res.mfa))
        return(FALSE)
      
      axes_ok <- private$.getValidAxes(res.mfa)
      if (is.null(axes_ok))
        return(FALSE)
      
      ok <- tryCatch({
        p <- FactoMineR::plot.MFA(
          res.mfa,
          choix = "var",
          axes = axes_ok,
          title = "Representation of the Variables"
        )
        print(p)
        TRUE
      }, error = function(e) {
        FALSE
      })
      
      ok
    },
    
    .plotgroups = function(image, ...) {
      groupdef_gui  <- self$options$groupdef
      grouptype_gui <- self$options$grouptype
      
      if (groupdef_gui == "Ex: 5,3,10,9,2,2" || grouptype_gui == "Ex: s,s,s,s,s,n")
        return(FALSE)
      
      res.mfa <- image$state
      if (is.null(res.mfa))
        return(FALSE)
      
      axes_ok <- private$.getValidAxes(res.mfa)
      if (is.null(axes_ok))
        return(FALSE)
      
      ok <- tryCatch({
        p <- FactoMineR::plot.MFA(
          res.mfa,
          choix = "group",
          axes = axes_ok,
          title = "Representation of the Groups"
        )
        print(p)
        TRUE
      }, error = function(e) {
        jmvcore::reject(paste("Plot of groups failed:", e$message))
        FALSE
      })
      
      ok
    },
    
    .plotaxes = function(image, ...) {
      groupdef_gui  <- self$options$groupdef
      grouptype_gui <- self$options$grouptype
      
      if (groupdef_gui == "Ex: 5,3,10,9,2,2" || grouptype_gui == "Ex: s,s,s,s,s,n")
        return(FALSE)
      
      res.mfa <- image$state
      if (is.null(res.mfa))
        return(FALSE)
      
      axes_ok <- private$.getValidAxes(res.mfa)
      if (is.null(axes_ok))
        return(FALSE)
      
      ok <- tryCatch({
        p <- FactoMineR::plot.MFA(
          res.mfa,
          choix = "axes",
          axes = axes_ok,
          title = "Representation of the Partial Axes"
        )
        print(p)
        TRUE
      }, error = function(e) {
        jmvcore::reject(paste("Plot of partial axes failed:", e$message))
        FALSE
      })
      
      ok
    },
    
    .plotclassif = function(image, ...) {
      groupdef_gui  <- self$options$groupdef
      grouptype_gui <- self$options$grouptype
      
      if (groupdef_gui %in% c(NULL, "", "Ex: 5,3,10,9,2,2") ||
          grouptype_gui %in% c(NULL, "", "Ex: s,s,s,s,s,n"))
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
          title = "Representation of the Individuals According to Clusters"
        )
        print(p)
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
      groupdef_gui  <- self$options$groupdef
      grouptype_gui <- self$options$grouptype
      
      if (groupdef_gui %in% c(NULL, "", "Ex: 5,3,10,9,2,2") ||
          grouptype_gui %in% c(NULL, "", "Ex: s,s,s,s,s,n"))
        return()
      
      n_quant <- if (is.null(self$options$quantivar)) 0 else length(self$options$quantivar)
      n_quali <- if (is.null(self$options$qualivar)) 0 else length(self$options$qualivar)
      group_sizes <- trimws(strsplit(groupdef_gui, ",")[[1]])
      group_sizes <- suppressWarnings(as.numeric(group_sizes))
      
      if (any(is.na(group_sizes)))
        jmvcore::reject("The definition of the groups is not valid")
      
      if ((n_quant + n_quali) != sum(group_sizes))
        jmvcore::reject("The definition of the groups is not good")
    },
    
    .output = function() {
      nFactors_out <- min(self$options$ncp, dim(self$MFAResult$eig)[1])
      
      if (self$results$newvar$isNotFilled()) {
        keys <- 1:nFactors_out
        measureTypes <- rep("continuous", nFactors_out)
        titles <- paste("Dim.", keys)
        descriptions <- character(length(keys))
        self$results$newvar$set(
          keys = keys,
          titles = titles,
          descriptions = descriptions,
          measureTypes = measureTypes
        )
      }
      
      for (i in seq_len(nFactors_out)) {
        scores <- as.numeric(self$MFAResult$ind$coord[, i])
        self$results$newvar$setValues(index = i, scores)
      }
      
      self$results$newvar$setRowNums(seq_len(nrow(self$dataProcessed)))
    },
    
    .output2 = function(res.classif) {
      groupdef_gui  <- self$options$groupdef
      grouptype_gui <- self$options$grouptype
      if (groupdef_gui == "Ex: 5,3,10,9,2,2" || grouptype_gui == "Ex: s,s,s,s,s,n")
        return()
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
      
      scores <- as.factor(res.classif$data.clust[, ncol(res.classif$data.clust)])
      output$setValues(index = 1, scores)
      output$setRowNums(seq_len(nrow(self$dataProcessed)))
    },
    
    .buildData = function() {
      data_list <- list()
      
      if (!is.null(self$options$quantivar) && length(self$options$quantivar) > 0) {
        dataquantivar <- data.frame(self$data[, self$options$quantivar, drop = FALSE])
        colnames(dataquantivar) <- self$options$quantivar
        data_list <- c(data_list, list(dataquantivar))
      }
      
      if (!is.null(self$options$qualivar) && length(self$options$qualivar) > 0) {
        dataqualivar <- data.frame(self$data[, self$options$qualivar, drop = FALSE])
        colnames(dataqualivar) <- self$options$qualivar
        data_list <- c(data_list, list(dataqualivar))
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