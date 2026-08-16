
# This file is a generated template, your changes will not be overwritten

catdesClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "catdesClass",
    inherit = catdesBase,
    active = list(
        dataProcessed = function() {
            if (is.null(private$.dataProcessed))
                private$.dataProcessed <- private$.buildData()

            return(private$.dataProcessed)
        },
        condesResult = function() {
            if (is.null(private$.condesResult))
                private$.condesResult <- private$.getcondesResult()

            return(private$.condesResult)
        },
        catdesResult = function() {
            if (is.null(private$.catdesResult))
                private$.catdesResult <- private$.getcatdesResult()

            return(private$.catdesResult)
        },
        catdesCategoryResult = function() {
            if (is.null(private$.catdesCategoryResult))
                private$.catdesCategoryResult <- private$.getcatdesCategoryResult()

            return(private$.catdesCategoryResult)
        },
        catdesCategoryQuantiResult = function() {
            if (is.null(private$.catdesCategoryQuantiResult))
                private$.catdesCategoryQuantiResult <- private$.getcatdesCategoryQuantiResult()

            return(private$.catdesCategoryQuantiResult)
        }
    ),
    private = list(

      .dataProcessed = NULL,
      .catdesResult = NULL,
      .condesResult = NULL,
      .catdesCategoryResult = NULL,
      .catdesCategoryQuantiResult = NULL,

      .resetCache = function() {
        private$.dataProcessed <- NULL
        private$.catdesResult <- NULL
        private$.condesResult <- NULL
        private$.catdesCategoryResult <- NULL
        private$.catdesCategoryQuantiResult <- NULL
      },

      .hasRows = function(x) {
        !is.null(x) && !is.null(dim(x)) && nrow(x) > 0
      },

    #---------------------------------------------
    #### Init + run functions ----

        .init = function() {
            if (is.null(self$options$descbyvar)) {
              if (self$options$tuto==TRUE){
                self$results$instructions$setVisible(visible = TRUE)
              }
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
      <b>What you should know before characterizing a variable in jamovi</b>
    </p>

    <div style='
        border-top: 1px solid #CBD8E8;
        margin-bottom: 12px;
    '></div>

    <p style='margin: 0 0 9px 0;'>
      <b>Purpose.</b>
      This analysis identifies the variables that are statistically associated
      with a selected variable and shows how they characterize it. The
      variable to characterize may be either categorical or quantitative, and
      it can be described by both categorical and quantitative variables.
    </p>

    <p style='margin: 0 0 7px 0;'>
      <b>When the variable to characterize is categorical:</b>
    </p>

    <ul style='margin: 0 0 9px 0; padding-left: 22px;'>

      <li style='margin-bottom: 6px;'>
        categorical descriptive variables are first tested globally using a
        chi-square test of independence;
      </li>

      <li style='margin-bottom: 6px;'>
        the detailed results identify categories that are significantly
        over-represented or under-represented within each category of the
        variable to characterize;
      </li>

      <li>
        quantitative descriptive variables are evaluated using a one-way
        analysis of variance, summarized by a squared correlation ratio and
        an F-test. Their means within each category are then compared with the
        overall mean.
      </li>

    </ul>

    <p style='margin: 0 0 7px 0;'>
      <b>When the variable to characterize is quantitative:</b>
    </p>

    <ul style='margin: 0 0 9px 0; padding-left: 22px;'>

      <li style='margin-bottom: 6px;'>
        its relationships with other quantitative variables are described
        using correlation coefficients and their associated tests;
      </li>

      <li style='margin-bottom: 6px;'>
        its relationships with categorical variables are summarized using
        R<sup>2</sup> and the associated significance test;
      </li>

      <li>
        the detailed results indicate which categories are associated with
        values that are significantly higher or lower than the overall mean
        of the quantitative variable.
      </li>

    </ul>

    <div style='
        margin: 12px 0;
        padding: 11px 13px;
        background-color: #FFF8E8;
        border: 1px solid #E6C878;
        border-left: 5px solid #E6AC40;
        border-radius: 4px;
    '>

      <p style='
          margin: 0 0 7px 0;
          color: #7A5A12;
      '>
        <b>Interpreting the significance threshold</b>
      </p>

      <p style='margin: 0;'>
        Only associations whose p-value is below the selected threshold are
        displayed. Increasing the threshold reveals weaker associations but
        does not make them stronger. These results describe statistical
        relationships and should not be interpreted as evidence of causality.
      </p>

    </div>

    <p style='margin: 0 0 9px 0;'>
      <b>Example.</b>
      Open the <b>decathlon</b> dataset. Select <i>Competition</i> as the
      <i>Variable to Characterize</i>, then place all the other variables
      except <i>Ident</i> and <i>Rank</i> in the <i>Described by</i> field.
    </p>

    <p style='margin: 0 0 9px 0;'>
      The <i>100m</i> variable is associated with <i>Competition</i>. In this
      sample, athletes recorded a slower average time at the Decastar than at
      the Olympic Games: 11.18 seconds compared with 10.92 seconds.
    </p>

    <p style='margin: 0;'>
      Increasing the significance threshold to 20% also displays the weaker
      association with <i>Shot.put</i>. In this sample, the average distance
      was 14.63 meters at the Olympic Games and 14.16 meters at the Decastar.
      The 20% threshold is useful for illustrating how the filtering works,
      but it represents much weaker statistical evidence than the conventional
      5% threshold.
    </p>

  </div>
  "
          )
        },

      .run = function() {
        if (is.null(self$options$vartochar) ||
            is.null(self$options$descbyvar) ||
            length(self$options$descbyvar) == 0)
          return()

        private$.resetCache()
        private$.errorCheck()

        data <- self$dataProcessed
        if (is.null(data) || nrow(data) == 0)
          return()

        show_code <- isTRUE(self$options$showCode)
        self$results$code$setVisible(visible = show_code)
        if (show_code)
          self$results$code$setContent(private$.code())

        # Reset parent visibility before populating the outputs relevant to the
        # current target type. This also prevents empty groups after an option
        # change that produces fewer significant results.
        self$results$chigroup$setVisible(visible = FALSE)
        self$results$categgroup$setVisible(visible = FALSE)
        self$results$qtvargroup$setVisible(visible = FALSE)
        self$results$qtgroup$setVisible(visible = FALSE)

        if (is.numeric(data[[1]])) {
          # Outputs specific to a categorical target are not relevant here.
          self$results$chigroup$setVisible(visible = FALSE)
          self$results$categgroup$categquali$setVisible(visible = FALSE)
          self$results$qtvargroup$setVisible(visible = FALSE)
          self$results$qtgroup$qt$setVisible(visible = FALSE)

          result <- self$condesResult
          if (is.null(result))
            return()

          if (private$.hasRows(result[["quanti"]])) {
            self$results$qtgroup$setVisible(visible = TRUE)
            self$results$qtgroup$qtcor$setVisible(visible = TRUE)
            private$.printCondesCorTable()
          } else {
            self$results$qtgroup$qtcor$setVisible(visible = FALSE)
          }

          if (private$.hasRows(result[["quali"]])) {
            self$results$categgroup$setVisible(visible = TRUE)
            self$results$categgroup$qualir2$setVisible(visible = TRUE)
            private$.printCondesR2Table()
          } else {
            self$results$categgroup$qualir2$setVisible(visible = FALSE)
          }

          if (private$.hasRows(result[["category"]])) {
            self$results$categgroup$setVisible(visible = TRUE)
            self$results$categgroup$categquanti$setVisible(visible = TRUE)
            private$.printCondesCategTable()
          } else {
            self$results$categgroup$categquanti$setVisible(visible = FALSE)
          }
        } else {
          # Outputs specific to a quantitative target are not relevant here.
          self$results$categgroup$categquanti$setVisible(visible = FALSE)
          self$results$qtgroup$qtcor$setVisible(visible = FALSE)
          self$results$categgroup$qualir2$setVisible(visible = FALSE)

          result <- self$catdesResult
          if (is.null(result))
            return()

          if (private$.hasRows(result[["test.chi2"]])) {
            self$results$chigroup$setVisible(visible = TRUE)
            private$.chiTable()
          } else {
            self$results$chigroup$setVisible(visible = FALSE)
          }

          if (!is.null(result[["category"]]) &&
              length(result[["category"]]) > 0 &&
              private$.categoryTable()) {
            self$results$categgroup$setVisible(visible = TRUE)
            self$results$categgroup$categquali$setVisible(visible = TRUE)
          } else {
            self$results$categgroup$categquali$setVisible(visible = FALSE)
          }

          if (private$.hasRows(result[["quanti.var"]])) {
            self$results$qtvargroup$setVisible(visible = TRUE)
            private$.qtvarTable()
          } else {
            self$results$qtvargroup$setVisible(visible = FALSE)
          }

          if (!is.null(result[["quanti"]]) &&
              length(result[["quanti"]]) > 0 &&
              private$.qtTable()) {
            self$results$qtgroup$setVisible(visible = TRUE)
            self$results$qtgroup$qt$setVisible(visible = TRUE)
          } else {
            self$results$qtgroup$qt$setVisible(visible = FALSE)
          }
        }
      },

      #Fonction

      .getcatdesResult = function() {
        threshold <- self$options$threshold / 100
        private$.catdesResult <- tryCatch(
          FactoMineR::catdes(
            self$dataProcessed,
            num.var = 1,
            proba = threshold
          ),
          error = function(e) {
            jmvcore::reject(paste(
              "Categorical variable description failed:",
              conditionMessage(e)
            ))
            NULL
          }
        )
        private$.catdesResult
      },

      .getcondesResult = function() {
        threshold <- self$options$threshold / 100
        private$.condesResult <- tryCatch(
          FactoMineR::condes(
            self$dataProcessed,
            num.var = 1,
            proba = threshold
          ),
          error = function(e) {
            jmvcore::reject(paste(
              "Quantitative variable description failed:",
              conditionMessage(e)
            ))
            NULL
          }
        )
        private$.condesResult
      },

      .code = function() {
        r_literal <- function(value) {
          paste(deparse(value, width.cutoff = 500L), collapse = "\n")
        }

        option_names <- function(value) {
          if (is.null(value) || length(value) == 0L)
            return(character(0))
          value <- as.character(unlist(value, use.names = FALSE))
          value[!is.na(value) & nzchar(value)]
        }

        target <- option_names(self$options$vartochar)
        descriptors <- option_names(self$options$descbyvar)
        variables <- c(target, descriptors)

        if (length(target) != 1L || !nzchar(target) ||
            length(descriptors) == 0L)
          return("# Select a variable to characterize and at least one descriptive variable.")

        proba <- suppressWarnings(as.numeric(self$options$threshold)) / 100
        if (length(proba) != 1L || !is.finite(proba))
          proba <- 0.05

        quantitative_target <- is.numeric(self$data[[target]])
        function_name <- if (quantitative_target) {
          "FactoMineR::condes"
        } else {
          "FactoMineR::catdes"
        }
        result_name <- if (quantitative_target) {
          "res_condes"
        } else {
          "res_catdes"
        }

        code <- c(
          "library(FactoMineR)",
          "",
          "# This script can be pasted directly into the jamovi Rj Editor.",
          "# The dataset open in jamovi is available as data.",
          "",
          "# Keep the variable to characterize in the first column.",
          paste0(
            "data_description <- data[, ", r_literal(variables),
            ", drop = FALSE]"
          ),
          "",
          "# num.var = 1 identifies the first column as the target variable.",
          paste0(
            "# proba = ", r_literal(proba),
            " keeps associations whose p-value is below this threshold."
          ),
          paste0(result_name, " <- ", function_name, "("),
          "  data_description,",
          "  num.var = 1,",
          paste0("  proba = ", r_literal(proba)),
          ")",
          "",
          result_name
        )

        paste(code, collapse = "\n")
      },

      .getcatdesCategoryResult = function() {
        res <- self$catdesResult
        private$.catdesCategoryResult <- private$.flattenLevelResults(
          if (is.null(res)) NULL else res[["category"]],
          levels(self$dataProcessed[[1]])
        )
        private$.catdesCategoryResult
      },

      .getcatdesCategoryQuantiResult = function() {
        res <- self$catdesResult
        private$.catdesCategoryQuantiResult <- private$.flattenLevelResults(
          if (is.null(res)) NULL else res[["quanti"]],
          levels(self$dataProcessed[[1]])
        )
        private$.catdesCategoryQuantiResult
      },

      .flattenLevelResults = function(results, target_levels) {
        if (is.null(results) || length(results) == 0)
          return(NULL)

        n_items <- min(length(results), length(target_levels))
        if (n_items < 1)
          return(NULL)

        pieces <- lapply(seq_len(n_items), function(i) {
          item <- results[[i]]
          if (is.null(item) || is.null(dim(item)) || nrow(item) == 0)
            return(NULL)

          item <- as.data.frame(item, stringsAsFactors = FALSE)
          labels <- rownames(item)
          if (is.null(labels) || length(labels) != nrow(item))
            labels <- rep("", nrow(item))

          piece <- data.frame(
            Level = rep(target_levels[i], nrow(item)),
            Category = labels,
            item,
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
          rownames(piece) <- NULL
          piece
        })

        pieces <- Filter(Negate(is.null), pieces)
        if (length(pieces) == 0)
          return(NULL)

        result <- do.call(rbind, pieces)
        rownames(result) <- NULL
        result
      },

      ### Table populating functions ----
      .chiTable = function(){
        table <- self$catdesResult[["test.chi2"]]
        if (!private$.hasRows(table)) {
          self$results$chigroup$setVisible(visible = FALSE)
          return(invisible(FALSE))
        }

        for (i in seq_len(nrow(table))) {
          row <- list(
            varchi = as.character(rownames(table)[i]),
            chipv = table[i, 1],
            df = table[i, 2]
          )
          self$results$chigroup$chi$addRow(rowKey = i, values = row)
        }
        invisible(TRUE)
      },

      .categoryTable = function(){
        tab <- self$catdesCategoryResult
        if (!private$.hasRows(tab) || ncol(tab) < 7)
          return(invisible(FALSE))

        for (i in seq_len(nrow(tab))) {
          row <- list(
            varcateg = as.character(tab[i, 1]),
            vardesccateg = as.character(tab[i, 2]),
            clamod = tab[i, 3],
            modcla = tab[i, 4],
            global = tab[i, 5],
            categpv = tab[i, 6],
            vtest = tab[i, 7]
          )
          self$results$categgroup$categquali$addRow(
            rowKey = i,
            values = row
          )
        }
        invisible(TRUE)
      },

      .printCondesCategTable = function() {
        table <- self$condesResult[["category"]]
        if (!private$.hasRows(table))
          return(invisible(FALSE))

        labels <- rownames(table)
        for (i in seq_len(nrow(table))) {
          parts <- strsplit(labels[i], "=", fixed = TRUE)[[1]]
          row <- list(
            varcateg = if (length(parts) > 0) parts[1] else "",
            vardesccateg = if (length(parts) > 1) {
              paste(parts[-1], collapse = "=")
            } else {
              ""
            },
            estimate = table[i, 1],
            categpv = table[i, 2]
          )
          self$results$categgroup$categquanti$addRow(
            rowKey = i,
            values = row
          )
        }
        invisible(TRUE)
      },

      .printCondesCorTable = function() {
        table <- self$condesResult[["quanti"]]
        if (!private$.hasRows(table))
          return(invisible(FALSE))

        for (i in seq_len(nrow(table))) {
          row <- list(
            varcor = rownames(table)[i],
            cor = table[i, 1],
            corpvalue = table[i, 2]
          )
          self$results$qtgroup$qtcor$addRow(rowKey = i, values = row)
        }
        invisible(TRUE)
      },

      .printCondesR2Table = function() {
        table <- self$condesResult[["quali"]]
        if (!private$.hasRows(table))
          return(invisible(FALSE))

        for (i in seq_len(nrow(table))) {
          row <- list(
            varr2 = rownames(table)[i],
            r2 = table[i, 1],
            r2pvalue = table[i, 2]
          )
          self$results$categgroup$qualir2$addRow(rowKey = i, values = row)
        }
        invisible(TRUE)
      },

      .qtvarTable = function(){
        table <- self$catdesResult[["quanti.var"]]
        if (!private$.hasRows(table))
          return(invisible(FALSE))

        for (i in seq_len(nrow(table))) {
          row <- list(
            varqtvar = as.character(rownames(table)[i]),
            scc = table[i, 1],
            qtvarpv = table[i, 2]
          )
          self$results$qtvargroup$qtvar$addRow(rowKey = i, values = row)
        }
        invisible(TRUE)
      },

      .qtTable = function(){
        tabqt <- self$catdesCategoryQuantiResult
        if (!private$.hasRows(tabqt) || ncol(tabqt) < 8)
          return(invisible(FALSE))

        for (i in seq_len(nrow(tabqt))) {
          row <- list(
            varqt = as.character(tabqt[i, 1]),
            vardescqt = as.character(tabqt[i, 2]),
            vtestqt = tabqt[i, 3],
            meancateg = tabqt[i, 4],
            overallmean = tabqt[i, 5],
            sdcateg = tabqt[i, 6],
            overallsd = tabqt[i, 7],
            qtpv = tabqt[i, 8]
          )
          self$results$qtgroup$qt$addRow(rowKey = i, values = row)
        }
        invisible(TRUE)
      },

      .errorCheck = function() {
        threshold <- suppressWarnings(as.numeric(self$options$threshold))
        if (length(threshold) != 1 || !is.finite(threshold) ||
            threshold < 0 || threshold > 100)
          jmvcore::reject("Significance threshold must be between 0 and 100")

        if (self$options$vartochar %in% self$options$descbyvar)
          jmvcore::reject(
            "The variable to characterize cannot also be used to describe itself"
          )

        target <- self$data[[self$options$vartochar]]
        if (is.numeric(target)) {
          observed <- target[is.finite(target)]
          if (length(observed) < 2 || length(unique(observed)) < 2)
            jmvcore::reject(
              "The quantitative variable to characterize must contain at least two distinct values"
            )
        } else {
          observed <- droplevels(as.factor(target[!is.na(target)]))
          if (nlevels(observed) < 2)
            jmvcore::reject(
              "The categorical variable to characterize must contain at least two observed levels"
            )
        }
      },

      .buildData = function() {
        data1 <- data.frame(
          self$data[[self$options$vartochar]],
          check.names = FALSE
        )
        colnames(data1) <- self$options$vartochar

        data2 <- as.data.frame(
          self$data[, self$options$descbyvar, drop = FALSE],
          check.names = FALSE
        )
        colnames(data2) <- self$options$descbyvar

        data.frame(data1, data2, check.names = FALSE)
      }
    )
)
