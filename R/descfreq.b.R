
# This file is a generated template, your changes will not be overwritten

descfreqClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "descfreqClass",
    inherit = descfreqBase,
    private = list(

    #---------------------------------------------
    #### Init + run functions ----

        .init = function() {
            if (is.null(self$options$columns)) {
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
      <b>What you should know before analyzing a contingency table in jamovi</b>
    </p>

    <div style='
        border-top: 1px solid #CBD8E8;
        margin-bottom: 12px;
    '></div>

    <p style='margin: 0 0 9px 0;'>
      <b>Purpose.</b>
      This analysis characterizes the rows and columns of a contingency table.
      A contingency table contains counts at the intersections of row and
      column categories. Each cell indicates the number of observations
      associated with both the corresponding row and column.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Statistical principle.</b>
      Each row is characterized by the columns, and each column is
      characterized by the rows. For every row-column combination, a test
      based on the hypergeometric distribution compares the observed frequency
      with the frequency expected from the overall distribution.
    </p>

    <p style='margin: 0 0 9px 0;'>
      A significant positive v-test indicates that the combination is
      over-represented, whereas a significant negative v-test indicates that
      it is under-represented. The internal percentages can be compared with
      the corresponding global percentages to understand the direction and
      magnitude of the difference.
    </p>

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
        <b>Data requirements</b>
      </p>

      <p style='margin: 0;'>
        The <i>Rows</i> field must contain one unique and non-missing label for
        each row of the contingency table. The <i>Columns</i> field must
        contain at least two numeric count variables. Counts must be finite and
        non-negative, and every row and column must have a positive total.
      </p>

    </div>

    <p style='margin: 0 0 9px 0;'>
      <b>Significance threshold.</b>
      Only row-column associations whose p-value is below the selected
      threshold are displayed. Because many combinations may be examined,
      these results are primarily intended for exploratory characterization.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Example.</b>
      Open the <b>music</b> dataset. Place <i>Occupation</i> in the
      <i>Rows</i> field. Its values provide the labels of the occupation
      categories represented by the rows. Place all the music genres in the
      <i>Columns</i> field; these numeric variables contain the corresponding
      frequencies.
    </p>

    <p style='margin: 0 0 9px 0;'>
      Two tables are produced. The first describes rows by columns. Compared
      with the overall distribution of music genres, French music is
      over-represented among farmers, whereas jazz is under-represented.
    </p>

    <p style='margin: 0;'>
      The second table describes columns by rows by applying the same analysis
      to the transposed contingency table. It shows that jazz is
      over-represented among executives and under-represented among farmers.
    </p>

  </div>
  "
          )
        },

        .run = function() {
            if (is.null(self$options$columns) ||
                is.null(self$options$rows) ||
                length(self$options$columns) == 0)
                return()

            private$.errorCheck()
            data <- private$.buildData()

            res.descfreqrow <- private$.descfreq(
                data,
                "Description of rows failed:"
            )
            res.descfreqcol <- private$.descfreq(
                t(data),
                "Description of columns failed:"
            )

            private$.printTables(
                res.descfreqrow,
                self$results$descoftablerow
            )
            private$.printTables(
                res.descfreqcol,
                self$results$descoftablecol
            )

            show_code <- isTRUE(self$options$showCode)
            self$results$code$setVisible(visible = show_code)
            if (show_code)
                self$results$code$setContent(private$.code())
        },

        ### Compute results ----
        .descfreq = function(data, error_prefix) {
            threshold <- self$options$threshold / 100
            tryCatch(
                FactoMineR::descfreq(data, proba = threshold),
                error = function(e) {
                    jmvcore::reject(paste(error_prefix, conditionMessage(e)))
                    NULL
                }
            )
        },

        .code = function() {
            r_literal <- function(value) {
                paste(deparse(value, width.cutoff = 500L), collapse = "\n")
            }

            row_variable <- as.character(self$options$rows)
            columns <- as.character(self$options$columns)
            if (length(row_variable) != 1L || !nzchar(row_variable) ||
                length(columns) < 2L)
                return("# Select one row-label variable and at least two numeric columns.")

            proba <- suppressWarnings(as.numeric(self$options$threshold)) / 100
            if (length(proba) != 1L || !is.finite(proba))
                proba <- 0.05

            code <- c(
                "library(FactoMineR)",
                "",
                "# This script can be pasted directly into the jamovi Rj Editor.",
                "# The dataset open in jamovi is available as data.",
                "",
                "# Build the contingency table and use the selected variable as row names.",
                paste0(
                    "table_descfreq <- data[, ", r_literal(columns),
                    ", drop = FALSE]"
                ),
                paste0(
                    "rownames(table_descfreq) <- as.character(data[[",
                    r_literal(row_variable), "]])"
                ),
                "",
                paste0(
                    "# proba = ", r_literal(proba),
                    " keeps associations whose p-value is below this threshold."
                ),
                "# Description of rows by columns",
                "res_descfreq_rows <- FactoMineR::descfreq(",
                "  table_descfreq,",
                paste0("  proba = ", r_literal(proba)),
                ")",
                "res_descfreq_rows",
                "",
                "# Transposing the table describes columns by rows.",
                "res_descfreq_columns <- FactoMineR::descfreq(",
                "  t(table_descfreq),",
                paste0("  proba = ", r_literal(proba)),
                ")",
                "res_descfreq_columns"
            )

            paste(code, collapse = "\n")
        },

        .printTables = function(table, desctable) {
            if (is.null(table) || length(table) == 0)
                return(invisible(FALSE))

            component_names <- names(table)
            if (is.null(component_names))
                component_names <- rep("", length(table))

            row_key <- 0L
            for (i in seq_along(table)) {
                component <- table[[i]]
                if (is.null(component) || is.null(dim(component)) ||
                    nrow(component) == 0 || ncol(component) < 6)
                    next

                row_labels <- rownames(component)
                if (is.null(row_labels))
                    row_labels <- rep("", nrow(component))

                for (j in seq_len(nrow(component))) {
                    row_key <- row_key + 1L
                    row <- list(
                        mod = component_names[i],
                        rowcol = row_labels[j],
                        intern = component[j, 1],
                        glob = component[j, 2],
                        intfreq = component[j, 3],
                        globfreq = component[j, 4],
                        pvalue = component[j, 5],
                        vtest = component[j, 6]
                    )
                    desctable$addRow(rowKey = row_key, values = row)
                }
            }

            invisible(row_key > 0L)
        },

        ### Helpers functions ----
        .errorCheck = function() {
            if (length(self$options$columns) < 2)
                jmvcore::reject("At least two columns are required")

            threshold <- suppressWarnings(as.numeric(self$options$threshold))
            if (length(threshold) != 1 || !is.finite(threshold) ||
                threshold < 0 || threshold > 100)
                jmvcore::reject(
                    "Significance threshold must be between 0 and 100"
                )

            labels <- as.character(self$data[[self$options$rows]])
            if (length(labels) != nrow(self$data) ||
                any(is.na(labels)) || any(!nzchar(labels)))
                jmvcore::reject("Row labels must be present for every row")
            if (anyDuplicated(labels))
                jmvcore::reject("Row labels must be unique")
        },

        .buildData = function() {
            data <- as.data.frame(
                self$data[, self$options$columns, drop = FALSE],
                check.names = FALSE
            )
            colnames(data) <- self$options$columns

            values <- as.matrix(data)
            storage.mode(values) <- "double"
            if (any(!is.finite(values)))
                jmvcore::reject(
                    "The contingency table cannot contain missing or non-finite values"
                )
            if (any(values < 0))
                jmvcore::reject(
                    "The contingency table cannot contain negative values"
                )
            if (sum(values) <= 0)
                jmvcore::reject("The contingency table must contain positive counts")
            if (any(rowSums(values) <= 0))
                jmvcore::reject(
                    "Every row of the contingency table must have a positive total"
                )
            if (any(colSums(values) <= 0))
                jmvcore::reject(
                    "Every column of the contingency table must have a positive total"
                )

            data[,] <- values
            rownames(data) <- as.character(self$data[[self$options$rows]])
            data
        })
)
