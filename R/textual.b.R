
# This file is a generated template, your changes will not be overwritten

textualClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "textualClass",
    inherit = textualBase,
    private = list(
    #---------------------------------------------
    #### Init + run functions ----

        .init = function() {
            if (is.null(self$options$words)) {
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
      <b>What you should know before analyzing textual data in jamovi</b>
    </p>

    <div style='
        border-top: 1px solid #CBD8E8;
        margin-bottom: 12px;
    '></div>

    <p style='margin: 0 0 9px 0;'>
      <b>Purpose.</b>
      This analysis characterizes the categories of a qualitative variable
      using textual descriptors. It identifies the words that are
      over-represented or under-represented within each category and provides
      a graphical representation of the relationships between categories and
      words.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Data format.</b>
      Each observation must contain a category in the
      <i>Variable to Characterize</i> field and a list of words in the
      <i>Described by</i> field. Within each textual observation, words must be
      separated by semicolons.
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
        <b>Text preparation</b>
      </p>

      <p style='margin: 0;'>
        Check spelling, capitalization, singular and plural forms before
        running the analysis. Different forms of the same descriptor are
        treated as different words and may artificially divide their
        frequencies.
      </p>

    </div>

    <p style='margin: 0 0 7px 0;'>
      <b>Results.</b>
      MEDA first constructs and displays:
    </p>

    <ul style='margin: 0 0 9px 0; padding-left: 22px;'>

      <li style='margin-bottom: 6px;'>
        a table containing the occurrence and list frequencies of each word;
      </li>

      <li style='margin-bottom: 6px;'>
        the complete categories-by-words contingency table;
      </li>

      <li>
        a Pearson chi-square test evaluating the overall relationship between
        categories and words.
      </li>

    </ul>

    <p style='margin: 0 0 9px 0;'>
      The frequency thresholds are then applied to select the words used in
      the detailed characterization and in the correspondence analysis map.
      The original word-frequency table, complete contingency table, and
      Pearson chi-square test remain based on all words.
    </p>

    <p style='margin: 0 0 7px 0;'>
      <b>Word-frequency thresholds.</b>
    </p>

    <ul style='margin: 0 0 9px 0; padding-left: 22px;'>

      <li style='margin-bottom: 6px;'>
        <i>Lowest frequency words</i> removes words whose total frequency is
        less than or equal to the selected value. For example, a value of
        <code>3</code> retains words occurring more than three times.
      </li>

      <li style='margin-bottom: 6px;'>
        <i>Highest frequency words</i> removes words whose total frequency is
        greater than the selected value. This can be useful for excluding very
        common and insufficiently discriminating words.
      </li>

      <li>
        A value of <code>0</code> deactivates the corresponding frequency
        filter. Therefore, when both frequency thresholds are set to
        <code>0</code>, all words are retained.
      </li>

    </ul>

    <p style='margin: 0 0 9px 0;'>
      <b>Significance threshold.</b>
      This threshold determines which associations between categories and
      words are displayed in the detailed description. A positive v-test
      indicates an over-represented word, whereas a negative v-test indicates
      an under-represented word.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Correspondence analysis.</b>
      The selected words are also used to construct a correspondence analysis
      map. This graph summarizes the main contrasts between categories and
      words. The detailed description table should be used to confirm the
      statistical associations suggested by the map.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Example.</b>
      Open the <b>beard_description</b> dataset. It contains descriptions of
      eight beard pictures provided by several assessors. Select
      <i>Stimuli</i> as the <i>Variable to Characterize</i> and
      <i>Description</i> as the <i>Described by</i> variable.
    </p>

    <p style='margin: 0;'>
      Set <i>Lowest frequency words</i> to <code>3</code>. The results show,
      for example, that <i>hipster</i> is over-represented in the descriptions
      of Beard 3, whereas <i>young</i> is under-represented. These associations
      indicate which words distinguish Beard 3 from the other stimuli.
    </p>

  </div>
  "
          )
        },

      .run = function() {
        if (is.null(self$options$individuals) || is.null(self$options$words))
          return()

        private$.errorCheck()
        data <- private$.buildData()

        show_code <- isTRUE(self$options$showCode)
        self$results$code$setVisible(visible = show_code)
        if (show_code)
          self$results$code$setContent(private$.code())

        res.textual <- private$.textual(data)
        if (is.null(res.textual))
          return()

        contingency <- res.textual[["cont.table"]]
        if (!private$.isValidContingency(contingency, require_two_columns = FALSE))
          jmvcore::reject("Textual analysis did not produce a valid contingency table")

        private$.populateWordsTable(res.textual[["nb.words"]])
        private$.populateTEXTUALTable(contingency)
        private$.populateCHIDEUXTable(contingency)

        filtered <- private$.filterContingency(contingency)
        if (!private$.isValidContingency(filtered, require_two_columns = TRUE)) {
          self$results$dfresgroup$dfres$setVisible(visible = FALSE)
          self$results$plottext$setVisible(visible = FALSE)
          return()
        }

        dfres <- private$.descfreq(filtered)
        tab <- private$.flattenDescfreq(dfres)
        if (private$.hasRows(tab)) {
          self$results$dfresgroup$dfres$setVisible(visible = TRUE)
          private$.populateDFTable(tab)
        } else {
          self$results$dfresgroup$dfres$setVisible(visible = FALSE)
        }

        self$results$plottext$setVisible(visible = TRUE)
        self$results$plottext$setState(filtered)
      },

      .textual = function(data) {
        tryCatch(
          FactoMineR::textual(
            data,
            num.text = 2,
            contingence.by = 1,
            sep.word = ";"
          ),
          error = function(e) {
            jmvcore::reject(paste(
              "Textual analysis failed:",
              conditionMessage(e)
            ))
            NULL
          }
        )
      },

      .code = function() {
        r_literal <- function(value) {
          paste(deparse(value, width.cutoff = 500L), collapse = "\n")
        }

        option_name <- function(value) {
          if (is.null(value) || length(value) == 0L)
            return(character(0))
          value <- as.character(unlist(value, use.names = FALSE))
          value[!is.na(value) & nzchar(value)]
        }

        group_variable <- option_name(self$options$individuals)
        text_variable <- option_name(self$options$words)
        if (length(group_variable) != 1L ||
            length(text_variable) != 1L) {
          return("# Select one grouping variable and one textual variable.")
        }

        proba <- suppressWarnings(as.numeric(self$options$thres)) / 100
        low_frequency <- suppressWarnings(as.numeric(self$options$lowfreq))
        high_frequency <- suppressWarnings(as.numeric(self$options$highfreq))
        if (length(proba) != 1L || !is.finite(proba))
          proba <- 0.05
        if (length(low_frequency) != 1L || !is.finite(low_frequency))
          low_frequency <- 0
        if (length(high_frequency) != 1L || !is.finite(high_frequency))
          high_frequency <- 0

        variables <- c(group_variable, text_variable)
        code <- c(
          "library(FactoMineR)",
          "",
          "# This script can be pasted directly into the jamovi Rj Editor.",
          "# The dataset open in jamovi is available as data.",
          "",
          "# Keep the grouping variable first and the textual variable second.",
          paste0(
            "data_textual <- data[, ", r_literal(variables),
            ", drop = FALSE]"
          ),
          "keep_rows <- !is.na(data_textual[[1]]) &",
          "  !is.na(data_textual[[2]]) &",
          "  nzchar(trimws(as.character(data_textual[[2]])))",
          "data_textual <- data_textual[keep_rows, , drop = FALSE]",
          "data_textual[[1]] <- droplevels(as.factor(data_textual[[1]]))",
          "",
          "# num.text = 2: the second column contains the text.",
          "# contingence.by = 1: words are counted within the first-column categories.",
          "# sep.word = \";\": words are separated by semicolons.",
          "res_textual <- FactoMineR::textual(",
          "  data_textual,",
          "  num.text = 2,",
          "  contingence.by = 1,",
          "  sep.word = \";\"",
          ")",
          "",
          "# Word frequencies and categories-by-words contingency table",
          "res_textual$nb.words",
          "contingency_textual <- res_textual$cont.table",
          "contingency_textual",
          "",
          "# Pearson chi-squared test",
          "stats::chisq.test(contingency_textual)",
          "",
          "# Select words according to their overall frequency.",
          paste0("low_frequency <- ", r_literal(low_frequency)),
          paste0("high_frequency <- ", r_literal(high_frequency)),
          "word_frequencies <- colSums(contingency_textual)",
          "keep_words <- rep(TRUE, length(word_frequencies))",
          "if (low_frequency > 0)",
          "  keep_words <- keep_words & word_frequencies > low_frequency",
          "if (high_frequency > 0)",
          "  keep_words <- keep_words & word_frequencies <= high_frequency",
          "contingency_filtered <- contingency_textual[, keep_words, drop = FALSE]",
          "",
          "if (ncol(contingency_filtered) >= 2) {",
          "  # Characteristic words for each category",
          "  res_descfreq <- FactoMineR::descfreq(",
          "    contingency_filtered,",
          paste0("    proba = ", r_literal(proba)),
          "  )",
          "  res_descfreq",
          "",
          "  # Correspondence analysis of categories and words",
          "  res_ca_textual <- FactoMineR::CA(",
          "    as.data.frame(contingency_filtered),",
          "    graph = FALSE",
          "  )",
          "",
          "  # classic is the safest graph type in the Rj Editor.",
          "  # In RStudio, it can be replaced with graph.type = \"ggplot\".",
          "  FactoMineR::plot.CA(",
          "    res_ca_textual,",
          "    title = \"Representation of the Words and the Categories\",",
          "    graph.type = \"classic\",",
          "    autoLab = \"no\"",
          "  )",
          "}"
        )

        paste(code, collapse = "\n")
      },

      .descfreq = function(contingency) {
        threshold <- self$options$thres / 100
        tryCatch(
          FactoMineR::descfreq(contingency, proba = threshold),
          error = function(e) {
            jmvcore::reject(paste(
              "Description of characteristic words failed:",
              conditionMessage(e)
            ))
            NULL
          }
        )
      },

      .plottextual = function(image, ...) {
        contingency <- image$state
        if (!private$.isValidContingency(contingency, require_two_columns = TRUE))
          return(FALSE)

        tryCatch({
          res.ca <- FactoMineR::CA(
            as.data.frame(contingency),
            graph = FALSE
          )
          plot <- FactoMineR::plot.CA(
            res.ca,
            title = "Representation of the Words and the Categories"
          )
          print(plot)
          TRUE
        }, error = function(e) {
          jmvcore::reject(paste(
            "Textual correspondence plot failed:",
            conditionMessage(e)
          ))
          FALSE
        })
      },

      .filterContingency = function(contingency) {
        contingency <- as.matrix(contingency)
        totals <- colSums(contingency)
        keep <- rep(TRUE, ncol(contingency))

        word_min <- as.numeric(self$options$lowfreq)
        word_max <- as.numeric(self$options$highfreq)
        if (word_min > 0)
          keep <- keep & totals > word_min
        if (word_max > 0)
          keep <- keep & totals <= word_max

        contingency[, keep, drop = FALSE]
      },

      .isValidContingency = function(contingency,
                                     require_two_columns = TRUE) {
        if (is.null(contingency) || is.null(dim(contingency)) ||
            nrow(contingency) < 2)
          return(FALSE)
        if (require_two_columns && ncol(contingency) < 2)
          return(FALSE)
        if (!require_two_columns && ncol(contingency) < 1)
          return(FALSE)

        values <- suppressWarnings(as.numeric(as.matrix(contingency)))
        if (length(values) == 0 || any(!is.finite(values)) || any(values < 0))
          return(FALSE)
        if (sum(values) <= 0)
          return(FALSE)

        matrix_values <- matrix(values, nrow = nrow(contingency))
        all(rowSums(matrix_values) > 0) && all(colSums(matrix_values) > 0)
      },

      .hasRows = function(x) {
        !is.null(x) && !is.null(dim(x)) && nrow(x) > 0
      },

      .flattenDescfreq = function(dfres) {
        if (is.null(dfres) || length(dfres) == 0)
          return(NULL)

        component_names <- names(dfres)
        if (is.null(component_names))
          component_names <- rep("", length(dfres))

        pieces <- lapply(seq_along(dfres), function(i) {
          item <- dfres[[i]]
          if (is.null(item) || is.null(dim(item)) ||
              nrow(item) == 0 || ncol(item) < 6)
            return(NULL)

          item <- as.data.frame(item, stringsAsFactors = FALSE)
          words <- rownames(item)
          if (is.null(words))
            words <- rep("", nrow(item))

          piece <- data.frame(
            Modality = rep(component_names[i], nrow(item)),
            Word = words,
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


      .populateWordsTable = function(nb_words) {
        output <- self$results$tc

        if (is.null(nb_words) || is.null(dim(nb_words)) ||
            nrow(nb_words) == 0 || ncol(nb_words) < 2) {
          output$setVisible(visible = FALSE)
          return(invisible(FALSE))
        }

        nb_words <- as.data.frame(
          nb_words,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )

        word_labels <- rownames(nb_words)
        default_labels <- as.character(seq_len(nrow(nb_words)))
        if (is.null(word_labels) || identical(word_labels, default_labels))
          word_labels <- paste0("Word ", seq_len(nrow(nb_words)))

        column_names <- colnames(nb_words)
        occurrences_column <- match("words", column_names)
        if (is.na(occurrences_column))
          occurrences_column <- 1L

        lists_column <- match("nb.list", column_names)
        if (is.na(lists_column)) {
          remaining_columns <- setdiff(seq_len(ncol(nb_words)), occurrences_column)
          lists_column <- remaining_columns[1]
        }

        occurrences <- suppressWarnings(
          as.numeric(nb_words[[occurrences_column]])
        )
        lists <- suppressWarnings(
          as.numeric(nb_words[[lists_column]])
        )
        keep <- nzchar(word_labels) & is.finite(occurrences) & is.finite(lists)

        if (!any(keep)) {
          output$setVisible(visible = FALSE)
          return(invisible(FALSE))
        }

        output$setVisible(visible = TRUE)
        selected <- which(keep)
        for (i in seq_along(selected)) {
          j <- selected[i]
          output$addRow(
            rowKey = i,
            values = list(
              word = word_labels[j],
              occurrences = as.integer(occurrences[j]),
              lists = as.integer(lists[j])
            )
          )
        }

        invisible(TRUE)
      },


      .populateTEXTUALTable = function(contingency) {
        textual <- self$results$textualgroup$textual
        word_titles <- colnames(contingency)
        if (is.null(word_titles))
          word_titles <- paste0("Word ", seq_len(ncol(contingency)))
        word_ids <- paste0("word_", seq_len(ncol(contingency)))

        textual$addColumn(name = "rownames", title = "", type = "text")
        for (j in seq_along(word_ids))
          textual$addColumn(
            name = word_ids[j],
            title = word_titles[j],
            type = "integer"
          )

        row_titles <- rownames(contingency)
        if (is.null(row_titles))
          row_titles <- paste0("Category ", seq_len(nrow(contingency)))

        for (i in seq_len(nrow(contingency))) {
          row <- list(rownames = row_titles[i])
          for (j in seq_along(word_ids))
            row[[word_ids[j]]] <- contingency[i, j]
          textual$addRow(rowKey = i, values = row)
        }

        total <- list(rownames = "Nb.words")
        for (j in seq_along(word_ids))
          total[[word_ids[j]]] <- sum(contingency[, j])
        textual$addRow(rowKey = nrow(contingency) + 1L, values = total)
      },


      .populateCHIDEUXTable = function(contingency) {
        output <- self$results$chideuxgroup$chideux
        if (!private$.isValidContingency(contingency, require_two_columns = TRUE)) {
          output$setVisible(visible = FALSE)
          return(invisible(FALSE))
        }

        res.chisq <- tryCatch(
          suppressWarnings(stats::chisq.test(contingency)),
          error = function(e) NULL
        )
        if (is.null(res.chisq)) {
          output$setVisible(visible = FALSE)
          return(invisible(FALSE))
        }

        output$setVisible(visible = TRUE)
        output$setRow(rowNo = 1, values = list(
          value = as.numeric(res.chisq$statistic),
          df = as.numeric(res.chisq$parameter),
          pvalue = res.chisq$p.value
        ))
        invisible(TRUE)
      },

      .populateDFTable = function(tab) {
        if (!private$.hasRows(tab) || ncol(tab) < 8)
          return(invisible(FALSE))

        for (i in seq_len(nrow(tab))) {
          row <- list(
            component = as.character(tab[i, 1]),
            word = as.character(tab[i, 2]),
            internper = tab[i, 3],
            globper = tab[i, 4],
            internfreq = tab[i, 5],
            globfreq = tab[i, 6],
            pvaluedfres = tab[i, 7],
            vtest = tab[i, 8]
          )
          self$results$dfresgroup$dfres$addRow(rowKey = i, values = row)
        }
        invisible(TRUE)
      },

      .errorCheck = function() {
        threshold <- suppressWarnings(as.numeric(self$options$thres))
        word_min <- suppressWarnings(as.numeric(self$options$lowfreq))
        word_max <- suppressWarnings(as.numeric(self$options$highfreq))

        if (length(threshold) != 1 || !is.finite(threshold) ||
            threshold < 0 || threshold > 100)
          jmvcore::reject("Significance threshold must be between 0 and 100")
        if (length(word_min) != 1 || !is.finite(word_min) || word_min < 0)
          jmvcore::reject("Lowest word-frequency threshold must be non-negative")
        if (length(word_max) != 1 || !is.finite(word_max) || word_max < 0)
          jmvcore::reject("Highest word-frequency threshold must be non-negative")
        if (word_max > 0 && word_max <= word_min)
          jmvcore::reject(
            "Highest word-frequency threshold must be greater than the lowest threshold"
          )
        if (identical(self$options$individuals, self$options$words))
          jmvcore::reject(
            "The grouping variable and the textual variable must be different"
          )
      },

      .buildData = function() {
        groups <- self$data[[self$options$individuals]]
        words <- self$data[[self$options$words]]
        word_text <- as.character(words)

        keep <- !is.na(groups) & !is.na(words) & nzchar(trimws(word_text))
        if (sum(keep) < 2)
          jmvcore::reject(
            "At least two non-missing textual observations are required"
          )

        groups <- droplevels(as.factor(groups[keep]))
        if (nlevels(groups) < 2)
          jmvcore::reject(
            "The variable to characterize must contain at least two observed levels"
          )

        data <- data.frame(
          groups,
          word_text[keep],
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        colnames(data) <- c(
          self$options$individuals,
          self$options$words
        )
        data
      }

    )
)
