MFAClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "MFAClass",
  inherit = MFABase,
  active = list(
    dataProcessed = function() {
      key <- private$.makeDataProcessingKey()
      if (is.null(private$.dataProcessed) ||
          !identical(private$.dataProcessedKey, key)) {
        private$.dataProcessed <- private$.buildData()
        private$.dataProcessedKey <- key
      }
      private$.dataProcessed
    },
    
    nbclust = function() {
      self$options$nbclust
    },
    
    classifResult = function() {
      key <- private$.makeClassifKey()

      if (!is.null(private$.classifResult) &&
          identical(private$.classifResultKey, key))
        return(private$.classifResult)

      cached <- self$results$classifCache$state
      if (!is.null(cached) && identical(
        attr(cached, "MEDA.cache.key", exact = TRUE), key
      )) {
        private$.classifResult <- cached
        private$.classifResultKey <- key
        return(cached)
      }
      
      value <- private$.getclassifResult()
      if (!is.null(value)) {
        attr(value, "MEDA.cache.key") <- key
        private$.classifResult <- value
        private$.classifResultKey <- key
        self$results$classifCache$setState(value)
      }
      value
    },
    
    MFAResult = function() {
      key <- private$.makeMFAKey()
      required_ncp <- private$.requiredNcp()
      data_key <- private$.dataValueSignature()

      cached <- private$.readMFAFromCache(
        key = key,
        required_ncp = required_ncp,
        data_key = data_key
      )
      if (!is.null(cached))
        return(cached)
      
      value <- private$.getMFAResult()
      if (!is.null(value)) {
        attr(value, "MEDA.cache.key") <- key
        attr(value, "MEDA.data.key") <- data_key
        private$.MFAResult <- value
        private$.MFAResultKey <- key
        self$results$mfaCache$setState(private$.packMFAState(value))
      }
      value
    }
  ),
  
  private = list(
    .dataProcessed = NULL,
    .dataProcessedKey = NULL,
    .MFAResult = NULL,
    .MFAResultKey = NULL,
    .classifResult = NULL,
    .classifResultKey = NULL,
    
    #---------------------------------------------  
    #### Init + run functions ----
    
    .init = function() {
      if (is.null(self$data) || (is.null(self$options$quantivar) && is.null(self$options$qualivar))) {
        if (isTRUE(self$options$tuto)) {
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
      <b>What you should know before running an MFA in jamovi</b>
    </p>

    <div style='
        border-top: 1px solid #CBD8E8;
        margin-bottom: 12px;
    '></div>

    <p style='margin: 0 0 9px 0;'>
      <b>Purpose.</b>
      Multiple Factor Analysis (MFA) is designed to analyze datasets in which
      variables are organized into groups. The definition of these groups is
      therefore a crucial step of the analysis.
    </p>

    <p style='margin: 0 0 9px 0;'>
      <b>Order of the variables.</b>
      Moving variables into the fields on the right creates the dataset used
      by MFA. Variables are ordered exactly as they appear in these fields:
      quantitative variables first, followed by categorical variables. The
      groups must be defined according to this resulting order.
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
        <b>Important setup check</b>
      </p>

      <p style='margin: 0;'>
        Most errors encountered when running MFA come from an inconsistent
        definition of the groups. Carefully check the four fields under
        <i>Definition of the Groups</i>: <i>Groups definition</i>,
        <i>Groups type</i>, <i>Groups name</i>, and
        <i>Supplementary groups</i>. Their values must describe the same
        groups, in the same order.
      </p>

    </div>

    <p style='margin: 0 0 7px 0;'>
      <b>How to define the groups.</b>
      In the <b>wine</b> example, select <i>Ident</i> as the
      <i>Individual Labels</i> variable, then place all quantitative and
      categorical variables in their respective fields.
    </p>

    <ol style='margin: 0 0 9px 0; padding-left: 22px;'>

      <li style='margin-bottom: 7px;'>
        <i>Groups definition</i> is mandatory. Enter the number of variables
        in each group, separated by commas. For example,
        <code>5,3,10,9,2,2</code> defines six groups containing respectively
        5, 3, 10, 9, 2, and 2 variables. The sum of these numbers must equal
        the total number of selected variables. Remove the characters
        <code>Ex:</code> before running the analysis.
      </li>

      <li style='margin-bottom: 7px;'>
        <i>Groups type</i> is mandatory. Enter one type for each group, in the
        same order as in <i>Groups definition</i>. In this example,
        <code>s,s,s,s,s,n</code> indicates five groups of standardized
        quantitative variables and one group of categorical variables.
      </li>

      <li style='margin-bottom: 7px;'>
        <i>Groups name</i> is optional but strongly recommended because clear
        group names make the results easier to interpret. If names are
        provided, enter one name for each group, in the same order.
      </li>

      <li>
        <i>Supplementary groups</i> is optional. Enter the numbers of the
        groups that should not contribute to the construction of the
        dimensions. These are group numbers, not variable numbers. For
        example, <code>5,6</code> makes the fifth and sixth groups
        supplementary.
      </li>

    </ol>

    <p style='margin: 0 0 9px 0;'>
      Supplementary groups do not determine the geometry of the MFA solution,
      but they are projected onto the dimensions and may provide important
      information for interpreting the structure of the individuals.
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
      if (is.null(self$options$quantivar) && is.null(self$options$qualivar))
        return()

      private$.errorCheck()

      # The complete FactoMineR object is cached once, in a lossless packed
      # form. Image states contain only a tiny marker, so the same large MFA
      # object is not stored once per graph.
      res.mfa <- self$MFAResult
      if (is.null(res.mfa))
        return()

      res.classif <- NULL
      need_classif <- isTRUE(self$options$graphclassif) ||
        (isTRUE(self$options$newvar2) &&
         self$results$newvar2$isNotFilled())
      if (need_classif)
        res.classif <- self$classifResult

      .meda_fill_dimdesc_group(self$results$dimdesc, private$.dimdesc())
      private$.printeigenTable()

      marker <- list(ready = TRUE)
      if (is.null(self$results$plotgroup$state))
        self$results$plotgroup$setState(marker)
      if (is.null(self$results$plotaxe$state))
        self$results$plotaxe$setState(marker)
      if (is.null(self$results$plotind$state))
        self$results$plotind$setState(marker)

      has_categories <-
        (!is.null(res.mfa$quali.var) && !is.null(res.mfa$quali.var$coord)) ||
        (!is.null(res.mfa$quali.var.sup) && !is.null(res.mfa$quali.var.sup$coord))
      self$results$plotcat$setVisible(visible = has_categories)
      if (has_categories && is.null(self$results$plotcat$state))
        self$results$plotcat$setState(marker)

      has_quanti <-
        (!is.null(res.mfa$quanti.var) && !is.null(res.mfa$quanti.var$cor)) ||
        (!is.null(res.mfa$quanti.var.sup) && !is.null(res.mfa$quanti.var.sup$cor))
      self$results$plotvar$setVisible(visible = has_quanti)
      if (has_quanti && is.null(self$results$plotvar$state))
        self$results$plotvar$setState(marker)

      if (isTRUE(self$options$graphclassif) &&
          !is.null(res.classif) &&
          is.null(self$results$plotclassif$state))
        self$results$plotclassif$setState(marker)

      if (!is.null(res.classif))
        private$.output2(res.classif)

      private$.output(res.mfa)
      if (isTRUE(self$options$showCode))
        self$results$code$setContent(private$.code())
    },

    #---------------------------------------------
    #### Compute results ----

    .rawSignature = function(value) {
      bytes <- as.integer(serialize(value, connection = NULL, version = 2))
      index <- seq_along(bytes)
      hash1 <- sum(
        (bytes + 1) * ((index %% 65521) + 1)
      ) %% 2147483647
      hash2 <- sum(
        (bytes + 1) * (((index * 17) %% 65519) + 1)
      ) %% 2147483629
      paste(
        length(bytes),
        sprintf("%.0f", hash1),
        sprintf("%.0f", hash2),
        sep = ":"
      )
    },
    
    .dataSignature = function() {
      paste(
        c(
          "quantivar", self$options$quantivar,
          "qualivar", self$options$qualivar,
          "individus", self$options$individus
        ),
        collapse = "\r"
      )
    },

    .dataValueSignature = function() {
      variables <- unique(c(
        self$options$quantivar,
        self$options$qualivar,
        self$options$individus
      ))
      variables <- variables[
        !is.na(variables) & nzchar(as.character(variables))
      ]
      if (length(variables) == 0L || is.null(self$data))
        return(NULL)

      snapshot <- tryCatch({
        columns <- self$data[, variables, drop = FALSE]
        if (ncol(columns) != length(variables))
          return(NULL)
        columns <- data.frame(columns, check.names = FALSE)
        colnames(columns) <- as.character(variables)
        list(
          columns = columns,
          row.names = rownames(columns)
        )
      }, error = function(e) {
        NULL
      })
      if (is.null(snapshot))
        return(NULL)

      private$.rawSignature(snapshot)
    },

    .makeDataProcessingKey = function() {
      data_key <- private$.dataValueSignature()
      if (is.null(data_key))
        data_key <- "unavailable"
      paste(
        private$.dataSignature(),
        "data", data_key,
        sep = "\r"
      )
    },
    
    .requiredNcp = function() {
      candidates <- c(
        self$options$ncp,
        self$options$nFactors,
        self$options$abs,
        self$options$ord
      )
      if (isTRUE(self$options$graphclassif) ||
          (isTRUE(self$options$newvar2) && self$results$newvar2$isNotFilled()))
        candidates <- c(candidates, 5)
      
      candidates <- suppressWarnings(as.numeric(candidates))
      candidates <- candidates[is.finite(candidates) & candidates > 0]
      max(c(3, candidates))
    },

    .makeMFAKey = function() {
      paste(
        c(
          private$.dataSignature(),
          "groupdef", self$options$groupdef,
          "grouptype", self$options$grouptype,
          "groupill", self$options$groupill,
          "groupname", self$options$groupname,
          "ncp", private$.requiredNcp()
        ),
        collapse = "\r"
      )
    },

    .makeClassifKey = function() {
      res.mfa <- private$.readMFAFromCache()
      data_key <- if (is.null(res.mfa)) {
        NULL
      } else {
        attr(res.mfa, "MEDA.data.key", exact = TRUE)
      }
      if (is.null(data_key))
        data_key <- "unavailable"
      paste(
        private$.makeMFAKey(),
        "data", data_key,
        "nbclust", self$options$nbclust,
        sep = "\r"
      )
    },

    .packMFAState = function(res.mfa) {
      if (is.null(res.mfa))
        return(NULL)

      payload <- serialize(res.mfa, connection = NULL, version = 3)
      payload <- memCompress(payload, type = "gzip")
      encoded <- paste0(format(payload), collapse = "")
      paste0("MEDA_MFA_CACHE_V3:", encoded)
    },

    .unpackMFAState = function(state) {
      if (inherits(state, "MFA"))
        return(state)

      if (!is.character(state) || length(state) != 1L ||
          is.na(state))
        return(NULL)

      prefixes <- c(
        "MEDA_MFA_CACHE_V3:",
        "MEDA_MFA_CACHE_V2:",
        "MEDA_MFA_CACHE_V1:"
      )
      prefix <- prefixes[startsWith(state, prefixes)]
      if (length(prefix) != 1L)
        return(NULL)

      payload <- substring(state, nchar(prefix) + 1L)
      n_payload <- nchar(payload, type = "bytes")
      if (n_payload == 0L || n_payload %% 2L != 0L)
        return(NULL)

      tryCatch({
        starts <- seq.int(1L, n_payload, by = 2L)
        bytes <- substring(payload, starts, starts + 1L)
        bytes <- as.raw(strtoi(bytes, base = 16L))
        value <- unserialize(memDecompress(bytes, type = "gzip"))
        if (!inherits(value, "MFA"))
          return(NULL)
        value
      }, error = function(e) NULL)
    },

    .readMFAFromCache = function(key = NULL, required_ncp = NULL,
                                 data_key = NULL) {
      if (is.null(key))
        key <- private$.makeMFAKey()
      if (is.null(required_ncp))
        required_ncp <- private$.requiredNcp()

      is_current <- function(value) {
        if (is.null(value) || !inherits(value, "MFA"))
          return(FALSE)
        if (!identical(
          attr(value, "MEDA.cache.key", exact = TRUE), key
        ))
          return(FALSE)
        if (!is.null(data_key) && !identical(
          attr(value, "MEDA.data.key", exact = TRUE), data_key
        ))
          return(FALSE)

        cached_ncp <- suppressWarnings(as.integer(
          attr(value, "MEDA.ncp.requested", exact = TRUE)
        ))
        if (length(cached_ncp) == 0L || is.na(cached_ncp))
          cached_ncp <- if (is.null(value$eig)) 0L else nrow(value$eig)
        cached_ncp >= required_ncp
      }

      if (is_current(private$.MFAResult))
        return(private$.MFAResult)

      cached <- private$.unpackMFAState(self$results$mfaCache$state)
      if (!is_current(cached))
        return(NULL)

      private$.MFAResult <- cached
      private$.MFAResultKey <- key
      cached
    },

    .getCategoryMFA = function() {
      # Image renderers may run without the source columns in self$data.
      # They must therefore read the validated fitted object and never try to
      # rebuild the dataset or refit the MFA themselves.
      private$.readMFAFromCache()
    },
    
    .computeNbclust = function() {
      self$options$nbclust
    },

    .parseMFAOptions = function(validate_data = FALSE) {
      groupdef <- as.character(self$options$groupdef)
      grouptype <- as.character(self$options$grouptype)
      if (length(groupdef) != 1L || length(grouptype) != 1L ||
          is.na(groupdef) || is.na(grouptype) ||
          groupdef %in% c("", "Ex: 5,3,10,9,2,2") ||
          grouptype %in% c("", "Ex: s,s,s,s,s,n"))
        return(NULL)

      split_csv <- function(value) trimws(strsplit(value, ",", fixed = TRUE)[[1L]])
      group <- suppressWarnings(as.numeric(split_csv(groupdef)))
      if (length(group) == 0L || any(!is.finite(group)) ||
          any(group %% 1 != 0) || any(group < 1))
        jmvcore::reject("Group sizes must be positive integers separated by commas")
      group <- as.integer(group)

      type <- tolower(split_csv(grouptype))
      allowed <- c("s", "c", "n", "m", "f")
      if (length(type) != length(group) || any(!type %in% allowed))
        jmvcore::reject("Group types must match the groups and use only s, c, n, m or f")

      n_variables <- length(c(self$options$quantivar, self$options$qualivar))
      if (sum(group) != n_variables)
        jmvcore::reject("The sum of group sizes must equal the number of selected variables")

      groupill <- as.character(self$options$groupill)
      has_sup <- length(groupill) == 1L && !is.na(groupill) &&
        !groupill %in% c("", "0", "Ex: 5,6")
      num_sup <- NULL
      if (has_sup) {
        num_sup <- suppressWarnings(as.numeric(split_csv(groupill)))
        if (any(!is.finite(num_sup)) || any(num_sup %% 1 != 0) ||
            any(num_sup < 1) || any(num_sup > length(group)) ||
            anyDuplicated(num_sup))
          jmvcore::reject("Supplementary group numbers must be unique integers within the group range")
        num_sup <- as.integer(num_sup)
        if (length(num_sup) == length(group))
          jmvcore::reject("At least one MFA group must remain active")
      }

      groupname <- as.character(self$options$groupname)
      has_names <- length(groupname) == 1L && !is.na(groupname) &&
        !groupname %in% c("", "0", "Ex: olf,vis,olfag,gust,ens,orig")
      name_group <- NULL
      if (has_names) {
        name_group <- split_csv(groupname)
        if (length(name_group) != length(group) || any(!nzchar(name_group)))
          jmvcore::reject("Group names must contain one non-empty name per group")
        if (anyDuplicated(name_group))
          jmvcore::reject("Group names must be unique")
      }

      if (isTRUE(validate_data)) {
        data <- self$dataProcessed
        starts <- c(
          1L,
          if (length(group) > 1L)
            cumsum(group)[seq_len(length(group) - 1L)] + 1L
          else
            integer(0)
        )
        ends <- cumsum(group)
        for (i in seq_along(group)) {
          block <- data[, starts[[i]]:ends[[i]], drop = FALSE]
          numeric_columns <- vapply(block, is.numeric, logical(1))
          if (type[[i]] %in% c("s", "c") && !all(numeric_columns))
            jmvcore::reject(paste0("Group ", i, " must contain only quantitative variables for type '", type[[i]], "'"))
          if (type[[i]] == "n" && any(numeric_columns))
            jmvcore::reject(paste0("Group ", i, " must contain only categorical variables for type 'n'"))
          if (type[[i]] == "m" && (all(numeric_columns) || !any(numeric_columns)))
            jmvcore::reject(paste0("Group ", i, " must contain both quantitative and categorical variables for type 'm'"))
          if (type[[i]] == "f") {
            if (!all(numeric_columns))
              jmvcore::reject(paste0("Frequency group ", i, " must contain only numeric columns"))
            values <- as.matrix(block)
            if (any(!is.finite(values)) || any(values < 0))
              jmvcore::reject(paste0("Frequency group ", i, " must contain finite non-negative values"))
          }
          if (type[[i]] != "f") {
            for (variable in names(block)) {
              values <- block[[variable]]
              if (is.numeric(values)) {
                if (any(is.infinite(values), na.rm = TRUE) ||
                    length(unique(values[is.finite(values)])) < 2L)
                  jmvcore::reject(paste0("Variable '", variable, "' has no usable quantitative variance"))
              } else {
                observed <- values[!is.na(values)]
                if (length(unique(as.character(observed))) < 2L)
                  jmvcore::reject(paste0("Variable '", variable, "' must contain at least two observed categories"))
              }
            }
          }
        }
      }

      list(
        group = group,
        type = type,
        num.group.sup = num_sup,
        name.group = name_group
      )
    },
    
    .getclassifResult = function() {
      parsed <- private$.parseMFAOptions(validate_data = TRUE)
      if (is.null(parsed))
        return(NULL)
      res.mfa <- self$MFAResult
      if (is.null(res.mfa) || is.null(res.mfa$ind$coord))
        return(NULL)
      .meda_hcpc_coordinates(
        res.mfa$ind$coord,
        self$options$ncp,
        self$nbclust,
        "MFA clustering"
      )
    },
    
    .getMFAResult = function() {
      data <- self$dataProcessed
      if (is.null(data)) return(NULL)
      
      parsed <- private$.parseMFAOptions(validate_data = TRUE)
      if (is.null(parsed))
        return(NULL)
      
      ncp_use <- private$.requiredNcp()
      
      r <- tryCatch({
        FactoMineR::MFA(
          data,
          group         = parsed$group,
          type          = parsed$type,
          ncp           = ncp_use,
          num.group.sup = parsed$num.group.sup,
          name.group    = parsed$name.group,
          graph         = FALSE
        )
      }, error = function(e) {
        jmvcore::reject(paste("MFA failed:", e$message))
        return(NULL)
      })
      
      if (!is.null(r))
        attr(r, "MEDA.ncp.requested") <- as.integer(ncp_use)
      r
    },
    
    .code = function() {
      res.mfa <- self$MFAResult
      if (is.null(res.mfa))
        return("# The MFA could not be computed.")

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

      split_option <- function(value, placeholder) {
        if (is.null(value) || length(value) == 0L || is.na(value[1]))
          return(character(0))
        value <- trimws(as.character(value)[1])
        if (!nzchar(value) || value %in% c(placeholder, "0"))
          return(character(0))
        parts <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
        parts[nzchar(parts)]
      }

      group_chr <- split_option(
        self$options$groupdef, "Ex: 5,3,10,9,2,2"
      )
      type_vec <- split_option(
        self$options$grouptype, "Ex: s,s,s,s,s,n"
      )
      group_num <- suppressWarnings(as.numeric(group_chr))
      if (length(group_num) == 0L || any(!is.finite(group_num)) ||
          length(type_vec) == 0L) {
        return("# Define valid MFA groups and group types to generate the R code.")
      }

      num_sup_chr <- split_option(self$options$groupill, "Ex: 5,6")
      num_sup <- suppressWarnings(as.numeric(num_sup_chr))
      if (length(num_sup) == 0L || any(!is.finite(num_sup)))
        num_sup <- NULL

      name_grp <- split_option(
        self$options$groupname, "Ex: olf,vis,olfag,gust,ens,orig"
      )
      if (length(name_grp) == 0L)
        name_grp <- NULL

      variable_names <- names(res.mfa$call$X)
      if (is.null(variable_names) || length(variable_names) == 0L)
        variable_names <- c(self$options$quantivar, self$options$qualivar)
      variable_names <- as.character(variable_names)

      ncp_use <- suppressWarnings(
        as.integer(attr(res.mfa, "MEDA.ncp.requested"))
      )
      if (length(ncp_use) == 0L || is.na(ncp_use) || ncp_use < 1L)
        ncp_use <- as.integer(private$.requiredNcp())

      n_desc <- min(
        suppressWarnings(as.integer(self$options$nFactors)),
        nrow(res.mfa$eig)
      )
      if (!is.finite(n_desc) || n_desc < 1L)
        n_desc <- 1L

      proba <- suppressWarnings(as.numeric(self$options$proba)) / 100
      axes_ok <- private$.getValidAxes(res.mfa)

      code <- c(
        "library(FactoMineR)",
        "",
        "# This script can be pasted directly into the jamovi Rj Editor.",
        "# The dataset open in jamovi is available as data.",
        "",
        "# Keep the MFA variables in the same order as in the MEDA analysis.",
        paste0(
          "data_MFA <- data[, ", r_literal(variable_names),
          ", drop = FALSE]"
        )
      )

      individus <- self$options$individus
      if (!is.null(individus) && length(individus) > 0L &&
          !is.na(individus[1]) && nzchar(as.character(individus)[1])) {
        code <- c(
          code,
          "",
          "# Use the selected identifier as row names.",
          paste0(
            "id_MFA <- as.character(data[[",
            r_literal(as.character(individus)[1]), "]])"
          ),
          "missing_id_MFA <- is.na(id_MFA) | id_MFA == \"\"",
          "id_MFA[missing_id_MFA] <- as.character(seq_len(sum(missing_id_MFA)))",
          "rownames(data_MFA) <- make.unique(id_MFA)"
        )
      }

      code <- c(
        code,
        "",
        "# Multiple Factor Analysis",
        "# group gives the number of variables in each group.",
        "# type: \"s\" = scaled quantitative, \"c\" = unscaled quantitative,",
        "#       \"n\" = qualitative, \"m\" = mixed, \"f\" = frequencies.",
        "# num.group.sup identifies supplementary groups.",
        "# name.group gives optional names to the groups.",
        "# ncp is the number of dimensions retained in the result."
      )

      mfa_arguments <- c(
        "data_MFA",
        paste0("group = ", r_literal(group_num)),
        paste0("type = ", r_literal(as.character(type_vec)))
      )
      if (!is.null(num_sup)) {
        mfa_arguments <- c(
          mfa_arguments,
          paste0("num.group.sup = ", r_literal(num_sup))
        )
      }
      if (!is.null(name_grp)) {
        mfa_arguments <- c(
          mfa_arguments,
          paste0("name.group = ", r_literal(as.character(name_grp)))
        )
      }
      mfa_arguments <- c(
        mfa_arguments,
        paste0("ncp = ", r_literal(as.integer(ncp_use))),
        "graph = FALSE"
      )
      code <- add_call(
        code, "res_mfa", "FactoMineR::MFA", mfa_arguments
      )

      code <- c(
        code,
        "",
        "# Eigenvalues and percentages of explained variance",
        "res_mfa$eig",
        "",
        "# Automatic description of the dimensions",
        "# axes selects the dimensions; proba is the significance threshold.",
        "# For example, proba = 0.05 corresponds to a 5% threshold."
      )
      code <- add_call(
        code,
        "desc_mfa",
        "FactoMineR::dimdesc",
        c(
          "res_mfa",
          paste0(
            "axes = ",
            r_literal(as.integer(seq_len(n_desc)))
          ),
          paste0("proba = ", r_literal(proba))
        )
      )
      code <- c(code, "desc_mfa")

      if (!is.null(axes_ok)) {
        code <- c(
          code,
          "",
          "# Dimensions used in the following maps",
          paste0("axes_mfa <- ", r_literal(as.integer(axes_ok))),
          "",
          "# graph.type = \"classic\" is the safest choice in the Rj Editor.",
          "# In RStudio, it can be replaced with graph.type = \"ggplot\".",
          "# choix selects the map: \"group\", \"axes\", \"ind\" or \"var\".",
          "# autoLab = \"yes\" avoids overlapping labels but may be slow.",
          "",
          "# Groups"
        )
        code <- add_call(
          code, NULL, "FactoMineR::plot.MFA",
          c(
            "res_mfa",
            "choix = \"group\"",
            "axes = axes_mfa",
            "graph.type = \"classic\"",
            "autoLab = \"no\""
          )
        )

        code <- c(code, "", "# Partial axes")
        code <- add_call(
          code, NULL, "FactoMineR::plot.MFA",
          c(
            "res_mfa",
            "choix = \"axes\"",
            "axes = axes_mfa",
            "graph.type = \"classic\"",
            "autoLab = \"no\""
          )
        )

        invisible_ind <- private$.getInvisibleMFA(
          res.mfa, c("quali", "quali.sup")
        )
        individual_arguments <- c(
          "res_mfa",
          "choix = \"ind\"",
          "axes = axes_mfa"
        )
        if (length(invisible_ind) > 0L) {
          individual_arguments <- c(
            individual_arguments,
            paste0("invisible = ", r_literal(invisible_ind))
          )
        }
        individual_arguments <- c(
          individual_arguments,
          "select = \"coord 20\"",
          "unselect = 0",
          "graph.type = \"classic\"",
          "autoLab = \"no\""
        )
        code <- c(
          code,
          "",
          "# Individuals",
          "# select = \"coord 20\" labels the 20 individuals with the largest",
          "# squared coordinates on the two displayed dimensions."
        )
        code <- add_call(
          code, NULL, "FactoMineR::plot.MFA", individual_arguments
        )

        has_quanti <-
          (!is.null(res.mfa$quanti.var) &&
           !is.null(res.mfa$quanti.var$cor)) ||
          (!is.null(res.mfa$quanti.var.sup) &&
           !is.null(res.mfa$quanti.var.sup$cor))
        if (has_quanti) {
          code <- c(
            code,
            "",
            "# Quantitative variables",
            "# Replace \"coord 30\" with \"contrib 30\" or \"cos2 30\"",
            "# to use another native selection criterion."
          )
          code <- add_call(
            code, NULL, "FactoMineR::plot.MFA",
            c(
              "res_mfa",
              "choix = \"var\"",
              "axes = axes_mfa",
              "select = \"coord 30\"",
              "unselect = 0",
              "graph.type = \"classic\"",
              "autoLab = \"no\""
            )
          )
        }

        has_categories <-
          (!is.null(res.mfa$quali.var) &&
           !is.null(res.mfa$quali.var$coord)) ||
          (!is.null(res.mfa$quali.var.sup) &&
           !is.null(res.mfa$quali.var.sup$coord))
        if (has_categories) {
          modality <- as.character(self$options$modality)[1]
          code <- c(
            code,
            "",
            "# Qualitative categories",
            paste0("# MEDA category display rule: ", modality),
            "# In plot.MFA(), select does not filter categories on this map.",
            "# This pure FactoMineR call therefore displays all categories."
          )
          invisible_cat <- private$.getInvisibleMFA(
            res.mfa, c("ind", "ind.sup")
          )
          category_arguments <- c(
            "res_mfa",
            "choix = \"ind\"",
            "axes = axes_mfa"
          )
          if (length(invisible_cat) > 0L) {
            category_arguments <- c(
              category_arguments,
              paste0("invisible = ", r_literal(invisible_cat))
            )
          }
          category_arguments <- c(
            category_arguments,
            "lab.ind = FALSE",
            "lab.var = TRUE",
            "partial = NULL",
            "graph.type = \"classic\"",
            "autoLab = \"no\""
          )
          code <- add_call(
            code, NULL, "FactoMineR::plot.MFA", category_arguments
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
          "# Hierarchical clustering on the retained MFA coordinates",
          "# nb.clust = -1 lets HCPC choose the number of clusters.",
          paste0(
            "coord_hcpc_mfa <- as.data.frame(res_mfa$ind$coord[, ",
            r_literal(as.integer(seq_len(n_classif))),
            ", drop = FALSE])"
          )
        )
        code <- add_call(
          code,
          "res_hcpc",
          "FactoMineR::HCPC",
          c(
            "coord_hcpc_mfa",
            paste0("nb.clust = ", r_literal(nbclust)),
            "graph = FALSE",
            "description = FALSE"
          )
        )
        if (isTRUE(self$options$newvar2)) {
          code <- c(
            code,
            "cluster_mfa <- as.factor(res_hcpc$data.clust[, \"clust\"])"
          )
        }
        if (isTRUE(self$options$graphclassif) && !is.null(axes_ok) &&
            max(axes_ok) <= n_classif) {
          code <- c(code, "", "# Cluster map")
          code <- add_call(
            code, NULL, "FactoMineR::plot.HCPC",
            c(
              "res_hcpc",
              "axes = axes_mfa",
              "choice = \"map\"",
              "draw.tree = FALSE",
              "ind.names = FALSE",
              "new.plot = FALSE",
              "centers.plot = TRUE"
            )
          )
        }
      }

      paste(code, collapse = "\n")
    },

    .dimdesc = function() {
      table <- self$MFAResult
      proba <- self$options$proba / 100
      groupdef_gui  <- self$options$groupdef
      grouptype_gui <- self$options$grouptype
      
      if (groupdef_gui == "Ex: 5,3,10,9,2,2" || grouptype_gui == "Ex: s,s,s,s,s,n")
        return(.meda_empty_dimdesc())
      
      if (is.null(table) || is.null(table$eig))
        return(.meda_empty_dimdesc())
      
      nFactors_out <- min(self$options$nFactors, nrow(table$eig))
      if (is.null(nFactors_out) || nFactors_out < 1)
        return(.meda_empty_dimdesc())
      
      res <- tryCatch(
        FactoMineR::dimdesc(
          table,
          axes = seq_len(nFactors_out),
          proba = proba
        ),
        error = function(e) NULL
      )
      if (is.null(res))
        return(.meda_empty_dimdesc())
      .meda_tidy_dimdesc(res)
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
      if (is.null(res.mfa) || is.null(res.mfa$eig))
        return(NULL)
      .meda_valid_axes(self$options$abs, self$options$ord, nrow(res.mfa$eig))
    },
    
    .getSharedMFA = function() {
      # Plot callbacks do not necessarily receive the source data. Reusing the
      # validated fitted object avoids an impossible rebuild from zero columns.
      private$.readMFAFromCache()
    },
    
    .getInvisibleMFA = function(res.mfa, hide) {
      hide <- unique(hide)
      available <- character(0)
      if (!is.null(res.mfa$ind))
        available <- c(available, "ind")
      if (!is.null(res.mfa$ind.sup))
        available <- c(available, "ind.sup")
      if (!is.null(res.mfa$quali.var))
        available <- c(available, "quali")
      if (!is.null(res.mfa$quali.var.sup))
        available <- c(available, "quali.sup")
      if (!is.null(res.mfa$quanti.var))
        available <- c(available, "quanti")
      if (!is.null(res.mfa$quanti.var.sup))
        available <- c(available, "quanti.sup")
      intersect(hide, available)
    },
    
    .selectCategoryRows = function(categories, specification) {
      if (is.null(categories) || nrow(categories) == 0)
        return(integer(0))
      
      all_rows <- seq_len(nrow(categories))
      if (is.null(specification) || length(specification) == 0 ||
          is.na(specification[1]))
        return(all_rows)
      
      specification <- trimws(as.character(specification[1]))
      if (!nzchar(specification) || tolower(specification) == "all")
        return(all_rows)
      
      requested_names <- trimws(strsplit(specification, ",", fixed = TRUE)[[1]])
      requested_names <- requested_names[nzchar(requested_names)]
      if (length(requested_names) > 0 &&
          any(requested_names %in% categories$label)) {
        matched <- match(requested_names, categories$label, nomatch = 0L)
        return(unique(matched[matched > 0L]))
      }
      
      parts <- strsplit(tolower(specification), "[[:space:]]+")[[1]]
      metric <- parts[1]
      value <- suppressWarnings(as.numeric(parts[length(parts)]))
      if (!is.finite(value) || value <= 0)
        return(all_rows)
      
      top_rows <- function(scores, number) {
        valid <- which(is.finite(scores))
        if (length(valid) == 0)
          return(integer(0))
        number <- max(1L, min(length(valid), as.integer(round(number))))
        valid[head(order(scores[valid], decreasing = TRUE), number)]
      }
      
      selected <- all_rows
      if (metric == "cos2") {
        selected <- if (value >= 1) {
          top_rows(categories$cos2, value)
        } else {
          which(is.finite(categories$cos2) & categories$cos2 > value)
        }
      } else if (metric == "contrib") {
        selected <- top_rows(categories$contrib, value)
      } else if (metric == "coord") {
        selected <- top_rows(categories$coord, value)
      } else if (metric %in% c("v.test", "vtest")) {
        selected <- which(is.finite(categories$vtest) & categories$vtest > value)
      }
      
      unique(selected)
    },
    
    .plotindividus = function(image, ...) {
      res.mfa <- private$.getSharedMFA()
      if (is.null(res.mfa))
        return(FALSE)
      axes_ok <- private$.getValidAxes(res.mfa)
      if (is.null(axes_ok))
        return(FALSE)
      
      invisible_vec <- private$.getInvisibleMFA(
        res.mfa, c("quali", "quali.sup")
      )
      
      tryCatch({
        args <- list(
          x = res.mfa,
          axes = axes_ok,
          choix = "ind",
          title = "Representation of the Individuals",
          graph.type = "classic",
          autoLab = "no",
          select = "coord 20",
          unselect = 0
        )
        if (length(invisible_vec) > 0)
          args$invisible <- invisible_vec
        do.call(FactoMineR::plot.MFA, args)
        TRUE
      }, error = function(e) {
        jmvcore::reject(paste("Plot of individuals failed:", e$message))
        FALSE
      })
    },
    
    .plotcategory = function(image, ...) {
      res.mfa <- private$.getCategoryMFA()
      if (is.null(res.mfa))
        return(FALSE)

      # Character extraction with `[[` is deliberately exact. With `$`, R
      # partially matches `quali.var` to `quali.var.sup` when there is no
      # active qualitative group. That used to duplicate the supplementary
      # component as an artificial active component and made plot.MFA() fail
      # because its call metadata contained no corresponding active group.
      quali_active <- res.mfa[["quali.var"]]
      quali_supplementary <- res.mfa[["quali.var.sup"]]

      if ((is.null(quali_active) || is.null(quali_active$coord)) &&
          (is.null(quali_supplementary) ||
           is.null(quali_supplementary$coord)))
        return(FALSE)
      
      axes_ok <- private$.getValidAxes(res.mfa)
      if (is.null(axes_ok))
        return(FALSE)
      
      collect_categories <- function(component, supplementary) {
        if (is.null(component) || is.null(component$coord) ||
            nrow(component$coord) == 0)
          return(NULL)

        labels <- rownames(component$coord)
        if (is.null(labels))
          labels <- paste0("Category ", seq_len(nrow(component$coord)))

        metric_sum <- function(metric) {
          if (is.null(metric) || ncol(metric) < max(axes_ok))
            return(rep(NA_real_, nrow(component$coord)))
          rowSums(metric[, axes_ok, drop = FALSE], na.rm = FALSE)
        }
        contribution_score <- function(metric) {
          if (is.null(metric) || ncol(metric) < max(axes_ok))
            return(rep(NA_real_, nrow(component$coord)))
          as.numeric(
            metric[, axes_ok[1], drop = TRUE] * res.mfa$eig[axes_ok[1], 1] +
              metric[, axes_ok[2], drop = TRUE] * res.mfa$eig[axes_ok[2], 1]
          )
        }
        vtest_score <- function(metric) {
          if (is.null(metric) || ncol(metric) < max(axes_ok))
            return(rep(NA_real_, nrow(component$coord)))
          apply(abs(metric[, axes_ok, drop = FALSE]), 1, max, na.rm = FALSE)
        }

        data.frame(
          x = as.numeric(component$coord[, axes_ok[1]]),
          y = as.numeric(component$coord[, axes_ok[2]]),
          label = labels,
          cos2 = metric_sum(component$cos2),
          contrib = contribution_score(component$contrib),
          coord = rowSums(component$coord[, axes_ok, drop = FALSE]^2,
                          na.rm = FALSE),
          vtest = vtest_score(component$v.test),
          component_index = seq_len(nrow(component$coord)),
          supplementary = supplementary,
          stringsAsFactors = FALSE
        )
      }

      categories <- rbind(
        collect_categories(quali_active, FALSE),
        collect_categories(quali_supplementary, TRUE)
      )
      if (is.null(categories) || nrow(categories) == 0)
        return(FALSE)

      categories <- categories[
        is.finite(categories$x) & is.finite(categories$y), , drop = FALSE
      ]
      selected <- private$.selectCategoryRows(
        categories, self$options$modality
      )
      if (length(selected) == 0) {
        jmvcore::reject("No category matches the requested display rule")
        return(FALSE)
      }

      # plot.MFA() does not apply its `select` argument to qualitative
      # categories in an MFA individual map. Keep the complete FactoMineR
      # result (and therefore every point and active/supplementary status), but
      # blank the unselected row labels in a local plotting copy. plot.MFA()
      # remains the sole renderer of points, symbols, colours and labels.
      res.plot <- res.mfa
      n_active <- if (is.null(quali_active) ||
                      is.null(quali_active$coord)) {
        0L
      } else {
        nrow(quali_active$coord)
      }
      n_supplementary <- if (is.null(quali_supplementary) ||
                             is.null(quali_supplementary$coord)) {
        0L
      } else {
        nrow(quali_supplementary$coord)
      }

      selected_categories <- categories[selected, , drop = FALSE]
      keep_active <- selected_categories$component_index[
        !selected_categories$supplementary
      ]
      keep_supplementary <- selected_categories$component_index[
        selected_categories$supplementary
      ]

      mask_labels <- function(component, keep) {
        if (is.null(component) || is.null(component$coord) ||
            nrow(component$coord) == 0)
          return(component)

        labels <- rownames(component$coord)
        if (is.null(labels))
          labels <- paste0("Category ", seq_len(nrow(component$coord)))
        hide <- setdiff(seq_len(nrow(component$coord)), keep)
        labels[hide] <- ""
        coord <- as.matrix(component$coord)
        rownames(coord) <- labels
        component$coord <- coord
        component
      }

      if (n_active > 0)
        res.plot[["quali.var"]] <- mask_labels(
          res.plot[["quali.var"]], keep_active
        )
      if (n_supplementary > 0)
        res.plot[["quali.var.sup"]] <- mask_labels(
          res.plot[["quali.var.sup"]], keep_supplementary
        )

      invisible_vec <- private$.getInvisibleMFA(
        res.plot, c("ind", "ind.sup")
      )
      
      make_args <- function(graph_type) {
        args <- list(
          x = res.plot,
          axes = axes_ok,
          choix = "ind",
          title = "Representation of the Categories",
          graph.type = graph_type,
          autoLab = "no",
          lab.ind = FALSE,
          lab.var = TRUE,
          partial = NULL,
          new.plot = FALSE
        )
        if (length(invisible_vec) > 0)
          args$invisible <- invisible_vec
        args
      }
      
      classic_error <- NULL
      classic_ok <- tryCatch({
        do.call(FactoMineR::plot.MFA, make_args("classic"))
        TRUE
      }, error = function(e) {
        classic_error <<- conditionMessage(e)
        FALSE
      })
      
      if (isTRUE(classic_ok))
        return(TRUE)
      
      ggplot_error <- NULL
      ggplot_ok <- tryCatch({
        plot <- do.call(FactoMineR::plot.MFA, make_args("ggplot"))
        if (!is.null(plot))
          print(plot)
        TRUE
      }, error = function(e) {
        ggplot_error <<- conditionMessage(e)
        FALSE
      })
      
      if (isTRUE(ggplot_ok))
        return(TRUE)

      cache_status <- if (identical(
        attr(res.plot, "MEDA.cache.key", exact = TRUE),
        private$.makeMFAKey()
      )) {
        "current"
      } else {
        "mismatch"
      }
      metadata_status <- paste0(
        "cache=", cache_status,
        ", group.mod=", typeof(res.plot$call$group.mod),
        ", nature.group=", typeof(res.plot$call$nature.group),
        ", quali.active.exact=", !is.null(res.plot[["quali.var"]]),
        ", quali.sup.exact=", !is.null(res.plot[["quali.var.sup"]])
      )
      
      jmvcore::reject(paste0(
        "Plot of categories failed. Classic renderer: ",
        classic_error,
        "; ggplot renderer: ",
        ggplot_error,
        ". MEDA MFA v5 diagnostics: ",
        metadata_status
      ))
      FALSE
    },
    
    .plotvariables = function(image, ...) {
      res.mfa <- private$.getSharedMFA()
      if (is.null(res.mfa))
        return(FALSE)
      axes_ok <- private$.getValidAxes(res.mfa)
      if (is.null(axes_ok))
        return(FALSE)
      
      tryCatch({
        FactoMineR::plot.MFA(
          res.mfa,
          choix = "var",
          axes = axes_ok,
          title = "Representation of the Variables",
          graph.type = "classic",
          autoLab = "no",
          select = "coord 30",
          unselect = 0
        )
        TRUE
      }, error = function(e) {
        jmvcore::reject(paste("Plot of variables failed:", e$message))
        FALSE
      })
    },
    
    .plotgroups = function(image, ...) {
      res.mfa <- private$.getSharedMFA()
      if (is.null(res.mfa))
        return(FALSE)
      axes_ok <- private$.getValidAxes(res.mfa)
      if (is.null(axes_ok))
        return(FALSE)
      
      tryCatch({
        FactoMineR::plot.MFA(
          res.mfa,
          choix = "group",
          axes = axes_ok,
          title = "Representation of the Groups",
          graph.type = "classic",
          autoLab = "no"
        )
        TRUE
      }, error = function(e) {
        jmvcore::reject(paste("Plot of groups failed:", e$message))
        FALSE
      })
    },
    
    .plotaxes = function(image, ...) {
      res.mfa <- private$.getSharedMFA()
      if (is.null(res.mfa))
        return(FALSE)
      axes_ok <- private$.getValidAxes(res.mfa)
      if (is.null(axes_ok))
        return(FALSE)
      
      tryCatch({
        FactoMineR::plot.MFA(
          res.mfa,
          choix = "axes",
          axes = axes_ok,
          ncp = 2,
          title = "Representation of the Partial Axes",
          graph.type = "classic",
          autoLab = "no"
        )
        TRUE
      }, error = function(e) {
        jmvcore::reject(paste("Plot of partial axes failed:", e$message))
        FALSE
      })
    },
    
    .plotclassif = function(image, ...) {
      groupdef_gui  <- self$options$groupdef
      grouptype_gui <- self$options$grouptype
      
      if (groupdef_gui %in% c(NULL, "", "Ex: 5,3,10,9,2,2") ||
          grouptype_gui %in% c(NULL, "", "Ex: s,s,s,s,s,n"))
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
          axes        = axes_ok,
          choice      = "map",
          draw.tree   = FALSE,
          ind.names   = FALSE,
          new.plot    = FALSE,
          centers.plot = TRUE,
          title       = "Representation of the Individuals According to Clusters"
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
      parsed <- private$.parseMFAOptions(validate_data = TRUE)
      if (is.null(parsed))
        return()
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
      proba <- suppressWarnings(as.numeric(self$options$proba))
      if (length(proba) != 1L || !is.finite(proba) ||
          proba < 0 || proba > 100)
        jmvcore::reject("The significance threshold must be between 0 and 100")
    },
    
    .output = function(res.mfa) {
      output <- self$results$newvar
      if (!isTRUE(self$options$newvar) || !output$isNotFilled())
        return()
      
      nFactors_out <- min(
        self$options$ncp,
        nrow(res.mfa$eig),
        ncol(res.mfa$ind$coord)
      )
      if (nFactors_out < 1)
        return()
      
      keys <- seq_len(nFactors_out)
      output$set(
        keys = keys,
        titles = paste("Dim.", keys),
        descriptions = rep("MFA component", nFactors_out),
        measureTypes = rep("continuous", nFactors_out)
      )
      
      for (i in keys)
        output$setValues(index = i, as.numeric(res.mfa$ind$coord[, i]))
      
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
      
      scores <- as.factor(res.classif$data.clust[, ncol(res.classif$data.clust)])
      output$setValues(index = 1, scores)
      row_nums <- attr(self$dataProcessed, "jamovi_row_nums")
      if (is.null(row_nums))
        row_nums <- rownames(self$dataProcessed)
      output$setRowNums(row_nums)
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
