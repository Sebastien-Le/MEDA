
# This file is automatically generated, you probably don't want to edit this

CAOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "CAOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            activecol = NULL,
            illustrativecol = NULL,
            indiv = NULL,
            tuto = TRUE,
            nbfact = 2,
            proba = 5,
            abs = 1,
            ord = 2,
            limcoscol = 0,
            limcosrow = 0,
            ellipsecol = FALSE,
            ellipserow = FALSE,
            addillucol = TRUE,
            coordcol = FALSE,
            contribcol = FALSE,
            coscol = FALSE,
            coordrow = FALSE,
            contribrow = FALSE,
            cosrow = FALSE,
            ncp = 5,
            graphclassif = FALSE,
            nbclust = -1, ...) {

            super$initialize(
                package="MEDA",
                name="CA",
                requiresData=TRUE,
                ...)

            private$..activecol <- jmvcore::OptionVariables$new(
                "activecol",
                activecol,
                suggested=list(
                    "continuous"),
                permitted=list(
                    "numeric"))
            private$..illustrativecol <- jmvcore::OptionVariables$new(
                "illustrativecol",
                illustrativecol,
                suggested=list(
                    "continuous"),
                permitted=list(
                    "numeric"))
            private$..indiv <- jmvcore::OptionVariable$new(
                "indiv",
                indiv,
                suggested=list(
                    "nominal"),
                permitted=list(
                    "factor"))
            private$..tuto <- jmvcore::OptionBool$new(
                "tuto",
                tuto,
                default=TRUE)
            private$..nbfact <- jmvcore::OptionInteger$new(
                "nbfact",
                nbfact,
                default=2)
            private$..proba <- jmvcore::OptionNumber$new(
                "proba",
                proba,
                default=5)
            private$..abs <- jmvcore::OptionInteger$new(
                "abs",
                abs,
                default=1)
            private$..ord <- jmvcore::OptionInteger$new(
                "ord",
                ord,
                default=2)
            private$..limcoscol <- jmvcore::OptionNumber$new(
                "limcoscol",
                limcoscol,
                default=0)
            private$..limcosrow <- jmvcore::OptionNumber$new(
                "limcosrow",
                limcosrow,
                default=0)
            private$..ellipsecol <- jmvcore::OptionBool$new(
                "ellipsecol",
                ellipsecol,
                default=FALSE)
            private$..ellipserow <- jmvcore::OptionBool$new(
                "ellipserow",
                ellipserow,
                default=FALSE)
            private$..addillucol <- jmvcore::OptionBool$new(
                "addillucol",
                addillucol,
                default=TRUE)
            private$..coordcol <- jmvcore::OptionBool$new(
                "coordcol",
                coordcol,
                default=FALSE)
            private$..contribcol <- jmvcore::OptionBool$new(
                "contribcol",
                contribcol,
                default=FALSE)
            private$..coscol <- jmvcore::OptionBool$new(
                "coscol",
                coscol,
                default=FALSE)
            private$..coordrow <- jmvcore::OptionBool$new(
                "coordrow",
                coordrow,
                default=FALSE)
            private$..contribrow <- jmvcore::OptionBool$new(
                "contribrow",
                contribrow,
                default=FALSE)
            private$..cosrow <- jmvcore::OptionBool$new(
                "cosrow",
                cosrow,
                default=FALSE)
            private$..ncp <- jmvcore::OptionInteger$new(
                "ncp",
                ncp,
                default=5)
            private$..graphclassif <- jmvcore::OptionBool$new(
                "graphclassif",
                graphclassif,
                default=FALSE)
            private$..newvar <- jmvcore::OptionOutput$new(
                "newvar")
            private$..nbclust <- jmvcore::OptionInteger$new(
                "nbclust",
                nbclust,
                default=-1)
            private$..newvar2 <- jmvcore::OptionOutput$new(
                "newvar2")

            self$.addOption(private$..activecol)
            self$.addOption(private$..illustrativecol)
            self$.addOption(private$..indiv)
            self$.addOption(private$..tuto)
            self$.addOption(private$..nbfact)
            self$.addOption(private$..proba)
            self$.addOption(private$..abs)
            self$.addOption(private$..ord)
            self$.addOption(private$..limcoscol)
            self$.addOption(private$..limcosrow)
            self$.addOption(private$..ellipsecol)
            self$.addOption(private$..ellipserow)
            self$.addOption(private$..addillucol)
            self$.addOption(private$..coordcol)
            self$.addOption(private$..contribcol)
            self$.addOption(private$..coscol)
            self$.addOption(private$..coordrow)
            self$.addOption(private$..contribrow)
            self$.addOption(private$..cosrow)
            self$.addOption(private$..ncp)
            self$.addOption(private$..graphclassif)
            self$.addOption(private$..newvar)
            self$.addOption(private$..nbclust)
            self$.addOption(private$..newvar2)
        }),
    active = list(
        activecol = function() private$..activecol$value,
        illustrativecol = function() private$..illustrativecol$value,
        indiv = function() private$..indiv$value,
        tuto = function() private$..tuto$value,
        nbfact = function() private$..nbfact$value,
        proba = function() private$..proba$value,
        abs = function() private$..abs$value,
        ord = function() private$..ord$value,
        limcoscol = function() private$..limcoscol$value,
        limcosrow = function() private$..limcosrow$value,
        ellipsecol = function() private$..ellipsecol$value,
        ellipserow = function() private$..ellipserow$value,
        addillucol = function() private$..addillucol$value,
        coordcol = function() private$..coordcol$value,
        contribcol = function() private$..contribcol$value,
        coscol = function() private$..coscol$value,
        coordrow = function() private$..coordrow$value,
        contribrow = function() private$..contribrow$value,
        cosrow = function() private$..cosrow$value,
        ncp = function() private$..ncp$value,
        graphclassif = function() private$..graphclassif$value,
        newvar = function() private$..newvar$value,
        nbclust = function() private$..nbclust$value,
        newvar2 = function() private$..newvar2$value),
    private = list(
        ..activecol = NA,
        ..illustrativecol = NA,
        ..indiv = NA,
        ..tuto = NA,
        ..nbfact = NA,
        ..proba = NA,
        ..abs = NA,
        ..ord = NA,
        ..limcoscol = NA,
        ..limcosrow = NA,
        ..ellipsecol = NA,
        ..ellipserow = NA,
        ..addillucol = NA,
        ..coordcol = NA,
        ..contribcol = NA,
        ..coscol = NA,
        ..coordrow = NA,
        ..contribrow = NA,
        ..cosrow = NA,
        ..ncp = NA,
        ..graphclassif = NA,
        ..newvar = NA,
        ..nbclust = NA,
        ..newvar2 = NA)
)

CAResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "CAResults",
    inherit = jmvcore::Group,
    active = list(
        instructions = function() private$.items[["instructions"]],
        plotirow = function() private$.items[["plotirow"]],
        ploticol = function() private$.items[["ploticol"]],
        plotell = function() private$.items[["plotell"]],
        xsqgroup = function() private$.items[["xsqgroup"]],
        eigengroup = function() private$.items[["eigengroup"]],
        descofdimgroup = function() private$.items[["descofdimgroup"]],
        code = function() private$.items[["code"]],
        rowgroup = function() private$.items[["rowgroup"]],
        colgroup = function() private$.items[["colgroup"]],
        plotclassif = function() private$.items[["plotclassif"]],
        newvar = function() private$.items[["newvar"]],
        newvar2 = function() private$.items[["newvar2"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Results of the Correspondence Analysis",
                refs=list(
                    "factominer",
                    "explo"))
            self$add(jmvcore::Html$new(
                options=options,
                name="instructions",
                title="Instructions",
                visible="(tuto)"))
            self$add(jmvcore::Image$new(
                options=options,
                name="plotirow",
                title="Representation of the Rows",
                width=600,
                height=500,
                renderFun=".plotrow"))
            self$add(jmvcore::Image$new(
                options=options,
                name="ploticol",
                title="Representation of the Columns",
                width=600,
                height=500,
                renderFun=".plotcol"))
            self$add(jmvcore::Image$new(
                options=options,
                name="plotell",
                title="Superimposed Representation with Ellipses",
                width=600,
                height=500,
                renderFun=".plotell"))
            self$add(R6::R6Class(
                inherit = jmvcore::Group,
                active = list(
                    xsq = function() private$.items[["xsq"]]),
                private = list(),
                public=list(
                    initialize=function(options) {
                        super$initialize(
                            options=options,
                            name="xsqgroup",
                            title="Chi-Squared Test")
                        self$add(jmvcore::Table$new(
                            options=options,
                            name="xsq",
                            title="Pearson's Chi-Squared Test",
                            rows=1,
                            columns=list(
                                list(
                                    `name`="xsquared", 
                                    `title`="X-squared", 
                                    `type`="number"),
                                list(
                                    `name`="df", 
                                    `title`="df", 
                                    `type`="integer"),
                                list(
                                    `name`="pvxsq", 
                                    `title`="p", 
                                    `type`="number", 
                                    `format`="zto,pvalue"))))}))$new(options=options))
            self$add(R6::R6Class(
                inherit = jmvcore::Group,
                active = list(
                    eigen = function() private$.items[["eigen"]]),
                private = list(),
                public=list(
                    initialize=function(options) {
                        super$initialize(
                            options=options,
                            name="eigengroup",
                            title="Eigenvalue Decomposition")
                        self$add(jmvcore::Table$new(
                            options=options,
                            name="eigen",
                            title="Eigenvalue and (Cumulative) Percentage of Variance",
                            columns=list(
                                list(
                                    `name`="component", 
                                    `title`="", 
                                    `type`="text"),
                                list(
                                    `name`="eigenvalue", 
                                    `title`="Eigenvalue", 
                                    `type`="number"),
                                list(
                                    `name`="purcent", 
                                    `title`="% of the variance", 
                                    `type`="number"),
                                list(
                                    `name`="purcentcum", 
                                    `title`="Cumulative %", 
                                    `type`="number"))))}))$new(options=options))
            self$add(R6::R6Class(
                inherit = jmvcore::Group,
                active = list(
                    descofdim = function() private$.items[["descofdim"]]),
                private = list(),
                public=list(
                    initialize=function(options) {
                        super$initialize(
                            options=options,
                            name="descofdimgroup",
                            title="Automatic Description of the Dimensions")
                        self$add(jmvcore::Table$new(
                            options=options,
                            name="descofdim",
                            title="",
                            columns=list(
                                list(
                                    `name`="dim", 
                                    `title`="", 
                                    `type`="text", 
                                    `combineBelow`=TRUE),
                                list(
                                    `name`="rowcol", 
                                    `title`="", 
                                    `type`="text", 
                                    `combineBelow`=TRUE),
                                list(
                                    `name`="cat", 
                                    `title`="Category", 
                                    `type`="text"),
                                list(
                                    `name`="coord", 
                                    `title`="Coordinate", 
                                    `type`="number"))))}))$new(options=options))
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="code",
                title="R code"))
            self$add(R6::R6Class(
                inherit = jmvcore::Group,
                active = list(
                    coordonnees = function() private$.items[["coordonnees"]],
                    contribution = function() private$.items[["contribution"]],
                    cosinus = function() private$.items[["cosinus"]]),
                private = list(),
                public=list(
                    initialize=function(options) {
                        super$initialize(
                            options=options,
                            name="rowgroup",
                            title="Row Tables")
                        self$add(jmvcore::Table$new(
                            options=options,
                            name="coordonnees",
                            title="Coordinates Table",
                            visible="(coordrow)",
                            clearWith=list(
                                "nbfact"),
                            columns=list()))
                        self$add(jmvcore::Table$new(
                            options=options,
                            name="contribution",
                            title="Contributions Table",
                            visible="(contribrow)",
                            clearWith=list(
                                "nbfact"),
                            columns=list()))
                        self$add(jmvcore::Table$new(
                            options=options,
                            name="cosinus",
                            title="Cosine Table",
                            visible="(cosrow)",
                            clearWith=list(
                                "nbfact"),
                            columns=list()))}))$new(options=options))
            self$add(R6::R6Class(
                inherit = jmvcore::Group,
                active = list(
                    coordonnees = function() private$.items[["coordonnees"]],
                    contribution = function() private$.items[["contribution"]],
                    cosinus = function() private$.items[["cosinus"]]),
                private = list(),
                public=list(
                    initialize=function(options) {
                        super$initialize(
                            options=options,
                            name="colgroup",
                            title="Column Tables")
                        self$add(jmvcore::Table$new(
                            options=options,
                            name="coordonnees",
                            title="Coordinates Table",
                            visible="(coordcol)",
                            clearWith=list(
                                "nbfact"),
                            columns=list()))
                        self$add(jmvcore::Table$new(
                            options=options,
                            name="contribution",
                            title="Contributions Table",
                            visible="(contribcol)",
                            clearWith=list(
                                "nbfact"),
                            columns=list()))
                        self$add(jmvcore::Table$new(
                            options=options,
                            name="cosinus",
                            title="Cosine Table",
                            visible="(coscol)",
                            clearWith=list(
                                "nbfact"),
                            columns=list()))}))$new(options=options))
            self$add(jmvcore::Image$new(
                options=options,
                name="plotclassif",
                title="Representation of the Rows According to Clusters",
                visible="(graphclassif)",
                width=600,
                height=500,
                renderFun=".plotclassif"))
            self$add(jmvcore::Output$new(
                options=options,
                name="newvar",
                title="Coordinates",
                measureType="continuous",
                initInRun=TRUE,
                clearWith=list(
                    "activecol",
                    "illustrativecol",
                    "indiv",
                    "nbfact")))
            self$add(jmvcore::Output$new(
                options=options,
                name="newvar2",
                title="Coordinates",
                measureType="continuous",
                initInRun=TRUE,
                clearWith=list(
                    "actvars",
                    "quantisup",
                    "qualisup",
                    "individus",
                    "nFactors",
                    "norme")))}))

CABase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "CABase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "MEDA",
                name = "CA",
                version = c(1,0,0),
                options = options,
                results = CAResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE)
        }))

#' Correspondence Analysis
#'
#' 
#' @param donnees .
#' @param activecol .
#' @param illustrativecol .
#' @param indiv .
#' @param tuto .
#' @param nbfact .
#' @param proba .
#' @param abs .
#' @param ord .
#' @param limcoscol .
#' @param limcosrow .
#' @param ellipsecol .
#' @param ellipserow .
#' @param addillucol .
#' @param coordcol .
#' @param contribcol .
#' @param coscol .
#' @param coordrow .
#' @param contribrow .
#' @param cosrow .
#' @param ncp .
#' @param graphclassif .
#' @param nbclust .
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$instructions} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$plotirow} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$ploticol} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$plotell} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$xsqgroup$xsq} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$eigengroup$eigen} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$descofdimgroup$descofdim} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$code} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$rowgroup$coordonnees} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$rowgroup$contribution} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$rowgroup$cosinus} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$colgroup$coordonnees} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$colgroup$contribution} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$colgroup$cosinus} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$plotclassif} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$newvar} \tab \tab \tab \tab \tab an output \cr
#'   \code{results$newvar2} \tab \tab \tab \tab \tab an output \cr
#' }
#'
#' @export
CA <- function(
    donnees,
    activecol,
    illustrativecol,
    indiv,
    tuto = TRUE,
    nbfact = 2,
    proba = 5,
    abs = 1,
    ord = 2,
    limcoscol = 0,
    limcosrow = 0,
    ellipsecol = FALSE,
    ellipserow = FALSE,
    addillucol = TRUE,
    coordcol = FALSE,
    contribcol = FALSE,
    coscol = FALSE,
    coordrow = FALSE,
    contribrow = FALSE,
    cosrow = FALSE,
    ncp = 5,
    graphclassif = FALSE,
    nbclust = -1) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("CA requires jmvcore to be installed (restart may be required)")

    if ( ! missing(activecol)) activecol <- jmvcore::resolveQuo(jmvcore::enquo(activecol))
    if ( ! missing(illustrativecol)) illustrativecol <- jmvcore::resolveQuo(jmvcore::enquo(illustrativecol))
    if ( ! missing(indiv)) indiv <- jmvcore::resolveQuo(jmvcore::enquo(indiv))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(activecol), activecol, NULL),
            `if`( ! missing(illustrativecol), illustrativecol, NULL),
            `if`( ! missing(indiv), indiv, NULL))

    for (v in indiv) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])

    options <- CAOptions$new(
        activecol = activecol,
        illustrativecol = illustrativecol,
        indiv = indiv,
        tuto = tuto,
        nbfact = nbfact,
        proba = proba,
        abs = abs,
        ord = ord,
        limcoscol = limcoscol,
        limcosrow = limcosrow,
        ellipsecol = ellipsecol,
        ellipserow = ellipserow,
        addillucol = addillucol,
        coordcol = coordcol,
        contribcol = contribcol,
        coscol = coscol,
        coordrow = coordrow,
        contribrow = contribrow,
        cosrow = cosrow,
        ncp = ncp,
        graphclassif = graphclassif,
        nbclust = nbclust)

    analysis <- CAClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

