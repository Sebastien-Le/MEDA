
# This file is a generated template, your changes will not be overwritten

PCAClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "PCAClass",
    inherit = PCABase,
    private = list(


      #### Init + run functions ----
      .run = function() {

        ready <- TRUE
        if (is.null(self$options$actvars) || length(self$options$actvars) < self$options$nFactors){
          return()
          ready <- FALSE
        }
        private$.errorCheck()

        if (ready) {

          data <- private$.buildData()
          res.pca <- private$.PCA(data)

          dimdesc=private$.dimdesc(res.pca)
          self$results$descdesdim$setContent(dimdesc)

          private$.printeigenTable(res.pca)
          private$.printTables(res.pca, "coord")
          private$.printTables(res.pca, "contrib")
          private$.printTables(res.pca, "cos2")

          imageind=self$results$plotind
          imageind$setState(res.pca)

          imagevar=self$results$plotvar
          imagevar$setState(res.pca)
        }
      },

      #### Compute results ----
      .PCA = function(data) {

        actvars_gui=self$options$actvars
        quantisup_gui=self$options$quantisup
        qualisup_gui=self$options$qualisup
        nFactors_gui=self$options$nFactors
        norme_gui=self$options$norme

        if (norme_gui==TRUE) norme=TRUE
        else norme=FALSE

        if (is.null(quantisup_gui) == FALSE && is.null(qualisup_gui)== TRUE) {
          FactoMineR::PCA(data, quanti.sup=(length(actvars_gui)+1):(length(actvars_gui)+length(quantisup_gui)),ncp=nFactors_gui, scale.unit=norme, graph=FALSE)
        }
        else if (is.null(quantisup_gui)==TRUE && is.null(qualisup_gui) == FALSE) {
          FactoMineR::PCA(data, quali.sup=(length(actvars_gui)+1):(length(actvars_gui)+length(qualisup_gui)), ncp=nFactors_gui, scale.unit=norme, graph=FALSE)
        }
        else if (is.null(quantisup_gui) == FALSE && is.null(qualisup_gui) == FALSE) {
          FactoMineR::PCA(data, quanti.sup=(length(actvars_gui)+1):(length(actvars_gui)+length(quantisup_gui)),quali.sup=(length(actvars_gui)+length(quantisup_gui)+1):(length(actvars_gui)+length(quantisup_gui)+length(qualisup_gui)), ncp=nFactors_gui, scale.unit=norme, graph=FALSE)
        }
        else {
          FactoMineR::PCA(data,ncp=nFactors_gui, scale.unit=norme, graph=FALSE)
        }
      },

      .dimdesc = function(table) {

        proba=self$options$proba/100
        nFactors=self$options$nFactors

        res=dimdesc(table, axes=1:nFactors, proba = proba)
        print(res[-length(res)])

      },

      .printeigenTable = function(table){

        for (i in 1:dim(table$eig)[1]){
          self$results$eigengroup$eigen$addRow(rowKey=i, values=list(component=as.character(i)))
        }
        eigen=table$eig[,1]
        purcent=table$eig[,2]
        purcentcum=table$eig[,3]

        for (i in seq_along(eigen)) {
          row=list()
          row[["component"]]=paste("Dim.",i)
          row[["eigenvalue"]]=eigen[i]
          row[["purcent"]]=purcent[i]
          row[["purcentcum"]]=purcentcum[i]
          self$results$eigengroup$eigen$setRow(rowNo=i, values = row)
        }

      },

      .printTables = function(table, quoi){

        actvars_gui=self$options$actvars
        nFactors_gui=self$options$nFactors
        if (is.null(self$options$individus)==FALSE)
          individus_gui=self$data[[self$options$individus]]
        else
          individus_gui=c(1:nrow(self$data))

        if (quoi=="coord") {
          quoivar=table$var$coord
          quoiind=table$ind$coord
          tablevar=self$results$variables$coordonnees
          tableind=self$results$individus$coordonnees
        }

        else if (quoi=="contrib") {
          quoivar=table$var$contrib
          quoiind=table$ind$contrib
          tablevar=self$results$variables$contribution
          tableind=self$results$individus$contribution
        }

        else if (quoi=="cos2") {
          quoivar=table$var$cos2
          quoiind=table$ind$cos2
          tablevar=self$results$variables$cosinus
          tableind=self$results$individus$cosinus
        }

        tableind$addColumn(name="individus", title="", type="text")
        for (i in seq(nrow(quoiind)))
          tableind$addRow(rowKey=i, value=NULL)

        tablevar$addColumn(name="variables", title="", type="text")
        for (i in seq(nrow(quoivar)))
          tablevar$addRow(rowKey=i, value=NULL)

        for (i in 1:nFactors_gui){
          tablevar$addColumn(name=paste0("dim",i), title=paste0("Dim.", as.character(i)),type='number') #, superTitle='Facteurs'
          tableind$addColumn(name=paste0("dim",i), title=paste0("Dim.", as.character(i)),type='number')
        }

        for (var in seq_along(actvars_gui)) {
          row=list()
          row[["variables"]]=rownames(quoivar)[var]
          for (i in 1:nFactors_gui) {
            row[[paste0("dim",i)]]=quoivar[var,i]
          }
          tablevar$setRow(rowNo=var, values=row)
        }

        for (ind in 1:length(individus_gui)) {
          row=list()
          if (is.null(self$options$individus))
            row[["individus"]]= individus_gui[ind]
          else
            row[["individus"]]= rownames(quoiind)[ind]
          for (i in 1:nFactors_gui){
            row[[paste0("dim",i)]]=quoiind[ind,i]
          }
          tableind$setRow(rowNo=ind, values=row)
        }

      },

      .plotindividus = function(image, ...){

        if (is.null(self$options$actvars)) return()

        else {
          res.pca=image$state
          habillage_gui=self$options$habillage
          actvars_gui=self$options$actvars
          quantisup_gui=self$options$quantisup
          qualisup_gui=self$options$qualisup
          modact_gui=self$options$modact
          modillus_gui=self$options$modillus
          abs_gui=self$options$abs
          ord_gui=self$options$ord

          if (habillage_gui > 0) habillage_value = length(actvars_gui)+length(quantisup_gui)+habillage_gui
          else habillage_value="none"

          if (is.null(qualisup_gui) == FALSE){
            if (modact_gui == TRUE && modillus_gui == TRUE)
              plot=plot.PCA(res.pca,axes=c(abs_gui, ord_gui), habillage = habillage_value, title = "Representation of the Individuals and the Categories")

            else if (modact_gui == TRUE && modillus_gui == FALSE)
              plot=plot.PCA(res.pca,axes=c(abs_gui, ord_gui), invisible="quali", habillage = habillage_value, title = "Representation of the Individuals")

            else if (modact_gui == FALSE && modillus_gui == TRUE)
              plot=plot.PCA(res.pca,axes=c(abs_gui, ord_gui), invisible="ind", habillage = habillage_value, title = "Representation of the Categories")

            else
              plot=plot.PCA(res.pca,axes=c(abs_gui, ord_gui), invisible=c("ind", "quali"), habillage = habillage_value, title = "Representation of the Individuals")
          }

          else plot=plot.PCA(res.pca,axes=c(abs_gui, ord_gui), title = "Representation of the Individuals")

          print(plot)
          TRUE
        }
      },

      .plotvariables = function(image, ...) {

        if (is.null(self$options$actvars)) return()

        else {

          res.pca=image$state
          quantisup_gui=self$options$quantisup
          varact_gui=self$options$varact
          varillus_gui=self$options$varillus
          abs_gui=self$options$abs
          ord_gui=self$options$ord

          if (is.null(quantisup_gui) == FALSE) {

            if (varact_gui == TRUE && varillus_gui == TRUE)
              plot=plot.PCA(res.pca, choix="var", axes=c(abs_gui, ord_gui), title = "Representation of the Variables (Active and Supplementary)")

            else if (varact_gui == TRUE && varillus_gui == FALSE)
              plot=plot.PCA(res.pca, choix="var", axes=c(abs_gui, ord_gui), invisible="quanti.sup", title = "Representation of the Active Variables")

            else if (varact_gui == FALSE && varillus_gui == TRUE)
              plot=plot.PCA(res.pca, choix="var", axes=c(abs_gui, ord_gui), invisible="var", title = "Representation of the Supplementary Variables")

            else
              plot=plot.PCA(res.pca, choix="var", axes=c(abs_gui, ord_gui), invisible = c("var", "quanti.sup"), title = "Correlation Circle")
          }

          else plot=plot.PCA(res.pca, choix="var", axes=c(abs_gui, ord_gui), title = "Representation of the Active Variables")

          print(plot)
          TRUE

        }
      },

      ### Helper functions ----
      .errorCheck = function() {
        if (length(self$options$actvars) < self$options$nFactors)
          jmvcore::reject(jmvcore::format('The number of factors is too low'))

      },

      .buildData = function() {

        dataactvars=data.frame(self$data[,self$options$actvars])
        colnames(dataactvars)=self$options$actvars
        dataquantisup=data.frame(self$data[,self$options$quantisup])
        colnames(dataquantisup)=self$options$quantisup
        dataqualisup=data.frame(self$data[,self$options$qualisup])
        colnames(dataqualisup)=self$options$qualisup
        data=data.frame(dataactvars,dataquantisup,dataqualisup)

        if (is.null(self$options$individus)==FALSE) {
          rownames(data)=self$data[[self$options$individus]]
        }
        else
          rownames(data)=c(1:nrow(data))

        return(data)
      }
    )
)
