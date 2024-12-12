
# This file is a generated template, your changes will not be overwritten

MCAClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "MCAClass",
    inherit = MCABase,
    active = list(
    nVaract = function() {
            if (is.null(private$.nVaract))
                private$.nVaract <- private$.computeNVaract()

            return(private$.nVaract)
        },      
    nbclust = function() {
            if (is.null(private$.nbclust))
                private$.nbclust <- private$.computeNbclust()

            return(private$.nbclust)
        }
    ),

    private = list(

      .nbclust = NULL,
      .nVaract = NULL,

    #---------------------------------------------  
    #### Init + run functions ----

      .init = function() {
            if (is.null(self$options$actvars) || self$nVaract < 2) {
              if (self$options$tuto==TRUE){
                self$results$instructions$setVisible(visible = TRUE)
              }
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
            This method can also be seen as a PCA on categorical variables. 
            Indeed, the datasets analysed in  and MCA are similar, with individuals in rows and variables in columns, 
            continuous variables in the case of PCA and categorical variables in the case of MCA.</p>

            <p> MCA is a useful technique in data exploration and visualization when working 
            with complex categorical data with multiple variables. It helps to identify patterns 
            and associations between the different categorical variables, as well as the relationships 
            among the categories within each variable.</p>

            <p> While the <I>Active Variables</I> field is mandatory, the <I>Supplementary Variables</I> fields are optional. 
            However, if you have supplementary variables, 
            they may be essential for interpreting the structure on the individuals.</p>
            
            <p> Once you have selected the active variables, you can choose to get rid of the categories that were rarely
            chosen to describe/measure your individuals. By default, categories that are used by less than 5% of the individuals
            are removed: new categories are then randomly assigned to those individuals.</p>

            <p> Clustering is based on the number of components saved. 
            By default, clustering is based on the first 5 components, 
            <I>i.e.</I> the distance between individuals is calculated on these 5 components.</p>

            <p> By default, the <I>Number of clusters</I> field is set to -1 which means that the number of 
            clusters is automatically chosen by the computer.</p>


            <p>______________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
            )
            
        },    

      .run = function() {

        ready <- TRUE
        if (is.null(self$options$actvars) || length(self$options$actvars) < self$options$nFactors){
          return()
          ready <- FALSE
        }
        
        private$.errorCheck()

        if (ready) {

          data <- private$.buildData()
          res.mca <- private$.MCA(data)
          res.classif <- private$.classif(res.mca)

          dimdesc=private$.dimdesc(res.mca)
          self$results$dimdesc$setContent(dimdesc)

          code=private$.code(res.mca)
          self$results$code$setContent(code)

          private$.printeigenTable(res.mca)
          private$.printTables(res.mca, "coord")
          private$.printTables(res.mca, "contrib")
          private$.printTables(res.mca, "cos2")

          imageindiv=self$results$plotindiv
          imageindiv$setState(res.mca)

          imagevar=self$results$plotvar
          imagevar$setState(res.mca)

          imageitemvar=self$results$plotitemvar
          imageitemvar$setState(res.mca)

          imagequantisup=self$results$plotquantisup
          imagequantisup$setState(res.mca)

          if (self$options$graphclassif==TRUE){
          imageclass = self$results$plotclassif
          imageclass$setState(res.classif)
          }          
          
          private$.output(res.mca)
          private$.output2(res.classif)
          
        }
        
      },

      #### Compute results ----

      .computeNbclust = function() {
          nbclust <- self$options$nbclust
          return(nbclust)
      },

      .computeNVaract = function() {
          nVaract <- length(self$options$actvars)
          return(nVaract)
      },      
      
      .MCA = function(data) {

        actvars_gui=self$options$actvars
        quantisup_gui=self$options$quantisup
        qualisup_gui=self$options$qualisup
        nFactors_gui=self$options$nFactors
        ventil=self$options$ventil/100

        if (is.null(quantisup_gui) == FALSE && is.null(qualisup_gui)== TRUE) {
          FactoMineR::MCA(data, quanti.sup=(length(actvars_gui)+1):(length(actvars_gui)+length(quantisup_gui)), ncp=self$options$ncp, level.ventil=ventil, graph=FALSE)
        }
        else if (is.null(quantisup_gui)==TRUE && is.null(qualisup_gui) == FALSE) {
          FactoMineR::MCA(data, quali.sup=(length(actvars_gui)+1):(length(actvars_gui)+length(qualisup_gui)), ncp=self$options$ncp, level.ventil=ventil, graph=FALSE)
        }
        else if (is.null(quantisup_gui) == FALSE && is.null(qualisup_gui) == FALSE) {
          FactoMineR::MCA(data, quanti.sup=(length(actvars_gui)+1):(length(actvars_gui)+length(quantisup_gui)),quali.sup=(length(actvars_gui)+length(quantisup_gui)+1):(length(actvars_gui)+length(quantisup_gui)+length(qualisup_gui)), ncp=self$options$ncp, level.ventil=ventil, graph=FALSE)
        }
        else {
          FactoMineR::MCA(data, ncp=self$options$ncp, level.ventil=ventil, graph=FALSE)
        }
      },

      .code = function(table) {

        actvars_gui=self$options$actvars
        quantisup_gui=self$options$quantisup
        qualisup_gui=self$options$qualisup
        nFactors_gui=self$options$nFactors
        ventil=self$options$ventil/100

        if (is.null(quantisup_gui) == FALSE && is.null(qualisup_gui)== TRUE) {
          #FactoMineR::MCA(data, quanti.sup=(length(actvars_gui)+1):(length(actvars_gui)+length(quantisup_gui)), ncp=self$options$ncp, level.ventil=ventil, graph=FALSE)          
          names_var <- paste(names(table$call$X), collapse = ", ")
          data <- paste("data_MCA <- data[ ,c(",names_var,")]",sep="")
          code <- paste("MCA(data_MCA, quanti.sup=",(length(actvars_gui)+1),":",(length(actvars_gui)+length(quantisup_gui)),", level.ventil=",ventil,", ncp=",self$options$ncp,")",sep="")
          a <- list("dataset"=data,"R code"=code)
          print(a)
        }
        else if (is.null(quantisup_gui)==TRUE && is.null(qualisup_gui) == FALSE) {
          #FactoMineR::MCA(data, quali.sup=(length(actvars_gui)+1):(length(actvars_gui)+length(qualisup_gui)), ncp=self$options$ncp, level.ventil=ventil, graph=FALSE)
          names_var <- paste(names(table$call$X), collapse = ", ")
          data <- paste("data_MCA <- data[ ,c(",names_var,")]",sep="")
          code <- paste("MCA(data_MCA, quali.sup=",(length(actvars_gui)+length(quantisup_gui)+1),":",(length(actvars_gui)+length(quantisup_gui)+length(qualisup_gui)),", level.ventil=",ventil,", ncp=",self$options$ncp,")",sep="")
          a <- list("dataset"=data,"R code"=code)
          print(a)
        }
        else if (is.null(quantisup_gui) == FALSE && is.null(qualisup_gui) == FALSE) {
          #FactoMineR::MCA(data, quanti.sup=(length(actvars_gui)+1):(length(actvars_gui)+length(quantisup_gui)),quali.sup=(length(actvars_gui)+length(quantisup_gui)+1):(length(actvars_gui)+length(quantisup_gui)+length(qualisup_gui)), ncp=self$options$ncp, level.ventil=ventil, graph=FALSE)
          names_var <- paste(names(table$call$X), collapse = ", ")
          data <- paste("data_MCA <- data[ ,c(",names_var,")]",sep="")
          code <- paste("MCA(data_MCA, quanti.sup=",(length(actvars_gui)+1),":",(length(actvars_gui)+length(quantisup_gui)),", quali.sup=",(length(actvars_gui)+length(quantisup_gui)+1),":",(length(actvars_gui)+length(quantisup_gui)+length(qualisup_gui)),", level.ventil=",ventil,", ncp=",self$options$ncp,")",sep="")
          a <- list("dataset"=data,"R code"=code)
          print(a)
        }
        else {
          #FactoMineR::MCA(data, ncp=self$options$ncp, level.ventil=ventil, graph=FALSE)
          names_var <- paste(names(table$call$X), collapse = ", ")
          data <- paste("data_MCA <- data[ ,c(",names_var,")]",sep="")
          code <- paste("MCA(data_MCA, level.ventil=",ventil,", ncp=",self$options$ncp,")",sep="")
          a <- list("dataset"=data,"R code"=code)
          print(a)
        }
      },

      .classif = function(res) {
          FactoMineR::HCPC(res,nb.clust=self$nbclust,graph=F)
      },      

      .dimdesc = function(table) {

        proba_gui=self$options$proba/100
        nFactors_gui=self$options$nFactors

        res=dimdesc(table, axes=1:nFactors_gui, proba = proba_gui)
        print(res[-length(res)])
      },

      .printeigenTable = function(table){

        for (i in 1:dim(table$eig)[1]){
          self$results$eigengroup$eigen$addRow(rowKey=i, values=list(component=as.character(i)))
        } #on crée les lignes du tableau, avec autant de facteurs qu'il y a de variables actives


        eigen=table$eig[,1]
        purcent=table$eig[,2]
        purcentcum=table$eig[,3]

        for (i in seq_along(eigen)) {
          row=list()
          row[["component"]]=paste("Dim.",i)
          row[["eigenvalue"]]=eigen[i] #   a chaque nom de colonne (eigenvalue, purcent et purcentcum)
          row[["purcent"]]=purcent[i] #    on associe
          row[["purcentcum"]]=purcentcum[i] #  une valeur des calculs precedents
          self$results$eigengroup$eigen$setRow(rowNo=i, values = row)
        }

      },

      .printTables = function(table, quoi){

        actvars_gui=self$options$actvars
        nFactors_gui=self$options$nFactors
        nFactors_out <- min(self$options$nFactors,dim(table$eig)[1])

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

        for (i in 1:nFactors_out){
          tablevar$addColumn(name=paste0("dim",i), title=paste0("Dim.", as.character(i)),type='number') #, superTitle='Facteurs'
          tableind$addColumn(name=paste0("dim",i), title=paste0("Dim.", as.character(i)),type='number')
        }

        for (var in seq(nrow(quoivar))) {
          row=list()
          row[["variables"]]=rownames(quoivar)[var]
          for (i in 1:nFactors_out) {
            row[[paste0("dim",i)]]=quoivar[var,i]
          }
          tablevar$setRow(rowNo=var, values=row) #on remplie le tableau en reprenant les résultats de results$var$coord
        }

        for (ind in 1:length(individus_gui)) {
          row=list()
          if (is.null(self$options$individus))
            row[["individus"]]= individus_gui[ind]
          else
            row[["individus"]]= rownames(quoiind)[ind]
          for (i in 1:nFactors_out){
            row[[paste0("dim",i)]]=quoiind[ind,i]
          }
          tableind$setRow(rowNo=ind, values=row) #on remplie le tableau en reprenant les résultats de results$var$coord
        }

      },

      .plotitemvar = function(image, ...) {

        if (is.null(self$options$actvars)) return()

        else {

          res.mca=image$state
          abs=self$options$abs
          ord=self$options$ord
          modqualisup_gui=self$options$varmodqualisup
          modvar_gui=self$options$varmodvar
          modality_gui=self$options$modality

            if (modqualisup_gui==TRUE && modvar_gui==TRUE)
            plot=plot.MCA(res.mca, axes=c(abs, ord), choix="ind", invisible=c("ind"), selectMod = paste(modality_gui), title="Representation of the Categories")

            else if (modqualisup_gui==TRUE && modvar_gui==FALSE)
            plot=plot.MCA(res.mca, axes=c(abs, ord), choix="ind", invisible=c("ind", "var"), selectMod = paste(modality_gui), title="Representation of the Categories")

            else if (modqualisup_gui==FALSE && modvar_gui==TRUE)
            plot=plot.MCA(res.mca, axes=c(abs, ord), choix="ind", invisible=c("ind", "quali.sup"), selectMod = paste(modality_gui), title="Representation of the Categories")

            else
            plot=plot.MCA(res.mca, axes=c(abs, ord), choix="ind", invisible=c("ind", "var", "quali.sup"), selectMod = paste(modality_gui), title="Representation of the Categories")

          print(plot)
          TRUE

        }
      },

      .plotindiv = function(image, ...) {

        if (is.null(self$options$actvars)) return()

        else {

          res.mca=image$state
          abs=self$options$abs
          ord=self$options$ord
          quantisup = self$options$quantimod
          qualisup = self$options$varmodqualisup

          plot=plot.MCA(res.mca, axes=c(abs, ord), choix="ind", invisible=c("var","quali.sup", "quanti.sup"),title = "Representation of the Individuals")

          print(plot)
          TRUE
        }
      },

      .plotvar = function(image, ...) {

        if (is.null(self$options$actvars)) return()

        else {

          res.mca=image$state
          abs=self$options$abs
          ord=self$options$ord
          quantisup = self$options$quantimod
          qualisup = self$options$varmodqualisup

          plot=plot.MCA(res.mca, axes=c(abs, ord), choix="var", title = "Representation of the Variables")

          print(plot)
          TRUE
        }
      },

      .plotquantisup = function(image, ...) {

        if (is.null(self$options$actvars)) return()

        else {

          res.mca=image$state
          abs=self$options$abs
          ord=self$options$ord

          plot=plot.MCA(res.mca, axes=c(abs, ord), choix="quanti.sup")

          print(plot)
          TRUE

        }
      },

      .plotclassif= function(image, ...){

        if (is.null(self$options$actvars)) return()

        else {
          abs_gui=self$options$abs
          ord_gui=self$options$ord

          res.classif=image$state
          plot=FactoMineR::plot.HCPC(res.classif, axes=c(abs_gui, ord_gui), choice="map", draw.tree = F, title="Representation of the Individuals According to Clusters")
          print(plot)
          TRUE
        }
      },      

      ### Helper functions ----
      .errorCheck = function() {
        if (length(self$options$actvars) < self$options$nFactors)
          jmvcore::reject(jmvcore::format('The number of factors is too low'))

      },
      
      .output = function(res.mca) {
        nFactors_out <- min(self$options$ncp,dim(res.mca$eig)[1])
        if (self$results$newvar$isNotFilled()) {
          keys <- 1:nFactors_out
          measureTypes <- rep("continuous", nFactors_out)
          titles <- paste(("Dim."), keys)
          descriptions <- character(length(keys))
          self$results$newvar$set(
            keys=keys,
            titles=titles,
            descriptions=descriptions,
            measureTypes=measureTypes
          )
          for (i in 1:nFactors_out) {
            scores <- as.numeric(res.mca$ind$coord[, i])
            self$results$newvar$setValues(index=i, scores)
          }
          self$results$newvar$setRowNums(rownames(self$data))
        }
      },

      .output2 = function(res.classif){
        #if (self$results$newvar2$isNotFilled()) {
        if (self$results$newvar2$isFilled()) {
          keys <- 1
          measureTypes <- "nominal"
          titles <- "Cluster"
          descriptions <- "Cluster variable"
          self$results$newvar2$set(
            keys=keys,
            titles=titles,
            descriptions=descriptions,
            measureTypes=measureTypes
          )
            scores <- as.factor(res.classif$data.clust[rownames(private$.buildData()),dim(res.classif$data.clust)[2]])
            self$results$newvar2$setValues(index=1, scores)

          self$results$newvar2$setRowNums(rownames(self$data))
        }
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
