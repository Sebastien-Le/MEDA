
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
      .nVaract = NULL,
      .nQuantsup = NULL,
      .nQualsup = NULL,
      .nbclust = NULL,
      .classifResult = NULL,      
      .PCAResult = NULL,
      
      
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

      ready <- TRUE
      if (is.null(self$options$actvars) || self$nVaract < 2){
        return()
        ready <- FALSE
      }

      if (ready) {                

          #private$.errorCheck()
          res.classif <- private$.getclassifResult()

          dimdesc=private$.dimdesc()
          self$results$descdesdim$setContent(dimdesc)

          code=private$.code()
          self$results$code$setContent(code)

          private$.printeigenTable()
          private$.printTables("coord")
          private$.printTables("contrib")
          private$.printTables("cos2")
          
          private$.plotindividus()
          private$.plotvariables()

          if (self$options$graphclassif==TRUE){
          imageclass = self$results$plotclassif
          imageclass$setState(res.classif)
          }
          
          private$.output()
          private$.output2(res.classif)
      }
      },

      #### Compute results ----
      .computeNbclust = function() {
          nbclust <- self$options$nbclust
          return(nbclust)
      },
      .computeNQuantsup = function() {
          nQuantsup <- length(self$options$quantisup)
          return(nQuantsup)
      },
      .computeNQualsup = function() {
          nQualsup <- length(self$options$qualisup)
          return(nQualsup)
      },
      .computeNVaract = function() {
          nVaract <- length(self$options$actvars)
          return(nVaract)
      },

      .getclassifResult = function() {
        if (is.null(self$options$actvars) || self$nVaract < 2){
          return()
        }
        else{
        reshcpc <- FactoMineR::HCPC(self$PCAResult,nb.clust=self$nbclust,graph=F)
        private$.classifResult <- reshcpc
        return(private$.classifResult)
        }
      },

      .getPCAResult = function() {
          if (is.null(self$options$quantisup) == FALSE && is.null(self$options$qualisup)== TRUE) {
          r <- FactoMineR::PCA(self$dataProcessed, quanti.sup=(self$nVaract+1):(self$nVaract+self$nQuantsup),ncp=self$options$ncp, scale.unit=(self$options$norme==TRUE), graph=FALSE)
          }
          else if (is.null(self$options$quantisup)==TRUE && is.null(self$options$qualisup) == FALSE) {
          r <- FactoMineR::PCA(self$dataProcessed, quali.sup=(self$nVaract+1):(self$nVaract+self$nQualsup), ncp=self$options$ncp, scale.unit=(self$options$norme==TRUE), graph=FALSE)
          }
          else if (is.null(self$options$quantisup) == FALSE && is.null(self$options$qualisup) == FALSE) {
          r <- FactoMineR::PCA(self$dataProcessed, quanti.sup=(self$nVaract+1):(self$nVaract+self$nQuantsup),quali.sup=(self$nVaract+self$nQuantsup+1):(self$nVaract+self$nQuantsup+self$nQualsup), ncp=self$options$ncp, scale.unit=(self$options$norme==TRUE), graph=FALSE)
          }
          else {
          r <- FactoMineR::PCA(self$dataProcessed, scale.unit=(self$options$norme==TRUE), graph=FALSE, ncp=self$options$ncp)
          }
          private$.PCAResult <- r
          return(private$.PCAResult)
      },

      .dimdesc = function() {
        table <- self$PCAResult
        proba <- self$options$proba/100
        nFactors_out <- min(self$options$nFactors,dim(self$PCAResult$eig)[1])

        res=dimdesc(table, axes=1:nFactors_out, proba = proba)
        print(res[-length(res)])
      },

      .code = function() {

          if (is.null(self$options$quantisup) == FALSE && is.null(self$options$qualisup)== TRUE) {
          #r <- FactoMineR::PCA(self$dataProcessed, quanti.sup=(self$nVaract+1):(self$nVaract+self$nQuantsup),ncp=self$options$ncp, scale.unit=(self$options$norme==TRUE), graph=FALSE)
          quantisup_1 <- self$nVaract+1
          quantisup_2 <- self$nVaract+self$nQuantsup
          qualisup_1 <- quantisup_2+1
          qualisup_2 <- quantisup_2+self$nQualsup
          names_var <- paste(names(self$PCAResult$call$X), collapse = ", ")
          data <- paste("data_PCA <- data[ ,c(",names_var,")]",sep="")
          code <- paste("PCA(data_PCA, quanti.sup=",quantisup_1,":",quantisup_2,", scale.unit=",(self$options$norme==TRUE),", ncp=",self$options$ncp,")",sep="")
          a <- list("dataset"=data,"R code"=code)
          print(a)
          }
          else if (is.null(self$options$quantisup)==TRUE && is.null(self$options$qualisup) == FALSE) {
          #r <- FactoMineR::PCA(self$dataProcessed, quali.sup=(self$nVaract+1):(self$nVaract+self$nQualsup), ncp=self$options$ncp, scale.unit=(self$options$norme==TRUE), graph=FALSE)
          quantisup_1 <- self$nVaract+1
          quantisup_2 <- self$nVaract+self$nQuantsup
          qualisup_1 <- quantisup_2+1
          qualisup_2 <- quantisup_2+self$nQualsup
          names_var <- paste(names(self$PCAResult$call$X), collapse = ", ")
          data <- paste("data_PCA <- data[ ,c(",names_var,")]",sep="")
          code <- paste("PCA(data_PCA, quali.sup=",qualisup_1,":",qualisup_2,", scale.unit=",(self$options$norme==TRUE),", ncp=",self$options$ncp,")",sep="")
          a <- list("dataset"=data,"R code"=code)
          print(a)
          }
          else if (is.null(self$options$quantisup) == FALSE && is.null(self$options$qualisup) == FALSE) {
          #r <- FactoMineR::PCA(self$dataProcessed, quanti.sup=(self$nVaract+1):(self$nVaract+self$nQuantsup),quali.sup=(self$nVaract+self$nQuantsup+1):(self$nVaract+self$nQuantsup+self$nQualsup), ncp=self$options$ncp, scale.unit=(self$options$norme==TRUE), graph=FALSE)
          quantisup_1 <- self$nVaract+1
          quantisup_2 <- self$nVaract+self$nQuantsup
          qualisup_1 <- quantisup_2+1
          qualisup_2 <- quantisup_2+self$nQualsup
          names_var <- paste(names(self$PCAResult$call$X), collapse = ", ")
          data <- paste("data_PCA <- data[ ,c(",names_var,")]",sep="")
          code <- paste("PCA(data_PCA, quanti.sup=",quantisup_1,":",quantisup_2,", quali.sup=",qualisup_1,":",qualisup_2,", scale.unit=",(self$options$norme==TRUE),", ncp=",self$options$ncp,")",sep="")
          a <- list("dataset"=data,"R code"=code)
          print(a)
          }
          else {
          #r <- FactoMineR::PCA(self$dataProcessed, scale.unit=(self$options$norme==TRUE), graph=FALSE, ncp=self$options$ncp)
          quantisup_1 <- self$nVaract+1
          quantisup_2 <- self$nVaract+self$nQuantsup
          qualisup_1 <- quantisup_2+1
          qualisup_2 <- quantisup_2+self$nQualsup
          names_var <- paste(names(self$PCAResult$call$X), collapse = ", ")
          data <- paste("data_PCA <- data[ ,c(",names_var,")]",sep="")
          code <- paste("PCA(data_PCA, scale.unit=",(self$options$norme==TRUE),", ncp=",self$options$ncp,")",sep="")
          a <- list("dataset"=data,"R code"=code)
          print(a)
          }
      },

      .printeigenTable = function(){

        table <- self$PCAResult
        table <- table$eig

        for (i in 1:dim(table)[1]){
          self$results$eigengroup$eigen$addRow(rowKey=i, values=list(component=as.character(i)))
        }
        eigen=table[,1]
        purcent=table[,2]
        purcentcum=table[,3]

        for (i in seq_along(eigen)) {
          row=list()
          row[["component"]]=paste("Dim.",i)
          row[["eigenvalue"]]=eigen[i]
          row[["purcent"]]=purcent[i]
          row[["purcentcum"]]=purcentcum[i]
          self$results$eigengroup$eigen$setRow(rowNo=i, values = row)
        }
        return(table)
      },

      .printTables = function(quoi){

        nFactors_out <- min(self$options$nFactors,dim(self$PCAResult$eig)[1])
        table <- self$PCAResult

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

        for (var in seq_along(self$options$actvars)) {
          row=list()
          row[["variables"]]=rownames(quoivar)[var]
          for (i in 1:nFactors_out) {
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
          for (i in 1:nFactors_out){
            row[[paste0("dim",i)]]=quoiind[ind,i]
          }
          tableind$setRow(rowNo=ind, values=row)
        }

      },

      .plotindividus = function(image, ...){

        if (self$nVaract<2) return()

        else {
          res.pca=self$PCAResult
          abs_gui=self$options$abs
          ord_gui=self$options$ord


          if (self$options$habillage > 0) habillage_value = self$nVaract+self$nQuantsup+self$options$habillage
          else habillage_value="none"

          if (is.null(self$options$qualisup) == FALSE){
            if (self$options$indact == TRUE && self$options$modillus == TRUE)
              plot=plot.PCA(res.pca,axes=c(abs_gui, ord_gui), habillage = habillage_value, title = "Representation of the Individuals and the Categories")

            else if (self$options$indact == TRUE && self$options$modillus == FALSE)
              plot=plot.PCA(res.pca,axes=c(abs_gui, ord_gui), invisible="quali", habillage = habillage_value, title = "Representation of the Individuals")

            else if (self$options$indact == FALSE && self$options$modillus == TRUE)
              plot=plot.PCA(res.pca,axes=c(abs_gui, ord_gui), invisible="ind", habillage = habillage_value, title = "Representation of the Categories")

            else
              plot=plot.PCA(res.pca,axes=c(abs_gui, ord_gui), invisible=c("ind", "quali"), habillage = habillage_value, title = "Representation of the Individuals")
          }

          else plot=plot.PCA(res.pca,axes=c(abs_gui, ord_gui), title = "Representation of the Individuals")

          return(plot)

        }
      },

      .plotvariables = function(image, ...) {

        if (self$nVaract<2) return()

        else {

          res.pca=self$PCAResult
          abs_gui=self$options$abs
          ord_gui=self$options$ord

          if (is.null(self$options$quantisup) == FALSE) {

            if (self$options$varact == TRUE && self$options$varillus == TRUE)
              plot=plot.PCA(res.pca, choix="var", axes=c(abs_gui, ord_gui), title = "Representation of the Variables (Active and Supplementary)")

            else if (self$options$varact == TRUE && self$options$varillus == FALSE)
              plot=plot.PCA(res.pca, choix="var", axes=c(abs_gui, ord_gui), invisible="quanti.sup", title = "Representation of the Active Variables")

            else if (self$options$varact == FALSE && self$options$varillus == TRUE)
              plot=plot.PCA(res.pca, choix="var", axes=c(abs_gui, ord_gui), invisible="var", title = "Representation of the Supplementary Variables")

            else
              plot=plot.PCA(res.pca, choix="var", axes=c(abs_gui, ord_gui), invisible = c("var", "quanti.sup"), title = "Correlation Circle")
          }

          else plot=plot.PCA(res.pca, choix="var", axes=c(abs_gui, ord_gui), title = "Representation of the Active Variables")

          return(plot)

        }
      },

      .plotclassif= function(image, ...){

        if (is.null(self$options$actvars) || self$nVaract < 2) return()

        else {
          abs_gui=self$options$abs
          ord_gui=self$options$ord

          res.classif=image$state
          plot=FactoMineR::plot.HCPC(res.classif, axes=c(abs_gui, ord_gui), choice="map", draw.tree = F, title="Representation of the Individuals According to Clusters")
          print(plot)
          TRUE
        }
      },

#---------------------------------------------
### Helper functions ----

      .errorCheck = function() {
            if (self$options$nFactors > self$nVaract) {
                jmvcore::reject(
                    jmvcore::format(
                        'Number of components cannot be bigger than number of variables'
                    )
                )
            }
      },
      
      .output = function(){
        nFactors_out <- min(self$options$ncp,dim(self$PCAResult$eig)[1])
        if (self$results$newvar$isNotFilled()) {
          keys <- 1:(nFactors_out)
          measureTypes <- c(rep("continuous", nFactors_out))
          titles <- paste(("Dim."), keys)
          descriptions <- "PCA component"
          self$results$newvar$set(
            keys=keys,
            titles=titles,
            descriptions=descriptions,
            measureTypes=measureTypes
          )
          for (i in 1:(nFactors_out)) {
            scores <- as.numeric(self$PCAResult$ind$coord[, i])
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
            #scores <- as.factor(res.classif$data.clust[,dim(res.classif$data.clust)[2]])
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