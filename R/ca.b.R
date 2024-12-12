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

        ready <- TRUE
        if (is.null(self$options$activecol) || length(self$options$activecol) < self$options$nbfact){
          return()
          ready <- FALSE
        }
        private$.errorCheck()

        if (ready) {

          data <- private$.buildData()
          res.ca <- private$.CA(data)
          res.classif <- private$.classif(res.ca)
          res.xsq <- private$.chisq(data)

          private$.chideux(res.xsq)
          tab = private$.dimdesc(res.ca)

          code <- private$.code(res.ca)
          self$results$code$setContent(code)

          private$.dodTable(tab)
          private$.printeigenTable(res.ca)
          private$.printTables(res.ca, "coord")
          private$.printTables(res.ca, "contrib")
          private$.printTables(res.ca, "cos2")

          imagecol = self$results$ploticol
          imagecol$setState(res.ca)

          imagerow = self$results$plotirow
          imagerow$setState(res.ca)

          imageell = self$results$plotell
          imageell$setState(res.ca)

          if (self$options$graphclassif==TRUE){
          imageclass = self$results$plotclassif
          imageclass$setState(res.classif)
          }
          
          private$.output(res.ca)
          private$.output2(res.classif)

        }
      },

      #### Compute results ----
      .computeNbclust = function() {
          nbclust <- self$options$nbclust
          return(nbclust)
      },
      
      .CA = function(data) {

        actcol_gui=self$options$activecol
        illucol_gui=self$options$illustrativecol
        nbfact_gui=self$options$nbfact

        if (is.null(illucol_gui) == FALSE) {
          FactoMineR::CA(data, ncp = self$options$ncp,
                        col.sup=(length(actcol_gui)+1):(length(actcol_gui)+length(illucol_gui)),
                        graph=FALSE)
        }
        else {
          FactoMineR::CA(data, ncp = self$options$ncp, graph=FALSE)
        }

      },

      .code = function(table) {

        actcol_gui=self$options$activecol
        illucol_gui=self$options$illustrativecol
        nbfact_gui=self$options$nbfact

        if (is.null(illucol_gui) == FALSE) {
          #FactoMineR::CA(data, ncp = self$options$ncp,
          #              col.sup=(length(actcol_gui)+1):(length(actcol_gui)+length(illucol_gui)),
          #              graph=FALSE)
          names_var <- paste(names(table$call$Xtot), collapse = ", ")
          data <- paste("data_CA <- data[ ,c(",names_var,")]",sep="")
          code <- paste("CA(data_CA, col.sup=",(length(actcol_gui)+1),":",(length(actcol_gui)+length(illucol_gui)),", ncp=",self$options$ncp,")",sep="")
          a <- list("dataset"=data,"R code"=code)
          print(a)

        }
        else {
          #FactoMineR::CA(data, ncp = self$options$ncp, graph=FALSE)
          names_var <- paste(names(table$call$Xtot), collapse = ", ")
          data <- paste("data_CA <- data[ ,c(",names_var,")]",sep="")
          code <- paste("CA(data_CA, ncp=",self$options$ncp,")",sep="")
          a <- list("dataset"=data,"R code"=code)
          print(a)
        }
      },

      .classif = function(res) {
          FactoMineR::HCPC(res,nb.clust=self$nbclust,graph=F)
      },

      .chisq = function(data) {

        dataactcol=data.frame(self$data[,self$options$activecol])
        colnames(dataactcol)=self$options$activecol

        datacolsup=data.frame(self$data[,self$options$illustrativecol])
        colnames(datacolsup)=self$options$illustrativecol

        data=data.frame(dataactcol,datacolsup)

        chisq.test(data)

      },

      .dimdesc = function(table) {

        nbfact_gui=self$options$nbfact
        proba = self$options$proba/100

        ddca = dimdesc(table, axes = 1:nbfact_gui, proba = proba)

        #Mise en place du tableau description of the axes

        tab=cbind(names(ddca)[1],names(ddca[[1]][1]),
                  rownames(as.data.frame(ddca[[1]][1])),as.data.frame(ddca[[1]][1])[[1]])
        tab=as.data.frame(tab)
        pretab=cbind(names(ddca)[1],names(ddca[[1]][2]),
                     rownames(as.data.frame(ddca[[1]][2])),as.data.frame(ddca[[1]][2])[[1]])
        tab=rbind(tab,pretab)

        colnames(tab)[1]="dim"
        colnames(tab)[2]="rowcol"
        colnames(tab)[3]="name"
        colnames(tab)[4]="coord"

        for (i in (2):length(ddca)) {
          for (k in 1:2) {
            if (dim(as.data.frame(ddca[[i]][k]))[1] != 0) {
              pretab=cbind(names(ddca)[i],names(ddca[[i]][k]),
                           rownames(as.data.frame(ddca[[i]][k])),as.data.frame(ddca[[i]][k])[[1]])
              pretab=as.data.frame(pretab)
              colnames(pretab)[1]="dim"
              colnames(pretab)[2]="rowcol"
              colnames(pretab)[3]="name"
              colnames(pretab)[4]="coord"
              tab=rbind(tab,pretab)
            }
          }
        }

        tab[,4]=as.numeric(as.character(tab[,4]))
        tab=as.data.frame(tab)
      },

      .chideux = function(res.xsq) {

        self$results$xsqgroup$xsq$setRow(rowNo=1, values=list(
          xsquared=res.xsq$statistic,
          df=res.xsq$parameter,
          pvxsq=res.xsq$p.value
        ))

      },

      .dodTable = function(tab){

        for (i in 1:dim(tab)[1]){
          self$results$descofdimgroup$descofdim$addRow(rowKey=i, values=list(dim=as.character(tab[,1])[i]))
        }

        for (i in seq_along(tab[,1])) {
          row=list()
          row[["rowcol"]]=as.character(tab[,2])[i]
          row[["cat"]]=as.character(tab[,3])[i]
          row[["coord"]]=tab[,4][i]
          self$results$descofdimgroup$descofdim$setRow(rowNo=i, values = row)
        }
      },
      
      .printTables = function(table, quoi){
        
        col_gui=self$options$activecol
        nbfact_gui=self$options$nbfact
        if (is.null(self$options$indiv)==FALSE)
          row_gui=self$data[[self$options$indiv]]
        else
          row_gui=c(1:nrow(self$data))
        
        if (quoi=="coord") {
          quoivar=table$col$coord
          quoiind=table$row$coord
          tablevar=self$results$colgroup$coordonnees
          tableind=self$results$rowgroup$coordonnees
        }
        
        else if (quoi=="contrib") {
          quoivar=table$col$contrib
          quoiind=table$row$contrib
          tablevar=self$results$colgroup$contribution
          tableind=self$results$rowgroup$contribution
        }
        
        else if (quoi=="cos2") {
          quoivar=table$col$cos2
          quoiind=table$row$cos2
          tablevar=self$results$colgroup$cosinus
          tableind=self$results$rowgroup$cosinus
        }
        
        tableind$addColumn(name="row", title="", type="text")
        for (i in seq(nrow(quoiind)))
          tableind$addRow(rowKey=i, value=NULL)
        
        tablevar$addColumn(name="column", title="", type="text")
        for (i in seq(nrow(quoivar)))
          tablevar$addRow(rowKey=i, value=NULL)
        
        for (i in 1:nbfact_gui){
          tablevar$addColumn(name=paste0("dim",i), title=paste0("Dim.", as.character(i)),type='number') #, superTitle='Facteurs'
          tableind$addColumn(name=paste0("dim",i), title=paste0("Dim.", as.character(i)),type='number')
        }
        
        for (var in seq_along(col_gui)) {
          row=list()
          row[["column"]]=rownames(quoivar)[var]
          for (i in 1:nbfact_gui) {
            row[[paste0("dim",i)]]=quoivar[var,i]
          }
          tablevar$setRow(rowNo=var, values=row)
        }
        
        for (ind in 1:length(row_gui)) {
          row=list()
          if (is.null(self$options$indiv))
            row[["row"]]= row_gui[ind]
          else
            row[["row"]]= rownames(quoiind)[ind]
          for (i in 1:nbfact_gui){
            row[[paste0("dim",i)]]=quoiind[ind,i]
          }
          tableind$setRow(rowNo=ind, values=row)
        }
        
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

      .plotcol= function(image, ...){

        if (is.null(self$options$activecol)) return()

        else {
          abs_gui=self$options$abs
          ord_gui=self$options$ord
          fcol=paste("cos2", self$options$limcoscol)
          frow=paste("cos2", self$options$limcosrow)
          addillucol=self$options$addillucol
          res.ca=image$state

          if (addillucol == TRUE)
            plot=FactoMineR::plot.CA(res.ca, axes=c(abs_gui, ord_gui),
                         selectCol=fcol, selectRow=frow, invisible="row", title = "Representation of the Columns")
          else
            plot=FactoMineR::plot.CA(res.ca, axes=c(abs_gui, ord_gui),
                         selectCol=fcol, selectRow=frow, invisible=c("row","col.sup"), title = "Representation of the Columns")

          print(plot)
          TRUE
        }
      },

      .plotrow= function(image, ...){

        if (is.null(self$options$activecol)) return()

        else {
          abs_gui=self$options$abs
          ord_gui=self$options$ord
          fcol=paste("cos2", self$options$limcoscol)
          frow=paste("cos2", self$options$limcosrow)

          res.ca=image$state

            plot=FactoMineR::plot.CA(res.ca, axes=c(abs_gui, ord_gui),
                         selectCol=fcol, selectRow=frow, invisible=c("col","col.sup"), title = "Representation of the Rows")

          print(plot)
          TRUE
        }
      },

      .plotell= function(image, ...){

        if (is.null(self$options$activecol)) return()

        else {
          abs_gui=self$options$abs
          ord_gui=self$options$ord
          fcol=paste("cos2", self$options$limcoscol)
          frow=paste("cos2", self$options$limcosrow)
          ellipsecol_gui=self$options$ellipsecol
          ellipserow_gui=self$options$ellipserow
          addillucol=self$options$addillucol

          res.ca=image$state

          if (addillucol == TRUE)
            adc = c("none")
          else
            adc = c("col.sup")

          if (ellipsecol_gui == TRUE && ellipserow_gui == TRUE)
            plot=ellipseCA(res.ca, axes=c(abs_gui, ord_gui), selectCol=fcol, selectRow=frow,
                           ellipse=c("col","row"), col.row="blue", col.col="red", invisible=adc, title = "Representation of the Ellipses for the Rows and the Columns")

          else if (ellipsecol_gui == TRUE && ellipserow_gui == FALSE)
            plot=ellipseCA(res.ca, axes=c(abs_gui, ord_gui), selectCol=fcol, selectRow=frow,
                           ellipse=c("col"), col.row="blue", col.col="red", invisible=adc, title = "Representation of the Ellipses for the Columns")

          else if (ellipsecol_gui == FALSE && ellipserow_gui == TRUE)
            plot=ellipseCA(res.ca, axes=c(abs_gui, ord_gui), selectCol=fcol, selectRow=frow,
                           ellipse=c("row"), col.row="blue", col.col="red", invisible=adc, title = "Representation of the Ellipses for the Rows")

          else
            plot=FactoMineR::plot.CA(res.ca, axes=c(abs_gui, ord_gui), selectCol=fcol, selectRow=frow, invisible=adc, title = "Superimposed Representation of the Rows and the Columns")

          print(plot)
          TRUE
        }
      },

      .plotclassif= function(image, ...){

        if (is.null(self$options$activecol)) return()

        else {
          abs_gui=self$options$abs
          ord_gui=self$options$ord

          res.classif=image$state
          plot=FactoMineR::plot.HCPC(res.classif, axes=c(abs_gui, ord_gui), choice="map", draw.tree = F, title="Representation of the Rows According to Clusters")
          print(plot)
          TRUE
        }
      },

      ### Helper functions ----
      .errorCheck = function() {
        if (length(self$options$activecol) < self$options$nbfact)
          jmvcore::reject(jmvcore::format('The number of factors is too low'))

      },
      
      .output = function(res.ca) {
        nFactors_out <- min(self$options$ncp,dim(res.ca)[1])
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
            scores <- as.numeric(res.ca$row$coord[, i])
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

        dataactcol=data.frame(self$data[,self$options$activecol])
        colnames(dataactcol)=self$options$activecol
        datacolsup=data.frame(self$data[,self$options$illustrativecol])
        colnames(datacolsup)=self$options$illustrativecol
        data=data.frame(dataactcol,datacolsup)

        if (is.null(self$options$indiv)==FALSE) {
          rownames(data)=self$data[[self$options$indiv]]
        }
        else
          rownames(data)=c(1:nrow(data))

        return(data)
      }
    )
)