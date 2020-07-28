# This file is a generated template, your changes will not be overwritten

CAClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "CAClass",
    inherit = CABase,
    private = list(
      
      #Fonction de calcul .CA
      
      .CA = function(donnees) {
        
        dataactcol=data.frame(self$data[,self$options$activecol])
        colnames(dataactcol)=self$options$activecol
        
        datacolsup=data.frame(self$data[,self$options$illustrativecol])
        colnames(datacolsup)=self$options$illustrativecol 
        
        donnees=data.frame(dataactcol,datacolsup)
        
        if (is.null(self$options$indiv)==FALSE) {
          rownames(donnees)=self$data[[self$options$indiv]]
        }
        else
          rownames(donnees)=c(1:nrow(donnees))
        
        actcol_gui=self$options$activecol
        illucol_gui=self$options$illustrativecol
        nbfact_gui=self$options$nbfact
        
        if (is.null(illucol_gui) == FALSE) {
          FactoMineR::CA(donnees, ncp = nbfact_gui, 
                        col.sup=(length(actcol_gui)+1):(length(actcol_gui)+length(illucol_gui)), 
                        graph=FALSE)
        }
        else {
          FactoMineR::CA(donnees, ncp = nbfact_gui, graph=FALSE)
        }
        
      },
      
      ##Fonction de calcul .chisq
      
      .chisq = function(donnees) {
        
        dataactcol=data.frame(self$data[,self$options$activecol])
        colnames(dataactcol)=self$options$activecol
        
        datacolsup=data.frame(self$data[,self$options$illustrativecol])
        colnames(datacolsup)=self$options$illustrativecol 
        
        donnees=data.frame(dataactcol,datacolsup)
        
        chisq.test(donnees)
        
      },
      
      ##Fonction de calcul .dimdesc
      
      .dimdesc = function(table) {
        
        nbfact_gui=self$options$nbfact
        
        ddca = dimdesc(table, axes=1:nbfact_gui)
        
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
      
      
      #Fonction .run
      
      .run = function() {
        
        if (is.null(self$options$activecol)) return()
        
        #Affichage d'un message d'erreur
        
        else if (length(self$options$activecol) <= self$options$nbfact)
          self$results$descofdimgroup$descofdim$setError("The number of factors is too low")
        
        else{

          res.ca = private$.CA(donnees)
          
          res.xsq = private$.chisq(donnees)
          private$.chideux(res.xsq)
          tab = private$.dimdesc(res.ca)
          private$.dodTable(tab)
          private$.printeigenTable(res.ca)
          
          self$results$ploticol$setState(res.ca)
          self$results$plotirow$setState(res.ca)
          self$results$plotell$setState(res.ca)
          
        }
      },
      
      #Fonction de mise en forme du tableau test du Khi-deux
      
      .chideux = function(res.xsq) {
        
        self$results$xsqgroup$xsq$setRow(rowNo=1, values=list(
          xsquared=res.xsq$statistic,
          df=res.xsq$parameter,
          pvxsq=res.xsq$p.value
        ))
        
      },
      
      
      #Fonction de mise en forme du tableau des coordonnees de chaque point

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

      #Fonction de mise en forme du tableau des valeurs propres
      
      .printeigenTable = function(table){
        
        for (i in 1:dim(table$eig)[1]){
          self$results$eigengroup$eigen$addRow(rowKey=i, values=list(component=as.character(i))) 
        }
        eigen=table$eig[,1]
        purcent=table$eig[,2]
        purcentcum=table$eig[,3]
        
        for (i in seq_along(eigen)) {
          row=list()
          row[["eigenvalue"]]=eigen[i]
          row[["purcent"]]=purcent[i]
          row[["purcentcum"]]=purcentcum[i]
          self$results$eigengroup$eigen$setRow(rowNo=i, values = row)
        }
      },
      
      #Fonction de mise en forme du graphique des modalites colonnes
      
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
            plot=plot.CA(res.ca, axes=c(abs_gui, ord_gui),
                         selectCol=fcol, selectRow=frow, invisible="row", title = "Representation of the columns")
          else
            plot=plot.CA(res.ca, axes=c(abs_gui, ord_gui),
                         selectCol=fcol, selectRow=frow, invisible=c("row","col.sup"), title = "Representation of the columns")
          
          print(plot)
          TRUE
        }
      },
      
      #Fonction de mise en forme du graphique des modalites lignes
      
      .plotrow= function(image, ...){
        
        if (is.null(self$options$activecol)) return()
        
        else {
          abs_gui=self$options$abs
          ord_gui=self$options$ord
          fcol=paste("cos2", self$options$limcoscol)
          frow=paste("cos2", self$options$limcosrow)
          
          res.ca=image$state
          
            plot=plot.CA(res.ca, axes=c(abs_gui, ord_gui),
                         selectCol=fcol, selectRow=frow, invisible=c("col","col.sup"), title = "Representation of the rows")
          
          print(plot)
          TRUE
        }
      },
      
      #Fonction de mise en forme du graphique avec ellipses
      
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
                           ellipse=c("col","row"), col.row="blue", col.col="red", invisible=adc, title = "Representation of the ellipses for the rows and the columns")
          
          else if (ellipsecol_gui == TRUE && ellipserow_gui == FALSE)
            plot=ellipseCA(res.ca, axes=c(abs_gui, ord_gui), selectCol=fcol, selectRow=frow,
                           ellipse=c("col"), col.row="blue", col.col="red", invisible=adc, title = "Representation of the ellipses for the columns")
          
          else if (ellipsecol_gui == FALSE && ellipserow_gui == TRUE)
            plot=ellipseCA(res.ca, axes=c(abs_gui, ord_gui), selectCol=fcol, selectRow=frow,
                           ellipse=c("row"), col.row="blue", col.col="red", invisible=adc, title = "Representation of the ellipses for the rows")
          
          else
            plot=plot.CA(res.ca, axes=c(abs_gui, ord_gui), selectCol=fcol, selectRow=frow, invisible=adc, title = "Superimposed representation of the rows and the columns")
          
          print(plot)
          TRUE
        }
      }        
    )
)
