
# This file is a generated template, your changes will not be overwritten

catdesClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "catdesClass",
    inherit = catdesBase,
    private = list(
      
      ### .run() ----
      .run = function() {
        
        if (is.null(self$options$vartochar) || is.null(self$options$descbyvar)) {
          
          return()
        
        }
        else {
          
          #Mise en place du nouveau jeu de donnée
          
          data1=data.frame(self$data[,self$options$vartochar])
          colnames(data1)=self$options$vartochar
          
          
          data2=data.frame(self$data[,self$options$descbyvar])
          colnames(data2)=self$options$descbyvar
          
          data=data.frame(data1, data2)
          
          if (is.numeric(data[,1])==TRUE) {
          
            res = private$.condes(data)
            
            # Hide the output tables of the catdes
            self$results$chigroup$setVisible(visible=FALSE)
            self$results$categgroup$categquali$setVisible(visible=FALSE)
            self$results$qtvargroup$setVisible(visible=FALSE)
            self$results$qtgroup$qt$setVisible(visible=FALSE)
          
            private$.printCondesCategTable(res)
            private$.printCondesCorTable(res)
            private$.printCondesR2Table(res)
          }
          else {
            
            res = private$.catdes(data)
            
            # Hide the output tables of the condes
            self$results$categgroup$categquanti$setVisible(visible=FALSE)
            self$results$qtgroup$qtcor$setVisible(visible=FALSE)
            self$results$categgroup$qualir2$setVisible(visible=FALSE)
            
            #Mise en place du tableau pour res$category sous R
            
            if (is.null(dim(res$category[[1]])) == FALSE) {
              
              j=1
              while (dim(res$category[[j]])[1] == 0)
                j=j+1 #j est le rang du premier data.frame de res$quanti dont le nombre de lignes n'est pas nul
              
              tab=cbind(names(res$category)[j],res$category[[j]])
              tab=as.data.frame(tab)
              A <- as.vector(rownames(tab)) #A contient les noms des modalités
              tab[,7] <- as.factor(A)
              colnames(tab)[1]="Category"
              
              for (i in (j+1):length(res$category)) {
                if (dim(res$category[[i]])[1] != 0) { #On ne rajoute au tableau final que les data.frame dont le nombre de lignes n'est pas nul
                  pretab=cbind(names(res$category)[i],res$category[[i]])
                  pretab=as.data.frame(pretab)
                  colnames(pretab)[1]="Category"
                  A <- as.vector(rownames(pretab)) #A contient les noms des modalités
                  pretab[,7] <-as.factor(A)
                  tab=rbind(tab,pretab)
                }
              }
              
              rownames(tab) <- c()
              tab <- tab[, c(1,7,2,3,4,5,6)] #Ordonner les colonnes
              colnames(tab)[2]="Category 2"
              
              for (i in 3:7)
                tab[,i]=as.numeric(as.character(tab[,i]))
            }
            
            
            #Mise en place du tableau pour res$quanti sous R
            
            if (is.null(dim(res$quanti[[1]])) == FALSE) {
              
              j=1
              while (dim(res$quanti[[j]])[1] == 0)
                j=j+1 #j est le rang du premier data.frame de res$quanti dont le nombre de lignes n'est pas nul
              
              tabqt=cbind(names(res$quanti)[j],res$quanti[[j]])
              tabqt=as.data.frame(tabqt)
              A <- as.vector(rownames(tabqt)) #A contient les noms des modalités
              tabqt[,8] <- as.factor(A)
              colnames(tabqt)[1]="Category"
              
              for (i in (j+1):length(res$quanti)) {
                if (dim(res$quanti[[i]])[1] != 0) { #On ne rajoute au tableau final que les data.frame dont le nombre de lignes n'est pas nul
                  pretab=cbind(names(res$quanti)[i],res$quanti[[i]])
                  pretab=as.data.frame(pretab)
                  colnames(pretab)[1]="Category"
                  A <- as.vector(rownames(pretab)) #A contient les noms des modalités
                  pretab[,8] <-as.factor(A)
                  tabqt=rbind(tabqt,pretab)
                }
              }
              
              rownames(tabqt) <- c()
              tabqt <- tabqt[, c(1,8,2,3,4,5,6,7)] #Ordonner les colonnes
              colnames(tabqt)[2]="Variable"
              
              for (i in 3:8)
                tabqt[,i]=as.numeric(as.character(tabqt[,i]))
            }
            
            
            #Affichage des résultats
            
            if ((is.null(res$category) == TRUE) && ((is.null(res$test.chi) == TRUE) || (dim(res$test.chi)[1] == 0)) && (is.null(res$quanti.var) == TRUE) && (is.null(res$quanti) == TRUE)) {
              return()
            }
            
            if ((is.null(dim(res$test.chi)[1]) == FALSE) && (dim(res$test.chi)[1] != 0)) {
              private$.chiTable(res)
            }
            else {
              self$results$chigroup$setVisible(visible=FALSE)
            }
            
            if (is.null(dim(res$category[[1]])) == FALSE) {
              private$.categoryTable(tab)
            }
            else {
              self$results$categgroup$setVisible(visible=FALSE)
            }
            
            if (is.null(res$quanti.var) == FALSE) {
              private$.qtvarTable(res)
            }
            else {
              self$results$qtvargroup$setVisible(visible=FALSE)
            }
            
            if (is.null(dim(res$quanti[[1]])) == FALSE) {
              private$.qtTable(tabqt)
            }
            else {
              self$results$qtgroup$setVisible(visible=FALSE)
            }
            
          }
            
        }
          
        
      },
      
      #Fonction de départ
      
      .catdes = function(res) {
        
        threshold=self$options$threshold/100
        
        FactoMineR::catdes(res, num.var=1, proba=threshold)
        
      },
      
      .condes = function(res) {
        
        threshold=self$options$threshold/100
        
        FactoMineR::condes(res, num.var=1, proba=threshold)
        
      },
      
      ### Table populating functions ----
      .chiTable = function(table){
        
        for (i in 1:dim(table$test.chi)[1]){
          self$results$chigroup$chi$addRow(rowKey=i, values=list(varchi=as.character(rownames(table$test.chi)[i]))) 
        }
        chipv=table$test.chi[,1]
        df=table$test.chi[,2]
        
        for (i in seq_along(chipv)) {
          row=list()
          row[["chipv"]]=chipv[i]
          row[["df"]]=df[i]
          self$results$chigroup$chi$setRow(rowNo=i, values = row)
        }
        
      },
      
      .categoryTable = function(tab){
        
        for (i in 1:dim(tab)[1]){
          self$results$categgroup$categquali$addRow(rowKey=i, values=list(varcateg=as.character(tab[,1])[i])) 
        }
        
        for (i in 1:dim(tab)[1]) {
          row=list()
          row[["vardesccateg"]]=as.character(tab[,2])[i]
          row[["clamod"]]=tab[,3][i]
          row[["modcla"]]=tab[,4][i]
          row[["global"]]=tab[,5][i]
          row[["categpv"]]=tab[,6][i]
          row[["vtest"]]=tab[,7][i]
          self$results$categgroup$categquali$setRow(rowNo=i, values = row)
        }
        
      },
      
      .printCondesCategTable = function(table) {
        
        if (is.null(table$category)==TRUE) {
          
          return()
          
        }
        else{
          
          for (i in 1:nrow(table$category)) {
            
            self$results$categgroup$categquanti$addRow(rowKey=i,value=NULL)
            
            row=list()
            row[["varcateg"]] = strsplit(row.names(table$category),"=")[[i]][1]
            row[["vardesccateg"]] = strsplit(row.names(table$category),"=")[[i]][2]
            row[["estimate"]] = table$category[i,1]
            row[["categpv"]] = table$category[i,2]
            
            self$results$categgroup$categquanti$setRow(rowKey=i, values=row)
            
          }
          
        }
        
      },
      
      .printCondesCorTable = function(table) {
        
        if (is.null(table$quanti)==TRUE) {
          
          return()
          
        }
        else {
          
          for (i in 1:nrow(table$quanti)) {
            
            self$results$qtgroup$qtcor$addRow(rowKey=i, value=NULL)
            
            row=list()
            row[["varcor"]] = row.names(table$quanti)[i]
            row[["cor"]] = table$quanti[i,1]
            row[["corpvalue"]] = table$quanti[i,2]
            
            self$results$qtgroup$qtcor$setRow(rowKey=i, values=row)
            
          }
        }
      },
      
      .printCondesR2Table = function(table) {
        
        if (is.null(table$quali)==TRUE) {
          
          return()
          
        }
        else {
          
          for (i in 1:nrow(table$quali)) {
            
            self$results$categgroup$qualir2$addRow(rowKey=i, value=NULL)
            
            row=list()
            row[["varr2"]] = row.names(table$quali)[i]
            row[["r2"]] = table$quali[i,1]
            row[["r2pvalue"]] = table$quali[i,2]
            
            self$results$categgroup$qualir2$setRow(rowKey=i, values=row)
            
          }
          
        }
        
      },
      
      .qtvarTable = function(table){
        
        for (i in 1:dim(table$quanti.var)[1]){
          self$results$qtvargroup$qtvar$addRow(rowKey=i, values=list(varqtvar=as.character(rownames(table$quanti.var)[i]))) 
        } 
        scc=table$quanti.var[,1]
        qtvarpv=table$quanti.var[,2]
        
        for (i in seq_along(qtvarpv)) {
          row=list()
          row[["scc"]]=scc[i]
          row[["qtvarpv"]]=qtvarpv[i]
          self$results$qtvargroup$qtvar$setRow(rowNo=i, values = row)
        }
        
      },
      
      .qtTable = function(tabqt){
        
        for (i in 1:dim(tabqt)[1]){
          self$results$qtgroup$qt$addRow(rowKey=i, values=list(varqt=as.character(tabqt[,1])[i])) 
        }
        
        for (i in 1:dim(tabqt)[1]) {
          row=list()
          row[["vardescqt"]]=as.character(tabqt[,2])[i]
          row[["vtestqt"]]=tabqt[,3][i]
          row[["meancateg"]]=tabqt[,4][i]
          row[["overallmean"]]=tabqt[,5][i]
          row[["sdcateg"]]=tabqt[,6][i]
          row[["overallsd"]]=tabqt[,7][i]
          row[["qtpv"]]=tabqt[,8][i]
          self$results$qtgroup$qt$setRow(rowNo=i, values = row)
        }
        
      }
      
    )
)