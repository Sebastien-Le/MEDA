
# This file is a generated template, your changes will not be overwritten

catdesClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "catdesClass",
    inherit = catdesBase,
    private = list(
      .run = function() {
        
        if (is.null(self$options$vartochar)) return()
        
        else if (is.null(self$options$descbyvar)) return()
        
        else {
          
          #Mise en place du nouveau jeu de donnée
          
          data1=data.frame(self$data[,self$options$vartochar])
          colnames(data1)=self$options$vartochar
          
          data2=data.frame(self$data[,self$options$descbyvar])
          colnames(data2)=self$options$descbyvar
          
          data=data.frame(data1, data2)
          
          res.catdes = private$.catdes(data)
          
          
          #Mise en place du tableau pour res.catdes$category sous R
          
          if (is.null(dim(res.catdes$category[[1]])) == FALSE) {
            
            j=1
            while (dim(res.catdes$category[[j]])[1] == 0)
              j=j+1 #j est le rang du premier data.frame de res.catdes$quanti dont le nombre de lignes n'est pas nul
            
            tab=cbind(names(res.catdes$category)[j],res.catdes$category[[j]])
            tab=as.data.frame(tab)
            A <- as.vector(rownames(tab)) #A contient les noms des modalités
            tab[,7] <- as.factor(A)
            colnames(tab)[1]="Category"
            
            for (i in (j+1):length(res.catdes$category)) {
              if (dim(res.catdes$category[[i]])[1] != 0) { #On ne rajoute au tableau final que les data.frame dont le nombre de lignes n'est pas nul
                pretab=cbind(names(res.catdes$category)[i],res.catdes$category[[i]])
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
          
          
          #Mise en place du tableau pour res.catdes$quanti sous R
          
          if (is.null(dim(res.catdes$quanti[[1]])) == FALSE) {
            
            j=1
            while (dim(res.catdes$quanti[[j]])[1] == 0)
              j=j+1 #j est le rang du premier data.frame de res.catdes$quanti dont le nombre de lignes n'est pas nul
            
            tabqt=cbind(names(res.catdes$quanti)[j],res.catdes$quanti[[j]])
            tabqt=as.data.frame(tabqt)
            A <- as.vector(rownames(tabqt)) #A contient les noms des modalités
            tabqt[,8] <- as.factor(A)
            colnames(tabqt)[1]="Category"
            
            for (i in (j+1):length(res.catdes$quanti)) {
              if (dim(res.catdes$quanti[[i]])[1] != 0) { #On ne rajoute au tableau final que les data.frame dont le nombre de lignes n'est pas nul
                pretab=cbind(names(res.catdes$quanti)[i],res.catdes$quanti[[i]])
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
          
          if ((is.null(res.catdes$category) == TRUE) && ((is.null(res.catdes$test.chi) == TRUE) || (dim(res.catdes$test.chi)[1] == 0)) && (is.null(res.catdes$quanti.var) == TRUE) && (is.null(res.catdes$quanti) == TRUE)) {
            return()
          }
          
          if ((is.null(dim(res.catdes$test.chi)[1]) == FALSE) && (dim(res.catdes$test.chi)[1] != 0)) {
            private$.chiTable(res.catdes)
          }
          else {
            self$results$chigroup$setVisible(visible=FALSE)
          }
          
          if (is.null(dim(res.catdes$category[[1]])) == FALSE) {
            private$.categoryTable(tab)
          }
          else {
            self$results$categgroup$setVisible(visible=FALSE)
          }
          
          if (is.null(res.catdes$quanti.var) == FALSE) {
            private$.qtvarTable(res.catdes)
          }
          else {
            self$results$qtvargroup$setVisible(visible=FALSE)
          }
          
          if (is.null(dim(res.catdes$quanti[[1]])) == FALSE) {
            private$.qtTable(tabqt)
          }
          else {
            self$results$qtgroup$setVisible(visible=FALSE)
          }
          
        }
        
      },
      
      #Fonction de départ
      
      .catdes = function(res) {
        
        threshold=self$options$threshold
        
        FactoMineR::catdes(res, num.var=1, proba=threshold)
        
      },
      
      #Fonctions de remplissage
      
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
          self$results$categgroup$categ$addRow(rowKey=i, values=list(varcateg=as.character(tab[,1])[i])) 
        }
        
        for (i in 1:dim(tab)[1]) {
          row=list()
          row[["vardesccateg"]]=as.character(tab[,2])[i]
          row[["clamod"]]=tab[,3][i]
          row[["modcla"]]=tab[,4][i]
          row[["global"]]=tab[,5][i]
          row[["categpv"]]=tab[,6][i]
          row[["vtest"]]=tab[,7][i]
          self$results$categgroup$categ$setRow(rowNo=i, values = row)
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