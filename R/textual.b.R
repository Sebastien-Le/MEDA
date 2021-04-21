
# This file is a generated template, your changes will not be overwritten

textualClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "textualClass",
    inherit = textualBase,
    private = list(
      .run = function() {
        
        if (is.null(self$options$individuals) || is.null(self$options$words)) 
          return()
        
        else {
          
          data=self$data
          res.textual=private$.textual(data)
          
          #Mise en place du tableau pour descfreq
          
          dfres=private$.descfreq(res.textual)
          
          t = 0
          for (i in 1:length(dfres)) {
            if (is.null(dfres[[i]]) == FALSE)
              t = t+1
          }
          
          #t sert a savoir si dfres est completement NULL ou non. 
          #Si oui, t=0 et la suite n'est pas faite. 
          #Si non, t=1 et la suite est faite.
          
          if (t != 0) {
            
            # j=1
            # while (is.null(dim(dfres[[j]])[1]))
            #   j=j+1
            # 
            # tab=cbind(names(dfres)[j],dfres[[j]])
            # A <- as.vector(rownames(tab))
            # tab = as.data.frame(tab)
            # tab[,8] <- as.factor(A)
            # 
            # if (j < length(dfres)) {
            #   for (i in (j+1):length(dfres)) {
            #     if (dim(dfres[[i]])[1] != 0) {
            #       pretab=cbind(names(dfres)[i],dfres[[i]])
            #       A <- as.vector(rownames(pretab))
            #       pretab = as.data.frame(pretab)
            #       pretab[,8] <-as.factor(A)
            #       tab=rbind(tab,pretab)
            #     }
            #   }
            # }
            # 
            # rownames(tab) <- c()
            # tab <- tab[, c(1,8,2,3,4,5,6,7)]
            # 
            # for (i in 3:8)
            #   tab[,i]=as.numeric(as.character(tab[,i]))
            ###############
            j <- 1
            xinit <- as.data.frame(dfres[[j]])
            xxinit <- cbind(rep(names(dfres)[j],dim(xinit)[1]),rownames(xinit),xinit)
            colnames(xxinit)[1] <- "Modality"
            colnames(xxinit)[2] <- "Word"
            rownames(xxinit) <- NULL

            for (j in 1:(length(dfres)-1)) {
              j <- j+1
              xj <- as.data.frame(dfres[[j]])
              xxj <- cbind(rep(names(dfres)[j],dim(xj)[1]),rownames(xj),xj)
              colnames(xxj)[1] <- "Modality"
              colnames(xxj)[2] <- "Word"
              rownames(xxj) <- NULL
              xxinit <- rbind(xxinit,xxj)
            }

            tab <- xxinit
            ###############
          #}
          
          #Results
          
          private$.populateTEXTUALTable(res.textual)
          private$.populateCHIDEUXTable(res.textual)
          
          
          # if (is.null(dim(dfres[[1]])[1]) == FALSE)
            private$.populateDFTable(tab)
          # else {
          #   self$results$dfresgroup$dfres$setVisible(FALSE)
          # }
           } #à la place de la ligne 80
          
          else return()
          
        }
      },
      
      
      .textual = function(data) {
        FactoMineR::textual(data, num.text = 2, contingence.by = 1, sep.word = ";")
      },
      
      .descfreq = function(res) {
        threshold=self$options$thres/100
        FactoMineR::descfreq(res$cont.table, proba = threshold)
      },
      
      
      .populateTEXTUALTable = function(table) {
        
        individuals=levels(self$data[[self$options$individuals]]) #nom de la variable à décrire
        words=self$data[[self$options$words]] #nom de la variable textuelle
        textual=self$results$textualgroup$textual #fait réf. au tableau de contingence dans les sorties jamovi
        coltable=colnames(table$cont.table) #tableau de contingence
        
        textual$addColumn(name="rownames",title="",type="text") #on renseigne le tableau de contingence de la sortie jamovi
        for (i in 1:length(coltable))
          textual$addColumn(name=coltable[i], title=coltable[i], type="integer")
        
        for (i in seq_along(individuals)) {#
          row=list()
          row[["rownames"]]=rownames(table$cont.table)[i]
          
          for (j in 1:length(coltable)) {
            row[[coltable[j]]]=table$cont.table[i,j] #on recopie le tableau de contingence
          }
          textual$addRow(rowKey=i, values=row)
                                           }#
        
        total=list()
        total[["rownames"]]="Nb.words"
        
        for (j in 1:length(coltable))
          total[[coltable[j]]]=sum(table$cont.table[,j])
        
        textual$addRow(rowKey=length(coltable)+1, values = total)
      },
      
      
      .populateCHIDEUXTable = function(table) {
        
        res.chisq=chisq.test(table$cont.table)
        
        self$results$chideuxgroup$chideux$setRow(rowNo=1, values = list(
          value=res.chisq$statistic,
          df=res.chisq$parameter,
          pvalue=res.chisq$p.value
        ))
        
      },
      
      .populateDFTable= function(tab){
        
        # for (i in 1:dim(tab)[1]){
        #   self$results$dfresgroup$dfres$addRow(rowKey=i, values=list(component=as.character(tab[,1])[i])) #Méthode addRow
        # }
        
        for (i in 1:(dim(tab)[1])) {
          row=list()
          self$results$dfresgroup$dfres$addRow(rowKey=i, values=list(component=as.character(tab[,1])[i]))
          row[["word"]]=as.character(tab[,2])[i]
          row[["internper"]]=tab[,3][i]
          row[["globper"]]=tab[,4][i]
          row[["internfreq"]]=tab[,5][i]
          row[["globfreq"]]=tab[,6][i]
          row[["pvaluedfres"]]=tab[,7][i]
          row[["vtest"]]=tab[,8][i]
          self$results$dfresgroup$dfres$setRow(rowKey=i, values = row) #Méthode setRow
        }
        
      }
      
    )
)