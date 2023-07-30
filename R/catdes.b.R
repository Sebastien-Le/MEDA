
# This file is a generated template, your changes will not be overwritten

catdesClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "catdesClass",
    inherit = catdesBase,
    active = list(
        dataProcessed = function() {
            if (is.null(private$.dataProcessed))
                private$.dataProcessed <- private$.buildData()

            return(private$.dataProcessed)
        },
        condesResult = function() {
            if (is.null(private$.condesResult))
                private$.condesResult <- private$.getcondesResult()

            return(private$.condesResult)
        },
        catdesResult = function() {
            if (is.null(private$.catdesResult))
                private$.catdesResult <- private$.getcatdesResult()

            return(private$.catdesResult)
        },
        catdesCategoryResult = function() {
            if (is.null(private$.catdesCategoryResult))
                private$.catdesCategoryResult <- private$.getcatdesCategoryResult()

            return(private$.catdesCategoryResult)
        },
        catdesCategoryQuantiResult = function() {
            if (is.null(private$.catdesCategoryQuantiResult))
                private$.catdesCategoryQuantiResult <- private$.getcatdesCategoryQuantiResult()

            return(private$.catdesCategoryQuantiResult)
        }
    ),
    private = list(
      
      .dataProcessed = NULL,
      .catdesResult = NULL,
      .condesResult = NULL,
      .catdesCategoryResult = NULL,
      .catdesCategoryQuantiResult = NULL,

    #---------------------------------------------  
    #### Init + run functions ----

        .init = function() {
            if (is.null(self$options$descbyvar)) {
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
            <p><b>What you should know before characterizing a variable in jamovi</b></p>
            <p>______________________________________________________________________________</p>
            <p> The MEDA module provides characterisation of both quantitative and qualitative variables based on their relationships 
            with all other variables in the dataset. MEDA uses specific statistical tests for different scenarios. 
            For example, when dealing with two quantitative variables, MEDA uses the correlation coefficient; when dealing with 
            two qualitative variables, it uses the Chi-square test of independence. Furthermore, when one variable is quantitative 
            and the other qualitative, the module uses the coefficient of determination to establish their relationship. 
            This comprehensive approach ensures a thorough analysis of the dataset and provides a comprehensive understanding of the 
            relationships between the variables.</p>

            <p> At a more granular level, the MEDA module performs association tests specifically for qualitative variables. 
            Where there are two qualitative variables, MEDA examines the potential over- or under-representation of one category 
            within another in relation to the total population. In cases where there is both a quantitative and a qualitative variable, 
            MEDA examines whether the average value of the quantitative variable within a subgroup defined by a particular category is 
            higher or lower than the average value for the whole population.</p>

           <p> Open the <b>decathlon</b> dataset. Choose <I>Competition</I> as the variable to characterize. Select all others except <I>Ident</I> and
           <I>Rank</I> to characterize <I>Competition</I>. The variable <I>100m</I> is linked to <I>Competition</I>. Athletes tend to perform 
           slower at the Decastar than at the Olympic Games, 
           with an average 100m time of 11.18 seconds at the Decastar compared to an average of 10.92 seconds at the Olympic Games.</p>

           <p> Adjusting the significance threshold to 20 would produce similar results for the shot put variable. 
           Athletes consistently perform better in the Olympic Games, with an average distance of 14.63 metres 
           compared to 14.16 metres in the Decastar event.</p>

            <p>______________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
            )
            
        },

      .run = function() {
        
        if (is.null(self$options$vartochar) || is.null(self$options$descbyvar)) {
          
          return()
        
        }
        else {
          if (is.numeric(self$dataProcessed[,1])==TRUE) {
            # Hide the output tables of the catdes
            self$results$chigroup$setVisible(visible=FALSE)
            self$results$categgroup$categquali$setVisible(visible=FALSE)
            self$results$qtvargroup$setVisible(visible=FALSE)
            self$results$qtgroup$qt$setVisible(visible=FALSE)

            if (names(self$condesResult)=="call"){
              return()
            }
          
            if (any(grepl("quanti",names(self$condesResult))) == TRUE) {
              private$.printCondesCorTable()
            }
            else {
              self$results$qtgroup$qtcor$setVisible(visible=FALSE)
            }

            if (any(grepl("quali",names(self$condesResult))) == TRUE) {
              private$.printCondesR2Table()
            }
            else {
              self$results$categgroup$qualir2$setVisible(visible=FALSE)
            }

            if (any(grepl("category",names(self$condesResult))) == TRUE) {
              private$.printCondesCategTable()
            }
            else {
              self$results$categgroup$categquanti$setVisible(visible=FALSE)
            }            
          }
          else {
            # Hide the output tables of the condes
            self$results$categgroup$categquanti$setVisible(visible=FALSE)
            self$results$qtgroup$qtcor$setVisible(visible=FALSE)
            self$results$categgroup$qualir2$setVisible(visible=FALSE)

            if (names(self$catdesResult)=="call"){
              return()
            }

            if (any(grepl("test.chi2",names(self$catdesResult))) == TRUE) {
              private$.chiTable()
            }
            else {
              self$results$chigroup$setVisible(visible=FALSE)
            }
            
            if (any(grepl("category",names(self$catdesResult))) == TRUE){
              private$.categoryTable()
            }
            else {
              self$results$categgroup$setVisible(visible=FALSE)
            }
            
            if (any(grepl("quanti.var",names(self$catdesResult))) == TRUE){
              private$.qtvarTable()
            }
            else {
              self$results$qtvargroup$setVisible(visible=FALSE)
            }

            if (any(grepl("quanti",names(self$catdesResult))) == TRUE){
              private$.qtTable()
            }
            else {
              self$results$qtgroup$setVisible(visible=FALSE)
            }            
          }
        }        
      },
      
      #Fonction

      .getcatdesResult = function() {        
        threshold=self$options$threshold/100
        r <- FactoMineR::catdes(self$dataProcessed, num.var=1, proba=threshold)

        private$.catdesResult <- r
        return(private$.catdesResult)
      },

      .getcondesResult = function() {
        threshold=self$options$threshold/100
        r <- FactoMineR::condes(self$dataProcessed, num.var=1, proba=threshold)

        private$.condesResult <- r
        return(private$.condesResult)
      },

      .getcatdesCategoryResult = function() {
        threshold=self$options$threshold/100
        res <- self$catdesResult
        nlev <- nlevels(self$dataProcessed[,1])
        lev <- levels(self$dataProcessed[,1])
        a <- is.null(dim(res$category[[1]]))
        for (i in 2:nlev) a <- c(a,is.null(dim(res$category[[i]])))
        dta=NULL
        
          if (length(which(a==FALSE))==1){
            dta <- data.frame(row.names(res$category[[which(a==FALSE)]]),res$category[[which(a==FALSE)]])
            niv <- rep(lev[which(a==FALSE)],dim(res$category[[which(a==FALSE)]])[1])
            dta <- data.frame(niv,dta)
            names(dta)[1:2] <- c("Level","Category")
            rownames(dta) <- NULL
          }
          else {
            dta <- data.frame(row.names(res$category[[which(a==FALSE)[1]]]),res$category[[which(a==FALSE)[1]]])
            niv <- rep(lev[which(a==FALSE)[1]],dim(res$category[[which(a==FALSE)[1]]])[1])
            dta <- data.frame(niv,dta)
            names(dta)[1:2] <- c("Level","Category")
            rownames(dta) <- NULL

            for (j in which(a==FALSE)[-1]){
              dtaj <- data.frame(row.names(res$category[[which(a==FALSE)[1]]]),res$category[[which(a==FALSE)[1]]])
              nivj <- rep(lev[which(a==FALSE)[1]],dim(res$category[[which(a==FALSE)[1]]])[1])
              dtaj <- data.frame(nivj,dtaj)
              names(dtaj)[1:2] <- c("Level","Category")
              rownames(dtaj) <- NULL
              rbind(dta,dtaj)
            }
          }

          if (length(which(a==FALSE))==1){
            dta <- data.frame(row.names(res$category[[which(a==FALSE)]]),res$category[[which(a==FALSE)]])
            niv <- rep(lev[which(a==FALSE)],dim(res$category[[which(a==FALSE)]])[1])
            dta <- data.frame(niv,dta)
            names(dta)[1:2] <- c("Level","Category")
            rownames(dta) <- NULL
          }
          else {
            dta <- data.frame(row.names(res$category[[which(a==FALSE)[1]]]),res$category[[which(a==FALSE)[1]]])
            niv <- rep(lev[which(a==FALSE)[1]],dim(res$category[[which(a==FALSE)[1]]])[1])
            dta <- data.frame(niv,dta)
            names(dta)[1:2] <- c("Level","Category")
            rownames(dta) <- NULL
            
            for (j in which(a==FALSE)[-1]){
              dtaj <- data.frame(row.names(res$category[[j]]),res$category[[j]])
              nivj <- rep(lev[j],dim(res$category[[j]])[1])
              dtaj <- data.frame(nivj,dtaj)
              names(dtaj)[1:2] <- c("Level","Category")
              rownames(dtaj) <- NULL
              dta <- rbind(dta,dtaj)
              }
          }

        private$.catdesCategoryResult <- dta
        return(private$.catdesCategoryResult)
      },

      .getcatdesCategoryQuantiResult = function() {
        threshold=self$options$threshold/100
        res <- self$catdesResult
        nlev <- nlevels(self$dataProcessed[,1])
        lev <- levels(self$dataProcessed[,1])
        a <- is.null(dim(res$quanti[[1]]))
        for (i in 2:nlev) a <- c(a,is.null(dim(res$quanti[[i]])))
        dta=NULL
        
          if (length(which(a==FALSE))==1){
            dta <- data.frame(row.names(res$quanti[[which(a==FALSE)]]),res$quanti[[which(a==FALSE)]])
            niv <- rep(lev[which(a==FALSE)],dim(res$quanti[[which(a==FALSE)]])[1])
            dta <- data.frame(niv,dta)
            names(dta)[1:2] <- c("Level","Category")
            rownames(dta) <- NULL
          }
          else {
            dta <- data.frame(row.names(res$quanti[[which(a==FALSE)[1]]]),res$quanti[[which(a==FALSE)[1]]])
            niv <- rep(lev[which(a==FALSE)[1]],dim(res$quanti[[which(a==FALSE)[1]]])[1])
            dta <- data.frame(niv,dta)
            names(dta)[1:2] <- c("Level","Category")
            rownames(dta) <- NULL

            for (j in which(a==FALSE)[-1]){
              dtaj <- data.frame(row.names(res$quanti[[which(a==FALSE)[1]]]),res$quanti[[which(a==FALSE)[1]]])
              nivj <- rep(lev[which(a==FALSE)[1]],dim(res$quanti[[which(a==FALSE)[1]]])[1])
              dtaj <- data.frame(nivj,dtaj)
              names(dtaj)[1:2] <- c("Level","Category")
              rownames(dtaj) <- NULL
              rbind(dta,dtaj)
            }
          }

          if (length(which(a==FALSE))==1){
            dta <- data.frame(row.names(res$quanti[[which(a==FALSE)]]),res$quanti[[which(a==FALSE)]])
            niv <- rep(lev[which(a==FALSE)],dim(res$quanti[[which(a==FALSE)]])[1])
            dta <- data.frame(niv,dta)
            names(dta)[1:2] <- c("Level","Category")
            rownames(dta) <- NULL
          }
          else {
            dta <- data.frame(row.names(res$quanti[[which(a==FALSE)[1]]]),res$quanti[[which(a==FALSE)[1]]])
            niv <- rep(lev[which(a==FALSE)[1]],dim(res$quanti[[which(a==FALSE)[1]]])[1])
            dta <- data.frame(niv,dta)
            names(dta)[1:2] <- c("Level","Category")
            rownames(dta) <- NULL
            
            for (j in which(a==FALSE)[-1]){
              dtaj <- data.frame(row.names(res$quanti[[j]]),res$quanti[[j]])
              nivj <- rep(lev[j],dim(res$quanti[[j]])[1])
              dtaj <- data.frame(nivj,dtaj)
              names(dtaj)[1:2] <- c("Level","Category")
              rownames(dtaj) <- NULL
              dta <- rbind(dta,dtaj)
              }
          }

        private$.catdesCategoryQuantiResult <- dta
        return(private$.catdesCategoryQuantiResult)

      },
      
      ### Table populating functions ----
      .chiTable = function(){
        table <- self$catdesResult

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
      
      .categoryTable = function(){
        tab <- self$catdesCategoryResult
        
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
      
      .printCondesCategTable = function() {
        table <- self$condesResult
          for (i in 1:nrow(table$category)) {
            
            self$results$categgroup$categquanti$addRow(rowKey=i,value=NULL)
            
            row=list()
            row[["varcateg"]] = strsplit(row.names(table$category),"=")[[i]][1]
            row[["vardesccateg"]] = strsplit(row.names(table$category),"=")[[i]][2]
            row[["estimate"]] = table$category[i,1]
            row[["categpv"]] = table$category[i,2]
            
            self$results$categgroup$categquanti$setRow(rowKey=i, values=row)
            
          }       
      },
      
      .printCondesCorTable = function() {
        table <- self$condesResult
          for (i in 1:nrow(table$quanti)) {
            
            self$results$qtgroup$qtcor$addRow(rowKey=i, value=NULL)
            
            row=list()
            row[["varcor"]] = row.names(table$quanti)[i]
            row[["cor"]] = table$quanti[i,1]
            row[["corpvalue"]] = table$quanti[i,2]
            
            self$results$qtgroup$qtcor$setRow(rowKey=i, values=row)
            
          }
      },
      
      .printCondesR2Table = function() {
        table <- self$condesResult
        for (i in 1:nrow(table$quali)) {
                      
            self$results$categgroup$qualir2$addRow(rowKey=i, value=NULL)
            
            row=list()
            row[["varr2"]] = row.names(table$quali)[i]
            row[["r2"]] = table$quali[i,1]
            row[["r2pvalue"]] = table$quali[i,2]
            
            self$results$categgroup$qualir2$setRow(rowKey=i, values=row)
          }
      },
      
      .qtvarTable = function(){
        table <- self$catdesResult
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
      
      .qtTable = function(){
        tabqt <- self$catdesCategoryQuantiResult
        
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
      },

      .buildData = function() {
        data1=data.frame(self$data[,self$options$vartochar])
        colnames(data1)=self$options$vartochar    
        data2=data.frame(self$data[,self$options$descbyvar])
        colnames(data2)=self$options$descbyvar
        data=data.frame(data1, data2)
        
        return(data)
      }
    )
)