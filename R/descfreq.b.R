
# This file is a generated template, your changes will not be overwritten

descfreqClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "descfreqClass",
    inherit = descfreqBase,
    private = list(

    #---------------------------------------------  
    #### Init + run functions ----

        .init = function() {
            if (is.null(self$options$columns)) {
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
            <p><b>What you should know before analyzing a contingency table in jamovi</b></p>
            <p>______________________________________________________________________________</p>
            <p> The MEDA module allows the statistical analysis of a contingency table, <I>i.e.</I> a 
            table that crosses the modalities of two qualitative variables, where, at the intersection of a row and a column, 
            the number of observations taking both the modality associated with the row and that associated with the column.</p>

            <p> Rows are characterised by columns (and vice versa) using a hypothesis test based on the hypergeometric law.</p>

            <p> One of the most common tests based on the hypergeometric distribution is the hypergeometric test. 
            It is used to assess whether an observed sample contains a statistically significant number of successes 
            (favorable outcomes) from a specific category when drawing a fixed-size sample from a finite population without replacement.</p>

            <p> Open the <b>music</b> dataset. Fill the Rows field with the Occupation variable, 
            which is actually the categories of our first qualitative variable of interest. 
            Fill the Columns field with all the other variables, the different genres of music.</p>

            <p> Two tables appear: the table of rows described by columns and the table of columns described by rows. 
            The first table shows that farmers are more likely to listen to French music and less likely to listen to jazz. 
            The second table shows that jazz is listened to more by executives and less by farmers.</p>
            <p>______________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
            )
            
        },

        .run = function() {
            
            ready <- TRUE
            if (is.null(self$options$columns) || length(self$options$columns)<2 || is.null(self$options$rows)){
                return()
                ready <- FALSE
                private$.errorCheck()
            }
            
            if (ready) {
                
                data <- private$.buildData()
                
                res.descfreqrow <- private$.descfreq(data)
                res.descfreqcol <- private$.descfreq(t(data))
                
                private$.printRowTables(res.descfreqrow)
                private$.printColTables(res.descfreqcol)
                
            }
        },
        
        ### Compute results ----
        .descfreq = function(data) {
            threshold = self$options$threshold/100
            FactoMineR::descfreq(data, proba = threshold)
        },
        
        .printRowTables = function(table) {
            
            desctable = self$results$descoftablerow
            
            for (i in 1:length(names(table))) {
                
                rowcolnames = row.names(table[[names(table)[i]]])
                
                if (is.null(dim(table[[names(table)[i]]])[1])==TRUE) next    
                
                for (j in 1:dim(table[[names(table)[i]]])[1]) {
                    
                    desctable$addRow(rowKey=as.numeric(paste0(i,j)), value=NULL)
                    
                    row = list()
                    row[["mod"]] = names(table)[i]
                    row[["rowcol"]] = rowcolnames[j]
                    row[["intern"]] = table[[names(table)[i]]][j,1]
                    row[["glob"]] = table[[names(table)[i]]][j,2]
                    row[["intfreq"]] = table[[names(table)[i]]][j,3]
                    row[["globfreq"]] = table[[names(table)[i]]][j,4]
                    row[["pvalue"]] = table[[names(table)[i]]][j,5]
                    row[["vtest"]] = table[[names(table)[i]]][j,6]
                    
                    desctable$setRow(rowKey=as.numeric(paste0(i,j)),values=row)
                }
            }
        },
        
        .printColTables = function(table) {
            
            desctable = self$results$descoftablecol
            
            for (i in 1:length(names(table))) {
                
                rowcolnames = row.names(table[[names(table)[i]]])
                
                if (is.null(dim(table[[names(table)[i]]])[1])==TRUE) next    
                
                for (j in 1:dim(table[[names(table)[i]]])[1]) {
                    
                    desctable$addRow(rowKey=as.numeric(paste0(i,j)), value=NULL)
                    
                    row = list()
                    row[["mod"]] = names(table)[i]
                    row[["rowcol"]] = rowcolnames[j]
                    row[["intern"]] = table[[names(table)[i]]][j,1]
                    row[["glob"]] = table[[names(table)[i]]][j,2]
                    row[["intfreq"]] = table[[names(table)[i]]][j,3]
                    row[["globfreq"]] = table[[names(table)[i]]][j,4]
                    row[["pvalue"]] = table[[names(table)[i]]][j,5]
                    row[["vtest"]] = table[[names(table)[i]]][j,6]
                    
                    desctable$setRow(rowKey=as.numeric(paste0(i,j)),values=row)
                }
            }
        },
        
        ### Helpers functions ----
        .errorCheck = function() {
            if (length(self$options$columns)<2) {
                jmvcore::reject("The number of columns is too low")
            }
        },
        
        .buildData = function() {
            
            data=data.frame(self$data[,self$options$columns])
            colnames(data)=self$options$columns
            
            if (is.null(self$options$rows)==FALSE) {
                rownames(data)=self$data[[self$options$rows]]
            }
            else
                rownames(data)=c(1:nrow(data))
            
            return(data)
        })
)
