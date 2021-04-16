
# This file is a generated template, your changes will not be overwritten

descfreqClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "descfreqClass",
    inherit = descfreqBase,
    private = list(
        .run = function() {
            
            ready <- TRUE
            if (is.null(self$options$columns) || is.null(self$options$rows)){
                return()
                ready <- FALSE
            }
            
            if (ready) {
                
                data <- private$.buildData()
                
                if (self$options$coldesc==FALSE) res.descfreq <- private$.descfreq(data)
                else res.descfreq <- private$.descfreq(t(data))
                
                private$.printTables(res.descfreq)
            }
        },
        
        ### Compute results ----
        .descfreq = function(data) {
            threshold = self$options$threshold
            FactoMineR::descfreq(data, proba = threshold)
        },
        
        .printTables = function(table) {
            
            desctable = self$results$descoftable
            
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
