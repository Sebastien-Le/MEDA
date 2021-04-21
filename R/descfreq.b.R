
# This file is a generated template, your changes will not be overwritten

descfreqClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "descfreqClass",
    inherit = descfreqBase,
    private = list(
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
