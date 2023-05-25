
# This file is a generated template, your changes will not be overwritten

#MFAClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
MFAClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "MFAClass",
  inherit = MFABase,
  private = list(
    
    
    #### Init + run functions ----
    .run = function() {
      
      ready <- TRUE
      if ((is.null(self$options$quantivar) || length(self$options$quantivar) < self$options$nFactors) && (is.null(self$options$qualivar) || length(self$options$qualivar) < self$options$nFactors)){
        return()
        ready <- FALSE
      }
      #private$.errorCheck()
      
      if (ready) {
        
        data <- private$.buildData()
        res.mfa <- private$.MFA(data)
        
        dimdesc=private$.dimdesc(res.mfa)
        self$results$descdesdim$setContent(dimdesc)
        
        private$.printeigenTable(res.mfa)
#        private$.printTables(res.mfa, "coord")
#        private$.printTables(res.mfa, "contrib")
#        private$.printTables(res.mfa, "cos2")
        
        imagevar=self$results$plotgroup
        imagevar$setState(res.mfa)

        imagevar=self$results$plotaxe
        imagevar$setState(res.mfa)

        imageind=self$results$plotind
        imageind$setState(res.mfa)
      
        imagevar=self$results$plotvar
        imagevar$setState(res.mfa)

        private$.output(res.mfa)
        
      }
    },
    
#### Compute results ----
.MFA = function(data) {
  
  quantivar_gui=self$options$quantivar
  qualivar_gui=self$options$qualivar
  nFactors_gui=self$options$nFactors
  groupdef_gui=self$options$groupdef
  groupill_gui=self$options$groupill
  grouptype_gui=self$options$grouptype
  groupname_gui=self$options$groupname

   if (groupdef_gui == "Ex: 2,3" || grouptype_gui == "Ex: s,n"){
          return()
        }
   else {
  
   if (groupname_gui == "Ex: senso,physico" || groupname_gui == "" || groupname_gui == 0 ) {

   if (groupill_gui == "Ex: 2,3" || groupill_gui == "" || groupill_gui == 0 ) {
    FactoMineR::MFA(data,group = as.numeric(strsplit(groupdef_gui, ",")[[1]]), type = unlist(strsplit(grouptype_gui, ",")), graph=FALSE)
   }
   else {
    FactoMineR::MFA(data,group = as.numeric(strsplit(groupdef_gui, ",")[[1]]), type = unlist(strsplit(grouptype_gui, ",")), num.group.sup=as.numeric(strsplit(groupill_gui, ",")[[1]]), graph=FALSE)
   }
   }

  else {
 if (groupill_gui == "Ex: 2,3" || groupill_gui == "" || groupill_gui == 0 ) {
    FactoMineR::MFA(data,group = as.numeric(strsplit(groupdef_gui, ",")[[1]]), type = unlist(strsplit(grouptype_gui, ",")), name.group = unlist(strsplit(groupname_gui, ",")), graph=FALSE)
   }
   else {
    FactoMineR::MFA(data,group = as.numeric(strsplit(groupdef_gui, ",")[[1]]), type = unlist(strsplit(grouptype_gui, ",")), num.group.sup=as.numeric(strsplit(groupill_gui, ",")[[1]]), name.group = unlist(strsplit(groupname_gui, ",")), graph=FALSE)
   }
  }

  }
},

.dimdesc = function(table) {
  
  proba=self$options$proba/100
  nFactors=self$options$nFactors
  groupdef_gui=self$options$groupdef
  grouptype_gui=self$options$grouptype

  if (groupdef_gui == "Ex: 2,3" || grouptype_gui == "Ex: s,n") return()
  else {
  res=dimdesc(table, axes=1:nFactors, proba = proba)
  print(res[-length(res)])
  }
},

.printeigenTable = function(table){

  groupdef_gui=self$options$groupdef
  grouptype_gui=self$options$grouptype

  if (groupdef_gui == "Ex: 2,3" || grouptype_gui == "Ex: s,n") return()
  else {  
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
  }
},

.printTables = function(table, quoi){
  
  quantivar_gui=self$options$quantivar
  nFactors_gui=self$options$nFactors
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
  
  for (i in 1:nFactors_gui){
    tablevar$addColumn(name=paste0("dim",i), title=paste0("Dim.", as.character(i)),type='number') #, superTitle='Facteurs'
    tableind$addColumn(name=paste0("dim",i), title=paste0("Dim.", as.character(i)),type='number')
  }
  
  for (var in seq_along(quantivar_gui)) {
    row=list()
    row[["variables"]]=rownames(quoivar)[var]
    for (i in 1:nFactors_gui) {
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
    for (i in 1:nFactors_gui){
      row[[paste0("dim",i)]]=quoiind[ind,i]
    }
    tableind$setRow(rowNo=ind, values=row)
  }
  
},

.plotindividus = function(image, ...){
  
  groupdef_gui=self$options$groupdef
  groupill_gui=self$options$groupill
  grouptype_gui=self$options$grouptype

  #if (is.null(self$options$quantivar) || groupdef_gui == "Ex: 2,3" || grouptype_gui == "Ex: s,n") return()
  if (groupdef_gui == "Ex: 2,3" || grouptype_gui == "Ex: s,n") return()
  
  else {
    res.mfa=image$state
    quantivar_gui=self$options$quantivar
    qualivar_gui=self$options$qualivar
    catevar_gui=self$options$catevar
    abs_gui=self$options$abs
    ord_gui=self$options$ord
    
    if (catevar_gui == TRUE) plot=plot.MFA(res.mfa, axes=c(abs_gui, ord_gui), title = "Representation of the Individuals and the Categories")
    else plot=plot.MFA(res.mfa, axes=c(abs_gui, ord_gui), invisible = c("quali","quali.sup"), title = "Representation of the Individuals")
    
    print(plot)
  }
},

.plotvariables = function(image, ...) {

  groupdef_gui=self$options$groupdef
  groupill_gui=self$options$groupill
  grouptype_gui=self$options$grouptype
  contivar_gui=self$options$contivar

  #if (is.null(self$options$quantivar)) return()
  if (is.null(self$options$quantivar) || groupdef_gui == "Ex: 2,3" || grouptype_gui == "Ex: s,n" || contivar_gui == FALSE) return()
  
  else {
    
    res.mfa=image$state    
    contivar_gui=self$options$contivar
    abs_gui=self$options$abs
    ord_gui=self$options$ord
    
    if (contivar_gui == TRUE) plot=plot(res.mfa, choix="var", axes=c(abs_gui, ord_gui), title = "Representation of the Variables")

    print(plot)
    TRUE
    
  }
},

.plotgroups = function(image, ...) {
  
  groupdef_gui=self$options$groupdef
  groupill_gui=self$options$groupill
  grouptype_gui=self$options$grouptype

  #if (is.null(self$options$quantivar) || groupdef_gui == "Ex: 2,3" || grouptype_gui == "Ex: s,n") return()
  if (groupdef_gui == "Ex: 2,3" || grouptype_gui == "Ex: s,n") return()
  
  else {
    
    res.mfa=image$state
    abs_gui=self$options$abs
    ord_gui=self$options$ord
    
    plot=plot(res.mfa, choix="group", axes=c(abs_gui, ord_gui), title = "Representation of the Groups")
    
    print(plot)
    TRUE
    
  }
},

.plotaxes = function(image, ...) {
  
  groupdef_gui=self$options$groupdef
  groupill_gui=self$options$groupill
  grouptype_gui=self$options$grouptype

  #if (is.null(self$options$quantivar) || groupdef_gui == "Ex: 2,3" || grouptype_gui == "Ex: s,n") return()
  if (groupdef_gui == "Ex: 2,3" || grouptype_gui == "Ex: s,n") return()
  
  else {
    
    res.mfa=image$state
    abs_gui=self$options$abs
    ord_gui=self$options$ord
    
    plot = FactoMineR::plot.MFA(res.mfa, choix="axes", axes=c(abs_gui, ord_gui), title = "Representation of the Partial Axes")
    
    print(plot)
    TRUE
    
  }
},

### Helper functions ----
.errorCheck = function() {
  if (length(self$options$quantivar) + length(self$options$quantivar) != sum(as.numeric(strsplit(self$options$groupdef, ",")[[1]])))
    jmvcore::reject(jmvcore::format('The definition of the groups is not good'))
  
},

.output = function(res.mfa){
        if (self$results$newvar$isNotFilled()) {
          keys <- 1:self$options$nFactors
          measureTypes <- rep("continuous", self$options$nFactors)
          titles <- paste(("Dim."), keys)
          descriptions <- character(length(keys))
          self$results$newvar$set(
            keys=keys,
            titles=titles,
            descriptions=descriptions,
            measureTypes=measureTypes
          )
          for (i in 1:self$options$nFactors) {
            scores <- as.numeric(res.mfa$ind$coord[, i])
            self$results$newvar$setValues(index=i, scores)
          }
          self$results$newvar$setRowNums(rownames(self$data))
        }
        
      },

.buildData = function() {
  
  dataquantivar=data.frame(self$data[,self$options$quantivar])
  colnames(dataquantivar)=self$options$quantivar
  
  dataqualivar=data.frame(self$data[,self$options$qualivar])
  colnames(dataqualivar)=self$options$qualivar

  data=data.frame(dataquantivar,dataqualivar)
  
  if (is.null(self$options$individus)==FALSE) {
    rownames(data)=self$data[[self$options$individus]]
  }
  else
    rownames(data)=c(1:nrow(data))
  
  return(data)
}
  )
)
