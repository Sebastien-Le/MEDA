MFAClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "MFAClass",
  inherit = MFABase,
  active = list(
      dataProcessed = function() {
      if (is.null(private$.dataProcessed))
      private$.dataProcessed <- private$.buildData()
      return(private$.dataProcessed)
      },

      nbclust = function() {
            if (is.null(private$.nbclust))
                private$.nbclust <- private$.computeNbclust()

            return(private$.nbclust)
        },
        
      classifResult = function() {
        if (is.null(private$.classifResult))
        private$.classifResult <- private$.getclassifResult()
        return(private$.classifResult)
      },

      MFAResult = function() {
        if (is.null(private$.MFAResult))
        private$.MFAResult <- private$.getMFAResult()
        return(private$.MFAResult)
      }
  ),
  private = list(
    .dataProcessed = NULL,
    .nbclust = NULL,
    .classifResult = NULL,
    .MFAResult = NULL,

    #---------------------------------------------  
    #### Init + run functions ----
        
        .init = function() {
            if (is.null(self$data) | is.null(self$options$quantivar)) {
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
            <p><b>What you should know before running an MFA in jamovi</b></p>
            <p>______________________________________________________________________________</p>
            <p> The main objective of MFA is to analyse datasets structured according to groups of variables. 
            Therefore, the definition of the groups of variables is of utmost importance.</p>

            <p> In order to define the groups properly, you have to consider the order of the variables in the dataset analysed by MFA.
            Moving variables to the right-hand blocks creates the dataset analysed by MFA. 
            The order in which the variables are displayed corresponds to the order of the variables in the dataset analysed by MFA: continuous variables first, then categorical variables.</p>


            <p> 0. Open the <b>wine</b> dataset. Choose the Ident variable as <I>Individual Labels</I>. Put all the continuous (<I>resp.</I> categorical) variables in the proper 
            block. The two first fields <I>\"Groups definition\"</I> and <I>\"Groups type\"</I> are <B>mandatory</B>, while the two other
            fields <I>\"Groups name\"</I> and <I>\"Supplementary fields\"</I> are <B>optional</B>.</p>

            <p> 1. To define the groups, get rid of the characters <I>\"Ex: \"</I>. In this example, 6 groups of variables are constituted
            with respectively 5,3,10,9,2, and 2 variables.</p>

            <p> 2. The first five groups are scaled to unit variance (<I>\"s\"</I>), the last
            group is considered as categorical (<I>\"n\"</I>).</p>

            <p> 3. Giving a name to the groups is important when interpreting the results but is not mandatory. Considering 
            supplementary groups is also important when interpreting the results but not mandatory. In this example, 
            the two last groups will not change the representation of the individuals for instance, as they are considered
            as supplementary.</p>

            <p> Clustering is based on the number of components saved. 
            By default, clustering is based on the first 5 components, <I>i.e.</I> the distance between individuals is calculated on these 5 components.</p>
          
            <p> By default, the <I>Number of clusters</I> field is set to -1 which means that the number of clusters is automatically chosen by the computer.</p>

            <p>______________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
            )
            
        },

    .run = function() {
      
      ready <- TRUE
      if (is.null(self$options$quantivar) && (is.null(self$options$qualivar))){
        return()
        ready <- FALSE
      }
      #private$.errorCheck()
      
      if (ready) {

        res.classif <- private$.getclassifResult()

        dimdesc=private$.dimdesc()
        self$results$descdesdim$setContent(dimdesc)
          
        code=private$.code()
        self$results$code$setContent(code)
        
        private$.printeigenTable()
        
        imagevar=self$results$plotgroup
        imagevar$setState(self$MFAResult)

        imagevar=self$results$plotaxe
        imagevar$setState(self$MFAResult)
        
        imageind=self$results$plotind
        imageind$setState(self$MFAResult)

        if (isTRUE(dim(self$MFAResult$summary.quali)[1]) && dim(self$MFAResult$summary.quali)[1]>0){
        self$results$plotcat$setVisible(visible=TRUE)
        imageind=self$results$plotcat
        imageind$setState(self$MFAResult)
        }

        if (any(grepl("quanti",names(self$MFAResult))) == TRUE){
        self$results$plotvar$setVisible(visible=TRUE)
        imagevar=self$results$plotvar
        imagevar$setState(self$MFAResult)
        }

        if (self$options$graphclassif==TRUE){
          imageclass = self$results$plotclassif
          imageclass$setState(res.classif)
        }

        private$.output()    
        #private$.output2()
        private$.output2(res.classif)    
  
      }
    },

#---------------------------------------------
#### Compute results ----
.computeNbclust = function() {
      nbclust <- self$options$nbclust
      return(nbclust)
},

.getclassifResult = function() {
  groupdef_gui=self$options$groupdef
  grouptype_gui=self$options$grouptype
  if (groupdef_gui == "Ex: 5,3,10,9,2,2" || grouptype_gui == "Ex: s,s,s,s,s,n"){
          return()
        }
  else{
        reshcpc <- FactoMineR::HCPC(self$MFAResult,nb.clust=self$nbclust,graph=F)
        private$.classifResult <- reshcpc
        return(private$.classifResult)
      }
      },

.getMFAResult= function() {
  data <- self$dataProcessed
  groupdef_gui=self$options$groupdef
  groupill_gui=self$options$groupill
  grouptype_gui=self$options$grouptype
  groupname_gui=self$options$groupname

   if (groupdef_gui == "Ex: 5,3,10,9,2,2" || grouptype_gui == "Ex: s,s,s,s,s,n"){
          return()
        }
   else {
  
   if (groupname_gui == "Ex: olf,vis,olfag,gust,ens,orig" || groupname_gui == "" || groupname_gui == 0 ) {

   if (groupill_gui == "Ex: 5,6" || groupill_gui == "" || groupill_gui == 0 ) {
    r <- FactoMineR::MFA(data,group = as.numeric(strsplit(groupdef_gui, ",")[[1]]), type = unlist(strsplit(grouptype_gui, ",")), ncp=self$options$ncp, graph=FALSE)
   }
   else {
    r <- FactoMineR::MFA(data,group = as.numeric(strsplit(groupdef_gui, ",")[[1]]), type = unlist(strsplit(grouptype_gui, ",")), num.group.sup=as.numeric(strsplit(groupill_gui, ",")[[1]]), ncp=self$options$ncp, graph=FALSE)
   }
   }

  else {
 if (groupill_gui == "Ex: 5,6" || groupill_gui == "" || groupill_gui == 0 ) {
    r <- FactoMineR::MFA(data,group = as.numeric(strsplit(groupdef_gui, ",")[[1]]), type = unlist(strsplit(grouptype_gui, ",")), name.group = unlist(strsplit(groupname_gui, ",")), ncp=self$options$ncp, graph=FALSE)
   }
   else {
    r <- FactoMineR::MFA(data,group = as.numeric(strsplit(groupdef_gui, ",")[[1]]), type = unlist(strsplit(grouptype_gui, ",")), num.group.sup=as.numeric(strsplit(groupill_gui, ",")[[1]]), name.group = unlist(strsplit(groupname_gui, ",")), ncp=self$options$ncp, graph=FALSE)
   }
  }
  private$.MFAResult <- r
  return(private$.MFAResult)
  }
},

.code= function() {
  data <- self$dataProcessed
  groupdef_gui=self$options$groupdef
  groupill_gui=self$options$groupill
  grouptype_gui=self$options$grouptype
  groupname_gui=self$options$groupname

   #if (groupdef_gui == "Ex: 5,3,10,9,2,2" || grouptype_gui == "Ex: s,s,s,s,s,n"){
   #       data <- paste("jdhfhfmsdhmlf")
   #       code <- paste("data_PCA <- data[ ,c(")
   #       a <- list("dataset"=data,"R code"=code)
   #       print(a)
   #     }
   #else {
  
   if (groupname_gui == "Ex: olf,vis,olfag,gust,ens,orig" || groupname_gui == "" || groupname_gui == 0 ) {

   if (groupill_gui == "Ex: 5,6" || groupill_gui == "" || groupill_gui == 0 ) {
    #r <- FactoMineR::MFA(data,group = as.numeric(strsplit(groupdef_gui, ",")[[1]]), type = unlist(strsplit(grouptype_gui, ",")), ncp=self$options$ncp, graph=FALSE)
    names_var <- paste(names(self$MFAResult$call$X), collapse = ", ")
    data <- paste("data_MFA <- data[ ,c(",names_var,")]",sep="")
    #formatted_string <- gsub(",", "\",\"", grouptype_gui)
    #formatted_string <- paste("\"", formatted_string, "\"")
    code <- paste("MFA(data_MFA, group=c(",groupdef_gui,"), type=c(",grouptype_gui,"))",sep="")
    warning <- paste("It should be","\"s\"","instead of s (the same for n or f)")
    a <- list("dataset"=data,"R code"=code,"Warning"=warning)
    #a <- list("dataset"=data,"R code"=code)
    print(a)
   }
   else {
    #r <- FactoMineR::MFA(data,group = as.numeric(strsplit(groupdef_gui, ",")[[1]]), type = unlist(strsplit(grouptype_gui, ",")), num.group.sup=as.numeric(strsplit(groupill_gui, ",")[[1]]), ncp=self$options$ncp, graph=FALSE)
    names_var <- paste(names(self$MFAResult$call$X), collapse = ", ")
    data <- paste("data_MFA <- data[ ,c(",names_var,")]",sep="")
    code <- paste("MFA(data_MFA, group=c(",groupdef_gui,"), type=c(",grouptype_gui,"), num.group.sup=c(",groupill_gui,"))",sep="")
    warning <- paste("It should be \"s\" instead of s (the same for n or f)")
    a <- list("dataset"=data,"R code"=code,"Warning"=warning)
    print(a)
   }
   }

  else {
 if (groupill_gui == "Ex: 5,6" || groupill_gui == "" || groupill_gui == 0 ) {
    #r <- FactoMineR::MFA(data,group = as.numeric(strsplit(groupdef_gui, ",")[[1]]), type = unlist(strsplit(grouptype_gui, ",")), name.group = unlist(strsplit(groupname_gui, ",")), ncp=self$options$ncp, graph=FALSE)
    names_var <- paste(names(self$MFAResult$call$X), collapse = ", ")
    data <- paste("data_MFA <- data[ ,c(",names_var,")]",sep="")
    code <- paste("MFA(data_MFA, group=c(",groupdef_gui,"), type=c(",grouptype_gui,"), name.group=c(",groupname_gui,"))",sep="")
    warning <- paste("It should be \"s\" instead of s (the same for n, or f, and the names of the groups)")
    a <- list("dataset"=data,"R code"=code,"Warning"=warning)
    print(a)
   }
   else {
    #r <- FactoMineR::MFA(data,group = as.numeric(strsplit(groupdef_gui, ",")[[1]]), type = unlist(strsplit(grouptype_gui, ",")), num.group.sup=as.numeric(strsplit(groupill_gui, ",")[[1]]), name.group = unlist(strsplit(groupname_gui, ",")), ncp=self$options$ncp, graph=FALSE)
    names_var <- paste(names(self$MFAResult$call$X), collapse = ", ")
    data <- paste("data_MFA <- data[ ,c(",names_var,")]",sep="")
    code <- paste("MFA(data_MFA, group=c(",groupdef_gui,"), type=c(",grouptype_gui,"), num.group.sup=c(",groupill_gui,"), name.group=c(",groupname_gui,"))",sep="")
    warning <- paste("It should be \"s\" instead of s (the same for n, or f, and the names of the groups)")
    a <- list("dataset"=data,"R code"=code,"Warning"=warning)
    print(a)
   }
  }
#  }
},

.dimdesc = function() {
  table <- self$MFAResult
  proba=self$options$proba/100
  nFactors=self$options$nFactors
  groupdef_gui=self$options$groupdef
  grouptype_gui=self$options$grouptype
  nFactors_out <- min(self$options$nFactors,dim(self$MFAResult$eig)[1])

  if (groupdef_gui == "Ex: 5,3,10,9,2,2" || grouptype_gui == "Ex: s,s,s,s,s,n") return()
  else {
  res=dimdesc(table, axes=1:nFactors_out, proba = proba)
  print(res[-length(res)])
  #print(res)
  }
},

.printeigenTable = function(){
  table <- self$MFAResult
  groupdef_gui=self$options$groupdef
  grouptype_gui=self$options$grouptype

  if (groupdef_gui == "Ex: 5,3,10,9,2,2" || grouptype_gui == "Ex: s,s,s,s,s,n") return()
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

.plotindividus = function(image, ...){
  groupdef_gui=self$options$groupdef
  groupill_gui=self$options$groupill
  grouptype_gui=self$options$grouptype

  #if (is.null(self$options$quantivar) || groupdef_gui == "Ex: 5,3,10,9,2,2" || grouptype_gui == "Ex: s,s,s,s,s,n") return()
  if (groupdef_gui == "Ex: 5,3,10,9,2,2" || grouptype_gui == "Ex: s,s,s,s,s,n") return()
  
  else {
    res.mfa=image$state
    abs_gui=self$options$abs
    ord_gui=self$options$ord
    
    plot=plot.MFA(res.mfa, axes=c(abs_gui, ord_gui), invisible = c("quali","quali.sup"), title = "Representation of the Individuals")
    
    print(plot)
    TRUE
  }
},

.plotcategory = function(image, ...){
  res.mfa=image$state
  abs_gui=self$options$abs
  ord_gui=self$options$ord

  plot=plot.MFA(res.mfa, axes=c(abs_gui, ord_gui), invisible = "ind", title = "Representation of the Categories")
  print(plot)
  TRUE
},

.plotvariables = function(image, ...) {
  groupdef_gui=self$options$groupdef
  groupill_gui=self$options$groupill
  grouptype_gui=self$options$grouptype

  if (groupdef_gui == "Ex: 5,3,10,9,2,2" || grouptype_gui == "Ex: s,s,s,s,s,n") return()
  else {    
    res.mfa=image$state
    abs_gui=self$options$abs
    ord_gui=self$options$ord
    
    plot=plot(res.mfa, choix="var", axes=c(abs_gui, ord_gui), title = "Representation of the Variables")
    print(plot)
    TRUE
  }
},

.plotgroups = function(image, ...) {  
  groupdef_gui=self$options$groupdef
  groupill_gui=self$options$groupill
  grouptype_gui=self$options$grouptype

  if (groupdef_gui == "Ex: 5,3,10,9,2,2" || grouptype_gui == "Ex: s,s,s,s,s,n") return()
  
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

  if (groupdef_gui == "Ex: 5,3,10,9,2,2" || grouptype_gui == "Ex: s,s,s,s,s,n") return()
  
  else {    
    res.mfa=image$state
    abs_gui=self$options$abs
    ord_gui=self$options$ord
    
    plot = FactoMineR::plot.MFA(res.mfa, choix="axes", axes=c(abs_gui, ord_gui), title = "Representation of the Partial Axes")
    print(plot)
    TRUE
    
  }
},

.plotclassif= function(image, ...){

        if (is.null(self$options$groupdef)) return()

        else {
          abs_gui=self$options$abs
          ord_gui=self$options$ord

          res.classif=image$state
          plot=FactoMineR::plot.HCPC(res.classif, axes=c(abs_gui, ord_gui), choice="map", draw.tree = F, title="Representation of the Individuals According to Clusters")
          print(plot)
          TRUE
        }
      },

#---------------------------------------------
### Helper functions ----
.errorCheck = function() {
  if (length(self$options$quantivar) + length(self$options$quantivar) != sum(as.numeric(strsplit(self$options$groupdef, ",")[[1]])))
    jmvcore::reject(jmvcore::format('The definition of the groups is not good'))
},

.output = function(){
        nFactors_out <- min(self$options$ncp,dim(self$MFAResult$eig)[1])
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
            scores <- as.numeric(self$MFAResult$ind$coord[, i])
            self$results$newvar$setValues(index=i, scores)
          }
          self$results$newvar$setRowNums(rownames(self$data))
        }
        
      },

.output2 = function(res.classif){
  groupdef_gui=self$options$groupdef
  grouptype_gui=self$options$grouptype
  if (groupdef_gui == "Ex: 5,3,10,9,2,2" || grouptype_gui == "Ex: s,s,s,s,s,n"){
          return()
        }
  else{
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