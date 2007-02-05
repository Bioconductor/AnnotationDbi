## countByUtil <- function(obj, countCol, groupCol) {
##     if (missing(countCol)) 
##         countCol="*"
##     else
##         countCol=paste("DISTINCT", countCol)
##     sql <- paste("SELECT", groupCol, ", count(", countCol, ") FROM",
##                 obj@tableName, "GROUP BY", groupCol, 
##                 sep=" ", collapse="")
##     if (globals$DEBUG)
##             cat("DEBUG: ", sql, "\n")
##     ans <- tryCatch(
##             dbGetQuery(obj@dbRefGetter(), sql),
##                     error=function(e) {
##                         cat("query attempted:\n", sql)
##                         stop(e)
##         })
##     split(ans[,2], ans[,1])
## }

## getSingleColumn <- function(column, table, db, rsProcessor=NULL, 
## 	distinct=FALSE, order=FALSE)
## {
##     if (distinct)
## 	distinctStr <- "DISTINCT "
##     else
## 	distinctStr <- ""
##     if (order)
## 	orderStr <- paste(" ORDER BY", column, "ASC")
##     else
## 	orderStr <- ""
##     query <- paste("SELECT ", distinctStr, column, " FROM ", table, orderStr, 
## 		sep="", collapse="")
##     ans <- tryCatch(dbGetQuerywoCvt(db, query),
##                     error=function(e) {
##                         cat("query attempted:\n", query)
##                         stop(e)
##                     })
##     if (!is.null(rsProcessor))
##         ans <- rsProcessor(ans)
##     ans[,1]
## }


## ###############################################
## ## functions to get the whole table contents
## ## such as: as.list, eapply
## ###############################################
## setGeneric("countBy", function(x, countCol, groupCol, ...) 
##     standardGeneric("countBy"))

## setMethod("countBy", 
##     signature(x="AnnotDbTableTwoWayMap", countCol="ANY", groupCol="missing"),
##     function(x, countCol, groupCol, ...) {
##         countByUtil(x, countCol, x@LHS)
##     })
    
## setMethod("countBy", 
##     signature(x="AnnotMultiColTable", countCol="ANY", groupCol="missing"),
##     function(x, countCol, groupCol, ...) {
##         countByUtil(x, countCol, x@keyCol)
##     }) 
       
## setMethod("countBy", 
##     signature(x="AnnotDbTable", countCol="ANY", groupCol="character"),
##     function(x, countCol, groupCol, ...) {
##         countByUtil(x, countCol, groupCol)
##     })


## ## The subByRow methods have a looot of overlap with mget
## ## I didn't let them to share utility functions because I am afraid that too
## ## many data will be copied during function calls
## setGeneric("subByRow", 
##     function(obj, whCol, whVal, ...) standardGeneric("subByRow"))

## setMethod("subByRow", 
##     signature(obj="AnnotMultiColTwoKeyTable", whCol="character", whVal="list"),
##         function(obj, whCol, whVal, include=!logical(length(whCol)), ...) {
##             ans <- doLookupComplex2(obj, selCol=obj@fieldNames, 
##                     whCol, whVal, include) 
##         if(is.null(ans)) {
##             ans <- list()
##         } else {
##             nc <- ncol(ans)-1
##             cname <- colnames(ans)[-1]
##             secKey <- obj@secKey
##             ans <- as.matrix(ans) ## WARNING: everything converted to character
##             ans <- split(ans[,-1], ans[,1])
##             ans <- lapply(ans, function(y) {
##                 dim(y) <- c(length(y)/nc, nc)
##                 thisAns <- lapply(seq(length(y)/nc), function(j) {
##                     rs <- as.list(y[j,])
##                     names(rs) <- cname
##                     rs
##                 })
##                 names(thisAns) <- unlist(lapply(thisAns, function(y) y[[secKey]]))
##                 thisAns
##             })
##         }
##         ans
##     })

## setMethod("subByRow", 
##     signature(obj="AnnotMultiColTable", whCol="character", whVal="list"),
##         function(obj, whCol, whVal, include=!logical(length(whCol)), ...) {
##             ans <- doLookupComplex2(obj, selCol=obj@fieldNames, 
##                     whCol, whVal, include)     
##         if(is.null(ans)) {
##             ans <- list()
##         } else {
##             nc <- ncol(ans)-1
##             cname <- colnames(ans)[-1]
##             ans <- as.matrix(ans) ## WARNING: everything converted to character
##             ans <- split(ans[,-1], ans[,1])
##             ans <- lapply(ans, function(y) {
##                 dim(y) <- c(length(y)/nc, nc)
##                 lapply(seq(length(y)/nc), function(j) {
##                     rs <- as.list(y[j,])
##                     names(rs) <- cname
##                     rs
##                 })
##             })
##         }
##         ans
##     })

## setMethod("subByRow", 
##     signature(obj="AnnotGOTermsTable", whCol="character", whVal="list"),
##         function(obj, whCol, whVal, include=!logical(length(whCol)), ...) {
##             ans <- doLookupComplex2(obj, selCol=obj@fieldNames, 
##                     whCol, whVal, include) 
##             if ( is.null(ans)) {
##                 goid <- character(0)
##             } else {
##                 goid <- as.factor(ans$GOID)
##                 term <- split(ans$Term, goid)
##                 synonym <- split(ans$Synonym, goid)
##                 secondary <- split(ans$Secondary, goid)
##                 definition <- split(ans$Definition, goid)
##                 ontology <- split(ans$Ontology, goid)   
##                 goid <- levels(goid)    
##             }
##             res <- lapply(goid, function(i) {
##                   ## create instances of Class 'GOTerms'
##                   ## implicitly assume that x is a data.frame with colnames as
##                   ## GOID, Term, Synonym, Secondary, Defintion, Ontology 
##                   ## and all values in col GOID are the same, so do cols
##                   ## Term and Definition. But cols Synonym and Secondary can
##                   ## have multiple values.
##                 if( all(secondary[[i]]=="NA"))
##                     theSecondary <- character(0)
##                 else
##                     theSecondary <- secondary[[i]]
##                 new("GOTerms", GOID=i,
##                     Term=as.character(term[[i]][1]),
##                     Synonym=as.character(synonym[[i]]),
##                     Secondary=theSecondary,
##                     Definition=as.character(definition[[i]][1]),
##                     Ontology=as.character(ontology[[i]][1]))
##             })
##             names(res) <- goid
##             res
##     })

## setMethod("subByRow", 
##     signature(obj="AnnotThreeColTable", whCol="character", whVal="list"),
##         function(obj, whCol, whVal, include=!logical(length(whCol)), ...) {
##             ans <- doLookupComplex2(obj, 
##                     selCol=c(obj@keyCol, obj@nameCol, obj@valCol), 
##                     whCol, whVal, include) 
##             if(is.null(ans)) {
##                 keyCol <- NULL
##             } else { 
##                 keyCol <- as.factor(ans[,1])
##                 valCol <- split(ans[,3], keyCol)
##                 nameCol <- split(ans[,2], keyCol)
##                 keyCol <- levels(keyCol)
##             }
##             res <- lapply(keyCol, function(i) {
##                     val <- valCol[[i]]
##                     names(val) <- nameCol[[i]]
##                     val
##             })
##             names(res) <- keyCol
##             res
##     })

## setGeneric("noMissing", 
##     function(obj, col, ...) standardGeneric("noMissing"))

## setMethod("noMissing", 
##     signature(obj="AnnotMultiColTwoKeyTable", col="character"),
##         function(obj, col, reverse=!logical(length(col)), ...) {
##         ans <- filterNull(obj, whCol=col, selCol=obj@fieldNames, reverse=reverse) 
##         if(is.null(ans)) {
##             ans <- list()
##         } else {
##             nc <- ncol(ans)-1
##             cname <- colnames(ans)[-1]
##             secKey <- obj@secKey
##             ans <- as.matrix(ans) ## WARNING: everything converted to character
##             ans <- split(ans[,-1], ans[,1])
##             ans <- lapply(ans, function(y) {
##                 dim(y) <- c(length(y)/nc, nc)
##                 thisAns <- lapply(seq(length(y)/nc), function(j) {
##                     rs <- as.list(y[j,])
##                     names(rs) <- cname
##                     rs
##                 })
##                 names(thisAns) <- unlist(lapply(thisAns, function(y) y[[secKey]]))
##                 thisAns
##             })
##         }
##         ans
##     })

## setMethod("noMissing", 
##     signature(obj="AnnotMultiColTable", col="character"),
##         function(obj, col, reverse=!logical(length(col)), ...) {
##         ans <- filterNull(obj, whCol=col, selCol=obj@fieldNames, reverse=reverse)     
##         if(is.null(ans)) {
##             ans <- list()
##         } else {
##             nc <- ncol(ans)-1
##             cname <- colnames(ans)[-1]
##             ans <- as.matrix(ans) ## WARNING: everything converted to character
##             ans <- split(ans[,-1], ans[,1])
##             ans <- lapply(ans, function(y) {
##                 dim(y) <- c(length(y)/nc, nc)
##                 lapply(seq(length(y)/nc), function(j) {
##                     rs <- as.list(y[j,])
##                     names(rs) <- cname
##                     rs
##                 })
##             })
##         }
##         ans
##     })

## setMethod("noMissing", 
##     signature(obj="AnnotGOTermsTable", col="character"),
##         function(obj, col, reverse=!logical(length(col)), ...) {
##         ans <- filterNull(obj, whCol=col, selCol=obj@fieldNames, reverse=reverse)     
##            if ( is.null(ans)) {
##                 goid <- character(0)
##             } else {
##                 goid <- as.factor(ans$GOID)
##                 term <- split(ans$Term, goid)
##                 synonym <- split(ans$Synonym, goid)
##                 secondary <- split(ans$Secondary, goid)
##                 definition <- split(ans$Definition, goid)
##                 ontology <- split(ans$Ontology, goid)   
##                 goid <- levels(goid)    
##             }
##             res <- lapply(goid, function(i) {
##                   ## create instances of Class 'GOTerms'
##                   ## implicitly assume that x is a data.frame with colnames as
##                   ## GOID, Term, Synonym, Secondary, Defintion, Ontology 
##                   ## and all values in col GOID are the same, so do cols
##                   ## Term and Definition. But cols Synonym and Secondary can
##                   ## have multiple values.
##                 if( all(secondary[[i]]=="NA"))
##                     theSecondary <- character(0)
##                 else
##                     theSecondary <- secondary[[i]]
##                 new("GOTerms", GOID=i,
##                     Term=as.character(term[[i]][1]),
##                     Synonym=as.character(synonym[[i]]),
##                     Secondary=theSecondary,
##                     Definition=as.character(definition[[i]][1]),
##                     Ontology=as.character(ontology[[i]][1]))
##             })
##             names(res) <- goid
##             res
##     })

## setMethod("noMissing", 
##     signature(obj="AnnotThreeColTable", col="character"),
##         function(obj, col, reverse=!logical(length(col)), ...) {
##         ans <- filterNull(obj, whCol=col, 
##                 selCol=c(obj@keyCol, obj@nameCol, obj@valCol),
##                 reverse=reverse) 
##             if(is.null(ans)) {
##                 keyCol <- NULL
##             } else { 
##                 keyCol <- as.factor(ans[,1])
##                 valCol <- split(ans[,3], keyCol)
##                 nameCol <- split(ans[,2], keyCol)
##                 keyCol <- levels(keyCol)
##             }
##             res <- lapply(keyCol, function(i) {
##                     val <- valCol[[i]]
##                     names(val) <- nameCol[[i]]
##                     val
##             })
##             names(res) <- keyCol
##             res
##     })
    
## setMethod("noMissing", 
##     signature(obj="AnnotDbTableTwoWayMap", col="character"),
##         function(obj, col, reverse=!logical(length(col)), ...) {
##         ans <- filterNull(obj, whCol=col, 
##                 selCol=c(obj@LHS, obj@RHS),
##                 reverse=reverse) 
##         if (is.null(ans)){
##             ans <- list()
##         } else {    
##             ans <- split(ans[,2], ans[,1])
##         }
##         res <- lapply(x, function(i) ans[[i]])
##             names(res) <- as.character(x)
##             res
##         })

## setMethod("noMissing", 
##     signature(obj="AnnotDbTableTwoWayMap", col="missing"),
##         function(obj, col, reverse=!logical(length(col)), ...) {
##         noMissing(obj, obj@RHS, reverse=reverse)
##     })
