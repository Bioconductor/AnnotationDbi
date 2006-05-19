#################################################
##  Utility Functions
#################################################
doLookupComplex <- function(obj, selCol, whCol=NULL, whVal) {
    if (missing(whVal)) {
        whStr <- ""
    } else {
	   quoteStr <- ifelse(is(whVal, "character"), "'", "")
	   whVal <-paste(quoteStr, whVal, quoteStr, sep="", collapse=", ")
       whStr <- paste("WHERE", whCol, "IN (", whVal, ")")
    }
	selCol <- paste(selCol, "AS", selCol)
	sql <- paste("SELECT", paste(selCol, sep="", collapse=", "),
		"FROM", obj@tableName, 
		whStr,
		sep=" ", collapse="") 
    if (globals$DEBUG)
      		cat("DEBUG: ", sql, "\n")
    	ans <- tryCatch(
		    dbGetQuery(obj@dbRefGetter(), sql),
                    error=function(e) {
                        cat("query attempted:\n", sql)
                        stop(e)
                    })
	if (!is.null(obj@rsProcessor) && !is.null(ans)){
		ans <- obj@rsProcessor(ans)
	}
	ans
}

countByUtil <- function(obj, countCol, groupCol) {
    if (missing(countCol)) 
        countCol="*"
    else
        countCol=paste("DISTINCT", countCol)
    sql <- paste("SELECT", groupCol, ", count(", countCol, ") FROM",
                obj@tableName, "GROUP BY", groupCol, 
                sep=" ", collapse="")
    if (globals$DEBUG)
            cat("DEBUG: ", sql, "\n")
    ans <- tryCatch(
            dbGetQuery(obj@dbRefGetter(), sql),
                    error=function(e) {
                        cat("query attempted:\n", sql)
                        stop(e)
        })
    split(ans[,2], ans[,1])
}

getSingleColumn <- function(column, table, db, rsProcessor=NULL, 
        query=paste("SELECT", column, "FROM", table)) 
{
    ans <- tryCatch(dbGetQuery(db, query),
                    error=function(e) {
                        cat("query attempted:\n", query)
                        stop(e)
                    })
    if (!is.null(rsProcessor))
        ans <- rsProcessor(ans)
    ans[,1]
}

lsVirtualKey <- function(key, table, db, rsProcessor=NULL) {
    query <- paste("SELECT DISTINCT", key, "FROM", table, "ORDER BY", key, "ASC")
    getSingleColumn(key, table, db, rsProcessor, query)
}

###############################################
## functions to get the whole table contents
## such as: as.list, eapply
###############################################
setMethod("as.list", signature(x="AnnotDbTableTwoWayMap"),
    function(x, ...) {
        ans <- doLookupComplex(x, selCol=c(x@LHS, x@RHS)) 
		split(ans[,2], ans[,1])
    })

setMethod("as.list", signature(x="AnnotMultiColTable"),
    function(x, ...) {
        ans <- doLookupComplex(x, selCol=x@fieldNames) 
 		nc <- ncol(ans)-1
		cname <- colnames(ans)[-1]
		ans <- as.matrix(ans) ## WARNING: everything converted to character
		ans <- split(ans[,-1], ans[,1])
		lapply(ans, function(y) {
			dim(y) <- c(length(y)/nc, nc)
			lapply(seq(length(y)/nc), function(j) {
				rs <- as.list(y[j,])
				names(rs) <- cname
				rs
			})
		})
	})

setMethod("as.list", signature(x="AnnotMultiColTwoKeyTable"),
    function(x, ...) {
        ans <- doLookupComplex(x, selCol=x@fieldNames) 
        nc <- ncol(ans)-1
        cname <- colnames(ans)[-1]
        ans <- as.matrix(ans) ## WARNING: everything converted to character
        ans <- split(ans[,-1], ans[,1])
        secKey <- x@secKey
        lapply(ans, function(y) {
            dim(y) <- c(length(y)/nc, nc)
            res <- lapply(seq(length(y)/nc), function(j) {
                rs <- as.list(y[j,])
                names(rs) <- cname
                rs
            })
            names(res) <- unlist(lapply(res, function(y) y[[secKey]]))
            res
        })
    })

setMethod("as.list", signature(x="AnnotGOTermsTable"),
    function(x, ...) {
        ans <- doLookupComplex(x, selCol=x@fieldNames) 
	    goid <- as.factor(ans$GOID)
	    term <- split(ans$Term, goid)
	    synonym <- split(ans$Synonym, goid)
	    secondary <- split(ans$Secondary, goid)
	    definition <- split(ans$Definition, goid)
	    ontology <- split(ans$Ontology, goid)	
	    goid <- levels(goid)	
        res <- lapply(seq(length(goid)), function(i) {
		  ## create instances of Class 'GOTerms'
		  ## implicitly assume that x is a data.frame with colnames as
		  ## GOID, Term, Synonym, Secondary, Defintion, Ontology 
		  ## and all values in col GOID are the same, so do cols
		  ## Term and Definition. But cols Synonym and Secondary can
		  ## have multiple values.
		  if( all(is.na(secondary[[i]])))
			theSecondary <- character(0)
		  else
			theSecondary <- secondary[[i]]
		  new("GOTerms", GOID=goid[[i]],
				Term=as.character(term[[i]][1]),
				Synonym=as.character(synonym[[i]]),
				Secondary=theSecondary,
				Definition=as.character(definition[[i]][1]),
				Ontology=as.character(ontology[[i]][[1]]))
	    } )
        names(res) <- goid
        res
    })

setMethod("as.list", signature(x="AnnotThreeColTable"),
    function(x, ...) {
        ans <- doLookupComplex(x, selCol=c(x@keyCol, x@nameCol, x@valCol))
		keyCol <- as.factor(ans[,1])
		valCol <- split(ans[,3], keyCol)
		nameCol <- split(ans[,2], keyCol)
		keyCol <- levels(keyCol)
        res <- lapply(seq(length(keyCol)), function(i) {
			val <- valCol[[i]]
			names(val) <- nameCol[[i]]
			val
        })
        names(res) <- as.character(keyCol)
        res
    })
    
setMethod("contents", signature(object="AnnotDbTable", all.names="ANY"), 
    function(object, all.names) {
        as.list(object)
    })
        
setGeneric("countBy", function(x, countCol, groupCol, ...) 
    standardGeneric("countBy"))

setMethod("countBy", 
    signature(x="AnnotDbTableTwoWayMap", countCol="ANY", groupCol="missing"),
    function(x, countCol, groupCol, ...) {
        countByUtil(x, countCol, x@LHS)
    })
    
setMethod("countBy", 
    signature(x="AnnotMultiColTable", countCol="ANY", groupCol="missing"),
    function(x, countCol, groupCol, ...) {
        countByUtil(x, countCol, x@keyCol)
    }) 
       
setMethod("countBy", 
    signature(x="AnnotDbTable", countCol="ANY", groupCol="character"),
    function(x, countCol, groupCol, ...) {
        countByUtil(x, countCol, groupCol)
    })

setMethod("eapply", signature(env="AnnotDbTable", FUN="function", all.names="ANY"),
    function(env, FUN, ..., all.names) {
        if (identical(FUN, length))
            countBy(env)
        else
            lapply(as.list(env), FUN)
    })

###############################################
## functions to subset table by row
## such as: mget, get, [, [[, $
###############################################   
setMethod("mget", signature(x="vector", envir="AnnotDbTableTwoWayMap",
                            mode="missing", ifnotfound="missing",
                            inherits="missing"),
    function(x, envir, mode, ifnotfound, inherits) {
        ans <- doLookupComplex(envir, selCol=c(envir@LHS, envir@RHS), 
                    whCol=envir@LHS, whVal=x)
    if (is.null(ans)){
        ans <- list()
    } else {    
        ans <- split(ans[,2], ans[,1])
    }
    res <- lapply(x, function(i) ans[[i]])
        names(res) <- as.character(x)
        res
    })
    
setMethod("mget", signature(x="vector", envir="AnnotMultiColTwoKeyTable",
                            mode="missing", ifnotfound="missing",
                            inherits="missing"),
    function(x, envir, mode, ifnotfound, inherits) {
        ans <- doLookupComplex(envir, selCol=envir@fieldNames, 
                    whCol=envir@keyCol, whVal=x)
    if(is.null(ans)) {
        ans <- list()
    } else {
        nc <- ncol(ans)-1
        cname <- colnames(ans)[-1]
        secKey <- envir@secKey
        ans <- as.matrix(ans) ## WARNING: everything converted to character
        ans <- split(ans[,-1], ans[,1])
        ans <- lapply(ans, function(y) {
            dim(y) <- c(length(y)/nc, nc)
            thisAns <- lapply(seq(length(y)/nc), function(j) {
                rs <- as.list(y[j,])
                names(rs) <- cname
                rs
            })
            names(thisAns) <- unlist(lapply(thisAns, function(y) y[[secKey]]))
            thisAns
        })
    }
    res <- lapply(x, function(i) ans[[i]])
        names(res) <- as.character(x)
        res
    })
    
setMethod("mget", signature(x="vector", envir="AnnotMultiColTable",
                            mode="missing", ifnotfound="missing",
                            inherits="missing"),
    function(x, envir, mode, ifnotfound, inherits) {
        ans <- doLookupComplex(envir, selCol=envir@fieldNames, 
                    whCol=envir@keyCol, whVal=x)
    if(is.null(ans)) {
        ans <- list()
    } else {
        nc <- ncol(ans)-1
        cname <- colnames(ans)[-1]
        ans <- as.matrix(ans) ## WARNING: everything converted to character
        ans <- split(ans[,-1], ans[,1])
        ans <- lapply(ans, function(y) {
            dim(y) <- c(length(y)/nc, nc)
            lapply(seq(length(y)/nc), function(j) {
                rs <- as.list(y[j,])
                names(rs) <- cname
                rs
            })
        })
    }
    res <- lapply(x, function(i) ans[[i]])
        names(res) <- as.character(x)
        res
    })

setMethod("mget", signature(x="vector", envir="AnnotGOTermsTable",
                            mode="missing", ifnotfound="missing",
                            inherits="missing"),
    function(x, envir, mode, ifnotfound, inherits) {
        ans <- doLookupComplex(envir, selCol=envir@fieldNames, 
                    whCol=envir@keyCol, whVal=x)
    if ( is.null(ans)) {
        goid <- character(0)
    } else {
        goid <- as.factor(ans$GOID)
        term <- split(ans$Term, goid)
        synonym <- split(ans$Synonym, goid)
        secondary <- split(ans$Secondary, goid)
        definition <- split(ans$Definition, goid)
        ontology <- split(ans$Ontology, goid)   
        goid <- levels(goid)    
    }
        res <- lapply(x, function(i) {
        if(i %in% goid) {
          ## create instances of Class 'GOTerms'
          ## implicitly assume that x is a data.frame with colnames as
          ## GOID, Term, Synonym, Secondary, Defintion, Ontology 
          ## and all values in col GOID are the same, so do cols
          ## Term and Definition. But cols Synonym and Secondary can
          ## have multiple values.
          if( all(is.na(secondary[[i]])))
            theSecondary <- character(0)
          else
            theSecondary <- secondary[[i]]
          new("GOTerms", GOID=i,
                Term=as.character(term[[i]][1]),
                Synonym=as.character(synonym[[i]]),
                Secondary=theSecondary,
                Definition=as.character(definition[[i]][1]),
                Ontology=as.character(ontology[[1]]))
       } else {
          NULL
           }
    })
        names(res) <- as.character(x)
        res
    })

setMethod("mget", signature(x="vector", envir="AnnotThreeColTable",
                            mode="missing", ifnotfound="missing",
                            inherits="missing"),
    function(x, envir, mode, ifnotfound, inherits) {
        ans <- doLookupComplex(envir, 
                selCol=c(envir@keyCol, envir@nameCol, envir@valCol), 
                whCol=envir@keyCol, whVal=x)
        if(is.null(ans)) {
        keyCol <- NULL
    } else { 
        keyCol <- as.factor(ans[,1])
        valCol <- split(ans[,3], keyCol)
        nameCol <- split(ans[,2], keyCol)
        keyCol <- levels(keyCol)
    }
        res <- lapply(x, function(i) {
        if (i %in% keyCol) {
            val <- valCol[[i]]
            names(val) <- nameCol[[i]]
            val
        } else {
            NULL
        }
        })
        names(res) <- as.character(x)
        res
    })

## This is soly for compatiability. In case someone called:
## mget(x, envir, ifnotfound=NA)
setMethod("mget", signature(x="vector", envir="AnnotDbTable",
                            mode="missing", ifnotfound="logical",
                            inherits="missing"),
    function(x, envir, mode, ifnotfound, inherits) {
            ans <- mget(x, envir)
            if (is.na(ifnotfound)) {
                ans <- lapply(ans, function(i) {
                        if (is.null(i))
                            NA
                        else
                            i
                })
            } else {
                warning("The value of ifnotfound is invalid and ignored.")
            }
            ans
    })

setMethod("mget", signature(x="vector", envir="AnnotDbTable",
                            mode="missing", ifnotfound="list",
                            inherits="missing"),
    function(x, envir, mode, ifnotfound, inherits) {
            ans <- mget(x, envir)
            if (length(ifnotfound)==length(x)) {
                ans <- lapply(seq(length(x)), function(i) {
                        if (is.null(ans[[i]]))
                            ifnotfound[[i]]
                        else
                            ans[[i]]
                })
            } else {
                warning("ifnotfound has a different length than x, therefore ignored.")
            }
            ans
    })
    
setMethod("[", signature(x="AnnotDbTable", i="vector",
                         j="missing", drop="missing"),
          function(x, i, j, drop) {
              mget(x=i, envir=x)
          })

setMethod("get", signature(x="vector", pos="missing",
            envir="AnnotDbTable", mode="missing",
            inherits="missing"),
          function(x, pos, envir, mode, inherits) {
              if (!identical(length(x), 1:1))
                stop("subsetting argument must have length 1")
              mget(x=x, envir=envir)[[1]]
          })

setMethod("get", signature(x="vector", pos="AnnotDbTable",
		envir="missing", mode="missing", inherits="missing"),
	function(x, pos, envir, mode, inherits) {
		get(x=x, envir=pos)
          })

setMethod("[[", signature(x="AnnotDbTable",
                          i="vector", j="missing"),
          function(x, i, j) {
              get(x=i, envir=x)
          })

setMethod("$", signature(x="AnnotDbEnv", name="character"),
          function(x, name) {
              get(x=name, envir=x)
          })

###############################################
## functions to subset table by column
## such as: $, ls
###############################################             
setGeneric("getCol", function(x, name, ...) standardGeneric("getCol"))

setMethod("getCol", signature(x="AnnotDbTable", name="character"),
          function(x, name, ...) {
              getSingleColumn(name, x@tableName, x@dbRefGetter(), x@rsProcessor)
          })
          
setMethod("getCol", signature(x="AnnotDbTableTwoWayMap", name="character"),
          function(x, name, ...) {
              if (!name %in% c(x@LHS, x@RHS))
                stop("invalid arg: ", sQuote(name), "\n",
                     "This object supports: ",
                     paste(x@LHS, x@RHS, sep=" and "))
              getSingleColumn(name, x@tableName, x@dbRefGetter(), x@rsProcessor)
          })

setMethod("$", signature(x="AnnotDbTable", name="character"),
          function(x, name) {
              getCol(x, name)
          })

setMethod("names", signature(x="AnnotDbTable"),
          function(x) x@fieldNames)

setMethod("names", signature(x="AnnotDbTableTwoWayMap"),
          function(x) {
              c(x@LHS, x@RHS)
          })

setMethod("nrow", signature(x="AnnotDbTable"), function(x) x@nrow)

setMethod("ncol", signature(x="AnnotDbTable"),
          function(x) length(x@fieldNames))

setMethod("dim", signature(x="AnnotDbTable"),
          function(x) list(rows=nrow(x), cols=ncol(x)))


setMethod("ls", signature(name="AnnotDbTableTwoWayMap", pos="missing", envir="missing", all.names="missing", pattern="missing"), 
	function(name, pos, envir, all.names, pattern) {
		lsVirtualKey(name@LHS, name@tableName, name@dbRefGetter(), name@rsProcessor)
	})

setMethod("ls", signature(name="AnnotMultiColTable", pos="missing", envir="missing", all.names="missing", pattern="missing"),
        function(name, pos, envir, all.names, pattern) {
                lsVirtualKey(name@keyCol, name@tableName, name@dbRefGetter(), name@rsProcessor)
        })
        
setMethod("is.environment", signature(obj="AnnotDbEnv"),
    function(obj) TRUE
    )

