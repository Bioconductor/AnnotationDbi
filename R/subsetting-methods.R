#################################################
##  Utility Functions
#################################################
## almost the same as sqliteFetch, just remove the for loop of type.convert
sqliteFetchwoCvt <- function (res, n = 0, ...)
{
    if (!isIdCurrent(res))
        stop("invalid result handle")
    n <- as(n, "integer")
    rsId <- as(res, "integer")
    rel <- .Call("RS_SQLite_fetch", rsId, nrec = n, PACKAGE = "RSQLite")
    if (length(rel) == 0 || length(rel[[1]]) == 0)
        return(data.frame(NULL))
    cnt <- dbGetRowCount(res)
    nrec <- length(rel[[1]])
    indx <- seq(from = cnt - nrec + 1, length = nrec)
    attr(rel, "row.names") <- as.character(indx)
    class(rel) <- "data.frame"
    rel
}

## almost the same as sqliteQuickSQL, just call sqliteFetchwoCvt instead of 
## sqliteFetch
dbGetQuerywoCvt <- function (con, statement, ...) 
{
    nr <- length(dbListResults(con))
    if (nr > 0) {
        new.con <- dbConnect(con)
        on.exit(dbDisconnect(new.con))
        rs <- sqliteExecStatement(new.con, statement)
    }
    else rs <- sqliteExecStatement(con, statement)
    if (dbHasCompleted(rs)) {
        dbClearResult(rs)
        invisible()
        return(NULL)
    }
    res <- sqliteFetchwoCvt(rs, n = -1, ...)
    if (dbHasCompleted(rs))
        dbClearResult(rs)
    else warning("pending rows")
    res
}

doLookupComplex <- function(obj, selCol, whCol=NULL, whVal) {
    if (missing(whVal)) {
        whStr <- ""
    } else {
	   quoteStr <- ifelse(is(whVal, "character"), '"', "")
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
		    dbGetQuerywoCvt(obj@dbRefGetter(), sql),
                    error=function(e) {
                        cat("query attempted:\n", sql)
                        stop(e)
                    })
	if (!is.null(obj@rsProcessor) && !is.null(ans)){
		ans <- obj@rsProcessor(ans)
	}
	ans
}

doLookupComplex2 <- function(obj, selCol, whCol, whVal, 
                        include=!logical(length(whCol))) {
    include <- ifelse(include, "IN", "NOT IN")
    whCond <- mapply(function(colName, colVal, inStr){
                quoteStr<- ifelse(is(colVal, "character"), '"', "")
                valStr <- paste(quoteStr, colVal, quoteStr, sep="", collapse=", ")
                paste(colName, inStr, "(", valStr, ")")  
            }, whCol, whVal, include) 
    whStr <- paste("WHERE", paste(whCond, sep="", collapse=" AND "))
    selCol <- paste(selCol, "AS", selCol)
    sql <- paste("SELECT", paste(selCol, sep="", collapse=", "),
        "FROM", obj@tableName, 
        whStr,
        sep=" ", collapse="") 
    if (globals$DEBUG)
            cat("DEBUG: ", sql, "\n")
        ans <- tryCatch(
            dbGetQuerywoCvt(obj@dbRefGetter(), sql),
                    error=function(e) {
                        cat("query attempted:\n", sql)
                        stop(e)
                    })
    if (!is.null(obj@rsProcessor) && !is.null(ans)){
        ans <- obj@rsProcessor(ans)
    }
    ans
}

filterNull <- function(obj, whCol, selCol=whCol, reverse=FALSE) {
    conStr <- ifelse(reverse, "NOTNULL", "ISNULL")
    whCond <- paste( "(", whCol, conStr, ")", sep=" ", collapse=" AND ")
    selCol <- paste(selCol, "AS", selCol)
    sql <- paste("SELECT", paste(selCol, sep="", collapse=", "), 
            "FROM", obj@tableName, 
            "WHERE", whCond,
            sep=" ", collapse="")
    if (globals$DEBUG)
            cat("DEBUG: ", sql, "\n")
        ans <- tryCatch(
            dbGetQuerywoCvt(obj@dbRefGetter(), sql),
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
	distinct=FALSE, order=FALSE)
{
    if (distinct)
	distinctStr <- "DISTINCT "
    else
	distinctStr <- ""
    if (order)
	orderStr <- paste(" ORDER BY", column, "ASC")
    else
	orderStr <- ""
    query <- paste("SELECT ", distinctStr, column, " FROM ", table, orderStr, 
		sep="", collapse="")
    ans <- tryCatch(dbGetQuerywoCvt(db, query),
                    error=function(e) {
                        cat("query attempted:\n", query)
                        stop(e)
                    })
    if (!is.null(rsProcessor))
        ans <- rsProcessor(ans)
    ans[,1]
}

lsVirtualKey <- function(key, table, db, rsProcessor=NULL) {
    getSingleColumn(key, table, db, rsProcessor, distinct=T, order=T)
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
    
setMethod("length", signature(x="AnnotMultiColTable"), 
    function(x) {
        sql <- paste("SELECT count(DISTINCT ", x@keyCol, ") FROM", x@tableName)
        if (globals$DEBUG)
            cat("DEBUG: ", sql, "\n")
        ans <- tryCatch(
            dbGetQuery(x@dbRefGetter(), sql),
                    error=function(e) {
                        cat("query attempted:\n", sql)
                        stop(e)
        })
        ans[[1]]
    })
    
setMethod("length", signature(x="AnnotDbTableTwoWayMap"), 
    function(x) {
        sql <- paste("SELECT count(DISTINCT ", x@LHS, ") FROM", x@tableName)
        if (globals$DEBUG)
            cat("DEBUG: ", sql, "\n")
        ans <- tryCatch(
            dbGetQuery(x@dbRefGetter(), sql),
                    error=function(e) {
                        cat("query attempted:\n", sql)
                        stop(e)
        })
        ans[[1]]
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
          if( all(secondary[[i]]=="NA"))
            theSecondary <- character(0)
          else
            theSecondary <- secondary[[i]]
          new("GOTerms", GOID=i,
                Term=as.character(term[[i]][1]),
                Synonym=as.character(synonym[[i]]),
                Secondary=theSecondary,
                Definition=as.character(definition[[i]][1]),
                Ontology=as.character(ontology[[i]][1]))
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

## The subByRow methods have a looot of overlap with mget
## I didn't let them to share utility functions because I am afraid that too
## many data will be copied during function calls
setGeneric("subByRow", 
    function(obj, whCol, whVal, ...) standardGeneric("subByRow"))

setMethod("subByRow", 
    signature(obj="AnnotMultiColTwoKeyTable", whCol="character", whVal="list"),
        function(obj, whCol, whVal, include=!logical(length(whCol)), ...) {
            ans <- doLookupComplex2(obj, selCol=obj@fieldNames, 
                    whCol, whVal, include) 
        if(is.null(ans)) {
            ans <- list()
        } else {
            nc <- ncol(ans)-1
            cname <- colnames(ans)[-1]
            secKey <- obj@secKey
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
        ans
    })

setMethod("subByRow", 
    signature(obj="AnnotMultiColTable", whCol="character", whVal="list"),
        function(obj, whCol, whVal, include=!logical(length(whCol)), ...) {
            ans <- doLookupComplex2(obj, selCol=obj@fieldNames, 
                    whCol, whVal, include)     
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
        ans
    })

setMethod("subByRow", 
    signature(obj="AnnotGOTermsTable", whCol="character", whVal="list"),
        function(obj, whCol, whVal, include=!logical(length(whCol)), ...) {
            ans <- doLookupComplex2(obj, selCol=obj@fieldNames, 
                    whCol, whVal, include) 
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
            res <- lapply(goid, function(i) {
                  ## create instances of Class 'GOTerms'
                  ## implicitly assume that x is a data.frame with colnames as
                  ## GOID, Term, Synonym, Secondary, Defintion, Ontology 
                  ## and all values in col GOID are the same, so do cols
                  ## Term and Definition. But cols Synonym and Secondary can
                  ## have multiple values.
                if( all(secondary[[i]]=="NA"))
                    theSecondary <- character(0)
                else
                    theSecondary <- secondary[[i]]
                new("GOTerms", GOID=i,
                    Term=as.character(term[[i]][1]),
                    Synonym=as.character(synonym[[i]]),
                    Secondary=theSecondary,
                    Definition=as.character(definition[[i]][1]),
                    Ontology=as.character(ontology[[i]][1]))
            })
            names(res) <- goid
            res
    })

setMethod("subByRow", 
    signature(obj="AnnotThreeColTable", whCol="character", whVal="list"),
        function(obj, whCol, whVal, include=!logical(length(whCol)), ...) {
            ans <- doLookupComplex2(obj, 
                    selCol=c(obj@keyCol, obj@nameCol, obj@valCol), 
                    whCol, whVal, include) 
            if(is.null(ans)) {
                keyCol <- NULL
            } else { 
                keyCol <- as.factor(ans[,1])
                valCol <- split(ans[,3], keyCol)
                nameCol <- split(ans[,2], keyCol)
                keyCol <- levels(keyCol)
            }
            res <- lapply(keyCol, function(i) {
                    val <- valCol[[i]]
                    names(val) <- nameCol[[i]]
                    val
            })
            names(res) <- keyCol
            res
    })

setGeneric("noMissing", 
    function(obj, col, ...) standardGeneric("noMissing"))

setMethod("noMissing", 
    signature(obj="AnnotMultiColTwoKeyTable", col="character"),
        function(obj, col, reverse=!logical(length(col)), ...) {
        ans <- filterNull(obj, whCol=col, selCol=obj@fieldNames, reverse=reverse) 
        if(is.null(ans)) {
            ans <- list()
        } else {
            nc <- ncol(ans)-1
            cname <- colnames(ans)[-1]
            secKey <- obj@secKey
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
        ans
    })

setMethod("noMissing", 
    signature(obj="AnnotMultiColTable", col="character"),
        function(obj, col, reverse=!logical(length(col)), ...) {
        ans <- filterNull(obj, whCol=col, selCol=obj@fieldNames, reverse=reverse)     
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
        ans
    })

setMethod("noMissing", 
    signature(obj="AnnotGOTermsTable", col="character"),
        function(obj, col, reverse=!logical(length(col)), ...) {
        ans <- filterNull(obj, whCol=col, selCol=obj@fieldNames, reverse=reverse)     
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
            res <- lapply(goid, function(i) {
                  ## create instances of Class 'GOTerms'
                  ## implicitly assume that x is a data.frame with colnames as
                  ## GOID, Term, Synonym, Secondary, Defintion, Ontology 
                  ## and all values in col GOID are the same, so do cols
                  ## Term and Definition. But cols Synonym and Secondary can
                  ## have multiple values.
                if( all(secondary[[i]]=="NA"))
                    theSecondary <- character(0)
                else
                    theSecondary <- secondary[[i]]
                new("GOTerms", GOID=i,
                    Term=as.character(term[[i]][1]),
                    Synonym=as.character(synonym[[i]]),
                    Secondary=theSecondary,
                    Definition=as.character(definition[[i]][1]),
                    Ontology=as.character(ontology[[i]][1]))
            })
            names(res) <- goid
            res
    })

setMethod("noMissing", 
    signature(obj="AnnotThreeColTable", col="character"),
        function(obj, col, reverse=!logical(length(col)), ...) {
        ans <- filterNull(obj, whCol=col, 
                selCol=c(obj@keyCol, obj@nameCol, obj@valCol),
                reverse=reverse) 
            if(is.null(ans)) {
                keyCol <- NULL
            } else { 
                keyCol <- as.factor(ans[,1])
                valCol <- split(ans[,3], keyCol)
                nameCol <- split(ans[,2], keyCol)
                keyCol <- levels(keyCol)
            }
            res <- lapply(keyCol, function(i) {
                    val <- valCol[[i]]
                    names(val) <- nameCol[[i]]
                    val
            })
            names(res) <- keyCol
            res
    })
    
setMethod("noMissing", 
    signature(obj="AnnotDbTableTwoWayMap", col="character"),
        function(obj, col, reverse=!logical(length(col)), ...) {
        ans <- filterNull(obj, whCol=col, 
                selCol=c(obj@LHS, obj@RHS),
                reverse=reverse) 
        if (is.null(ans)){
            ans <- list()
        } else {    
            ans <- split(ans[,2], ans[,1])
        }
        res <- lapply(x, function(i) ans[[i]])
            names(res) <- as.character(x)
            res
        })

setMethod("noMissing", 
    signature(obj="AnnotDbTableTwoWayMap", col="missing"),
        function(obj, col, reverse=!logical(length(col)), ...) {
        noMissing(obj, obj@RHS, reverse=reverse)
    })
###############################################
## functions to subset table by column
## such as: $, ls
###############################################             
setGeneric("getCol", function(x, name, ...) standardGeneric("getCol"))

setMethod("getCol", signature(x="AnnotDbTable", name="character"),
          function(x, name, distinct=FALSE, order=FALSE, ...) {
              getSingleColumn(name, x@tableName, x@dbRefGetter(), 
		x@rsProcessor, distinct, order)
          })
          
setMethod("getCol", signature(x="AnnotDbTableTwoWayMap", name="character"),
          function(x, name, distinct=FALSE, order=FALSE, ...) {
              if (!name %in% c(x@LHS, x@RHS))
                stop("invalid arg: ", sQuote(name), "\n",
                     "This object supports: ",
                     paste(x@LHS, x@RHS, sep=" and "))
              getSingleColumn(name, x@tableName, x@dbRefGetter(), 
		x@rsProcessor, distinct, order)
          })

setMethod("$", signature(x="AnnotDbTable", name="character"),
          function(x, name) {
              getCol(x, name)
          })

setMethod("names", signature(x="AnnotDbTable"),
          function(x) x@fieldNames)

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

###############################################
## other functions
## such as: getdb
###############################################             
setMethod("dbGetQuery", signature(conn="AnnotDbTable", statement="character"),
          function(conn, statement, ...) {
            ans <- tryCatch(
                dbGetQuerywoCvt(conn@dbRefGetter(), statement),
                error=function(e) {
                        cat("query attempted:\n", statement)
                        stop(e)
                }
             )
             if (!is.null(conn@rsProcessor) && !is.null(ans)){
                ans <- conn@rsProcessor(ans)
            }
            ans
})
