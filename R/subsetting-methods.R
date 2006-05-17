doLookupComplex <- function(obj, selCol, whCol, whVal) {
	quoteStr <- ifelse(is(whVal, "character"), "'", "")
	whVal <-paste(quoteStr, whVal, quoteStr, sep="", collapse=", ")
	selCol <- paste(selCol, "AS", selCol)
	sql <- paste("SELECT", paste(selCol, sep="", collapse=", "),
		"FROM", obj@tableName, 
		"WHERE", whCol, "IN (", whVal, ")",
		sep=" ", collapse="") 
    	if (globals$DEBUG)
      		cat("DEBUG: ", sql, "\n")
	cat("got sql...\n")
    	ans <- tryCatch(
		    dbGetQuery(obj@dbRefGetter(), sql),
                    error=function(e) {
                        cat("query attempted:\n", sql)
                        stop(e)
                    })
	cat("get ans...\n")
	if (!is.null(obj@rsProcessor) && !is.null(ans)){
		cat("post process...\n")
		ans <- obj@rsProcessor(ans)
	}
	ans
}

setMethod("mget", signature(x="vector", envir="AnnotDbTableTwoWayMap",
                            mode="missing", ifnotfound="missing",
                            inherits="missing"),
    function(x, envir, mode, ifnotfound, inherits) {
        ans <- doLookupComplex(envir, selCol=c(envir@LHS, envir@RHS), 
                    whCol=envir@LHS, whVal=x)
	if (is.null(ans)){
		ans <- list()
	} else {	
		cat("split...\n")
		ans <- split(ans[,2], ans[,1])
	}
	cat("assemble res...\n")
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
		cat("split data ...\n")
		ans <- split(ans[,-1], ans[,1])
	}
	cat("assemble result...\n")
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
		ans <- list()
	} else {
	    cat("split data...\n")
	    ans <- split(ans, ans[,1])
	    cat("construct GOTerms...\n")
            ans <- lapply(ans, function(i) {
		## create instances of Class 'GOTerms'
		## implicitly assume that x is a data.frame with colnames as
		## GOID, Term, Synonym, Secondary, Defintion, Ontology 
		## and all values in col GOID are the same, so do cols
		## Term and Definition. But cols Synonym and Secondary can
		## have multiple values.
		if( all(is.na(i$Secondary)))
			theSecondary <- character(0)
		else
			theSecondary <- i$Secondary
		new("GOTerms", GOID=i$GOID[1],
				Term=as.character(i$Term[1]),
				Synonym=as.character(i$Synonym),
				Secondary=theSecondary,
				Definition=as.character(i$Definition[1]),
				Ontology=as.character(i$Ontology[1]))
	    })
        }
	cat("assemble result...\n")
	res <- lapply(x, function(i) ans[[i]])
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
                ans <- list()
        } else {
		cat("split...\n")
                ans <- split(ans, ans[,1])
		cat("construct named vector...\n")
                ans <- lapply(ans, function(i) {
				val <- i[, envir@valCol]
				names(val) <- i[, envir@nameCol]
				val
		})
        }
	cat("assemble results...\n")
        res <- lapply(x, function(i) ans[[i]])
        names(res) <- as.character(x)
        res
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

setMethod("$", signature(x="AnnotDbTable", name="character"),
          function(x, name) {
              getSingleColumn(name, x@tableName, x@dbRefGetter(), x@rsProcessor)
          })

setMethod("$", signature(x="AnnotDbTableTwoWayMap", name="character"),
          function(x, name) {
              if (!name %in% c(x@LHS, x@RHS))
                stop("invalid arg: ", sQuote(name), "\n",
                     "This object supports: ",
                     paste(x@LHS, x@RHS, sep=" and "))
              getSingleColumn(name, x@tableName, x@dbRefGetter(), x@rsProcessor)
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

