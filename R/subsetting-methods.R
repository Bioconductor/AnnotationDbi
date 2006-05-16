doLookupComplex <- function(obj, selCol, whCol, whVal) {
	quoteStr <- ifelse(is(whVal, "character"), "'", "")
	whVal <-paste(quoteStr, whVal, quoteStr, sep="")
	selCol <- paste(selCol, "AS", selCol)
	sql <- paste("SELECT", paste(selCol, sep="", collapse=", "),
		"FROM", obj@tableName, 
		"WHERE", whCol, "=", whVal, 
		sep=" ") 
    	if (globals$DEBUG)
      		cat("DEBUG: ", sql, "\n")
    	ans <- tryCatch(
		    lapply(sql, function(x) dbGetQuery(obj@dbRefGetter(), x)),
                    error=function(e) {
                        cat("query attempted:\n", sql)
                        stop(e)
                    })
	if (!is.null(obj@rsProcessor)){
		ans <- lapply(ans, function(x) 
				if (!is.null(ans)) obj@rsProcessor(x)
			)
	}
	ans
}

setMethod("mget", signature(x="vector", envir="AnnotDbTableTwoWayMap",
                            mode="missing", ifnotfound="missing",
                            inherits="missing"),
    function(x, envir, mode, ifnotfound, inherits) {
        ans <- doLookupComplex(envir, selCol=c(envir@LHS, envir@RHS), 
                    whCol=envir@LHS, whVal=x)
        res <- lapply(ans, function(x) {
            if (is.null(ans)) 
                val <- NULL
            else 
                val <- x[, envir@RHS]   
            val
        })
        names(res) <- as.character(x)
        res
    })

setMethod("mget", signature(x="vector", envir="AnnotMultiColTable",
                            mode="missing", ifnotfound="missing",
                            inherits="missing"),
    function(x, envir, mode, ifnotfound, inherits) {
        ans <- doLookupComplex(envir, selCol=envir@fieldNames, 
                    whCol=envir@keyCol, whVal=x)
        res <- lapply(ans, function(x) {
            if (is.null(ans)) 
                val <- NULL
            else 
                val <-lapply(seq(dim(x)[1]), function(i) as.list(x[i, -1]))
            val
        })
        names(res) <- as.character(x)
        res
    })

setMethod("mget", signature(x="vector", envir="AnnotGOTermsTable",
                            mode="missing", ifnotfound="missing",
                            inherits="missing"),
    function(x, envir, mode, ifnotfound, inherits) {
        ans <- doLookupComplex(envir, selCol=envir@fieldNames, 
                    whCol=envir@keyCol, whVal=x)
        res <- lapply(ans, function(x) {
            if (is.null(x)) 
                NULL
            else {
		## create instances of Class 'GOTerms'
		## implicitly assume that x is a data.frame with colnames as
		## GOID, Term, Synonym, Secondary, Defintion, Ontology 
		## and all values in col GOID are the same, so do cols
		## Term and Definition. But cols Synonym and Secondary can
		## have multiple values.
		if( all(is.na(x$Secondary)))
			theSecondary <- character(0)
		else
			theSecondary <- x$Secondary
		new("GOTerms", GOID=x$GOID[1],
				Term=as.character(x$Term[1]),
				Synonym=as.character(x$Synonym),
				Secondary=theSecondary,
				Definition=as.character(x$Definition[1]),
				Ontology=as.character(x$Ontology[1]))
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
        res <- lapply(ans, function(x) {
            if (is.null(ans)) val <- NULL
            else {
                val <- x[, envir@valCol]
                names(val) <- x[,envir@nameCol]
            }
            val
        })  
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

