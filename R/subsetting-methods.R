doLookup <- function(obj, wh) {
    lhs <- obj@LHS
    rhs <- obj@RHS
    sql <- paste("SELECT", lhs, ",", rhs, "FROM", obj@tableName,
                 "WHERE", lhs, "IN (",
                 paste("'", wh, "'", collapse=", ", sep=""), ")")
    if (globals$DEBUG)
      cat("DEBUG: ", sql, "\n")
    ans <- tryCatch(dbGetQuery(obj@dbRefGetter(), sql),
                    error=function(e) {
                        cat("query attempted:\n", sql)
                        stop(e)
                    })
    convertToList(ans, c(lhs, rhs))
}

setMethod("[", signature(x="AnnotDbTableTwoWayMap", i="character",
                         j="missing", drop="missing"),
          function(x, i, j, drop) {
              doLookup(obj=x, wh=i)
          })

setMethod("[[", signature(x="AnnotDbTableTwoWayMap",
                          i="character", j="missing"),
          function(x, i, j) {
              if (!identical(length(i), 1:1))
                stop("subsetting argument must have length 1")
              doLookup(obj=x, wh=i)
          })

setMethod("mget", signature(x="character", envir="AnnotDbTableTwoWayMap",
                            mode="missing", ifnotfound="missing",
                            inherits="missing"),
          function(x, envir, mode, ifnotfound, inherits) {
              doLookup(obj=envir, wh=x)
          })

getSingleColumn <- function(column, table, db) {
    query <- paste("SELECT", column, "FROM", table)
    ans <- tryCatch(dbGetQuery(db, query),
                    error=function(e) {
                        cat("query attempted:\n", sql)
                        stop(e)
                    })
    convertToList(ans, column)
}

setMethod("$", signature(x="AnnotDbTable", name="character"),
          function(x, name) {
              getSingleColumn(name, x@tableName, x@dbRefGetter())
          })

setMethod("$", signature(x="AnnotDbTableTwoWayMap", name="character"),
          function(x, name) {
              if (!name %in% c(x@LHS, x@RHS))
                stop("invalid arg: ", sQuote(name), "\n",
                     "This object supports: ",
                     paste(x@LHS, x@RHS, sep=" and "))
              getSingleColumn(name, x@tableName, x@dbRefGetter())
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

