updateCache <- function(obj) {
    ## Retreive and cache field names, row count, and first row data
    tableName <- obj@tableName
    db <- obj@dbRefGetter()
    
    obj@fieldNames <- dbListFields(db, tableName)

    rowCntSql <- paste("SELECT count(*) FROM", tableName)
    obj@nrow <- as.integer(dbGetQuery(db, rowCntSql)[1, 1])

    rowOneSql <- paste("SELECT * FROM", tableName, "LIMIT 1")
    obj@firstRow <- dbGetQuery(db, rowOneSql)
    obj
}
    
setMethod("initialize", signature(.Object="AnnotDbTable"),
          function(.Object, tableName, dbRefGetter, ...) {
              .Object@tableName <- tableName
              .Object@dbRefGetter <- dbRefGetter
              ## Retreive and cache field names, row count, first row
              .Object <- updateCache(.Object)
              .Object
          })

## FIXME:
##    The documentation for the initialize generic suggests that it
##    should be possible/desirable to set things up so that in calls to
##    new(), any unnamed args are treated as template instances.  I
##    don't really see why this is useful.  I can't get it to work
##    here; it seems that the call to callNextMethod must have the
##    arguments specified.


setMethod("initialize", signature(.Object="AnnotDbTableTwoWayMap"),
          function(.Object, tableName, dbRefGetter, LHS, RHS, ...) {
              .Object <- callNextMethod(.Object=.Object,
                                        tableName=tableName,
                                        dbRefGetter=dbRefGetter)
              if (!all(c(LHS, RHS) %in% .Object@fieldNames))
                stop("LHS and RHS must be one of:\n",
                     paste(.Object@fieldNames, collapse=", "))
              .Object@LHS <- LHS
              .Object@RHS <- RHS
              .Object
          })
