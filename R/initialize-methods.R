## fieldNames must be valid column names of obj@tableName
## keyCol must be the first item in obj@fieldNames, if not, switch the order
updateCache <- function(obj, tableName, dbRefGetter,
			fieldNames=NULL, keyCol=NULL, 
			rsProcessor=NULL) {
    ## Retreive and cache field names, row count, and first row data
    obj@tableName <- tableName
    obj@dbRefGetter <- dbRefGetter
    db <- dbRefGetter()
    if (!is.null(rsProcessor))
	obj@rsProcessor=rsProcessor

    validFieldNames <- dbListFields(db, tableName)
    if (is.null(fieldNames)) {
	fieldNames <- validFieldNames
	if (!is.null(keyCol) && (match(keyCol, fieldNames)>1)) {
		fieldNames[match(keyCol, fieldNames)] <- fieldNames[1]
		fieldNames[1]<- keyCol
	}	
    } else {
	if (!all(fieldNames %in% validFieldNames)) 
                stop(fieldNames, " must be one of:\n",
                     paste(validFieldNames, collapse=", "))
    }
    obj@fieldNames <- fieldNames
    if (!is.null(keyCol))
	obj@keyCol <- keyCol

    rowCntSql <- paste("SELECT count(*) FROM", tableName)
    obj@nrow <- as.integer(dbGetQuery(db, rowCntSql)[1, 1])
    rowOneSql <- paste("SELECT ", 
			paste(fieldNames, sep="", collapse=", "),
			"FROM", tableName, "LIMIT 1")
    obj@firstRow <- dbGetQuery(db, rowOneSql)

    obj
}
    
setMethod("initialize", signature(.Object="AnnotDbTable"),
          function(.Object, tableName, dbRefGetter, rsProcessor=NULL, ...){ 
              .Object <- updateCache(.Object, tableName, 
				dbRefGetter, rsProcessor=rsProcessor)
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
          function(.Object, tableName, dbRefGetter, LHS, RHS, rsProcessor=NULL, ...) {
              .Object <- updateCache(.Object, tableName, dbRefGetter, 
				fieldNames=c(LHS, RHS), 
				rsProcessor=rsProcessor)
              .Object@LHS <- LHS
              .Object@RHS <- RHS
              .Object
          })

setMethod("initialize", signature(.Object="AnnotMultiColTable"),
	function(.Object, tableName, dbRefGetter, keyCol, rsProcessor=NULL, ...) {
		.Object <- updateCache(.Object, tableName, dbRefGetter,
				keyCol=keyCol, rsProcessor=rsProcessor)
		.Object
	})

setMethod("initialize", signature(.Object="AnnotMultiColTwoKeyTable"),
          function(.Object, tableName, dbRefGetter, keyCol, secKey, rsProcessor=NULL, ...) {
        .Object <- updateCache(.Object, tableName, dbRefGetter,
			keyCol=keyCol, rsProcessor=rsProcessor)
        if (!(secKey %in% .Object@fieldNames))
            stop("Secondary key column ", secKey, " is not a valid field name. Valid field names are:\n", paste(.Object@fieldNames, collapse=", "))
        .Object@secKey <- secKey
        .Object
    })
    
setMethod("initialize", signature(.Object="AnnotThreeColTable"),
          function(.Object, tableName, dbRefGetter, keyCol, nameCol, valCol, rsProcessor=NULL, ...) {
              .Object <- updateCache(.Object, tableName, dbRefGetter,
				fieldNames=c(keyCol, nameCol, valCol),
				keyCol=keyCol,
				rsProcessor=rsProcessor)
		.Object@nameCol <- nameCol
		.Object@valCol <- valCol
		.Object
	})
	
