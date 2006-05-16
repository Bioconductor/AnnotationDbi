updateCache <- function(obj) {
    ## Retreive and cache field names, row count, and first row data
    tableName <- obj@tableName
    db <- obj@dbRefGetter()
    
    obj@fieldNames <- dbListFields(db, tableName)

    rowCntSql <- paste("SELECT count(*) FROM", tableName)
    obj@nrow <- as.integer(dbGetQuery(db, rowCntSql)[1, 1])

    obj
}
    
setMethod("initialize", signature(.Object="AnnotDbTable"),
          function(.Object, tableName, dbRefGetter, rsProcessor=NULL, ...) {
              .Object@tableName <- tableName
              .Object@dbRefGetter <- dbRefGetter
	      .Object@rsProcessor <- rsProcessor
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
          function(.Object, tableName, dbRefGetter, LHS, RHS, rsProcessor=NULL, ...) {
              .Object <- callNextMethod(.Object=.Object,
                                        tableName=tableName,
                                        dbRefGetter=dbRefGetter,
					rsProcessor=rsProcessor)
              if (!all(c(LHS, RHS) %in% .Object@fieldNames))
                stop("LHS and RHS must be one of:\n",
                     paste(.Object@fieldNames, collapse=", "))
              .Object@LHS <- LHS
              .Object@RHS <- RHS
              .Object
          })

setMethod("initialize", signature(.Object="AnnotMultiColTable"),
	function(.Object, tableName, dbRefGetter, keyCol, rsProcessor=NULL, ...) {
#              .Object <- callNextMethod(.Object=.Object,
#                                        tableName=tableName,
#                                        dbRefGetter=dbRefGetter)
		#copy from parent initializer
                .Object@tableName <- tableName
                .Object@dbRefGetter <- dbRefGetter
		.Object@rsProcessor <- rsProcessor
                .Object <- updateCache(.Object)
		#end of copy
		keyIndex <- match(keyCol, .Object@fieldNames)
		if (is.na(keyIndex))
			stop("Key column ", keyCol, " is not a valid field name. Valid field names are:\n", paste(.Object@fieldNames, collapse=", "))	
		.Object@keyCol <- keyCol
		if (keyIndex != 1) {
			.Object@fieldNames[[keyIndex]] <- .Object@fieldNames[[1]]
			.Object@fieldNames[[1]] <- keyCol
			rowOneSql <- paste("SELECT ", 
				paste(.Object@fieldNames, sep="", collapse=", "),
				"FROM",
				.Object@tableName, 
				"LIMIT 1")
    			.Object@firstRow <- dbGetQuery(.Object@dbRefGetter(), rowOneSql)
		}	
		.Object
	})

setMethod("initialize", signature(.Object="AnnotThreeColTable"),
          function(.Object, tableName, dbRefGetter, keyCol, nameCol, valCol, rsProcessor=NULL, ...) {
              .Object <- callNextMethod(.Object=.Object,
                                        tableName=tableName,
                                        dbRefGetter=dbRefGetter,
					keyCol=keyCol,
					rsProcessor=rsProcessor)
		if (!(nameCol %in% .Object@fieldNames))
			stop("Name column ", nameCol, " is not a valid field name. Valid field names are:\n", paste(.Object@fieldNames, collapse=", "))
	
		if (!(valCol %in% .Object@fieldNames))
			stop("Value column ", valCol, " is not a valid field name. Valid field names are:\n", paste(.Object@fieldNames, collapse=", "))
		.Object@nameCol <- nameCol
		.Object@valCol <- valCol
		.Object
	})
	
