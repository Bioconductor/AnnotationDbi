## Most of this file belongs in an AnnotationDBI or similarly named package.
## However, the first bits that reference the 'globals' env probably need
## to be generated and part of the actual data package.  The exception is
## globals$DEBUG which really belongs to the interface layer.

## We use a package-global environment to store table names, DB path,
## and the connection to the DB itself.  Using an env is key because it
## allows us to establish the DB con inside .onLoad via the
## initDbConnection() function.
globals <- new.env(parent=emptyenv())
globals$DEBUG <- FALSE

toggleDebug <- function() {
    globals$DEBUG <- !(globals$DEBUG)
}

validateFields <- function(fields, theFields) {
    ## Raise an error if fields are not columns of table.
    badFields <- fields[!(fields %in% theFields)]
    if (length(badFields) > 0)
      stop("unknown field names:\n", paste(badFields, collapse=", "),
           "\nknown fields are:\n", paste(theFields, collapse=", "))
}

processWhereArg <- function(where, whereOp, theFields) {
    ## Build an SQL WHERE clause from the 'where' argument passed
    ## to doQueryTable.  Here, 'where' should be a named list.  The
    ## names are column names, the values are column values to
    ## restrict to using IN.
    WHERE <- ""
    where <- where[sapply(where, function(x) !is.null(x))]
    if (length(where) > 0) {
        whereFields <- names(where)
        if (is.null(whereFields))
          stop("invalid arg: where must have names")
        if (any(duplicated(whereFields)))
          stop("invalid arg: where must have unique names")
        validateFields(whereFields, theFields)
        for (fld in whereFields) {
            where[[fld]] <- paste(fld, " IN (",
                                  paste("'", where[[fld]], "'",
                                        sep="", collapse=", "),
                                  ")", sep="")
        }
        whereOp <- paste(" ", whereOp, " ", sep="")
        WHERE <- paste("WHERE", paste(unlist(where), collapse=whereOp))
    }
    WHERE
}


doQueryTable <- function(table, fields, where, whereOp=c("AND", "OR"))
{
    ## Return a data.frame representing the requested subset of a
    ## a given DB table.
    ## This function provides a *simple* and purposely incomplete
    ## alternative to using SQL to query the table.  See the
    ## vignette for a few examples.
    ##
    ## The globals$DEBUG should be at the interface layer, I think
    ## The dbGetter is being passed in from an AnnotDbTable instance
    ## so passing this on to validateFields and processWhereArg would
    ## make sense.
    if (!is.character(fields) || length(fields) < 1)
      stop("invalid arg: ", sQuote("fields"),
           " must be a character vector with positive length")
    if (!is.list(where))
      stop("invalid arg: ", sQuote("where"), " must be a list")
    validateFields(fields, names(table))
    WHERE <- ""
    if (length(where) > 0)
      WHERE <- processWhereArg(where, match.arg(whereOp), names(table))
    theFields <- paste(fields, collapse=", ")
    query <- paste("SELECT", theFields, "FROM", table@tableName, WHERE)
    if (globals$DEBUG)
      cat("DEBUG:", query, "\n\n")
    ans <- tryCatch(dbGetQuery(table@dbRefGetter(), query),
                    error=function(e) {
                        cat("query attempted:\n", query)
                        stop(e)
                    })
    ans
}


convertToList <- function(ans, fields) {
    ## Given a data.frame 'ans' that came from a DBI query and
    ## a list of fields (columns) used in the SELECT, list-ify
    ## to put the data in a compact list of vectors format.
    if (is.null(ans) || (is.data.frame(ans) && nrow(ans) == 0))
      return(character(0))
    ## FIXME: we need a better way to deal with missing LHS (primary key).
    ans[, fields[1]][is.na(ans[, fields[1]])] <- "NA"
    if (length(fields) > 1)
      ans <- split(ans[, -1], ans[, 1])
    else {
        ## since split() sorts, we will sort here
        ans <- sort(ans[, 1])
    }
    if (length(fields) > 2) {
        ans <- lapply(ans, as.list)
    }
    ans
}

queryAndConvert <- function(fields, where, table, dbGetter) {
    ans <- doQueryTable(fields, where, table=table, dbGetter=dbGetter)
    convertToList(ans, fields)
}


## queryTable methods
## FIXME: doQueryTable is slow, I suspect that the defensive programming
##        is taking its toll.

setGeneric("queryTable",
           function(table, fields=1, where=1, whereOp="AND") {
               standardGeneric("queryTable")
           })

setMethod("queryTable",
          signature(table="AnnotDbTable",
                    fields="ANY",
                    where="ANY",
                    whereOp="ANY"),
          function(table, fields=chracter(0), where=NULL, whereOp="AND") {
              doQueryTable(table, fields, where, whereOp)
          })


