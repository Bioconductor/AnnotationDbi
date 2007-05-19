### =========================================================================
### DB functions
### ------------
###
### Helper functions used by the low-level API.
### Nothing from this file should need to be exported.
###
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Generate SQL code.
###

### Some string constants
.SINGLE_QUOTE <- '\''
.TWO_SINGLE_QUOTE <- paste(.SINGLE_QUOTE, .SINGLE_QUOTE, sep="")
.LBRACE_REGX <- '\\{'
.RBRACE_REGX <- '\\}'
.OUTOFCONTEXT_COLNAME_REGX <- paste(
    .LBRACE_REGX,
    "([^{}", .SINGLE_QUOTE, "]*)",
    .RBRACE_REGX,
    sep=""
)

### Replace out-of-context SQL colnames by their "contextualized" versions.
### Example:
###   > .contextualizeColnames("{aa} < {bb}", "SOMECONTEXT")
###   [1] "SOMECONTEXT.aa < SOMECONTEXT.bb"
###
### 'template': a character vector where elements are out-of-context SQL code.
### 'context': a single string providing the "context" i.e. a table name
###            or table alias.
.contextualizeColnames <- function(template, context)
{
    replacement <- "\\1"
    if (!missing(context) && !is.na(context) && context != "")
        replacement <- paste(context, replacement, sep=".")
    gsub(.OUTOFCONTEXT_COLNAME_REGX, replacement, template)
}

.toSQLStringSet <- function(names)
{
    if (length(names) == 0)
        return("")
    names <- gsub(.SINGLE_QUOTE, .TWO_SINGLE_QUOTE, names, fixed=TRUE)
    names <- paste(.SINGLE_QUOTE, names, .SINGLE_QUOTE, sep="")
    paste(names, collapse=",")
}

.toSQLWhere <- function(colname, names)
{
    if (is.null(names))
        paste(colname, "IS NOT NULL")
    else
        paste(colname, " IN (", .toSQLStringSet(names), ")", sep="")
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### L2Rbrick/L2Rpath manipulation.
###
### AnnDbMap objects have an L2Rpath slot which must be a non-empty list of
### L2Rbrick objects.
###

### Unlike the default "initialize" method, ours allows partial matching.
setMethod("initialize", "L2Rbrick",
    function(.Object, ...)
    {
        args <- list(...)
        if (length(args) != 0) {
            argnames <- names(args)
            if (is.null(argnames) || any(argnames == ""))
                stop("all arguments must be named")
            argnames <- match.arg(argnames, slotNames(.Object), several.ok=TRUE)
            for (i in seq_len(length(args)))
                slot(.Object, argnames[i]) <- args[[i]]
        }
        .Object
    }
)

L2Rbrick <- function(...) new("L2Rbrick", ...)

setMethod("toString", "L2Rbrick",
    function(x)
    {
        paste("{", x@Lcolname, "}", x@table, "{", x@Rcolname, "}", sep="")
    }
)

setMethod("show", "L2Rbrick",
    function(object)
    {
        s <- paste("{Lcolname}table{Rcolname}:", toString(object))
        if (!is.na(object@tagJoin[1]))
            s <- c(s, paste("tagJoin:", object@tagJoin))
        if (!is.na(object@tagCols[1])) {
            tagCols <- object@tagCols
            if (!is.null(names(tagCols)))
                tagCols <- paste(tagCols, "AS", names(tagCols))
            tagCols <- paste(tagCols, collapse=", ")
            s <- c(s, paste("tagCols:", tagCols))
        }
        s <- c(s, paste("filter:", object@filter))
        cat(strwrap(s, exdent=4), sep="\n")
    }
)

setMethod("rev", "L2Rbrick",
    function(x)
    {
        tmp <- x@Lcolname
        x@Lcolname <- x@Rcolname
        x@Rcolname <- tmp
        x
    }
)

L2Rpath.leftmostTable <- function(L2Rpath) L2Rpath[[1]]@table
L2Rpath.rightmostTable <- function(L2Rpath) L2Rpath[[length(L2Rpath)]]@table

L2Rpath.leftmostColname <- function(L2Rpath) L2Rpath[[1]]@Lcolname
L2Rpath.rightmostColname <- function(L2Rpath) L2Rpath[[length(L2Rpath)]]@Rcolname

L2Rpath.leftmostFilter <- function(L2Rpath)
{
    filter <- L2Rpath[[1]]@filter
    if (filter == "1")
        return(filter)
    paste("(", .contextualizeColnames(filter), ")", sep="")
}

L2Rpath.rightmostFilter <- function(L2Rpath)
{
    filter <- L2Rpath[[length(L2Rpath)]]@filter
    if (filter == "1")
        return(filter)
    paste("(", .contextualizeColnames(filter), ")", sep="")
}

L2Rpath.tagnames <- function(L2Rpath)
{
    tag_names <- NULL
    pathlen <- length(L2Rpath)
    for (i in seq_len(pathlen)) {
        tagCols <- L2Rpath[[i]]@tagCols
        if (is.na(tagCols[1]))
            next
        cols <- names(tagCols)
        if (is.null(cols))
            cols <- .contextualizeColnames(tagCols)
        tag_names <- c(tag_names, cols)
    }
    tag_names
}

L2Rpath.rev <- function(L2Rpath) rev(lapply(L2Rpath, rev))

.L2Rpath.toString <- function(L2Rpath) paste(sapply(L2Rpath, toString), collapse="-")


### Return a named list of 5 elements. Those elements are pieces of an SQL
### SELECT statement used by some of the DB functions in this file to build
### appropriate ( and hopefully valid ;-) ) full SELECT statements.
### Those code chunks correspond to the following parts of the SELECT
### statement:
###   SELECT what FROM from WHERE where
### hence the 5 elements returned by .getSelectChunks() are divided in
### 3 groups:
###   1) The "what" group:
###      - what_leftCol: single string containing the "contextualized" name of
###        the leftmost col of 'L2Rpath'.
###      - what_rightCol: same but for the rightmost col.
###      - what_tagCols: character vector of length the total number of
###        tag cols contained in 'L2Rpath' (could be 0). Each element
###        has been "contextualized" and right-pasted with " AS tag-name".
###   2) The "from" group:
###      - from: single string containing the "from" part of the SELECT.
###   3) The "where" group:
###      - where: single string obtained by "contextualizing" all filters
###        contained in 'L2Rpath', putting them in parenthezis and pasting
###        them together with the " AND " separator.
###        If 'L2Rpath' contains no filters then 'where' is the string "1".
.getSelectChunks <- function(L2Rpath, with.tags=TRUE)
{
    what_tagCols <- where <- character(0)
    pathlen <- length(L2Rpath)
    for (i in seq_len(pathlen)) {
        L2Rbrick <- L2Rpath[[i]]
        table <- L2Rbrick@table
        Lcolname <- L2Rbrick@Lcolname
        Rcolname <- L2Rbrick@Rcolname
        if (pathlen == 1) {
            context <- from <- table
            what_leftCol <- paste(context, Lcolname, sep=".")
            what_rightCol <- paste(context, Rcolname, sep=".")
        } else {
            if (i == 1) {
                context <- "_left"
                what_leftCol <- paste(context, Lcolname, sep=".")
                from <- paste(table, "AS", context)
            } else {
                if (i == pathlen) {
                    context <- "_right"
                    what_rightCol <- paste(context, Rcolname, sep=".")
                } else {
                    context <- paste("_", i, sep="")
                }
                on <- paste(prev_context, ".", prev_Rcolname, "=",
                            context, ".", Lcolname, sep="")
                from <- paste(from, "INNER JOIN", table, "AS", context, "ON", on)
            }
            prev_context <- context
            prev_Rcolname <- Rcolname
        }
        if (with.tags) {
            tagJoin <- L2Rbrick@tagJoin
            tagCols <- L2Rbrick@tagCols
            if (!is.na(tagJoin))
                from <- paste(from, .contextualizeColnames(tagJoin, context))
            if (!is.na(tagCols[1])) {
                tmp <- .contextualizeColnames(tagCols, context)
                if (!is.null(names(tagCols)))
                    tmp <- paste(tmp, "AS", names(tagCols))
                what_tagCols <- c(what_tagCols, tmp)
            }
        }
        filter <- L2Rbrick@filter
        if (filter != "1")
            where <- c(where, .contextualizeColnames(filter, context))
    }
    if (length(where) == 0)
        where <- "1"
    else
        where <- paste(paste("(", where, ")", sep=""), collapse=" AND ")
    list(
        what_leftCol=what_leftCol,
        what_rightCol=what_rightCol,
        what_tagCols=what_tagCols,
        from=from,
        where=where
    )
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### DB functions.
###

.dbGetQuery <- function(conn, sql)
{
    dbGetQuery(conn, sql)
}

dbGetTable <- function(conn, table, where=NULL)
{
    sql <- paste("SELECT * FROM ", table, sep="")
    if (!is.null(where))
        sql <- paste(sql, " WHERE ", where, sep="")
    .dbGetQuery(conn, sql)
}

### CURRENTLY BROKEN!
dbRawAnnDbMapToTable <- function(conn, left.db_table, left.colname, left.names,
                                       right.db_table, right.colname, right.names,
                                       show.colnames, from, verbose=FALSE)
{
#    if (!is.null(right.db_table))
#        right.colname <- paste(right.db_table, right.colname, sep=".")
#    left.colname <- paste(left.db_table, left.colname, sep=".")
    sql <- paste("SELECT", paste(show.colnames, collapse=","), "FROM", from)
    sql <- paste(sql, "WHERE", .toSQLWhere(left.colname, left.names))
    if (!is.null(right.db_table))
        sql <- paste(sql, "AND", .toSQLWhere(right.colname, right.names))
    if (verbose)
        cat(sql, "\n", sep="")
    .dbGetQuery(conn, sql)
}

### CURRENTLY BROKEN!
dbCountRawAnnDbMapRows <- function(conn, left.db_table, left.colname, 
                                         right.db_table, right.colname, from)
{
    sql <- paste("SELECT COUNT(*) FROM", from)
    sql <- paste(sql, "WHERE", .toSQLWhere(left.colname, NULL))
    if (!is.null(right.db_table))
        sql <- paste(sql, "AND", .toSQLWhere(right.colname, NULL))
    .dbGetQuery(conn, sql)[[1]]
}

dbSelectFromL2Rpath <- function(conn, L2Rpath, left.names, right.names,
                                      extra.colnames, verbose=FALSE)
{
    chunks <- .getSelectChunks(L2Rpath)
    what_leftCol <- chunks$what_leftCol
    what_rightCol <- chunks$what_rightCol
    what_tagCols <- chunks$what_tagCols
    what <- paste(c(what_leftCol, what_rightCol, what_tagCols, extra.colnames), collapse=",")
    where <- c(
        .toSQLWhere(what_leftCol, left.names),
        chunks$where,
        .toSQLWhere(what_rightCol, right.names)
    )
    where <- paste(where, collapse=" AND ")
    sql <- paste("SELECT", what, "FROM", chunks$from, "WHERE", where)
    if (verbose)
        cat(sql, "\n", sep="")
    .dbGetQuery(conn, sql)
}

dbCountRowsFromL2Rpath <- function(conn, L2Rpath, verbose=FALSE)
{
    chunks <- .getSelectChunks(L2Rpath, with.tags=FALSE)
    what_leftCol <- chunks$what_leftCol
    what_rightCol <- chunks$what_rightCol
    what <- "COUNT(*)"
    where <- c(
        .toSQLWhere(what_leftCol, NULL),
        chunks$where,
        .toSQLWhere(what_rightCol, NULL)
    )
    where <- paste(where, collapse=" AND ")
    sql <- paste("SELECT", what, "FROM", chunks$from, "WHERE", where)
    if (verbose)
        cat(sql, "\n", sep="")
    .dbGetQuery(conn, sql)[[1]]
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### DB functions with caching mechanism.
###

dbUniqueVals <- function(conn, table, colname, filter, datacache=NULL)
{
    use_cache <- !is.null(datacache) && filter == "1"
    if (use_cache) {
        full.colname <- paste(table, colname, sep=".")
        objname <- paste("dbUniqueVals", full.colname, sep="-")
        if (exists(objname, envir=datacache)) {
            vals <- get(objname, envir=datacache)
            return(vals)
        }
    }
    what <- paste("DISTINCT", colname)
    where <- .toSQLWhere(colname, NULL)
    sql <- paste("SELECT", what, "FROM", table, "WHERE", where)
    if (filter != "1")
        sql <- paste(sql, "AND", filter)
    vals <- .dbGetQuery(conn, sql)[[1]]
    if (!is.character(vals))
        vals <- as.character(vals)
    if (use_cache) {
        assign(objname, vals, envir=datacache)
    }
    vals
}

### Read-only caching!
dbCountUniqueVals <- function(conn, table, colname, filter, datacache=NULL)
{
    use_cache <- !is.null(datacache) && filter == "1"
    if (use_cache) {
        full.colname <- paste(table, colname, sep=".")
        objname <- paste("dbUniqueVals", full.colname, sep="-")
        if (exists(objname, envir=datacache)) {
            count <- length(get(objname, envir=datacache))
            return(count)
        }
    }
    what <- paste("COUNT(DISTINCT ", colname, ")", sep="")
    sql <- paste("SELECT", what, "FROM", table)
    if (filter != "1")
        sql <- paste(sql, "WHERE", filter)
    .dbGetQuery(conn, sql)[[1]]
}

dbUniqueMappedVals <- function(conn, L2Rpath, datacache=NULL)
{
    chunks <- .getSelectChunks(L2Rpath, with.tags=FALSE)
    use_cache <- !is.null(datacache) && chunks$where == "1"
    if (use_cache) {
        objname <- paste("dbUniqueMappedVals", .L2Rpath.toString(L2Rpath), sep="-")
        if (exists(objname, envir=datacache)) {
            vals <- get(objname, envir=datacache)
            return(vals)
        }
    }
    what_leftCol <- chunks$what_leftCol
    what_rightCol <- chunks$what_rightCol
    what <- paste("DISTINCT", what_leftCol)
    where <- c(
        .toSQLWhere(what_leftCol, NULL),
        chunks$where,
        .toSQLWhere(what_rightCol, NULL)
    )
    where <- paste(where, collapse=" AND ")
    sql <- paste("SELECT", what, "FROM", chunks$from, "WHERE", where)
    vals <- .dbGetQuery(conn, sql)[[1]]
    if (use_cache) {
        assign(objname, vals, envir=datacache)
    }
    vals
}

### Read-only caching!
dbCountUniqueMappedVals <- function(conn, L2Rpath, datacache=NULL)
{
    chunks <- .getSelectChunks(L2Rpath, with.tags=FALSE)
    use_cache <- !is.null(datacache) && chunks$where == "1"
    if (use_cache) {
        objname <- paste("dbUniqueMappedVals", .L2Rpath.toString(L2Rpath), sep="-")
        if (exists(objname, envir=datacache)) {
            count <- length(get(objname, envir=datacache))
            return(count)
        }
    }
    what_leftCol <- chunks$what_leftCol
    what_rightCol <- chunks$what_rightCol
    what <- paste("COUNT(DISTINCT ", what_leftCol, ")", sep="")
    where <- c(
        #.toSQLWhere(what_leftCol, NULL), # not needed, COUNT(DISTINCT ...) ignores NULLs
        chunks$where,
        .toSQLWhere(what_rightCol, NULL)
    )
    sql <- paste("SELECT", what, "FROM", chunks$from, "WHERE", where)
    .dbGetQuery(conn, sql)[[1]]
}

