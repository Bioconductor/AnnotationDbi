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
    if (length(names) == 1 && is.na(names))
        paste(colname, "IS NOT NULL")
    else
        paste(colname, " IN (", .toSQLStringSet(names), ")", sep="")
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### L2Rbrick/L2Rpath manipulation.
###
### AnnDbBimap objects have an L2Rpath slot which must be a non-empty list of
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
        paste("{", x@Lcolname, "}", x@tablename, "{", x@Rcolname, "}", sep="")
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

L2Rpath.rev <- function(L2Rpath) rev(lapply(L2Rpath, rev))

.L2Rpath.toString <- function(L2Rpath) paste(sapply(L2Rpath, toString), collapse="-")

L2Rpath.Ltablename <- function(L2Rpath) L2Rpath[[1]]@tablename
L2Rpath.Rtablename <- function(L2Rpath) L2Rpath[[length(L2Rpath)]]@tablename

L2Rpath.Lcolname <- function(L2Rpath) L2Rpath[[1]]@Lcolname
L2Rpath.Rcolname <- function(L2Rpath) L2Rpath[[length(L2Rpath)]]@Rcolname

L2Rpath.Lfilter <- function(L2Rpath)
{
    filter <- L2Rpath[[1]]@filter
    if (filter == "1")
        return(filter)
    paste("(", .contextualizeColnames(filter), ")", sep="")
}

L2Rpath.Rfilter <- function(L2Rpath)
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

### THIS IS THE CURRENT DESIGN: the left col is the 1st col, the right col is
### the 2nd col and then we have all the tags,  IT MUST BE KEPT CONSISTENT
### THROUGH ALL THE REST OF THIS FILE... FOR NOW.
L2Rpath.collabels <- function(L2Rpath)
    c("left", "right", rep("tag", length(L2Rpath.tagnames(L2Rpath))))

L2Rpath.colnames <- function(L2Rpath)
    c(L2Rpath.Lcolname(L2Rpath),
      L2Rpath.Rcolname(L2Rpath),
      L2Rpath.tagnames(L2Rpath))

### Return a named list of 5 elements. Those elements are pieces of an SQL
### SELECT statement used by some of the DB functions in this file to build
### appropriate ( and hopefully valid ;-) ) full SELECT statements.
### Those code chunks correspond to the following parts of the SELECT
### statement:
###   SELECT what FROM from WHERE where
### hence the 5 elements returned by .makeSQLchunks() are divided in
### 3 groups:
###   1) The "what" group:
###      - what_Lcol: single string containing the "contextualized" name of
###        the leftmost col of 'L2Rpath'.
###      - what_Rcol: same but for the rightmost col.
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
.makeSQLchunks <- function(L2Rpath, with.tags=TRUE)
{
    what_tagCols <- where <- character(0)
    pathlen <- length(L2Rpath)
    for (i in seq_len(pathlen)) {
        L2Rbrick <- L2Rpath[[i]]
        tablename <- L2Rbrick@tablename
        Lcolname <- L2Rbrick@Lcolname
        Rcolname <- L2Rbrick@Rcolname
        if (pathlen == 1) {
            context <- from <- tablename
            what_Lcol <- paste(context, Lcolname, sep=".")
            what_Rcol <- paste(context, Rcolname, sep=".")
        } else {
            if (i == 1) {
                context <- "_left"
                what_Lcol <- paste(context, Lcolname, sep=".")
                from <- paste(tablename, "AS", context)
            } else {
                if (i == pathlen) {
                    context <- "_right"
                    what_Rcol <- paste(context, Rcolname, sep=".")
                } else {
                    context <- paste("_", i, sep="")
                }
                on <- paste(prev_context, ".", prev_Rcolname, "=",
                            context, ".", Lcolname, sep="")
                from <- paste(from, "INNER JOIN", tablename, "AS", context, "ON", on)
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
        what_Lcol=what_Lcol,
        what_Rcol=what_Rcol,
        what_tagCols=what_tagCols,
        from=from,
        where=where
    )
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### DB functions.
###

assign("debugSQL", FALSE, envir=RTobjs)

debug.sql <- function()
{
    debugSQL <- !get("debugSQL", envir=RTobjs)
    assign("debugSQL", debugSQL, envir=RTobjs)
    debugSQL
}

### Use .dbGetQuery(conn, SQL, 1) instead of .dbGetQuery(conn, SQL)[[1]],
### it's much safer!
.dbGetQuery <- function(conn, SQL, j0=NA)
{
    if (get("debugSQL", envir=RTobjs)) {
        if (!is.character(SQL) || length(SQL) != 1 || is.na(SQL))
            stop("'SQL' must be a single string")
        cat("SQL query: ", SQL, "\n", sep="")
        st <- system.time(data0 <- dbGetQuery(conn, SQL))
        cat("     time: ", st["user.self"], " seconds\n", sep="")
    } else {
        data0 <- dbGetQuery(conn, SQL)
    }
    if (is.na(j0))
        return(data0)
    ## Work around pb with the SQLite driver returning a data frame with
    ## 0 columns when the number of rows is 0
    if (nrow(data0) == 0)
        character(0)
    else
        data0[[j0]]
}

dbGetTable <- function(conn, tablename, extra.SQL=NULL)
{
    SQL <- paste("SELECT * FROM ", tablename, sep="")
    if (!is.null(extra.SQL))
        SQL <- paste(SQL, extra.SQL)
    .dbGetQuery(conn, SQL)
}

### CURRENTLY BROKEN!
dbRawAnnDbMapToTable <- function(conn, Ltablename, Lcolname, Lkeys,
                                       Rtablename, Rcolname, Rkeys,
                                       show.colnames, from)
{
#    if (!is.null(Rtablename))
#        Rcolname <- paste(Rtablename, Rcolname, sep=".")
#    Lcolname <- paste(Ltablename, Lcolname, sep=".")
    SQL <- paste("SELECT", paste(show.colnames, collapse=","), "FROM", from)
    SQL <- paste(SQL, "WHERE", .toSQLWhere(Lcolname, Lkeys))
    if (!is.null(Rtablename))
        SQL <- paste(SQL, "AND", .toSQLWhere(Rcolname, Rkeys))
    .dbGetQuery(conn, SQL)
}

### CURRENTLY BROKEN!
dbCountRawAnnDbMapRows <- function(conn, Ltablename, Lcolname, 
                                         Rtablename, Rcolname, from)
{
    SQL <- paste("SELECT COUNT(*) FROM", from)
    SQL <- paste(SQL, "WHERE", .toSQLWhere(Lcolname, NA))
    if (!is.null(Rtablename))
        SQL <- paste(SQL, "AND", .toSQLWhere(Rcolname, NA))
    .dbGetQuery(conn, SQL, 1)
}

dbGetMapLinks <- function(conn, L2Rpath)
{
    stop("COMING SOON, SORRY!")
}

dbCountMapLinks <- function(conn, L2Rpath)
{
    stop("COMING SOON, SORRY!")
}

.makeSQL <- function(SQLchunks, SQLwhat, Lkeys, Rkeys)
{
    where <- c(
        .toSQLWhere(SQLchunks$what_Lcol, Lkeys),
        SQLchunks$where,
        .toSQLWhere(SQLchunks$what_Rcol, Rkeys)
    )
    where <- paste(where, collapse=" AND ")
    paste("SELECT", SQLwhat, "FROM", SQLchunks$from, "WHERE", where)
}

dbSelectFromL2Rpath <- function(conn, L2Rpath, Lkeys, Rkeys)
{
    SQLchunks <- .makeSQLchunks(L2Rpath)
    what_Lcol <- SQLchunks$what_Lcol
    what_Rcol <- SQLchunks$what_Rcol
    what_tagCols <- SQLchunks$what_tagCols
    SQLwhat <- paste(c(what_Lcol, what_Rcol, what_tagCols), collapse=",")
    SQL <- .makeSQL(SQLchunks, SQLwhat, Lkeys, Rkeys)
    .dbGetQuery(conn, SQL)
}

dbCountRowsFromL2Rpath <- function(conn, L2Rpath, Lkeys, Rkeys)
{
    SQLchunks <- .makeSQLchunks(L2Rpath, with.tags=FALSE)
    what_Lcol <- SQLchunks$what_Lcol
    what_Rcol <- SQLchunks$what_Rcol
    SQLwhat <- "COUNT(*)"
    SQL <- .makeSQL(SQLchunks, SQLwhat, Lkeys, Rkeys)
    .dbGetQuery(conn, SQL, 1)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### dbUniqueVals() and dbCountUniqueVals()
###
### IMPORTANT: Use shared cached symbols!
###

.dbUniqueVals.cached.symbol <- function(datacache, tablename, colname, filter)
{
    if (is.null(datacache) || filter != "1")
        return(NULL)
    full.colname <- paste(tablename, colname, sep=".")
    paste("dbUniqueVals", full.colname, sep="-")
}

dbUniqueVals <- function(conn, tablename, colname, filter, datacache=NULL)
{
    cached_symbol <- .dbUniqueVals.cached.symbol(datacache,
                         tablename, colname, filter)
    if (!is.null(cached_symbol)) {
        if (exists(cached_symbol, envir=datacache)) {
            vals <- get(cached_symbol, envir=datacache)
            return(vals)
        }
    }
    what <- paste("DISTINCT", colname)
    where <- .toSQLWhere(colname, NA)
    SQL <- paste("SELECT", what, "FROM", tablename, "WHERE", where)
    if (filter != "1")
        SQL <- paste(SQL, "AND", filter)
    vals <- .dbGetQuery(conn, SQL, 1)
    if (!is.character(vals))
        vals <- as.character(vals)
    if (!is.null(cached_symbol))
        assign(cached_symbol, vals, envir=datacache)
    vals
}

### Read-only caching!
dbCountUniqueVals <- function(conn, tablename, colname, filter, datacache=NULL)
{
    cached_symbol <- .dbUniqueVals.cached.symbol(datacache,
                         tablename, colname, filter)
    if (!is.null(cached_symbol)) {
        if (exists(cached_symbol, envir=datacache)) {
            count <- length(get(cached_symbol, envir=datacache))
            return(count)
        }
    }
    what <- paste("COUNT(DISTINCT ", colname, ")", sep="")
    SQL <- paste("SELECT", what, "FROM", tablename)
    if (filter != "1")
        SQL <- paste(SQL, "WHERE", filter)
    .dbGetQuery(conn, SQL, 1)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### dbUniqueMappedKeys() and dbCountUniqueMappedKeys()
###
### IMPORTANT: Use shared cached symbols!
###

.dbUniqueMappedKeys.cached.symbol <- function(datacache,
                                              L2Rpath, Lkeys, Rkeys,
                                              where, direction)
{
    if (is.null(datacache) || !is.na(Lkeys) || !is.na(Rkeys) || where != "1")
        return(NULL)
    if (direction == 1)
        symbol <- "uniqueLeftMappedKeys"
    else
        symbol <- "uniqueRightMappedKeys"
    paste(symbol, .L2Rpath.toString(L2Rpath), sep="-")
}

dbUniqueMappedKeys <- function(conn, L2Rpath, Lkeys, Rkeys,
                                     direction, datacache=NULL)
{
    SQLchunks <- .makeSQLchunks(L2Rpath, with.tags=FALSE)
    cached_symbol <- .dbUniqueMappedKeys.cached.symbol(datacache,
                         L2Rpath, Lkeys, Rkeys,
                         SQLchunks$where, direction)
    if (!is.null(cached_symbol)) {
        if (exists(cached_symbol, envir=datacache)) {
            vals <- get(cached_symbol, envir=datacache)
            return(vals)
        }
    }
    what_Lcol <- SQLchunks$what_Lcol
    what_Rcol <- SQLchunks$what_Rcol
    if (direction == 1)
        distinct_col <- what_Lcol
    else
        distinct_col <- what_Rcol
    what <- paste("DISTINCT", distinct_col)
    SQL <- .makeSQL(SQLchunks, what, Lkeys, Rkeys)
    vals <- .dbGetQuery(conn, SQL, 1)
    if (!is.null(cached_symbol))
        assign(cached_symbol, vals, envir=datacache)
    vals
}

### Read-only caching!
dbCountUniqueMappedKeys <- function(conn, L2Rpath, Lkeys, Rkeys,
                                          direction, datacache=NULL)
{
    SQLchunks <- .makeSQLchunks(L2Rpath, with.tags=FALSE)
    cached_symbol <- .dbUniqueMappedKeys.cached.symbol(datacache,
                         L2Rpath, Lkeys, Rkeys,
                         SQLchunks$where, direction)
    if (!is.null(cached_symbol)) {
        if (exists(cached_symbol, envir=datacache)) {
            count <- length(get(cached_symbol, envir=datacache))
            return(count)
        }
    }
    what_Lcol <- SQLchunks$what_Lcol
    what_Rcol <- SQLchunks$what_Rcol
    if (direction == 1)
        distinct_col <- what_Lcol
    else
        distinct_col <- what_Rcol
    SQLwhat <- paste("COUNT(DISTINCT ", distinct_col, ")", sep="")
    SQL <- .makeSQL(SQLchunks, SQLwhat, Lkeys, Rkeys)
    .dbGetQuery(conn, SQL, 1)
}

