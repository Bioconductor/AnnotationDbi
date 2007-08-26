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
### L2Rlink/L2Rchain manipulation.
###
### AnnDbBimap objects have an L2Rchain slot which must be a non-empty list of
### L2Rlink objects.
###

### Unlike the default "initialize" method, ours allows partial matching.
setMethod("initialize", "L2Rlink",
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

L2Rlink <- function(...) new("L2Rlink", ...)

setMethod("toString", "L2Rlink",
    function(x)
    {
        paste("{", x@Lkeyname, "}", x@tablename, "{", x@Rkeyname, "}", sep="")
    }
)

setMethod("show", "L2Rlink",
    function(object)
    {
        s <- paste("{Lkeyname}table{Rkeyname}:", toString(object))
        extracols <- c(
            if (!is.na(L2Rlink@tagname)) L2Rlink@tagname,
            L2Rlink@Rattribnames
        )
        if (length(extracols) != 0) {
            if (!is.null(names(extracols)))
                extracols <- paste(extracols, "AS", names(extracols))
            extracols <- paste(extracols, collapse=", ")
            s <- c(s, paste("extracols:", extracols))
        }
        if (!is.na(object@Rattrib_join[1]))
            s <- c(s, paste("Rattrib_join:", object@Rattrib_join))
        s <- c(s, paste("filter:", object@filter))
        cat(strwrap(s, exdent=4), sep="\n")
    }
)

setMethod("rev", "L2Rlink",
    function(x)
    {
        tmp <- x@Lkeyname
        x@Lkeyname <- x@Rkeyname
        x@Rkeyname <- tmp
        x
    }
)

L2Rchain.rev <- function(L2Rchain) rev(lapply(L2Rchain, rev))

.L2Rchain.toString <- function(L2Rchain) paste(sapply(L2Rchain, toString), collapse="-")

L2Rchain.Ltablename <- function(L2Rchain) L2Rchain[[1]]@tablename
L2Rchain.Rtablename <- function(L2Rchain) L2Rchain[[length(L2Rchain)]]@tablename

L2Rchain.Lkeyname <- function(L2Rchain) L2Rchain[[1]]@Lkeyname
L2Rchain.Rkeyname <- function(L2Rchain) L2Rchain[[length(L2Rchain)]]@Rkeyname

L2Rchain.Lfilter <- function(L2Rchain)
{
    filter <- L2Rchain[[1]]@filter
    if (filter == "1")
        return(filter)
    paste("(", .contextualizeColnames(filter), ")", sep="")
}

L2Rchain.Rfilter <- function(L2Rchain)
{
    filter <- L2Rchain[[length(L2Rchain)]]@filter
    if (filter == "1")
        return(filter)
    paste("(", .contextualizeColnames(filter), ")", sep="")
}

### Return the first tag colname found (from left to right) or NA if the
### L2Rchain chain has no tag.
L2Rchain.tagname <- function(L2Rchain)
{
    colname <- sapply(L2Rchain, function(L2Rlink) L2Rlink@tagname)
    colname <- colname[!is.na(colname)][1]
    if (is.na(colname))
        return(colname)
    if (!is.null(names(colname)))
        return(names(colname))
    .contextualizeColnames(colname)
}

L2Rchain.Rattribnames <- function(L2Rchain)
{
    colnames <- L2Rchain[[length(L2Rchain)]]@Rattribnames
    if (!is.null(names(colnames)))
        return(names(colnames))
    .contextualizeColnames(colnames)
}

### THIS IS THE CURRENT DESIGN: the left col is the 1st col, the right col is
### the 2nd col and then we have all the tags,  IT MUST BE KEPT CONSISTENT
### THROUGH ALL THE REST OF THIS FILE... FOR NOW.
L2Rchain.colmetanames <- function(L2Rchain)
{
    tagname <- L2Rchain.tagname(L2Rchain)
    Rattribnames <- L2Rchain.Rattribnames(L2Rchain)
    c(
        "Lkeyname",
        "Rkeyname",
        if (!is.na(tagname)) "tagname",
        rep("Rattribname", length(Rattribnames))
    )
}

L2Rchain.colnames <- function(L2Rchain)
{
    tagname <- L2Rchain.tagname(L2Rchain)
    Rattribnames <- L2Rchain.Rattribnames(L2Rchain)
    c(
        L2Rchain.Lkeyname(L2Rchain),
        L2Rchain.Rkeyname(L2Rchain),
        if (!is.na(tagname)) tagname,
        Rattribnames
    )
}

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
###        the leftmost col of 'L2Rchain'.
###      - what_Rcol: same but for the rightmost col.
###      - what_extracols: character vector of length the total number of
###        tag cols contained in 'L2Rchain' (could be 0). Each element
###        has been "contextualized" and right-pasted with " AS tag-name".
###   2) The "from" group:
###      - from: single string containing the "from" part of the SELECT.
###   3) The "where" group:
###      - where: single string obtained by "contextualizing" all filters
###        contained in 'L2Rchain', putting them in parenthezis and pasting
###        them together with the " AND " separator.
###        If 'L2Rchain' contains no filters then 'where' is the string "1".
.makeSQLchunks <- function(L2Rchain, with.extracols=TRUE)
{
    what_extracols <- where <- character(0)
    chainlen <- length(L2Rchain)
    for (i in seq_len(chainlen)) {
        L2Rlink <- L2Rchain[[i]]
        tablename <- L2Rlink@tablename
        Lkeyname <- L2Rlink@Lkeyname
        Rkeyname <- L2Rlink@Rkeyname
        if (chainlen == 1) {
            context <- from <- tablename
            what_Lcol <- paste(context, Lkeyname, sep=".")
            what_Rcol <- paste(context, Rkeyname, sep=".")
        } else {
            if (i == 1) {
                context <- "_left"
                what_Lcol <- paste(context, Lkeyname, sep=".")
                from <- paste(tablename, "AS", context)
            } else {
                if (i == chainlen) {
                    context <- "_right"
                    what_Rcol <- paste(context, Rkeyname, sep=".")
                } else {
                    context <- paste("_", i, sep="")
                }
                on <- paste(prev_context, ".", prev_Rkeyname, "=",
                            context, ".", Lkeyname, sep="")
                from <- paste(from, "INNER JOIN", tablename, "AS", context, "ON", on)
            }
            prev_context <- context
            prev_Rkeyname <- Rkeyname
        }
        if (with.extracols) {
            extracols <- c(
                if (!is.na(L2Rlink@tagname)) L2Rlink@tagname,
                L2Rlink@Rattribnames
            )
            if (length(extracols) != 0) {
                tmp <- .contextualizeColnames(extracols, context)
                if (!is.null(names(extracols)))
                    tmp <- paste(tmp, "AS", names(extracols))
                what_extracols <- c(what_extracols, tmp)
            }
            Rattrib_join <- L2Rlink@Rattrib_join
            if (!is.na(Rattrib_join))
                from <- paste(from, .contextualizeColnames(Rattrib_join, context))
        }
        filter <- L2Rlink@filter
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
        what_extracols=what_extracols,
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
dbRawAnnDbMapToTable <- function(conn, Ltablename, Lkeyname, Lkeys,
                                       Rtablename, Rkeyname, Rkeys,
                                       show.colnames, from)
{
#    if (!is.null(Rtablename))
#        Rkeyname <- paste(Rtablename, Rkeyname, sep=".")
#    Lkeyname <- paste(Ltablename, Lkeyname, sep=".")
    SQL <- paste("SELECT", paste(show.colnames, collapse=","), "FROM", from)
    SQL <- paste(SQL, "WHERE", .toSQLWhere(Lkeyname, Lkeys))
    if (!is.null(Rtablename))
        SQL <- paste(SQL, "AND", .toSQLWhere(Rkeyname, Rkeys))
    .dbGetQuery(conn, SQL)
}

### CURRENTLY BROKEN!
dbCountRawAnnDbMapRows <- function(conn, Ltablename, Lkeyname, 
                                         Rtablename, Rkeyname, from)
{
    SQL <- paste("SELECT COUNT(*) FROM", from)
    SQL <- paste(SQL, "WHERE", .toSQLWhere(Lkeyname, NA))
    if (!is.null(Rtablename))
        SQL <- paste(SQL, "AND", .toSQLWhere(Rkeyname, NA))
    .dbGetQuery(conn, SQL, 1)
}

dbGetMapLinks <- function(conn, L2Rchain)
{
    stop("COMING SOON, SORRY!")
}

dbCountMapLinks <- function(conn, L2Rchain)
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

dbSelectFromL2Rchain <- function(conn, L2Rchain, Lkeys, Rkeys)
{
    SQLchunks <- .makeSQLchunks(L2Rchain)
    what_Lcol <- SQLchunks$what_Lcol
    what_Rcol <- SQLchunks$what_Rcol
    what_extracols <- SQLchunks$what_extracols
    SQLwhat <- paste(c(what_Lcol, what_Rcol, what_extracols), collapse=",")
    SQL <- .makeSQL(SQLchunks, SQLwhat, Lkeys, Rkeys)
    .dbGetQuery(conn, SQL)
}

dbCountRowsFromL2Rchain <- function(conn, L2Rchain, Lkeys, Rkeys)
{
    SQLchunks <- .makeSQLchunks(L2Rchain, with.extracols=FALSE)
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
                                              L2Rchain, Lkeys, Rkeys,
                                              where, direction)
{
    if (is.null(datacache) || !is.na(Lkeys) || !is.na(Rkeys) || where != "1")
        return(NULL)
    if (direction == 1)
        symbol <- "uniqueLeftMappedKeys"
    else
        symbol <- "uniqueRightMappedKeys"
    paste(symbol, .L2Rchain.toString(L2Rchain), sep="-")
}

dbUniqueMappedKeys <- function(conn, L2Rchain, Lkeys, Rkeys,
                                     direction, datacache=NULL)
{
    SQLchunks <- .makeSQLchunks(L2Rchain, with.extracols=FALSE)
    cached_symbol <- .dbUniqueMappedKeys.cached.symbol(datacache,
                         L2Rchain, Lkeys, Rkeys,
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
dbCountUniqueMappedKeys <- function(conn, L2Rchain, Lkeys, Rkeys,
                                          direction, datacache=NULL)
{
    SQLchunks <- .makeSQLchunks(L2Rchain, with.extracols=FALSE)
    cached_symbol <- .dbUniqueMappedKeys.cached.symbol(datacache,
                         L2Rchain, Lkeys, Rkeys,
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

