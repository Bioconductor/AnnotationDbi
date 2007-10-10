### =========================================================================
### SQL functions
### -------------
###
### -------------------------------------------------------------------------


assign("debugSQL", FALSE, envir=RTobjs)

debugSQL <- function()
{
    debugSQL <- !get("debugSQL", envir=RTobjs)
    assign("debugSQL", debugSQL, envir=RTobjs)
    debugSQL
}

## Needed to deal properly with "NULL data frames with 0 rows" returned
## by RSQLite when the result of a SELECT query has 0 row
.make0rowDataFrame <- function(colnames)
{
    args <- sapply(colnames, function(col) character(0))
    args <- c(args, list(check.names=FALSE, stringsAsFactors=FALSE))
    do.call("data.frame", args)
}

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
    paste(.SINGLE_QUOTE, names, .SINGLE_QUOTE, sep="", collapse=",")
}
toSQLStringSet <- .toSQLStringSet       # an alias for export

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
        paste("{", x@Lcolname, "}", x@tablename, "{", x@Rcolname, "}", sep="")
    }
)

setMethod("show", "L2Rlink",
    function(object)
    {
        callNextMethod()
        return(invisible(NULL))
        ## FIXME: code below is broken
        s <- paste("{Lcolname}table{Rcolname}:", toString(object))
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
        tmp <- x@Lcolname
        x@Lcolname <- x@Rcolname
        x@Rcolname <- tmp
        x
    }
)

L2Rchain.rev <- function(L2Rchain) rev(lapply(L2Rchain, rev))

.L2Rchain.toString <- function(L2Rchain) paste(sapply(L2Rchain, toString), collapse="-")

L2Rchain.Ltablename <- function(L2Rchain) L2Rchain[[1]]@tablename
L2Rchain.Rtablename <- function(L2Rchain) L2Rchain[[length(L2Rchain)]]@tablename

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

L2Rchain.Lkeyname <- function(L2Rchain) L2Rchain[[1]]@Lcolname
L2Rchain.Rkeyname <- function(L2Rchain) L2Rchain[[length(L2Rchain)]]@Rcolname

### Return the first tag colname found (from left to right) or NA if the
### L2Rchain chain has no tag.
L2Rchain.tagname <- function(L2Rchain)
{
    tagname <- sapply(L2Rchain, function(L2Rlink) L2Rlink@tagname)
    tagname <- tagname[!is.na(tagname)][1]
    if (is.na(tagname))
        return(tagname)
    if (!is.null(names(tagname)))
        return(names(tagname))
    .contextualizeColnames(tagname)
}

L2Rchain.Rattribnames <- function(L2Rchain)
{
    Rattribnames0 <- L2Rchain[[length(L2Rchain)]]@Rattribnames
    if (length(Rattribnames0) == 0)
        return(character(0))
    names(Rattribnames0)
}

`L2Rchain.Rattribnames<-` <- function(L2Rchain, value)
{
    if (is.null(value)) {
        L2Rchain[[length(L2Rchain)]]@Rattribnames <- character(0)
        L2Rchain[[length(L2Rchain)]]@Rattrib_join <- as.character(NA)
        return(L2Rchain)
    }
    if (!is.character(value))
        stop("Rattrib names must be a character vector or NULL")
    Rattribnames0 <- L2Rchain[[length(L2Rchain)]]@Rattribnames
    if (!all(value %in% names(Rattribnames0)))
        stop("invalid Rattrib names")
    if (any(duplicated(value)))
        stop("can't assign duplicated Rattrib names")
    L2Rchain[[length(L2Rchain)]]@Rattribnames <- Rattribnames0[value]
    L2Rchain
}

### THIS IS THE CURRENT DESIGN: the left col is the 1st col, the right col is
### the 2nd col and the (optional) tag is the 3rd col,  IT MUST BE KEPT
### CONSISTENT THROUGH ALL THE REST OF THIS FILE... FOR NOW.
L2Rchain.colmetanames <- function(L2Rchain)
{
    tagname <- L2Rchain.tagname(L2Rchain)
    c(
        "Lkeyname",
        "Rkeyname",
        if (!is.na(tagname)) "tagname"
    )
}

L2Rchain.colnames <- function(L2Rchain)
{
    tagname <- L2Rchain.tagname(L2Rchain)
    c(
        L2Rchain.Lkeyname(L2Rchain),
        L2Rchain.Rkeyname(L2Rchain),
        if (!is.na(tagname)) tagname,
        L2Rchain.Rattribnames(L2Rchain)
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
###      - what_Lkey: single string containing the "contextualized" name of
###        the leftmost col of 'L2Rchain'.
###      - what_Rkey: same but for the rightmost col.
###      - what_tag: same for the tag col (character(0) if no tag) which in
###        addition is right-pasted with " AS <tagname>".
###      - what_Rattribs: character vector (can be of length 0) where each
###        element is the "contextualized" name of a right attribute
###        right-pasted with " AS <Rattribname>".
###   2) The "from" group:
###      - from: single string containing the "from" part of the SELECT.
###   3) The "where" group:
###      - where: single string obtained by "contextualizing" all filters
###        contained in 'L2Rchain', putting them in parenthezis and pasting
###        them together with the " AND " separator.
###        If 'L2Rchain' contains no filters then 'where' is the string "1".
.makeSQLchunks <- function(L2Rchain)
{
    what_tag <- what_Rattribs <- where <- character(0)
    chainlen <- length(L2Rchain)
    for (i in seq_len(chainlen)) {
        L2Rlink <- L2Rchain[[i]]
        tablename <- L2Rlink@tablename
        Lcolname <- L2Rlink@Lcolname
        Rcolname <- L2Rlink@Rcolname
        if (chainlen == 1) {
            context <- from <- tablename
            what_Lkey <- paste(context, Lcolname, sep=".")
            what_Rkey <- paste(context, Rcolname, sep=".")
        } else {
            if (i == 1) {
                context <- "_L"
                what_Lkey <- paste(context, Lcolname, sep=".")
                from <- paste(tablename, "AS", context)
            } else {
                if (i == chainlen) {
                    context <- "_R"
                    what_Rkey <- paste(context, Rcolname, sep=".")
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
        if (!is.na(L2Rlink@tagname)) {
            if (length(what_tag) != 0)
                stop("'L2Rchain' has more than one tag")
            tagname <- L2Rlink@tagname
            what_tag <- .contextualizeColnames(tagname, context)
            if (!is.null(names(tagname)))
                what_tag <- paste(what_tag, "AS", names(tagname))
        }
        if (i == chainlen && length(L2Rlink@Rattribnames) != 0) {
            Rattribnames <- L2Rlink@Rattribnames
            what_Rattribs <- .contextualizeColnames(Rattribnames, context)
            if (is.null(names(Rattribnames)))
                stop("Rattribnames in 'L2Rchain' must be a named character vector")
            what_Rattribs <- paste(what_Rattribs, "AS", names(Rattribnames))
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
    SQLchunks <- list(
        what_Lkey=what_Lkey,
        what_Rkey=what_Rkey,
        what_tag=what_tag,
        what_Rattribs=what_Rattribs,
        from=from,
        where=where
    )
    #if (get("debugSQL", envir=RTobjs)) {
    #    cat("SQLchunks:\n")
    #    cat("  what_Lkey: ", SQLchunks$what_Lkey, "\n", sep="")
    #    cat("  what_Rkey: ", SQLchunks$what_Rkey, "\n", sep="")
    #    cat("  what_tag: ", SQLchunks$what_tag, "\n", sep="")
    #    cat("  what_Rattribs:\n")
    #    show(SQLchunks$what_Rattribs)
    #    cat("  from: ", SQLchunks$from, "\n", sep="")
    #    cat("  where:\n")
    #    show(SQLchunks$where)
    #}
    SQLchunks
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### DB functions.
###

### Use dbQuery(conn, SQL, 1) instead of dbQuery(conn, SQL)[[1]],
### it's much safer!
dbQuery <- function(conn, SQL, j0=NA)
{
    if (get("debugSQL", envir=RTobjs)) {
        if (!is.character(SQL) || length(SQL) != 1 || is.na(SQL))
            stop("[debugSQL] 'SQL' must be a single string")
        cat("[debugSQL] SQL query: ", SQL, "\n", sep="")
        st <- system.time(data0 <- dbGetQuery(conn, SQL))
        cat("[debugSQL]      time: ", st["user.self"], " seconds\n", sep="")
    } else {
        data0 <- dbGetQuery(conn, SQL)
    }
    if (is.na(j0))
        return(data0)
    ## Needed to deal properly with data frame with 0 column ("NULL data
    ## frames with 0 rows") returned by RSQLite when the result of a SELECT
    ## query has 0 row
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
    dbQuery(conn, SQL)
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
    dbQuery(conn, SQL)
}

### CURRENTLY BROKEN!
dbCountRawAnnDbMapRows <- function(conn, Ltablename, Lkeyname, 
                                         Rtablename, Rkeyname, from)
{
    SQL <- paste("SELECT COUNT(*) FROM", from)
    SQL <- paste(SQL, "WHERE", .toSQLWhere(Lkeyname, NA))
    if (!is.null(Rtablename))
        SQL <- paste(SQL, "AND", .toSQLWhere(Rkeyname, NA))
    dbQuery(conn, SQL, 1)
}

.makeSQL <- function(SQLchunks, SQLwhat, Lkeys, Rkeys)
{
    where <- c(
        .toSQLWhere(SQLchunks$what_Lkey, Lkeys),
        SQLchunks$where,
        .toSQLWhere(SQLchunks$what_Rkey, Rkeys)
    )
    where <- paste(where, collapse=" AND ")
    paste("SELECT", SQLwhat, "FROM", SQLchunks$from, "WHERE", where)
}

dbSelectFromL2Rchain <- function(conn, L2Rchain, Lkeys, Rkeys)
{
    SQLchunks <- .makeSQLchunks(L2Rchain)
    cols <- c(
        SQLchunks$what_Lkey,
        SQLchunks$what_Rkey,
        SQLchunks$what_tag,
        SQLchunks$what_Rattribs
    )
    SQLwhat <- paste(cols, collapse=",")
    SQL <- .makeSQL(SQLchunks, SQLwhat, Lkeys, Rkeys)
    data0 <- dbQuery(conn, SQL)
    if (nrow(data0) != 0)
        return(data0)
    .make0rowDataFrame(L2Rchain.colnames(L2Rchain))
}

dbCountRowsFromL2Rchain <- function(conn, L2Rchain, Lkeys, Rkeys)
{
    SQLchunks <- .makeSQLchunks(L2Rchain)
    SQLwhat <- "COUNT(*)"
    SQL <- .makeSQL(SQLchunks, SQLwhat, Lkeys, Rkeys)
    dbQuery(conn, SQL, 1)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### dbUniqueVals() and dbCountUniqueVals()
###

.dbUniqueVals.SQLresultname <- function(datacache, tablename, colname, filter)
{
    if (is.null(datacache) || filter != "1")
        return(NULL)
    full.colname <- paste(tablename, colname, sep=".")
    paste("dbUniqueVals", full.colname, sep="-")
}

dbUniqueVals <- function(conn, tablename, colname, filter, datacache=NULL)
{
    SQLresultname <- .dbUniqueVals.SQLresultname(datacache,
                         tablename, colname, filter)
    if (!is.null(SQLresultname)) {
        if (exists(SQLresultname, envir=datacache)) {
            SQLresult <- get(SQLresultname, envir=datacache)
            if (get("debugSQL", envir=RTobjs))
                cat("[debugSQL] Using cached result from SQL query: ", SQLresult$SQL, "\n", sep="")
            return(SQLresult$result)
        }
    }
    what <- paste("DISTINCT", colname)
    where <- .toSQLWhere(colname, NA)
    SQL <- paste("SELECT", what, "FROM", tablename, "WHERE", where)
    if (filter != "1")
        SQL <- paste(SQL, "AND", filter)
    vals <- dbQuery(conn, SQL, 1)
    if (!is.character(vals))
        vals <- as.character(vals)
    if (!is.null(SQLresultname)) {
        if (get("debugSQL", envir=RTobjs))
            cat("[debugSQL] Putting last SQL query and result in cache\n")
        SQLresult <- list(SQL=SQL, result=vals)
        assign(SQLresultname, SQLresult, envir=datacache)
    }
    vals
}

### Read-only caching!
dbCountUniqueVals <- function(conn, tablename, colname, filter, datacache=NULL)
{
    SQLresultname <- .dbUniqueVals.SQLresultname(datacache,
                         tablename, colname, filter)
    if (!is.null(SQLresultname)) {
        if (exists(SQLresultname, envir=datacache)) {
            SQLresult <- get(SQLresultname, envir=datacache)
            if (get("debugSQL", envir=RTobjs))
                cat("[debugSQL] Using cached result from SQL query: ", SQLresult$SQL, "\n", sep="")
            return(length(SQLresult$result))
        }
    }
    what <- paste("COUNT(DISTINCT ", colname, ")", sep="")
    SQL <- paste("SELECT", what, "FROM", tablename)
    if (filter != "1")
        SQL <- paste(SQL, "WHERE", filter)
    dbQuery(conn, SQL, 1)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### dbUniqueMappedKeys() and dbCountUniqueMappedKeys()
###
### IMPORTANT: Use shared cached symbols!
###

.dbUniqueMappedKeys.SQLresultname <- function(datacache,
                                              L2Rchain, Lkeys, Rkeys,
                                              where, direction)
{
    if (is.null(datacache)
     || length(Lkeys) != 1 || !is.na(Lkeys)
     || length(Rkeys) != 1 || !is.na(Rkeys)
     || where != "1")
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
    SQLchunks <- .makeSQLchunks(L2Rchain)
    SQLresultname <- .dbUniqueMappedKeys.SQLresultname(datacache,
                         L2Rchain, Lkeys, Rkeys,
                         SQLchunks$where, direction)
    if (!is.null(SQLresultname)) {
        if (exists(SQLresultname, envir=datacache)) {
            SQLresult <- get(SQLresultname, envir=datacache)
            if (get("debugSQL", envir=RTobjs))
                cat("[debugSQL] Using cached result from SQL query: ", SQLresult$SQL, "\n", sep="")
            return(SQLresult$result)
        }
    }
    what_Lkey <- SQLchunks$what_Lkey
    what_Rkey <- SQLchunks$what_Rkey
    if (direction == 1)
        distinct_col <- what_Lkey
    else
        distinct_col <- what_Rkey
    what <- paste("DISTINCT", distinct_col)
    SQL <- .makeSQL(SQLchunks, what, Lkeys, Rkeys)
    vals <- dbQuery(conn, SQL, 1)
    if (!is.null(SQLresultname)) {
        if (get("debugSQL", envir=RTobjs))
            cat("[debugSQL] Putting last SQL query and result in cache\n")
        SQLresult <- list(SQL=SQL, result=vals)
        assign(SQLresultname, SQLresult, envir=datacache)
    }
    vals
}

### Read-only caching!
dbCountUniqueMappedKeys <- function(conn, L2Rchain, Lkeys, Rkeys,
                                          direction, datacache=NULL)
{
    SQLchunks <- .makeSQLchunks(L2Rchain)
    SQLresultname <- .dbUniqueMappedKeys.SQLresultname(datacache,
                         L2Rchain, Lkeys, Rkeys,
                         SQLchunks$where, direction)
    if (!is.null(SQLresultname)) {
        if (exists(SQLresultname, envir=datacache)) {
            SQLresult <- get(SQLresultname, envir=datacache)
            if (get("debugSQL", envir=RTobjs))
                cat("[debugSQL] Using cached result from SQL query: ", SQLresult$SQL, "\n", sep="")
            return(length(SQLresult$result))
        }
    }
    what_Lkey <- SQLchunks$what_Lkey
    what_Rkey <- SQLchunks$what_Rkey
    if (direction == 1)
        distinct_col <- what_Lkey
    else
        distinct_col <- what_Rkey
    SQLwhat <- paste("COUNT(DISTINCT ", distinct_col, ")", sep="")
    SQL <- .makeSQL(SQLchunks, SQLwhat, Lkeys, Rkeys)
    dbQuery(conn, SQL, 1)
}

