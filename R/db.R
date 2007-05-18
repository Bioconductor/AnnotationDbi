### =========================================================================
### DB functions
### ------------
###
### Helper functions used by the low-level API.
### Nothing from this file should need to be exported exported.
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
.L2RBRICK_LOCAL_COLNAME_REGX <- paste(
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
    gsub(.L2RBRICK_LOCAL_COLNAME_REGX, replacement, template)
}

.toSQLStringSet <- function(names)
{

    names <- gsub(.SINGLE_QUOTE, .TWO_SINGLE_QUOTE, names, fixed=TRUE)
    if (length(names) != 0)
        names <- paste(.SINGLE_QUOTE, names, .SINGLE_QUOTE, sep="")
    paste(names, collapse=",")
}

.toSQLWhere <- function(colname, names)
{
    where <- colname
    if (is.null(names))
        where <- paste(where, "IS NOT NULL")
    else
        where <- paste(where, " IN (", .toSQLStringSet(names), ")", sep="")
    where
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### L2Rpath manipulation.
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
        if (!is.na(object@attribJoin[1]))
            s <- c(s, paste("attribJoin:", object@attribJoin))
        if (!is.na(object@attribCols[1])) {
            attribCols <- object@attribCols
            if (!is.null(names(attribCols)))
                attribCols <- paste(attribCols, "AS", names(attribCols))
            attribCols <- paste(attribCols, collapse=", ")
            s <- c(s, paste("attribCols:", attribCols))
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
    attrib_cols <- NULL
    pathlen <- length(L2Rpath)
    for (i in seq_len(pathlen)) {
        attribCols <- L2Rpath[[i]]@attribCols
        if (is.na(attribCols[1]))
            next
        cols <- names(attribCols)
        if (is.null(cols))
            cols <- .contextualizeColnames(attribCols)
        attrib_cols <- c(attrib_cols, cols)
    }
    attrib_cols
}

L2Rpath.rev <- function(L2Rpath) rev(lapply(L2Rpath, rev))

.L2Rpath.toString <- function(L2Rpath) paste(sapply(L2Rpath, toString), collapse="-")


### Return a named list of 5 elements. Those elements are pieces of SQL
### code that can be put together to build all the possible SELECT statements
### used by this low-level API.
### Currently, this API only needs to build SELECT queries of the form
###   SELECT what FROM from WHERE where
### hence the 5 elements returned by .getSelectPieces() are divided in
### 3 groups:
###   1) The "what" group:
###      - Lrescol: single string containing the "contextualized" name of
###        the leftmost col of 'L2Rpath'.
###      - Rrescol: same but for the rightmost col.
###      - attrib_rescols: character vector of length the total number of
###        attribute cols contained in 'L2Rpath' (could be 0). Each element
###        has been "contextualized" and right-pasted with " AS attrib-name".
###   2) The "from" group:
###      - from: single string containing the "from" part of the SELECT.
###   3) The "where" group:
###      - where: single string obtained by "contextualizing" all filters
###        contained in 'L2Rpath', putting them in parenthezis and pasting
###        them together with the " AND " separator.
###        If 'L2Rpath' contains no filters then 'where' is the string "1".
.getSelectPieces <- function(L2Rpath, with.attribs=TRUE)
{
    attrib_rescols <- where <- character(0)
    pathlen <- length(L2Rpath)
    for (i in seq_len(pathlen)) {
        L2Rbrick <- L2Rpath[[i]]
        table <- L2Rbrick@table
        Lcolname <- L2Rbrick@Lcolname
        Rcolname <- L2Rbrick@Rcolname
        if (pathlen == 1) {
            context <- from <- table
            Lrescol <- paste(context, Lcolname, sep=".")
            Rrescol <- paste(context, Rcolname, sep=".")
        } else {
            if (i == 1) {
                context <- "_left"
                Lrescol <- paste(context, Lcolname, sep=".")
                from <- paste(table, "AS", context)
            } else {
                if (i == pathlen) {
                    context <- "_right"
                    Rrescol <- paste(context, Rcolname, sep=".")
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
        if (with.attribs) {
            attribJoin <- L2Rbrick@attribJoin
            attribCols <- L2Rbrick@attribCols
            if (!is.na(attribJoin))
                from <- paste(from, .contextualizeColnames(attribJoin, context))
            if (!is.na(attribCols[1])) {
                tmp <- .contextualizeColnames(attribCols, context)
                if (!is.null(names(attribCols)))
                    tmp <- paste(tmp, "AS", names(attribCols))
                attrib_rescols <- c(attrib_rescols, tmp)
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
        Lrescol=Lrescol,
        Rrescol=Rrescol,
        attrib_rescols=attrib_rescols,
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
    pieces <- .getSelectPieces(L2Rpath)
    Lrescol <- pieces$Lrescol
    Rrescol <- pieces$Rrescol
    attrib_rescols <- pieces$attrib_rescols
    what <- paste(c(Lrescol, Rrescol, attrib_rescols, extra.colnames), collapse=",")
    where <- c(
        .toSQLWhere(Lrescol, left.names),
        pieces$where,
        .toSQLWhere(Rrescol, right.names)
    )
    where <- paste(where, collapse=" AND ")
    sql <- paste("SELECT", what, "FROM", pieces$from, "WHERE", where)
    if (verbose)
        cat(sql, "\n", sep="")
    .dbGetQuery(conn, sql)
}

dbCountRowsFromL2Rpath <- function(conn, L2Rpath, verbose=FALSE)
{
    pieces <- .getSelectPieces(L2Rpath, with.attribs=FALSE)
    Lrescol <- pieces$Lrescol
    Rrescol <- pieces$Rrescol
    what <- "COUNT(*)"
    where <- c(
        .toSQLWhere(Lrescol, NULL),
        pieces$where,
        .toSQLWhere(Rrescol, NULL)
    )
    where <- paste(where, collapse=" AND ")
    sql <- paste("SELECT", what, "FROM", pieces$from, "WHERE", where)
    if (verbose)
        cat(sql, "\n", sep="")
    .dbGetQuery(conn, sql)[[1]]
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### SQL helper functions with caching mechanism.
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
    pieces <- .getSelectPieces(L2Rpath, with.attribs=FALSE)
    use_cache <- !is.null(datacache) && pieces$where == "1"
    if (use_cache) {
        objname <- paste("dbUniqueMappedVals", .L2Rpath.toString(L2Rpath), sep="-")
        if (exists(objname, envir=datacache)) {
            vals <- get(objname, envir=datacache)
            return(vals)
        }
    }
    Lrescol <- pieces$Lrescol
    Rrescol <- pieces$Rrescol
    what <- paste("DISTINCT", Lrescol)
    where <- c(
        .toSQLWhere(Lrescol, NULL),
        pieces$where,
        .toSQLWhere(Rrescol, NULL)
    )
    where <- paste(where, collapse=" AND ")
    sql <- paste("SELECT", what, "FROM", pieces$from, "WHERE", where)
    vals <- .dbGetQuery(conn, sql)[[1]]
    if (use_cache) {
        assign(objname, vals, envir=datacache)
    }
    vals
}

### Read-only caching!
dbCountUniqueMappedVals <- function(conn, L2Rpath, datacache=NULL)
{
    pieces <- .getSelectPieces(L2Rpath, with.attribs=FALSE)
    use_cache <- !is.null(datacache) && pieces$where == "1"
    if (use_cache) {
        objname <- paste("dbUniqueMappedVals", .L2Rpath.toString(L2Rpath), sep="-")
        if (exists(objname, envir=datacache)) {
            count <- length(get(objname, envir=datacache))
            return(count)
        }
    }
    Lrescol <- pieces$Lrescol
    Rrescol <- pieces$Rrescol
    what <- paste("COUNT(DISTINCT ", Lrescol, ")", sep="")
    where <- c(
        #.toSQLWhere(Lrescol, NULL), # not needed, COUNT(DISTINCT ...) ignores NULLs
        pieces$where,
        .toSQLWhere(Rrescol, NULL)
    )
    sql <- paste("SELECT", what, "FROM", pieces$from, "WHERE", where)
    .dbGetQuery(conn, sql)[[1]]
}

