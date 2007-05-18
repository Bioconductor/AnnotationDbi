### =========================================================================
### Low-level API for AnnDbObj objects
### ----------------------------------
###
### The "AnnDbObj" class is a general purpose container for SQLite-based
### annotation data (refer to AllClasses.R for the definition of the "AnnDbObj"
### class and its derived classes).
###
### This file defines and implements the low-level API for AnnDbObj objects.
### It is divided in 2 sections:
###
###   A. Helper functions used by the low-level API.
###
###   B. The low-level API for AnnDbObj objects.
###      This API consists of the following set of methods for AnnDbObj objects:
###          reverse
###          db, left.db_table, left.colname, right.db_table, right.colname,
###          toTable, as.data.frame, nrow
###          left.names, right.names, names
###          left.length, right.length, length
###          show,
###          as.character, toList,
###          mapped.left.names, count.mapped.left.names
###          mapped.right.names, count.mapped.right.names
###          mapped.names, count.mapped.names
###          is.na
###      NB: "names", "length", "mapped.names" and "count.mapped.names" are
###      "oriented" methods i.e. they give a different result on a map and its
###      "reverse" map (this result depends on the orientation of the map).
###      For each of these methods, there are 2 "unoriented" methods: a left
###      method and a right method.
###
### The environment-like API for AnnDbMap objects (ls, mget, etc...) is defined
### in the AnnDbMap-envirAPI.R file.
###
### -------------------------------------------------------------------------



### =========================================================================
### A. Helper functions used by the low-level API.
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
### Manipulate the L2Rpath slot (list of L2Rbrick objects).
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

.left.db_table <- function(L2Rpath) L2Rpath[[1]]@table
.right.db_table <- function(L2Rpath) L2Rpath[[length(L2Rpath)]]@table

.left.colname <- function(L2Rpath) L2Rpath[[1]]@Lcolname
.right.colname <- function(L2Rpath) L2Rpath[[length(L2Rpath)]]@Rcolname

.left.filter <- function(L2Rpath)
{
    filter <- L2Rpath[[1]]@filter
    if (filter == "1")
        return(filter)
    paste("(", .contextualizeColnames(filter), ")", sep="")
}

.right.filter <- function(L2Rpath)
{
    filter <- L2Rpath[[length(L2Rpath)]]@filter
    if (filter == "1")
        return(filter)
    paste("(", .contextualizeColnames(filter), ")", sep="")
}

.tagnames <- function(L2Rpath)
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

.revL2Rpath <- function(L2Rpath) rev(lapply(L2Rpath, rev))

.L2RpathToString <- function(L2Rpath) paste(sapply(L2Rpath, toString), collapse="-")


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
        objname <- paste("dbUniqueMappedVals", .L2RpathToString(L2Rpath), sep="-")
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
        objname <- paste("dbUniqueMappedVals", .L2RpathToString(L2Rpath), sep="-")
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



### =========================================================================
### B. The low-level API for AnnDbObj objects.
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "revmap" new generic.
###
### Note that I'd like to use "reverse" instead of "revmap" but "reverse" is
### already defined as a generic in Biostrings and it seems that the second
### of the 2 packages to be loaded breaks the generic and attached methods
### defined in the first. Don't know how to deal with this situation :-/
### The "rev" generic defined in package:base doesn't work neither because
### we want to be able to use a different signature (2 args).
###

.revmap <- function(class, x, objName)
{
    if (is.null(objName))
        objName <- paste("revmap(", x@objName, ")", sep="")
    else
        objName <- as.character(objName)
    new(class, x, objName=objName)
}

setMethod("revmap", "AtomicAnnDbMap",
    function(x, objName=NULL) .revmap("RevAtomicAnnDbMap", x, objName)
)
setMethod("revmap", "RevAtomicAnnDbMap",
    function(x, objName=NULL) stop("already a reverse map")
)

setMethod("revmap", "GoAnnDbMap",
    function(x, objName=NULL) .revmap("RevGoAnnDbMap", x, objName)
)
setMethod("revmap", "RevGoAnnDbMap",
    function(x, objName=NULL) stop("already a reverse map")
)

setMethod("revmap", "Go3AnnDbMap",
    function(x, objName=NULL) .revmap("RevGo3AnnDbMap", x, objName)
)
setMethod("revmap", "RevGo3AnnDbMap",
    function(x, objName=NULL) stop("already a reverse map")
)

setMethod("revmap", "environment",
    function(x, objName=NULL) l2e(reverseSplit(as.list(x)))
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "db", "left.db_table", "right.db_table", "left.colname",
### "right.colname", "tagnames", "colnames", "left.filter"
### and "right.filter" generics.
###

setMethod("db", "AnnDbObj", function(object) object@conn)

setMethod("left.db_table", "AnnDbMap", function(x) .left.db_table(x@L2Rpath))
setMethod("right.db_table", "AnnDbMap", function(x) .right.db_table(x@L2Rpath))
setMethod("right.db_table", "Go3AnnDbMap", function(x) x@rightTables)

setMethod("left.colname", "AnnDbMap", function(x) .left.colname(x@L2Rpath))
setMethod("right.colname", "AnnDbMap", function(x) .right.colname(x@L2Rpath))
setMethod("tagnames", "AnnDbMap", function(x) .tagnames(x@L2Rpath))
setMethod("tagnames", "Go3AnnDbMap", function(x) c(.tagnames(x@L2Rpath), "Ontology"))
setMethod("colnames", "AnnDbMap",
    function(x, do.NULL=TRUE, prefix="col")
        c(left.colname(x), right.colname(x), tagnames(x))
)

setMethod("left.filter", "AnnDbMap", function(x) .left.filter(x@L2Rpath))
setMethod("right.filter", "AnnDbMap", function(x) .right.filter(x@L2Rpath))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "toTable" new generic.
###
### Note that because we use the same JOIN for a map and its corresponding
### "reverse" map (this is made possible thanks to the use of INNER joins),
### then the result returned by "toTable" or "nrow" does not depend on the
### orientation (direct/reverse) of the map which is a nice property
### (e.g. 'nrow(map) == nrow(revmap(map))').
###
### The 'left.names' and 'right.names' args can be one of the following:
###   - NULL: the arg is ignored.
###   - A NA-free character vector: only the rows with a "left name" (1st
###     field) matching one of the names in 'left.names' and a "right name"
###     (2nd field) matching one of the names in 'right.names' are
###     retrieved.
### Note that the 'left.names' and 'right.names' args are _not_ checked i.e.
### only NULL and NA-free character vectors are guaranted to work properly.
###

setMethod("toTable", "AnnDbTable",
    function(x, left.names=NULL, verbose=FALSE)
    {
        dbRawAnnDbMapToTable(db(x), left.db_table(x), left.colname(x), left.names,
                                    NULL, NULL, NULL,
                                    x@showCols, x@from, verbose)
    }
)

setMethod("toTable", "AnnDbMap",
    function(x, left.names=NULL, right.names=NULL, extra.colnames=NULL, verbose=FALSE)
    {
        dbSelectFromL2Rpath(db(x), x@L2Rpath, left.names, right.names,
                                   extra.colnames, verbose)
    }
)

### This method needs to retrieve and bind data from the 3 GO tables.
### Binding the results of the 3 SELECTs can be done early in SQLite with
### a UNION:
###   dbGetQuery("query1 UNION query2 UNION query3")
### or later in R with rbind():
###   rbind(dbGetQuery("query1"), dbGetQuery("query2"), dbGetQuery("query3"))
### Surprisingly the latter is almost twice faster than the former!
setMethod("toTable", "Go3AnnDbMap",
    function(x, left.names=NULL, right.names=NULL, extra.colnames=NULL, verbose=FALSE)
    {
        extra.colnames <- c("evidence", extra.colnames)
        getPartialSubmap <- function(ontology)
        {
            table <- right.db_table(x)[ontology]
            L2Rpath <- x@L2Rpath
            L2Rpath[[length(L2Rpath)]]@table <- table
            data <- dbSelectFromL2Rpath(db(x), L2Rpath, left.names, right.names,
                                               extra.colnames, verbose)
            if (nrow(data) != 0)
                data[["Ontology"]] <- ontology
            data
        }
        rbind(getPartialSubmap("BP"),
              getPartialSubmap("CC"),
              getPartialSubmap("MF"))
    }
)

### "as.data.frame" is equivalent to "toTable". Might be deprecated soon.
setMethod("as.data.frame", "AnnDbObj",
    function(x, row.names=NULL, optional=FALSE,
             left.names=NULL, right.names=NULL, ...)
    {
        if (missing(left.names))
            left.names <- row.names
        if (missing(right.names)) {
            if (missing(optional))
                return(toTable(x, left.names, ...))
            if (!identical(optional, FALSE))
                right.names <- optional
        }
        toTable(x, left.names, right.names=right.names, ...)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "nrow" new generic.
###
### Conceptual definition (for AnnDbMap object x):
###     nrow(x) :== nrow(toTable(x))
###
### Since "toTable" is unoriented, then "nrow" is unoriented too.
###

setMethod("nrow", "AnnDbTable",
    function(x)
    {
        dbCountRawAnnDbMapRows(db(x), left.db_table(x), left.colname(x), NULL, NULL, x@from)
    }
)

setMethod("nrow", "AnnDbMap",
    function(x) dbCountRowsFromL2Rpath(db(x), x@L2Rpath)
)

setMethod("nrow", "Go3AnnDbMap",
    function(x)
    {
        countRows <- function(ontology)
        {
            table <- right.db_table(x)[ontology]
            L2Rpath <- x@L2Rpath
            L2Rpath[[length(L2Rpath)]]@table <- table
            dbCountRowsFromL2Rpath(db(x), x@L2Rpath)
        }
        countRows("BP") + countRows("CC") + countRows("MF")
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "left.names", "right.names" and "names" generics.
###

setMethod("left.names", "AnnDbObj",
    function(x)
    {
        dbUniqueVals(db(x), left.db_table(x), left.colname(x),
                            left.filter(x), x@datacache)
    }
)

setMethod("right.names", "AnnDbMap",
    function(x)
    {
        dbUniqueVals(db(x), right.db_table(x), right.colname(x),
                            right.filter(x), x@datacache)
    }
)

setMethod("right.names", "Go3AnnDbMap",
    function(x)
    {
        getNames <- function(ontology)
        {
            table <- right.db_table(x)[ontology]
            dbUniqueVals(db(x), table, "go_id", right.filter(x), x@datacache)
        }
        ## Because a given go_id can only belong to 1 of the 3 ontologies...
        ## (if not, then apply unique to this result)
        c(getNames("BP"), getNames("CC"), getNames("MF"))
    }
)

setMethod("names", "AnnDbObj", function(x) left.names(x))
setMethod("names", "RevAnnDbMap", function(x) right.names(x))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "left.length", "right.length" and "length" generic.
###
### Conceptual definitions (for AnnDbMap object x):
###     left.length(x) :== length(left.names(x))
###     right.length(x) :== length(right.names(x))
###

setMethod("left.length", "AnnDbObj",
    function(x)
    {
        dbCountUniqueVals(db(x), left.db_table(x), left.colname(x),
                                 left.filter(x), x@datacache)
    }
)

setMethod("right.length", "AnnDbMap",
    function(x)
    {
        dbCountUniqueVals(db(x), right.db_table(x), right.colname(x),
                                 right.filter(x), x@datacache)
    }
)

setMethod("right.length", "Go3AnnDbMap",
    function(x)
    {
        countNames <- function(ontology)
        {
            table <- right.db_table(x)[ontology]
            dbCountUniqueVals(db(x), table, "go_id", right.filter(x), x@datacache)
        }
        ## Because a given go_id can only belong to 1 of the 3 ontologies...
        countNames("BP") + countNames("CC") + countNames("MF")
    }
)

setMethod("length", "AnnDbObj", function(x) left.length(x))
setMethod("length", "RevAnnDbMap", function(x) right.length(x))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" methods.
###

setMethod("show", "AnnDbTable",
    function(object)
    {
        cat(object@objName, " table for ", object@objTarget,
            " (object of class \"", class(object), "\")\n", sep="")
    }
)

setMethod("show", "AnnDbMap",
    function(object)
    {
        cat(object@objName, " map for ", object@objTarget,
            " (object of class \"", class(object), "\")\n", sep="")
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "as.character" generic.
###
### For untagged Reverse/AtomicAnnDbMap obects only!
###

### R doesn't let me add a 'names' arg here:
###  Error in rematchDefinition(definition, fdef, mnames, fnames, signature) :
###          methods can add arguments to the generic only if '...' is an argument to the generic
setMethod("as.character", "AtomicAnnDbMap",
    function(x)
    {
        if (length(tagnames(x)) != 0)
            stop("AtomicAnnDbMap object with tags cannot be coerced to a character vector")
        data <- toTable(x)
        ans <- data[[2]] # could also use [[right.colname(x)]]
        if (!is.character(ans))
            ans <- as.character(ans)
        names(ans) <- data[[1]] # could also use [[left.colname(x)]]
        if (any(duplicated(names(ans))))
            warning("returned vector has duplicated names")
        ans
    }
)

setMethod("as.character", "RevAtomicAnnDbMap",
    function(x)
    {
        if (length(tagnames(x)) != 0)
            stop("cannot coerce to character an AtomicAnnDbMap object with tags")
        data <- toTable(x)
        ans <- data[[1]] # could also use [[left.colname(x)]]
        if (!is.character(ans))
            ans <- as.character(ans)
        names(ans) <- data[[2]] # could also use [[right.colname(x)]]
        if (any(duplicated(names(ans))))
            warning("returned vector has duplicated names")
        ans
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "toList" new generic.
###
### The "toList" methods below have a 'names' arg.
### The 'names' arg. can be one of the following:
###   - NULL: all map elements are extracted (equivalent to passing
###           'names=names(x)' but more efficient).
###   - A NA-free character vector: the names of the returned list will
###     be those passed in 'names' in the same order. Names that are not
###     in the map are associated with NAs. Note that, unlike "mget",
###     "toList" doesn't treat differently names that are not in the map
###     from names that are in the map but associated with NAs.
###   - A NA-free numeric vector: for conveniency 'toList(x, 1:3)' is a
###     shortcut for 'toList(x, names(x)[1:3])'. This is identical to
###     'toList(x)[1:3]' but much faster.
### Note that the 'names' arg. is _not_ checked i.e. only NULL, NA-free
### character vectors and NA-free numeric vectors are guaranted to work.
###

.checkNamesExist <- function(names, all.names)
{
    if (!is.null(names)) {
        not_found <- which(!(names %in% all.names))
        if (length(not_found) != 0)
            stop("value for '", names[not_found[1]], "' not found")
    }
}

alignAnnList <- function(x, names)
{
    y <- l2e(x)
    name2val <- function(name)
    {
        val <- y[[name]]
        if (is.null(val)) {
            val <- NA
        } else if (class(val) == "data.frame") {
            row.names(val) <- NULL
        }
        val
    }
    names(names) <- names
    lapply(names, name2val)
}

setMethod("toList", "AnnDbMap",
    function(x, names=NULL)
    {
        if (!is.null(names) && length(names) == 0)
            return(list())
        data0 <- toTable(x, left.names=names)
        if (nrow(data0) == 0) {
            ann_list <- list()
        } else {
            ## Just to make sure that toTable() is not broken
            if (!identical(names(data0)[1:2], c(left.colname(x), right.colname(x))))
                stop("annotationDbi internal problem, please report to the maintainer")
            data1 <- data0[ , -1]
            if (length(x@rightColType) == 1
             && typeof(data1[[1]]) != x@rightColType) {
                converter <- get(paste("as.", x@rightColType, sep=""))
                data1[[1]] <- converter(data1[[1]])
            }
            ann_list <- split(data1, data0[[1]])
        }
        if (is.null(names))
            names <- names(x)
        alignAnnList(ann_list, names)
    }
)

setMethod("toList", "RevAnnDbMap",
    function(x, names=NULL)
    {
        if (!is.null(names) && length(names) == 0)
            return(list())
        data0 <- toTable(x, right.names=names)
        if (!is.null(names) && !all(names %in% data0[[2]])) # could also use [[right.colname(x)]]
            .checkNamesExist(names, names(x))
        if (nrow(data0) == 0) {
            ann_list <- list()
        } else {
            ## Just to make sure that toTable() is not broken
            if (!identical(names(data0)[1:2], c(left.colname(x), right.colname(x))))
                stop("annotationDbi internal problem, please report to the maintainer")
            data2 <- data0[ , -2]
            ann_list <- split(data2, data0[[2]])
        }
        if (is.null(names))
            names <- names(x)
        alignAnnList(ann_list, names)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "mapped names" family of generics:
###   - mapped.left.names, count.mapped.left.names
###   - mapped.right.names, count.mapped.right.names
###   - mapped.names, count.mapped.names
###
### Conceptual definitions (for AnnDbMap object x):
###
###     mapped.left.names(x) :== unique values in left col (col 1) of
###                              toTable(x)
###     count.mapped.left.names(x) :== length(mapped.left.names(x))
###
###     mapped.right.names(x) :== unique values in right col (col 2) of
###                               toTable(x)
###     count.mapped.right.names(x) :== length(mapped.right.names(x))
###
### Note that all "right names" should be mapped to a "left name" hence
### mapped.right.names(x) should be the same as right.names(x) (something
### worth checking in a test unit).
###

### For an AtomicAnnDbMap, x@replace.single and x@replace.multiple will be
### ignored, hence will give wrong results if one of those 2 fields has a
### non-default value like silly maps ENTREZID and MULTIHIT in AG_DB schema.
### But who cares, those maps are silly anyway...
setMethod("mapped.left.names", "AnnDbMap",
    function(x) dbUniqueMappedVals(db(x), x@L2Rpath, x@datacache)
)
setMethod("count.mapped.left.names", "AnnDbMap",
    function(x) dbCountUniqueMappedVals(db(x), x@L2Rpath, x@datacache)
)

setMethod("mapped.right.names", "AnnDbMap",
    function(x) dbUniqueMappedVals(db(x), .revL2Rpath(x@L2Rpath), x@datacache)
)
setMethod("count.mapped.right.names", "AnnDbMap",
    function(x) dbCountUniqueMappedVals(db(x), .revL2Rpath(x@L2Rpath), x@datacache)
)

setMethod("mapped.left.names", "Go3AnnDbMap",
    function(x)
    {
        getMappedNames <- function(ontology)
        {
            table <- right.db_table(x)[ontology]
            L2Rpath <- x@L2Rpath
            L2Rpath[[length(L2Rpath)]]@table <- table
            dbUniqueMappedVals(db(x), L2Rpath, x@datacache)
        }
        names1 <- getMappedNames("BP")
        names2 <- getMappedNames("CC")
        names3 <- getMappedNames("MF")
        unique(c(names1, names2, names3))
    }
)
setMethod("count.mapped.left.names", "Go3AnnDbMap",
    function(x) length(mapped.left.names(x))
)

setMethod("mapped.right.names", "Go3AnnDbMap",
    function(x)
    {
        getMappedNames <- function(ontology)
        {
            table <- right.db_table(x)[ontology]
            L2Rpath <- x@L2Rpath
            L2Rpath[[length(L2Rpath)]]@table <- table
            dbUniqueMappedVals(db(x), .revL2Rpath(L2Rpath), x@datacache)
        }
        names1 <- getMappedNames("BP")
        names2 <- getMappedNames("CC")
        names3 <- getMappedNames("MF")
        ## Because a given go_id can only belong to 1 of the 3 ontologies...
        ## (if not, then apply unique to this result)
        c(names1, names2, names3)
    }
)
setMethod("count.mapped.right.names", "Go3AnnDbMap",
    function(x)
    {
        countMappedNames <- function(ontology)
        {
            table <- right.db_table(x)[ontology]
            L2Rpath <- x@L2Rpath
            L2Rpath[[length(L2Rpath)]]@table <- table
            dbCountUniqueMappedVals(db(x), .revL2Rpath(L2Rpath), x@datacache)
        }
        ## Because a given go_id can only belong to 1 of the 3 ontologies...
        countMappedNames("BP") + countMappedNames("CC") + countMappedNames("MF")
    }
)

setMethod("mapped.names", "AnnDbMap", function(x) mapped.left.names(x))
setMethod("mapped.names", "RevAnnDbMap", function(x) mapped.right.names(x))
setMethod("count.mapped.names", "AnnDbMap", function(x) count.mapped.left.names(x))
setMethod("count.mapped.names", "RevAnnDbMap", function(x) count.mapped.right.names(x))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "is.na" generic.
###
### 'is.na(x)' is a named logical vector that associates each name in the map
### with TRUE except for those names that are actually mapped to something
### (other than an NA).
###

setMethod("is.na", "AnnDbMap",
    function(x)
    {
        mapped_names <- mapped.names(x)
        names <- names(x)
        ans <- !(names %in% mapped_names)
        names(ans) <- names
        ans
    }
)

