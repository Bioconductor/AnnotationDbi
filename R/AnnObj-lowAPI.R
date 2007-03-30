### =========================================================================
### Low-level API for AnnObj objects
### --------------------------------
###
### The "AnnObj" class is a general purpose container for SQLite-based
### annotation data (refer to AllClasses.R for the definition of the "AnnObj"
### class and its derived classes).
###
### This file defines and implements the low-level API for AnnObj objects.
### It is divided in 2 sections:
###
###   A. Helper functions used by the low-level API.
###
###   B. The low-level API for AnnObj objects.
###      This API consists of the following set of methods for AnnObj objects:
###          reverse
###          db
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
### The environment-like API for AnnMap objects (ls, mget, etc...) is defined
### in the AnnMap-envirAPI.R file.
###
### -------------------------------------------------------------------------



### =========================================================================
### A. Helper functions used by the low-level API.
### -------------------------------------------------------------------------

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### SQL helper functions.
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

toSQLStringSet <- function(names)
{
    names <- gsub("'", "''", names, fixed=TRUE)
    paste("'", paste(names, collapse="','"), "'", sep="")
}

toSQLWhere <- function(col, names)
{
    if (is.null(names))
        return(paste(col, "IS NOT NULL"))
    #if (length(names) == 1)
    #    return(paste(col, " LIKE ", toSQLStringSet(names), sep=""))
    paste(col, " IN (", toSQLStringSet(names), ")", sep="")
}

dbRawAnnMapToTable <- function(conn, left.table, left.col, left.names,
                                         right.table, right.col, right.names,
                                         show.cols, from, verbose=FALSE)
{
#    if (!is.null(right.table))
#        right.col <- paste(right.table, right.col, sep=".")
#    left.col <- paste(left.table, left.col, sep=".")
    sql <- paste("SELECT", paste(show.cols, collapse=","), "FROM", from)
    sql <- paste(sql, "WHERE", toSQLWhere(left.col, left.names))
    if (!is.null(right.table))
        sql <- paste(sql, "AND", toSQLWhere(right.col, right.names))
    if (verbose)
        cat(sql, "\n", sep="")
    .dbGetQuery(conn, sql)
}

dbCountRawAnnMapRows <- function(conn, left.table, left.col, 
                                       right.table, right.col, from)
{
    sql <- paste("SELECT COUNT(*) FROM", from)
    sql <- paste(sql, "WHERE", toSQLWhere(left.col, NULL))
    if (!is.null(right.table))
        sql <- paste(sql, "AND", toSQLWhere(right.col, NULL))
    .dbGetQuery(conn, sql)[[1]]
}

### May be we don't need this anymore. Maybe dbRawAnnMapToTable() could
### always be used instead?
dbAnnMapToTable <- function(conn, table, join, left.col, left.names,
                                right.col, right.names, extra.cols, verbose=FALSE)
{
    ## Full col name is needed because of ambiguous column name "accession"
    ## in hgu95av2REFSEQ map.
    full.right.col <- paste(table, right.col, sep=".")
    show.cols <- c(left.col, full.right.col, extra.cols)
    sql <- paste("SELECT", paste(show.cols, collapse=","), "FROM", table)
    if (length(join) == 1) # will be FALSE for NULL or character(0)
        sql <- paste(sql, join)
    where1 <- toSQLWhere(left.col, left.names)
    where2 <- toSQLWhere(full.right.col, right.names)
    sql <- paste(sql, "WHERE", where1, "AND", where2)
    if (verbose)
        cat(sql, "\n", sep="")
    .dbGetQuery(conn, sql)
}

dbCountAnnMapRows <- function(conn, table, join, left.col, right.col)
{
    ## Full col name is needed because of ambiguous column name "accession"
    ## in hgu95av2REFSEQ map.
    full.right.col <- paste(table, right.col, sep=".")
    sql <- paste("SELECT COUNT(*) FROM", table)
    if (length(join) == 1) # will be FALSE for NULL or character(0)
        sql <- paste(sql, join)
    where1 <- toSQLWhere(left.col, NULL)
    where2 <- toSQLWhere(full.right.col, NULL)
    sql <- paste(sql, "WHERE", where1, "AND", where2)
    .dbGetQuery(conn, sql)[[1]]
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### SQL helper functions with caching mechanism.
###

dbUniqueVals <- function(conn, table, col, datacache=NULL)
{
    full.col <- paste(table, col, sep=".")
    if (!is.null(datacache)) {
        objname <- paste("dbUniqueVals", full.col, sep="-")
        if (exists(objname, envir=datacache)) {
            vals <- get(objname, envir=datacache)
            return(vals)
        }
    }
    sql <- paste("SELECT DISTINCT", col, "FROM", table,
                 "WHERE", col, "IS NOT NULL")
    vals <- .dbGetQuery(conn, sql)[[col]]
    if (!is.character(vals))
        vals <- as.character(vals)
    if (!is.null(datacache)) {
        assign(objname, vals, envir=datacache)
    }
    vals
}

### Read-only caching!
dbCountUniqueVals <- function(conn, table, col, datacache=NULL)
{
    full.col <- paste(table, col, sep=".")
    if (!is.null(datacache)) {
        objname <- paste("dbUniqueVals", full.col, sep="-")
        if (exists(objname, envir=datacache)) {
            count <- length(get(objname, envir=datacache))
            return(count)
        }
    }
    sql <- paste("SELECT COUNT(DISTINCT ", col, ") FROM ", table, sep="")
    .dbGetQuery(conn, sql)[[1]]
}

dbUniqueMappedVals <- function(conn, table, join,
                               from.table, from.col,
                               to.table, to.col, datacache=NULL)
{
    full.from.col <- paste(from.table, from.col, sep=".")
    full.to.col <- paste(to.table, to.col, sep=".")
    if (!is.null(datacache)) {
        objname <- paste("dbUniqueMappedVals", full.from.col, full.to.col, sep="-")
        if (exists(objname, envir=datacache)) {
            vals <- get(objname, envir=datacache)
            return(vals)
        }
    }
    sql <- paste("SELECT DISTINCT", full.from.col, "FROM", table)
    if (length(join) == 1) # will be FALSE for NULL or character(0)
        sql <- paste(sql, join)
    where1 <- toSQLWhere(full.from.col, NULL)
    where2 <- toSQLWhere(full.to.col, NULL)
    sql <- paste(sql, "WHERE", where1, "AND", where2)
    vals <- .dbGetQuery(conn, sql)[[from.col]]
    if (!is.null(datacache)) {
        assign(objname, vals, envir=datacache)
    }
    vals
}

### Read-only caching!
dbCountUniqueMappedVals <- function(conn, table, join,
                                    from.table, from.col,
                                    to.table, to.col, datacache=NULL)
{
    full.from.col <- paste(from.table, from.col, sep=".")
    full.to.col <- paste(to.table, to.col, sep=".")
    if (!is.null(datacache)) {
        objname <- paste("dbUniqueMappedVals", full.from.col, full.to.col, sep="-")
        if (exists(objname, envir=datacache)) {
            count <- length(get(objname, envir=datacache))
            return(count)
        }
    }
    sql <- paste("SELECT COUNT(DISTINCT ", full.from.col, ") FROM ", table, sep="")
    if (length(join) == 1) # will be FALSE for NULL or character(0)
        sql <- paste(sql, join)
    sql <- paste(sql, "WHERE", full.to.col, "IS NOT NULL")
    .dbGetQuery(conn, sql)[[1]]
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Other helper functions.
###

GOtables <- function(all=FALSE)
{
    tables <- c("go_bp", "go_cc", "go_mf")
    if (all)
        tables <- paste(tables, "_all", sep="")
    names(tables) <- c("BP", "CC", "MF")
    tables
}



### =========================================================================
### B. The low-level API for AnnObj objects.
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

setMethod("revmap", "AtomicAnnMap",
    function(x, objName=NULL)
    {
        if (is.null(objName))
            objName <- paste("revmap(", x@objName, ")", sep="")
        else
            objName <- as.character(objName)
        new("ReverseAtomicAnnMap", x, objName=objName)
    }
)
setMethod("revmap", "ReverseAtomicAnnMap",
    function(x, objName=NULL)
    {
        stop("already a reverse map")
    }
)
setMethod("revmap", "GOAnnMap",
    function(x, objName=NULL)
    {
        if (is.null(objName))
            objName <- paste("revmap(", x@objName, ")", sep="")
        else
            objName <- as.character(objName)
        new("ReverseGOAnnMap", x, objName=objName)
    }
)
setMethod("revmap", "ReverseGOAnnMap",
    function(x, objName=NULL)
    {
        stop("already a reverse map")
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "db" new generic.
###

setMethod("db", "AnnObj", function(object) object@conn)


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

setMethod("toTable", "AnnTable",
    function(x, left.names=NULL, verbose=FALSE)
    {
        dbRawAnnMapToTable(db(x), x@leftTable, x@leftCol, left.names,
                                      NULL, NULL, NULL,
                                      x@showCols, x@from, verbose)
    }
)

setMethod("toTable", "AnnMap",
    function(x, left.names=NULL, right.names=NULL, extra.cols=NULL, verbose=FALSE)
    {
        if (length(x@tagCols) != 0)
            extra.cols <- c(x@tagCols, extra.cols)
        dbAnnMapToTable(db(x), x@rightTable, x@join,
                                   x@leftCol, left.names,
                                   x@rightCol, right.names,
                                   extra.cols, verbose)
    }
)

### This method needs to retrieve and bind data from the 3 GO tables.
### Binding the results of the 3 SELECTs can be done early in SQLite with
### a UNION:
###   dbGetQuery("query1 UNION query2 UNION query3")
### or later in R with rbind():
###   rbind(dbGetQuery("query1"), dbGetQuery("query2"), dbGetQuery("query3"))
### Surprisingly the latter is almost twice faster than the former!
setMethod("toTable", "GOAnnMap",
    function(x, left.names=NULL, right.names=NULL, extra.cols=NULL, verbose=FALSE)
    {
        extra.cols <- c("evidence", extra.cols)
        getPartialSubmap <- function(Ontology)
        {
            table <- x@rightTable[Ontology]
            data <- dbAnnMapToTable(db(x), table, x@join,
                                               x@leftCol, left.names,
                                               "go_id", right.names,
                                               extra.cols, verbose)
            if (nrow(data) != 0)
                data[["Ontology"]] <- Ontology
            data
        }
        rbind(getPartialSubmap("BP"),
              getPartialSubmap("CC"),
              getPartialSubmap("MF"))
    }
)

### "as.data.frame" is equivalent to "toTable". Might be deprecated soon.
setMethod("as.data.frame", "AnnObj",
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
### Conceptual definition (for AnnMap object x):
###     nrow(x) :== nrow(toTable(x))
###
### Since "toTable" is unoriented, then "nrow" is unoriented too.
###

setMethod("nrow", "AnnTable",
    function(x)
    {
        dbCountRawAnnMapRows(db(x), x@leftTable, x@leftCol, NULL, NULL, x@from)
    }
)

setMethod("nrow", "AnnMap",
    function(x)
    {
        dbCountAnnMapRows(db(x), x@rightTable, x@join, x@leftCol, x@rightCol)
    }
)

setMethod("nrow", "GOAnnMap",
    function(x)
    {
        countRows <- function(Ontology)
        {
            table <- x@rightTable[Ontology]
            dbCountAnnMapRows(db(x), table, x@join, x@leftCol, "go_id")
        }
        countRows("BP") + countRows("CC") + countRows("MF")
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "left.names", "right.names" and "names" generics.
###

setMethod("left.names", "AnnObj",
    function(x)
    {
        dbUniqueVals(db(x), x@leftTable, x@leftCol, x@datacache)
    }
)

setMethod("right.names", "AnnMap",
    function(x)
    {
        dbUniqueVals(db(x), x@rightTable, x@rightCol, x@datacache)
    }
)

setMethod("right.names", "GOAnnMap",
    function(x)
    {
        getNames <- function(Ontology)
        {
            table <- x@rightTable[Ontology]
            dbUniqueVals(db(x), table, "go_id", x@datacache)
        }
        ## Because a given go_id can only belong to 1 of the 3 ontologies...
        ## (if not, then apply unique to this result)
        c(getNames("BP"), getNames("CC"), getNames("MF"))
    }
)

setMethod("names", "AnnObj", function(x) left.names(x))
setMethod("names", "ReverseAnnMap", function(x) right.names(x))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "left.length", "right.length" and "length" generic.
###
### Conceptual definitions (for AnnMap object x):
###     left.length(x) :== length(left.names(x))
###     right.length(x) :== length(right.names(x))
###

setMethod("left.length", "AnnObj",
    function(x)
    {
        dbCountUniqueVals(db(x), x@leftTable, x@leftCol, x@datacache)
    }
)

setMethod("right.length", "AnnMap",
    function(x)
    {
        dbCountUniqueVals(db(x), x@rightTable, x@rightCol, x@datacache)
    }
)

setMethod("right.length", "GOAnnMap",
    function(x)
    {
        countNames <- function(Ontology)
        {
            table <- x@rightTable[Ontology]
            dbCountUniqueVals(db(x), table, "go_id", x@datacache)
        }
        ## Because a given go_id can only belong to 1 of the 3 ontologies...
        countNames("BP") + countNames("CC") + countNames("MF")
    }
)

setMethod("length", "AnnObj", function(x) left.length(x))
setMethod("length", "ReverseAnnMap", function(x) right.length(x))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" generic.
###

setMethod("show", "AnnTable",
    function(object)
    {
        cat(object@objName, " table for ", object@objTarget,
            " (object of class \"", class(object), "\")\n", sep="")
    }
)

setMethod("show", "AnnMap",
    function(object)
    {
        cat(object@objName, " map for ", object@objTarget,
            " (object of class \"", class(object), "\")\n", sep="")
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "as.character" generic.
###
### For untagged Reverse/AtomicAnnMap obects only!
###

### R doesn't let me add a 'names' arg here:
###  Error in rematchDefinition(definition, fdef, mnames, fnames, signature) :
###          methods can add arguments to the generic only if '...' is an argument to the generic
setMethod("as.character", "AtomicAnnMap",
    function(x)
    {
        if (length(x@tagCols) != 0)
            stop("cannot coerce to character an AtomicAnnMap object with tags")
        data <- toTable(x)
        ans <- data[[x@rightCol]]
        if (!is.character(ans))
            ans <- as.character(ans)
        names(ans) <- data[[x@leftCol]]
        if (any(duplicated(names(ans))))
            warning("returned vector has duplicated names")
        ans
    }
)

setMethod("as.character", "ReverseAtomicAnnMap",
    function(x)
    {
        if (length(x@tagCols) != 0)
            stop("cannot coerce to character an AtomicAnnMap object with tags")
        data <- toTable(x)
        ans <- data[[x@leftCol]]
        if (!is.character(ans))
            ans <- as.character(ans)
        names(ans) <- data[[x@rightCol]]
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

setMethod("toList", "AnnMap",
    function(x, names=NULL)
    {
        if (!is.null(names) && length(names) == 0)
            return(list())
        data0 <- toTable(x, left.names=names)
        if (nrow(data0) == 0) {
            ann_list <- list()
        } else {
            ## Just to make sure that toTable() is not broken
            if (!identical(names(data0)[1:2], c(x@leftCol, x@rightCol)))
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

setMethod("toList", "ReverseAnnMap",
    function(x, names=NULL)
    {
        if (!is.null(names) && length(names) == 0)
            return(list())
        data0 <- toTable(x, right.names=names)
        if (!is.null(names) && !all(names %in% data0[[x@rightCol]]))
            .checkNamesExist(names, names(x))
        if (nrow(data0) == 0) {
            ann_list <- list()
        } else {
            ## Just to make sure that toTable() is not broken
            if (!identical(names(data0)[1:2], c(x@leftCol, x@rightCol)))
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
### Conceptual definitions (for AnnMap object x):
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

### For an AtomicAnnMap, x@replace.single and x@replace.multiple will be
### ignored, hence will give wrong results if one of those 2 fields has a
### non-default value like silly maps ENTREZID and MULTIHIT in AG_DB schema.
### But who cares, those maps are silly anyway...
setMethod("mapped.left.names", "AnnMap",
    function(x)
    {
        dbUniqueMappedVals(db(x), x@rightTable, x@join,
                           x@leftTable, x@leftCol,
                           x@rightTable, x@rightCol, x@datacache)
    }
)
setMethod("count.mapped.left.names", "AnnMap",
    function(x)
    {
        dbCountUniqueMappedVals(db(x), x@rightTable, x@join,
                                x@leftTable, x@leftCol,
                                x@rightTable, x@rightCol, x@datacache)
    }
)

setMethod("mapped.right.names", "AnnMap",
    function(x)
    {
        dbUniqueMappedVals(db(x), x@rightTable, x@join,
                           x@rightTable, x@rightCol,
                           x@leftTable, x@leftCol, x@datacache)
    }
)
setMethod("count.mapped.right.names", "AnnMap",
    function(x)
    {
        dbCountUniqueMappedVals(db(x), x@rightTable, x@join,
                                x@rightTable, x@rightCol,
                                x@leftTable, x@leftCol, x@datacache)
    }
)

setMethod("mapped.left.names", "GOAnnMap",
    function(x)
    {
        getMappedNames <- function(Ontology)
        {
            table <- x@rightTable[Ontology]
            dbUniqueMappedVals(db(x), table, x@join,
                               x@leftTable, x@leftCol,
                               table, "go_id", x@datacache)
        }
        names1 <- getMappedNames("BP")
        names2 <- getMappedNames("CC")
        names3 <- getMappedNames("MF")
        unique(c(names1, names2, names3))
    }
)
setMethod("count.mapped.left.names", "GOAnnMap",
    function(x) length(mapped.left.names(x))
)

setMethod("mapped.right.names", "GOAnnMap",
    function(x)
    {
        getMappedNames <- function(Ontology)
        {
            table <- x@rightTable[Ontology]
            dbUniqueMappedVals(db(x), table, x@join,
                               table, "go_id",
                               x@leftTable, x@leftCol, x@datacache)
        }
        names1 <- getMappedNames("BP")
        names2 <- getMappedNames("CC")
        names3 <- getMappedNames("MF")
        ## Because a given go_id can only belong to 1 of the 3 ontologies...
        ## (if not, then apply unique to this result)
        c(names1, names2, names3)
    }
)
setMethod("count.mapped.right.names", "GOAnnMap",
    function(x)
    {
        countMappedNames <- function(Ontology)
        {
            table <- x@rightTable[Ontology]
            dbCountUniqueMappedVals(db(x), table, x@join,
                                    table, "go_id",
                                    x@leftTable, x@leftCol, x@datacache)
        }
        ## Because a given go_id can only belong to 1 of the 3 ontologies...
        countMappedNames("BP") + countMappedNames("CC") + countMappedNames("MF")
    }
)

setMethod("mapped.names", "AnnMap", function(x) mapped.left.names(x))
setMethod("mapped.names", "ReverseAnnMap", function(x) mapped.right.names(x))
setMethod("count.mapped.names", "AnnMap", function(x) count.mapped.left.names(x))
setMethod("count.mapped.names", "ReverseAnnMap", function(x) count.mapped.right.names(x))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "is.na" generic.
###
### 'is.na(x)' is a named logical vector that associates each name in the map
### with TRUE except for those names that are actually mapped to something
### (other than an NA).
###

setMethod("is.na", "AnnMap",
    function(x)
    {
        mapped_names <- mapped.names(x)
        names <- names(x)
        ans <- !(names %in% mapped_names)
        names(ans) <- names
        ans
    }
)

