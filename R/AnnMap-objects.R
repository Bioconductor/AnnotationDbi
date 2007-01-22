### =========================================================================
### AnnMap objects
### --------------
### AnnMap objects are SQLite-based annotation maps.
### This file defines:
###   - the "AnnMap" class and subclasses,
###   - an environment-like API for the "AnnMap" objects (length, ls, mget,
###     as.list, eapply, get, [[ and $ methods),
###   - some helper functions used by this environment-like API.
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### SQL helper functions

.dbGetQuery <- function(con, sql)
{
    dbGetQuery(con, sql)
}

getTable <- function(con, table, where=NULL)
{
    sql <- paste("SELECT * FROM ", table, sep="")
    if (!is.null(where))
        sql <- paste(sql, " WHERE ", where, sep="")
    .dbGetQuery(con, sql)
}

toSQLStringSet <- function(strings)
{
    strings <- gsub("'", "''", strings, fixed=TRUE)
    paste("'", paste(strings, collapse="','"), "'", sep="")
}

toSQLInExpr <- function(col, vals)
{
    if (is.null(vals))
        return("1")
    if (length(vals) == 1)
        paste(col, " LIKE ", toSQLStringSet(vals), sep="")
    else
        paste(col, " IN (", toSQLStringSet(vals), ")", sep="")
}

subsetTable <- function(con, table, join, where_col1, where_vals1,
                                          where_col2, where_vals2, cols, verbose=FALSE)
{
    sql <- paste("SELECT", paste(cols, collapse=","), "FROM", table)
    if (length(join) == 1) # will be FALSE for NULL or character(0)
        sql <- paste(sql, join)
    where1 <- toSQLInExpr(where_col1, where_vals1)
    where2 <- toSQLInExpr(where_col2, where_vals2)
    sql <- paste(sql, "WHERE", where1, "AND", where2)
    if (verbose)
        cat(sql, "\n", sep="")
    .dbGetQuery(con, sql)
}

countUniqueSubsetsInSubsettedTable <- function(con, table, join, where_col, where_vals, cols)
{
    sql <- paste("SELECT COUNT(DISTINCT ", where_col, ") FROM ", table, sep="")
    if (length(join) == 1) # will be FALSE for NULL or character(0)
        sql <- paste(sql, join)
    sql <- paste(sql, " WHERE ", cols[1], " IS NOT NULL", sep="")
    if (!is.null(where_vals))
        sql <- paste(sql, " AND ", where_col, " IN (", toSQLStringSet(where_vals), ")", sep="")
    .dbGetQuery(con, sql)[[1]]
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### SQL helper functions with caching mechanism.

dbUniqueColVals <- function(con, table, col, datacache=NULL)
{
    if (!is.null(datacache)) {
        objname <- paste("dbUniqueColVals", table, col, sep=".")
        if (exists(objname, envir=datacache, inherits=FALSE)) {
            vals <- get(objname, envir=datacache, inherits=FALSE)
            return(vals)
        }
    }
    sql <- paste("SELECT DISTINCT", col, "FROM", table,
                 "WHERE", col, "IS NOT NULL")
    vals <- .dbGetQuery(con, sql)[[col]]
    if (!is.null(datacache)) {
        assign(objname, vals, envir=datacache, inherits=FALSE)
    }
    vals
}

### Read only caching!
dbCountUniqueColVals <- function(con, table, col, datacache=NULL)
{
    if (!is.null(datacache)) {
        objname <- paste("dbUniqueColVals", table, col, sep=".")
        if (exists(objname, envir=datacache, inherits=FALSE)) {
            count <- length(get(objname, envir=datacache, inherits=FALSE))
            return(count)
        }
    }
    sql <- paste("SELECT COUNT(DISTINCT ", col, ") FROM ", table, sep="")
    .dbGetQuery(con, sql)[[1]]
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Other helper functions

checkUserLeftIds <- function(symbols, map)
{
    if (is.null(symbols) || !is.character(symbols) || any(is.na(symbols)))
        stop("invalid first argument")
    all_symbols <- dbUniqueColVals(map@con, map@leftTable, map@leftCol, map@datacache)
    not_found <- which(!(symbols %in% all_symbols))
    if (length(not_found) != 0)
        stop("value for '", symbols[not_found[1]], "' not found")
}

formatSubmap <- function(submap, names, type=NULL, replace.single=NULL, replace.multiple=NULL)
{
    formatVal <- function(key)
    {
        val <- submap[key][[1]]
        lval <- length(val)
        if (lval == 0)
            val <- NA
        else {
            if (lval == 1) {
                if (length(replace.single) != 0)
                    return(replace.single)
            } else {
                if (length(replace.multiple) != 0)
                    return(replace.multiple)
            }
        }
        ## We don't want to use 'type' if it's NULL or character(0)
        if (length(type) == 1 && storage.mode(val) != type)
            storage.mode(val) <- type
        return(val)
    }
    ans <- lapply(names, formatVal)
    names(ans) <- names
    ans
}

GOtables <- function(all=FALSE)
{
    tables <- c("go_bp", "go_cc", "go_mf")
    if (all)
        tables <- paste(tables, "_all", sep="")
    names(tables) <- c("BP", "CC", "MF")
    tables
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Classes representing SQLite-based annotation maps

### An "AnnMap" object is a mapping between left values and right values.
### The left values are the strings stored in the SQL col 'leftCol' of
### table 'leftTable'.
### For direct "AnnMap" objects, the mapping is "left-to-right". The left
### values are the names (or symbols, or keys) of the map and are retrieved
### with the "names" or "ls" methods. The type, format and location in the
### DB of the right values depends on the particular subclass of the "AnnMap"
### object. For reverse "AnnMap" objects, the mapping is "right-to-left".
setClass("AnnMap",
    representation(
        join="character",
        leftCol="character",
        leftTable="character",
        con="DBIConnection",
        datacache="environment",
        chipShortname="character",
        mapName="character"
    )
)

### For a "AtomicAnnMap" object, the right values are unnamed atomic vectors
### (character or integer).
### The last 2 slots ('replace.single' and 'replace.multiple') allow dealing
### with silly maps ENTREZID and MULTIHIT in AGDB schema: they are complementary
### maps that both map probeset ids to Entrez ids. In the ENTREZID map, probeset
### ids that have multiple matches are mapped to "multiple". In the MULTIHIT
### map, probeset ids that have <= 1 match are mapped to NAs. Sooo:
###   - for ENTREZID: don't set replace.single (default is character(0)),
###                   use replace.multiple="multiple",
###   - for MULTIHIT: use replace.single=NA,
###                   don't set replace.multiple (default is character(0)),
###   - for any other map: don't set those fields (defaults will be just fine).
setClass("AtomicAnnMap",
    contains="AnnMap",
    representation(
        rightTable="character",
        rightCol="character",
        rightColType="character", # set only if the extracted submap needs coercion
        replace.single="character",
        replace.multiple="character"
    )
)

setClass("NamedAtomicAnnMap",
    contains="AtomicAnnMap",
    representation(
        rightNamesCol="character"
    )
)

setClass("ReverseAtomicAnnMap", contains="AtomicAnnMap")

### For a "GOAnnMap" object, the right values are named lists of GO nodes,
### each GO node being represented as a 3-element list of the form
###   list(GOID="GO:0006470" , Evidence="IEA" , Ontology="BP")
setClass("GOAnnMap", contains="AnnMap")

### Maps a GO term to a named character vector containing left values tagged
### with the Evidence code.
setClass("ReverseGOAnnMap",
    contains="GOAnnMap",
    representation(
        all="logical"
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "db" new generic

setGeneric("db", function(object) standardGeneric("db"))

setMethod("db", "AnnMap", function(object) object@con)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "as.data.frame" generic.
###
### WORK IN PROGRESS!!! Experimenting a new as.data.frame interface. Use at
### your own risk!
###
### Arguments 'row.names' and 'optional' are ignored.
### The 'left.vals' and 'right.vals' args can be one of the following:
###   - NULL: the arg is ignored.
###   - A NA-free character vector: only the rows with a left-value (1st
###     field) matching one of the strings in 'left.vals' and a right-value
###     (2nd field) matching one of the strings in 'right.vals' are
###     retrieved.
### Note that the 'left.vals' and 'right.vals' args are _not_ checked i.e.
### only NULL and NA-free character vectors are guaranted to work properly.

setMethod("as.data.frame", "AtomicAnnMap",
    function(x, row.names=NULL, optional=FALSE,
             left.vals=NULL, right.vals=NULL, extra.cols=NULL, verbose=FALSE)
    {
        if (missing(left.vals))
            left.vals <- row.names
        if (missing(right.vals) && !identical(optional, FALSE))
            right.vals <- optional
        cols <- c(x@leftCol, x@rightCol)
        if (class(x) == "NamedAtomicAnnMap")
            cols <- c(cols, x@rightNamesCol)
        if (!is.null(extra.cols))
            cols <- c(cols, extra.cols)
        subsetTable(db(x), x@rightTable, x@join,
                           x@leftCol, left.vals,
                           x@rightCol, right.vals,
                           cols, verbose)
    }
)

setMethod("as.data.frame", "GOAnnMap",
    function(x, row.names=NULL, optional=FALSE,
             left.vals=NULL, right.vals=NULL, extra.cols=NULL, verbose=FALSE)
    {
        if (missing(left.vals))
            left.vals <- row.names
        if (missing(right.vals) && !identical(optional, FALSE))
            right.vals <- optional
        cols <- c(x@leftCol, "go_id", "evidence")
        if (!is.null(extra.cols))
            cols <- c(cols, extra.cols)
        all <- FALSE
        if (class(x) == "ReverseGOAnnMap")
            all <- x@all
        getPartialSubmap <- function(Ontology)
        {
            table <- GOtables(all)[Ontology]
            data <- subsetTable(db(x), table, x@join,
                                x@leftCol, left.vals,
                                "go_id", right.vals,
                                cols, verbose)
            if (nrow(data) != 0)
                data[["Ontology"]] <- Ontology
            data
        }
        rbind(getPartialSubmap("BP"),
              getPartialSubmap("CC"),
              getPartialSubmap("MF"))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "length" generic.
### length(x) should always be the same as length(names(x)).

setMethod("length", "AnnMap",
    function(x)
    {
        dbCountUniqueColVals(db(x), x@leftTable, x@leftCol, x@datacache)
    }
)

setMethod("length", "ReverseAtomicAnnMap",
    function(x)
    {
        dbCountUniqueColVals(db(x), x@rightTable, x@rightCol, x@datacache)
    }
)

setMethod("length", "ReverseGOAnnMap",
    function(x)
    {
        rightTables <- GOtables(x@all)
        ## Our assumption is that a given go_id can only belong to
        ## 1 of the 3 ontologies!
        dbCountUniqueColVals(db(x), rightTables["BP"], "go_id", x@datacache)
          + dbCountUniqueColVals(db(x), rightTables["CC"], "go_id", x@datacache)
          + dbCountUniqueColVals(db(x), rightTables["MF"], "go_id", x@datacache)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "names" and "ls" generics.

setMethod("names", "AnnMap",
    function(x)
    {
        dbUniqueColVals(db(x), x@leftTable, x@leftCol, x@datacache)
    }
)

setMethod("names", "ReverseAtomicAnnMap",
    function(x)
    {
        dbUniqueColVals(db(x), x@rightTable, x@rightCol)
    }
)

setMethod("names", "ReverseGOAnnMap",
    function(x)
    {
        rightTables <- GOtables(x@all)
        ## Our assumption is that a given go_id can only belong to 1 of the 3
        ## ontologies! If we are wrong, then we should apply unique() to the
        ## result of this concantenation and fix the "length" method above.
        c(dbUniqueColVals(db(x), rightTables["BP"], "go_id"),
          dbUniqueColVals(db(x), rightTables["CC"], "go_id"),
          dbUniqueColVals(db(x), rightTables["MF"], "go_id"))
    }
)

setMethod("ls", signature(name="AnnMap"),
    function(name, pos, envir, all.names, pattern) names(name)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" generic

setMethod("show", "AnnMap",
    function(object)
    {
        cat(object@mapName, " map for chip ", object@chipShortname,
            " (object of class “", class(object), "”)\n", sep="")
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "as.list" generic.
### Note that all the "as.list" methods below have an extra 'names' arg.
### The 'names' arg. can be one of the following:
###   - NULL: all map elements are extracted (equivalent to passing
###           'names=names(x)' but more efficient).
###   - A NA-free character vector: the names of the returned list will
###     be those passed in 'names' in the same order. Names that are not
###     in the map are associated with NAs. Note that, unlike "mget",
###     "as.list" doesn't treat differently names that are not in the map
###     from names that are in the map but associated with NAs.
###   - A NA-free numeric vector: for conveniency 'as.list(x, 1:3)' is a
###     shortcut for 'as.list(x, names(x)[1:3])'. It's identical to
###     'as.list(x)[1:3]' but _much_ more efficent of course.
### Note that the 'names' arg. is _not_ checked i.e. only NULL, NA-free
### character vectors and NA-free numeric vectors are guaranted to work.

setMethod("as.list", "AtomicAnnMap",
    function(x, names=NULL)
    {
        if (!is.null(names) && length(names) == 0)
            return(list())
        if (is.numeric(names))
            names <- names(x)[names]
        data <- as.data.frame(x, left.vals=names)
        submap <- split(data[[x@rightCol]], data[[x@leftCol]])
        if (is.null(names))
            names <- names(x)
        formatSubmap(submap, names, x@rightColType, x@replace.single, x@replace.multiple)
    }
)

setMethod("as.list", "NamedAtomicAnnMap",
    function(x, names=NULL)
    {
        if (!is.null(names) && length(names) == 0)
            return(list())
        if (is.numeric(names))
            names <- names(x)[names]
        data <- as.data.frame(x, left.vals=names)
        submap <- data[[x@rightCol]]
        names(submap) <- data[[x@rightNamesCol]]
        submap <- split(submap, data[[x@leftCol]])
        if (is.null(names))
            names <- names(x)
        formatSubmap(submap, names, x@rightColType, x@replace.single, x@replace.multiple)
    }
)

setMethod("as.list", "ReverseAtomicAnnMap",
    function(x, names=NULL)
    {
        if (!is.null(names) && length(names) == 0)
            return(list())
        if (is.numeric(names))
            names <- names(x)[names]
        data <- as.data.frame(x, right.vals=names)
        if (!is.null(names)) {
            not_found <- which(!(names %in% data[[x@rightCol]]))
            if (length(not_found) != 0)
                stop("value for '", names[not_found[1]], "' not found")
        }
        submap <- split(data[[x@leftCol]], data[[x@rightCol]])
        if (is.null(names))
            names <- names(x)
        formatSubmap(submap, names, x@rightColType, x@replace.single, x@replace.multiple)
    }
)

### This new version (0.0.27) is 3 times faster than previous version (0.0.26):
### Old version:
###   > system.time(aa<-as.list(hgu95av2GO))
###      user  system elapsed
###    76.968   5.692  85.080
### New version:
###   > system.time(aa<-as.list(hgu95av2GO))
###      user  system elapsed
###    25.305   1.012  27.658
### Reference (envir-based):
###   > system.time(aa<-as.list(hgu95av2GO))
###      user  system elapsed
###     4.456   0.228   4.953
setMethod("as.list", "GOAnnMap",
    function(x, names=NULL)
    {
        if (!is.null(names) && length(names) == 0)
            return(list())
        if (is.numeric(names))
            names <- names(x)[names]
        data <- as.data.frame(x, left.vals=names)
        if (nrow(data) == 0)
            return(list())
        if (is.null(names))
            names <- names(x)
        makeGONodeList <- function(GOIDs, Evidences, Ontologies)
        {
            ans <- lapply(1:length(GOIDs), function(y)
                          list(GOID=GOIDs[y],
                               Evidence=Evidences[y],
                               Ontology=Ontologies[y]))
            names(ans) <- GOIDs
            ans
        }
        GOIDs <- split(data[["go_id"]], data[[x@leftCol]])
        Evidences <- split(data[["evidence"]], data[[x@leftCol]])
        Ontologies <- split(data[["Ontology"]], data[[x@leftCol]])
        mapped_names <- unique(data[[x@leftCol]])
        submap <- lapply(names,
                         function(y)
                         {
                             if (!(y %in% mapped_names))
                                 NA
                             else
                                 makeGONodeList(GOIDs[[y]], Evidences[[y]], Ontologies[[y]])
                         })
        names(submap) <- names
        submap
    }
)

setMethod("as.list", "ReverseGOAnnMap",
    function(x, names=NULL)
    {
        if (!is.null(names) && length(names) == 0)
            return(list())
        if (is.numeric(names))
            names <- names(x)[names]
        data <- as.data.frame(x, right.vals=names)
        if (!is.null(names)) {
            not_found <- which(!(names %in% data[["go_id"]]))
            if (length(not_found) != 0)
                stop("value for '", names[not_found[1]], "' not found")
        }
        submap <- data[[x@leftCol]]
        names(submap) <- data[["evidence"]]
        submap <- split(submap, data[["go_id"]])
        if (is.null(names))
            names <- names(x)
        formatSubmap(submap, names)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "mget" new generic.
### 'mget(x, map)' vs 'as.list(map, names=x)':
###   1. mget checks its 'x' arg. and gracefully fails if it's not of
###      the expected type (i.e. NULL or NA-free character vector),
###   2. mget will error on the first string in 'x' not in 'names(map)',
###      as.list will accept those strings and map them to NAs.
###   3. if 'x' is a subset of 'names(map)', then 'mget(x, map)'
###      is identical to 'as.list(map, names=x)'.
###   4. 'mget(names(map), map)' is identical to 'as.list(map)'.
###      Note that for a real "environment", 'as.list(envir)' is not identical
###      to 'mget(ls(envir), envir)': the 2 lists have the same elements but
###      not necesarily in the same order!

setMethod("mget", signature(envir="AtomicAnnMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        checkUserLeftIds(x, envir)
        as.list(envir, names=x)
    }
)

setMethod("mget", signature(envir="ReverseAtomicAnnMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        if (is.null(x) || !is.character(x) || any(is.na(x)))
            stop("invalid first argument")
        as.list(envir, names=x)
    }
)

setMethod("mget", signature(envir="GOAnnMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        checkUserLeftIds(x, envir)
        as.list(envir, names=x)
    }
)

setMethod("mget", signature(envir="ReverseGOAnnMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        if (is.null(x) || !is.character(x) || any(is.na(x)))
            stop("invalid first argument")
        as.list(envir, names=x)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "eapply" new generic

setMethod("eapply", signature(env="AnnMap"),
    function(env, FUN, ..., all.names)
    {
        lapply(as.list(env), FUN)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "get" new generic.
### We want this:
###   get("1027_at", envir=hgu95av2GO)
### and this
###   get("1027_at", hgu95av2GO)
### to work so we need to dispatch on the 'pos' arg too.

setMethod("get", signature(envir="AnnMap"),
    function(x, pos, envir, mode, inherits)
    {
        mget(x[1], envir)[[1]]
    }
)

setMethod("get", signature(pos="AnnMap", envir="missing"),
    function(x, pos, envir, mode, inherits)
    {
        get(x, envir=pos)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "[[" and "$" generics

setMethod("[[", "AnnMap",
    function(x, i, j, ...)
    {
        # 'x' is guaranteed to be a "AnnMap" object (if it's not, then the
        # method dispatch algo will not call this method in the first place),
        # so nargs() is guaranteed to be >= 1
        if (nargs() >= 3)
            stop("too many subscripts")
        subscripts <- list(...)
        if (!missing(i))
            subscripts$i <- i
        if (!missing(j))
            subscripts$j <- j
        # At this point, 'subscripts' should be guaranteed
        # to be of length <= 1
        if (length(subscripts) == 0)
            stop("no index specified")
        i <- subscripts[[1]]
        if (length(i) < 1)
            stop("attempt to select less than one element")
        if (length(i) > 1)
            stop("attempt to select more than one element")
        if (!is.character(i) || is.na(i))
            stop("wrong argument for subsetting an object of class “", class(x), "“")
        get(i, envir=x)
    }
)

setMethod("$", "AnnMap", function(x, name) x[[name]])


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

createMAPCOUNTS <- function(con, chipShortname)
{
    data <- getTable(con, "qcdata", "map_name != 'TOTAL'")
    MAPCOUNTS <- data[["count"]]
    names(MAPCOUNTS) <- paste(chipShortname, data[["map_name"]], sep="")
    MAPCOUNTS
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

createAtomicAnnMapObjects <- function(seeds, seed0)
{
    maps <- list()
    for (seed in seeds) {
        if (is.null(seed["rightNamesCol"][[1]]))
            seed$Class <- "AtomicAnnMap"
        else
            seed$Class <- "NamedAtomicAnnMap"
        for (slot in names(seed0)) {
            if (is.null(seed[slot][[1]]))
                seed[[slot]] <- seed0[[slot]]
        }
        maps[[seed$mapName]] <- do.call("new", seed)
    }
    maps
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "countMappedNames" new generic.
### 'countMappedNames(map)' is the number of names that are mapped to a non-NA
### value. It could be defined by
###   sum(sapply(as.list(map), function(x) length(x)!=1 || !is.na(x)))
### but this would be way too slow...
### Note that if map is a "reverse" map, then this would be the same as its
### length.

setGeneric("countMappedNames", function(map) standardGeneric("countMappedNames"))

### Ignore map@replace.single and map@replace.multiple, hence will give
### wrong results for maps that have one of those 2 fields with non-default
### values like silly maps ENTREZID and MULTIHIT in AGDB schema, but who
### cares, those maps are so silly anyway...
setMethod("countMappedNames", "AtomicAnnMap",
    function(map)
    {
        cols <- map@rightCol
        countUniqueSubsetsInSubsettedTable(db(map),
            map@rightTable, map@join, map@leftCol, NULL, cols)
    }
)

setMethod("countMappedNames", "ReverseAtomicAnnMap",
    function(map) length(map)
)

setMethod("countMappedNames", "GOAnnMap",
    function(map)
    {
        cols <- character(0)
        getMappedLeftVals <- function(table)
        {
            data <- subsetTable(db(map), table, map@join,
                                map@leftCol, NULL,
                                "go_id", NULL,
                                cols)
            unique(data[[map@leftCol]])
        }
        names1 <- getMappedLeftVals("go_bp")
        names2 <- getMappedLeftVals("go_cc")
        names3 <- getMappedLeftVals("go_mf")
        names <- c(names1, names2, names3)
        # Equivalent to 'length(unique(names))' but slightly faster
        length(names) - sum(duplicated(names))
    }
)

setMethod("countMappedNames", "ReverseGOAnnMap",
    function(map) length(map)
)

checkAnnDataObjects <- function(pkgname, chipShortname)
{
    require(pkgname, character.only=TRUE) || stop(pkgname, " package needed")
    getMap <- function(mapname)
    {
        get(mapname, envir=asNamespace(pkgname))
    }
    MAPCOUNTS <- getMap(paste(chipShortname, "MAPCOUNTS", sep=""))
    for (mapname in names(MAPCOUNTS)) {
        cat("Checking ", mapname, " map:\n", sep="")
        map <- getMap(mapname)
        nnames <- length(map)
        cat("  - nnames = ", nnames, "\n", sep="")
        count0 <- MAPCOUNTS[mapname]
        cat("  - count0 = ", count0, "\n", sep="")
        t1 <- system.time(count1 <- countMappedNames(map))
        cat("  - count1 = ", count1, " (", t1[3], " s)\n", sep="")
        t2 <- system.time(count2 <- sum(sapply(as.list(map), function(x) length(x)!=1 || !is.na(x))))
        cat("  - count2 = ", count2, " (", t2[3], " s)\n", sep="")
        if (count1 != count0 || count2 != count0)
            stop("count0, count1 and count2 not the same")
    }
    sort()
}

