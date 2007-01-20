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

subsetTable <- function(con, table, index, subset, cols, joins=NULL)
{
    cols <- append(index, cols)
    sql <- paste("SELECT", paste(cols, collapse=","), "FROM", table)
    if (length(joins) == 1) # will be FALSE for NULL or character(0)
        sql <- paste(sql, joins)
    if (!is.null(subset))
        sql <- paste(sql, " WHERE ", index, " IN (", toSQLStringSet(subset), ")", sep="")
    .dbGetQuery(con, sql)
}

countUniqueSubsetsInSubsettedTable <- function(con, table, index, subset, cols, joins=NULL)
{
    sql <- paste("SELECT COUNT(DISTINCT ", index, ") FROM ", table, sep="")
    if (length(joins) == 1) # will be FALSE for NULL or character(0)
        sql <- paste(sql, joins)
    sql <- paste(sql, " WHERE ", cols[1], " IS NOT NULL", sep="")
    if (!is.null(subset))
        sql <- paste(sql, " AND ", index, " IN (", toSQLStringSet(subset), ")", sep="")
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
    sql <- paste("SELECT DISTINCT ", col, " FROM ", table,
                 " WHERE ", col, " IS NOT NULL", sep="")
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

checkUserLeftIds <- function(leftids, map)
{
    if (is.null(leftids) || !is.character(leftids) || any(is.na(leftids)))
        stop("invalid first argument")
    all_leftids <- dbUniqueColVals(map@con, map@leftTable, map@leftCol, map@datacache)
    not_found <- which(!(leftids %in% all_leftids))
    if (length(not_found) != 0)
        stop("value for '", leftids[not_found[1]], "' not found")
}

formatSubmap <- function(submap, keys, type=NULL, replace.single=NULL, replace.multiple=NULL)
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
    ans <- lapply(keys, formatVal)
    names(ans) <- keys
    ans
}

GOtables <- function(all)
{
    tables <- c("go_bp", "go_cc", "go_mf")
    if (all)
        tables <- paste(tables, "_all", sep="")
    tables
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Classes representing SQLite-based annotation maps

setClass("AnnMap",
    representation(
        baseJoins="character",
        leftCol="character",
        leftTable="character",
        con="DBIConnection",
        datacache="environment",
        chipShortname="character",
        mapName="character"
    )
)

### An "AtomicAnnMap" object maps each "left" id (string) to an unnamed atomic
### vector (character or integer).
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
        mapTable="character",
        mapCol="character",
        mapColType="character", # set only if the extracted submap needs coercion
        replace.single="character",
        replace.multiple="character"
    )
)

setClass("ReverseAtomicAnnMap", contains="AtomicAnnMap")

setClass("NamedAtomicAnnMap",
    contains="AtomicAnnMap",
    representation(
        namesCol="character"
    )
)

### Maps each "left" id to a named list of GO nodes, each GO node being
### represented as a 3-element list of the form
###   list(GOID="GO:0006470" , Evidence="IEA" , Ontology="BP")
setClass("GOAnnMap", contains="AnnMap")

### Maps a GO term to a named character vector containing "left" ids.
### Each element of the vector is named with the Evidence code.
setClass("ReverseGOAnnMap",
    contains="AnnMap",
    representation(
        all="logical"
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "db" new generic

setGeneric("db", function(object) standardGeneric("db"))

setMethod("db", "AnnMap", function(object) object@con)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "length" generic.
### length(x) should always be the same as length(ls(x)).

setMethod("length", "AnnMap",
    function(x)
    {
        dbCountUniqueColVals(db(x), x@leftTable, x@leftCol, x@datacache)
    }
)

setMethod("length", "ReverseAtomicAnnMap",
    function(x)
    {
        dbCountUniqueColVals(db(x), x@mapTable, x@mapCol, x@datacache)
    }
)

setMethod("length", "ReverseGOAnnMap",
    function(x)
    {
        mapTables <- GOtables(x@all)
        ## Our assumption is that a given go_id can only belong to
        ## 1 of the 3 ontologies!
        dbCountUniqueColVals(db(x), mapTables[1], "go_id", x@datacache)
          + dbCountUniqueColVals(db(x), mapTables[2], "go_id", x@datacache)
          + dbCountUniqueColVals(db(x), mapTables[3], "go_id", x@datacache)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "ls" new generic

setMethod("ls", signature(name="AnnMap"),
    function(name, pos, envir, all.names, pattern)
    {
        dbUniqueColVals(db(name), name@leftTable, name@leftCol, name@datacache)
    }
)

setMethod("ls", signature(name="ReverseAtomicAnnMap"),
    function(name, pos, envir, all.names, pattern)
    {
        dbUniqueColVals(db(name), name@mapTable, name@mapCol)
    }
)

setMethod("ls", signature(name="ReverseGOAnnMap"),
    function(name, pos, envir, all.names, pattern)
    {
        mapTables <- GOtables(name@all)
        ## Our assumption is that a given go_id can only belong to 1 of the 3
        ## ontologies! If we are wrong, then we should apply unique() to the
        ## result of this concantenation and fix the "length" method above.
        c(dbUniqueColVals(db(name), mapTables[1], "go_id"),
          dbUniqueColVals(db(name), mapTables[2], "go_id"),
          dbUniqueColVals(db(name), mapTables[3], "go_id"))
    }
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
### Note that all these "as.list" methods have an extra 'subset' arg. and
### that this arg. is _not_ checked i.e. only NULL or NA-free character
### vectors are guaranted to work.

setMethod("as.list", "AtomicAnnMap",
    function(x, subset=NULL)
    {
        cols <- x@mapCol
        data <- subsetTable(db(x), x@mapTable, x@leftCol, subset, cols, x@baseJoins)
        submap <- split(data[[x@mapCol]], data[[x@leftCol]])
        if (is.null(subset))
            subset <- ls(x)
        formatSubmap(submap, subset, x@mapColType, x@replace.single, x@replace.multiple)
    }
)

setMethod("as.list", "ReverseAtomicAnnMap",
    function(x, subset=NULL)
    {
        cols <- x@leftCol
        data <- subsetTable(db(x), x@mapTable, x@mapCol, subset, cols, x@baseJoins)
        if (!is.null(subset)) {
            not_found <- which(!(subset %in% data[[x@mapCol]]))
            if (length(not_found) != 0)
                stop("value for '", subset[not_found[1]], "' not found")
        }
        submap <- split(data[[x@leftCol]], data[[x@mapCol]])
        if (is.null(subset))
            subset <- ls(x)
        formatSubmap(submap, subset, x@mapColType, x@replace.single, x@replace.multiple)
    }
)

setMethod("as.list", "NamedAtomicAnnMap",
    function(x, subset=NULL)
    {
        cols <- c(x@mapCol, x@namesCol)
        data <- subsetTable(db(x), x@mapTable, x@leftCol, subset, cols, x@baseJoins)
        submap <- data[[x@mapCol]]
        names(submap) <- data[[x@namesCol]]
        submap <- split(submap, data[[x@leftCol]])
        if (is.null(subset))
            subset <- ls(x)
        formatSubmap(submap, subset, x@mapColType, x@replace.single, x@replace.multiple)
    }
)

setMethod("as.list", "GOAnnMap",
    function(x, subset=NULL)
    {
        makeGONodeList <- function(GOIDs, Evidences, Ontology)
        {
            ans <- lapply(1:length(GOIDs), function(y)
                          list(GOID=GOIDs[y], Evidence=Evidences[y], Ontology=Ontology))
            names(ans) <- GOIDs
            ans
        }
        cols <- c("go_id", "evidence")
        getPartialSubmap <- function(table, Ontology)
        {
            data <- subsetTable(db(x), table, x@leftCol, subset, cols, x@baseJoins)
            if (nrow(data) == 0)
                return(list())
            GOIDs <- split(data[["go_id"]], data[[x@leftCol]])
            Evidences <- split(data[["evidence"]], data[[x@leftCol]])
            ans <- lapply(names(GOIDs), function(y)
                          makeGONodeList(GOIDs[[y]], Evidences[[y]], Ontology))
            names(ans) <- names(GOIDs)
            ans
        }
        submap1 <- getPartialSubmap("go_bp", "BP")
        submap2 <- getPartialSubmap("go_cc", "CC")
        submap3 <- getPartialSubmap("go_mf", "MF")
        if (is.null(subset))
            subset <- ls(x)
        ## submap1[y][[1]] is a trick to ensure _exact_ matching! (we don't want partial matching)
        submap <- lapply(subset,
                         function(y)
                         {
                             z <- c(submap1[y][[1]], submap2[y][[1]], submap3[y][[1]])
                             if (length(z) == 0) NA else z
                         })
        names(submap) <- subset
        submap
    }
)

setMethod("as.list", "ReverseGOAnnMap",
    function(x, subset=NULL)
    {
        cols <- c(x@leftCol, "evidence")
        getPartialSubmap <- function(table)
        {
            data <- subsetTable(db(x), table, "go_id", subset, cols, x@baseJoins)
            if (nrow(data) == 0)
                return(list())
            submap <- data[[x@leftCol]]
            names(submap) <- data[["evidence"]]
            split(submap, data[["go_id"]])
        }
        mapTables <- GOtables(x@all)
        submap <- c(getPartialSubmap(mapTables[1]),
                    getPartialSubmap(mapTables[2]),
                    getPartialSubmap(mapTables[3]))
        if (is.null(subset))
            subset <- ls(x)
        formatSubmap(submap, subset)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "mget" new generic.
### 'mget(subset, map)' vs 'as.list(map, subset)':
###   1. mget checks its 'subset' arg. and gracefully fails if it's not of
###      the expected type (i.e. NULL or NA-free character vector),
###   2. mget will error on the first string in 'subset' not in 'ls(map)',
###      as.list will accept those strings and map them to NAs.
###   3. if 'subset' is a subset of 'ls(map)', then 'mget(subset, map)'
###      is identical to 'as.list(map, subset)'.
###   4. 'mget(ls(map), map)' is identical to 'as.list(map)'.
###      Note that for a real "environment", 'as.list(envir)' is not identical
###      to 'mget(ls(envir), envir)': the 2 lists have the same elements but
###      not necesarily in the same order!

setMethod("mget", signature(envir="AtomicAnnMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        checkUserLeftIds(x, envir)
        if (length(x) == 0)
            return(list())
        as.list(envir, subset=x)
    }
)

setMethod("mget", signature(envir="ReverseAtomicAnnMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        if (is.null(x) || !is.character(x) || any(is.na(x)))
            stop("invalid first argument")
        if (length(x) == 0)
            return(list())
        as.list(envir, subset=x)
    }
)

setMethod("mget", signature(envir="ReverseGOAnnMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        if (length(x) == 0)
            return(list())
        as.list(envir, subset=x)
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
### The "get" new generic

setMethod("get", signature(envir="AnnMap"),
    function(x, pos, envir, mode, inherits)
    {
        mget(x[1], envir)[[1]]
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
        if (is.null(seed["namesCol"][[1]]))
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
### The "countMappedKeys" new generic.
### 'countMappedKeys(map)' is the number of keys that are mapped to a non-NA
### value. It could be defined by
###   sum(sapply(as.list(map), function(x) length(x)!=1 || !is.na(x)))
### but this would be too slow...
### Note that if map is a "reverse" map, then this would be the same than its
### length.

setGeneric("countMappedKeys", function(map) standardGeneric("countMappedKeys"))

### Ignore map@replace.single and map@replace.multiple, hence will give
### wrong results for maps that have one of those 2 fields with non-default
### values like silly maps ENTREZID and MULTIHIT in AGDB schema, but who
### cares, those maps are so silly anyway...
countMappedKeys.AtomicAnnMap <- function(map, subset=NULL)
{
    cols <- map@mapCol
    countUniqueSubsetsInSubsettedTable(db(map), map@mapTable, map@leftCol,
                                       subset, cols, map@baseJoins)
}

setMethod("countMappedKeys", "AtomicAnnMap",
    function(map)
    {
        countMappedKeys.AtomicAnnMap(map)
    }
)

setMethod("countMappedKeys", "ReverseAtomicAnnMap",
    function(map) length(map)
)

countMappedKeys.GOAnnMap <- function(map, subset=NULL)
{
    cols <- character(0)
    getMappedKeys <- function(table)
    {
        data <- subsetTable(db(map), table, map@leftCol, subset, cols, map@baseJoins)
        unique(data[[map@leftCol]])
    }
    keys1 <- getMappedKeys("go_bp")
    keys2 <- getMappedKeys("go_cc")
    keys3 <- getMappedKeys("go_mf")
    keys <- c(keys1, keys2, keys3)
    # Equivalent to length(unique(keys)) but slightly faster
    length(keys) - sum(duplicated(keys))
}

setMethod("countMappedKeys", "GOAnnMap",
    function(map)
    {
        countMappedKeys.GOAnnMap(map)
    }
)

setMethod("countMappedKeys", "ReverseGOAnnMap",
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
        nkeys <- length(map)
        cat("  - nkeys = ", nkeys, "\n", sep="")
        count0 <- MAPCOUNTS[mapname]
        cat("  - count0 = ", count0, "\n", sep="")
        t1 <- system.time(count1 <- countMappedKeys(map))
        cat("  - count1 = ", count1, " (", t1[3], " s)\n", sep="")
        t2 <- system.time(count2 <- sum(sapply(as.list(map), function(x) length(x)!=1 || !is.na(x))))
        cat("  - count2 = ", count2, " (", t2[3], " s)\n", sep="")
        if (count1 != count0 || count2 != count0)
            stop("count0, count1 and count2 not the same")
    }
    sort()
}

