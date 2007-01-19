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

countUniqueColValues <- function(con, table, col)
{
    sql <- paste("SELECT COUNT(DISTINCT ", col, ") FROM ", table, sep="")
    .dbGetQuery(con, sql)[[1]]
}

uniqueColValues <- function(con, table, col)
{
    sql <- paste("SELECT DISTINCT ", col, " FROM ", table,
                 " WHERE ", col, " IS NOT NULL", sep="")
    .dbGetQuery(con, sql)[[col]]
}

subsetTable <- function(con, table, index, subset, cols, joins=NULL)
{
    cols <- append(index, cols)
    sql <- paste("SELECT", paste(cols, collapse=","), "FROM", table)
    if (length(joins) == 1) # will be FALSE for NULL or character(0)
        sql <- paste(sql, joins)
    if (!is.null(subset))
        sql <- paste(sql, " WHERE ", index,
                     " IN ('", paste(subset, collapse="','"), "')", sep="")
    .dbGetQuery(con, sql)
}

countUniqueSubsetsInSubsettedTable <- function(con, table, index, subset, cols, joins=NULL)
{
    sql <- paste("SELECT COUNT(DISTINCT ", index, ") FROM ", table, sep="")
    if (length(joins) == 1) # will be FALSE for NULL or character(0)
        sql <- paste(sql, joins)
    sql <- paste(sql, " WHERE ", cols[1], " IS NOT NULL", sep="")
    if (!is.null(subset))
        sql <- paste(sql, " AND ", index,
                     " IN ('", paste(subset, collapse="','"), "')", sep="")
    .dbGetQuery(con, sql)[[1]]
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Other helper functions

cacheBASEIDS <- function(con, baseTable, baseCol, datacache)
{
    BASEIDS <- uniqueColValues(con, baseTable, baseCol)
    assign("BASEIDS", BASEIDS, envir=datacache)
}

checkUserBaseIds <- function(baseids, datacache)
{
    if (is.null(baseids) || !is.character(baseids) || any(is.na(baseids)))
        stop("invalid first argument")
    BASEIDS <- get("BASEIDS", envir=datacache)
    not_found <- which(!(baseids %in% BASEIDS))
    if (length(not_found) != 0)
        stop("value for '", baseids[not_found[1]], "' not found")
}

GOtables <- function(all)
{
    tables <- c("go_bp", "go_cc", "go_mf")
    if (all)
        tables <- paste(tables, "_all", sep="")
    tables
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Classes representing SQLite-based annotation maps

setClass("AnnMap",
    representation(
        mapName="character",
        chipShortname="character",
        baseCol="character",
        baseJoins="character",
        con="DBIConnection",
        datacache="environment"
    )
)

### An "AtomicAnnMap" object maps each BASEID (string) to an unnamed atomic
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

### Maps each BASEID to a named list of GO nodes, each GO node being
### represented as a 3-element list of the form
###   list(GOID="GO:0006470" , Evidence="IEA" , Ontology="BP")
setClass("GOAnnMap", contains="AnnMap")

### Maps a GO term to a named character vector containing BASEIDs.
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
        length(get("BASEIDS", envir=x@datacache))
    }
)

setMethod("length", "ReverseAtomicAnnMap",
    function(x)
    {
        countUniqueColValues(db(x), x@mapTable, x@mapCol)
    }
)

setMethod("length", "ReverseGOAnnMap",
    function(x)
    {
        mapTables <- GOtables(x@all)
        ## Our assumption is that a given go_id can't belong to more
        ## than 1 of the 3 ontologies!
        countUniqueColValues(db(x), mapTables[1], "go_id") +
            countUniqueColValues(db(x), mapTables[2], "go_id") +
            countUniqueColValues(db(x), mapTables[3], "go_id")
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "ls" new generic

setMethod("ls", signature(name="AnnMap"),
    function(name, pos, envir, all.names, pattern)
    {
        get("BASEIDS", envir=name@datacache)
    }
)

setMethod("ls", signature(name="ReverseAtomicAnnMap"),
    function(name, pos, envir, all.names, pattern)
    {
        uniqueColValues(db(name), name@mapTable, name@mapCol)
    }
)

setMethod("ls", signature(name="ReverseGOAnnMap"),
    function(name, pos, envir, all.names, pattern)
    {
        mapTables <- GOtables(name@all)
        ## Our assumption is that a given go_id can only belong to 1 of the 3
        ## ontologies! If we are wrong, then we should apply unique() to the
        ## result of this concantenation and fix the "length" method above.
        c(uniqueColValues(db(name), mapTables[1], "go_id"),
          uniqueColValues(db(name), mapTables[2], "go_id"),
          uniqueColValues(db(name), mapTables[3], "go_id"))
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
### Those functions are called by the "mget", "as.list" and "countMappedKeys"
### methods for doing the real job.

subset.AtomicAnnMap <- function(map, subset=NULL)
{
    cols <- map@mapCol
    data <- subsetTable(db(map), map@mapTable, map@baseCol, subset, cols, map@baseJoins)
    submap <- split(data[[map@mapCol]], data[[map@baseCol]])
    if (is.null(subset))
        subset <- ls(map)
    formatSubmap(submap, subset, map@mapColType, map@replace.single, map@replace.multiple)
}

### Ignore map@replace.single and map@replace.multiple, hence will give
### wrong results for maps that have one of those 2 fields with non-default
### values like silly maps ENTREZID and MULTIHIT in AGDB schema, but who
### cares, those maps are so silly anyway...
countMappedKeys.AtomicAnnMap <- function(map, subset=NULL)
{
    cols <- map@mapCol
    countUniqueSubsetsInSubsettedTable(db(map), map@mapTable, map@baseCol, subset, cols, map@baseJoins)
}

subset.ReverseAtomicAnnMap <- function(map, subset=NULL)
{
    cols <- map@baseCol
    data <- subsetTable(db(map), map@mapTable, map@mapCol, subset, cols, map@baseJoins)
    if (!is.null(subset)) {
        not_found <- which(!(subset %in% data[[map@mapCol]]))
        if (length(not_found) != 0)
            stop("value for '", subset[not_found[1]], "' not found")
    }
    submap <- split(data[[map@baseCol]], data[[map@mapCol]])
    if (is.null(subset))
        subset <- ls(map)
    formatSubmap(submap, subset, map@mapColType, map@replace.single, map@replace.multiple)
}

subset.NamedAtomicAnnMap <- function(map, subset=NULL)
{
    cols <- c(map@mapCol, map@namesCol)
    data <- subsetTable(db(map), map@mapTable, map@baseCol, subset, cols, map@baseJoins)
    submap <- data[[map@mapCol]]
    names(submap) <- data[[map@namesCol]]
    submap <- split(submap, data[[map@baseCol]])
    if (is.null(subset))
        subset <- ls(map)
    formatSubmap(submap, subset, map@mapColType, map@replace.single, map@replace.multiple)
}

subset.GOAnnMap <- function(map, subset=NULL)
{
    makeGONodeList <- function(GOIDs, Evidences, Ontology)
    {
        ans <- lapply(1:length(GOIDs), function(x) list(GOID=GOIDs[x], Evidence=Evidences[x], Ontology=Ontology))
        names(ans) <- GOIDs
        ans
    }
    cols <- c("go_id", "evidence")
    getPartialSubmap <- function(table, Ontology)
    {
        data <- subsetTable(db(map), table, map@baseCol, subset, cols, map@baseJoins)
        if (nrow(data) == 0)
            return(list())
        GOIDs <- split(data[["go_id"]], data[[map@baseCol]])
        Evidences <- split(data[["evidence"]], data[[map@baseCol]])
        ans <- lapply(names(GOIDs), function(x) makeGONodeList(GOIDs[[x]], Evidences[[x]], Ontology))
        names(ans) <- names(GOIDs)
        ans
    }
    submap1 <- getPartialSubmap("go_bp", "BP")
    submap2 <- getPartialSubmap("go_cc", "CC")
    submap3 <- getPartialSubmap("go_mf", "MF")
    if (is.null(subset))
        subset <- ls(map)
    ## submap1[x][[1]] is a trick to ensure _exact_ matching! (we don't want partial matching)
    submap <- lapply(subset,
                     function(x)
                     {
                         y <- c(submap1[x][[1]], submap2[x][[1]], submap3[x][[1]])
                         if (length(y) == 0) NA else y
                     })
    names(submap) <- subset
    submap
}

countMappedKeys.GOAnnMap <- function(map, subset=NULL)
{
    cols <- character(0)
    getMappedKeys <- function(table)
    {
        data <- subsetTable(db(map), table, map@baseCol, subset, cols, map@baseJoins)
        unique(data[[map@baseCol]])
    }
    keys1 <- getMappedKeys("go_bp")
    keys2 <- getMappedKeys("go_cc")
    keys3 <- getMappedKeys("go_mf")
    keys <- c(keys1, keys2, keys3)
    # Equivalent to length(unique(keys)) but slightly faster
    length(keys) - sum(duplicated(keys))
}

subset.ReverseGOAnnMap <- function(map, subset=NULL)
{
    cols <- c(map@baseCol, "evidence")
    getPartialSubmap <- function(table)
    {
        data <- subsetTable(db(map), table, "go_id", subset, cols, map@baseJoins)
        if (nrow(data) == 0)
            return(list())
        submap <- data[[map@baseCol]]
        names(submap) <- data[["evidence"]]
        split(submap, data[["go_id"]])
    }
    mapTables <- GOtables(map@all)
    submap <- c(getPartialSubmap(mapTables[1]),
                getPartialSubmap(mapTables[2]),
                getPartialSubmap(mapTables[3]))
    if (is.null(subset))
        subset <- ls(map)
    formatSubmap(submap, subset)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "mget" new generic

setMethod("mget", signature(envir="AtomicAnnMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        checkUserBaseIds(x, envir@datacache)
        if (length(x) == 0)
            return(list())
        subset.AtomicAnnMap(envir, x)
    }
)

setMethod("mget", signature(envir="ReverseAtomicAnnMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        if (is.null(x) || !is.character(x) || any(is.na(x)))
            stop("invalid first argument")
        if (length(x) == 0)
            return(list())
        subset.ReverseAtomicAnnMap(envir, x)
    }
)

setMethod("mget", signature(envir="NamedAtomicAnnMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        checkUserBaseIds(x, envir@datacache)
        if (length(x) == 0)
            return(list())
        subset.NamedAtomicAnnMap(envir, x)
    }
)

setMethod("mget", signature(envir="GOAnnMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        checkUserBaseIds(x, envir@datacache)
        if (length(x) == 0)
            return(list())
        subset.GOAnnMap(envir, x)
    }
)

setMethod("mget", signature(envir="ReverseGOAnnMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        if (length(x) == 0)
            return(list())
        subset.ReverseGOAnnMap(envir, x)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "as.list" generic
### 'as.list(map)' could be simply defined as 'mget(ls(map), map)' but this
### would result in having a very long IN clause in the SELECT that we send
### to the database.
### Note that for a real "environment", 'as.list(envir)' is not identical
### to 'mget(ls(envir), envir)': the 2 lists have the same elements but not
### necesarily in the same order!

setMethod("as.list", "AtomicAnnMap",
    function(x, ...)
    {
        subset.AtomicAnnMap(x)
    }
)

setMethod("as.list", "ReverseAtomicAnnMap",
    function(x, ...)
    {
        subset.ReverseAtomicAnnMap(x)
    }
)

setMethod("as.list", "NamedAtomicAnnMap",
    function(x, ...)
    {
        subset.NamedAtomicAnnMap(x)
    }
)

setMethod("as.list", "GOAnnMap",
    function(x, ...)
    {
        subset.GOAnnMap(x)
    }
)

setMethod("as.list", "ReverseGOAnnMap",
    function(x, ...)
    {
        subset.ReverseGOAnnMap(x)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "countMappedKeys" new generic.
### 'countMappedKeys(map)' is the number of keys that are mapped to a non-NA
### value. It could be defined by
###   sum(sapply(as.list(map), function(x) length(x)!=1 || !is.na(x)))
### but this would be too slow...
### Note that if map is a "reverse" map, then this would be the same than its
### length.

setGeneric("countMappedKeys", function(map) standardGeneric("countMappedKeys"))

setMethod("countMappedKeys", "AtomicAnnMap",
    function(map)
    {
        countMappedKeys.AtomicAnnMap(map)
    }
)

setMethod("countMappedKeys", "ReverseAtomicAnnMap",
    function(map) length(map)
)

setMethod("countMappedKeys", "GOAnnMap",
    function(map)
    {
        countMappedKeys.GOAnnMap(map)
    }
)

setMethod("countMappedKeys", "ReverseGOAnnMap",
    function(map) length(map)
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

