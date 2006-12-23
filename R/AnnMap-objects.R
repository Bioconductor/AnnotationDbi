### =========================================================================
### Environment-like annotation maps
### -------------------------------------------------------------------------


PROBESETID_COL <- "probe_id"


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### SQL helper functions

getTable <- function(con, table)
{
    sql <- paste("SELECT * FROM ", table, sep="")
    dbGetQuery(con, sql)
}

countUniqueColValues <- function(con, table, col)
{
    sql <- paste("SELECT COUNT(DISTINCT ", col, ") FROM ", table, sep="")
    dbGetQuery(con, sql)[[1]]
}

uniqueColValues <- function(con, table, col)
{
    sql <- paste("SELECT DISTINCT ", col, " FROM ", table, sep="")
    dbGetQuery(con, sql)[[col]]
}

sqlJoin <- function(table1, table2, using, joinType="LEFT")
{
    paste(table1, " ", joinType, " JOIN ", table2, " USING (", using, ")", sep="")
}

subsetTable <- function(con, table, index, subset, cols, add_probes=FALSE)
{
    cols <- append(index, cols)
    if (add_probes) {
        if (!(PROBESETID_COL %in% cols))
            cols <- append(cols, PROBESETID_COL)
        table <- sqlJoin(table, "probes", "id", "INNER")
    }
    sql <- paste("SELECT ", paste(cols, collapse=","), " FROM ", table, sep="")
    if (!is.null(subset))
        sql <- paste(sql, " WHERE ", index, " IN ('", paste(subset, collapse="','"), "')", sep="")
    dbGetQuery(con, sql)
}

countUniqueSubsetsInSubsettedTable <- function(con, table, index, subset, cols, add_probes=FALSE)
{
    if (add_probes) {
        table <- sqlJoin(table, "probes", "id", "INNER")
    }
    sql <- paste("SELECT COUNT(DISTINCT ", index, ") FROM ", table, sep="")
    sql <- paste(sql, " WHERE ", cols[1], " IS NOT NULL", sep="")
    if (!is.null(subset))
        sql <- paste(sql, " AND ", index, " IN ('", paste(subset, collapse="','"), "')", sep="")
    dbGetQuery(con, sql)[[1]]
}

fromValueToProbesetIDs  <- function(con, value, mapTable, mapCol)
{
    sql <- paste("SELECT ", mapCol, ",", PROBESETID_COL, sep="")
    sql <- paste(sql, " FROM ", sqlJoin(mapTable, "probes", "id"), sep="")
    if (!is.null(value))
        sql <- paste(sql, " WHERE ", mapCol, " IN ('", paste(value, collapse="','"), "')", sep="")
    data <- dbGetQuery(con, sql)
    if (!is.null(value)) {
        not_found <- which(!(value %in% data[[mapCol]]))
        if (length(not_found) != 0)
            stop("value for '", value[not_found[1]], "' not found")
    }
    data
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Other helper functions

cachePROBESET2GENE <- function(con, datacache)
{
    data <- getTable(con, "probes")
    PROBESET2GENE <- data[["id"]]
    names(PROBESET2GENE) <- data[[PROBESETID_COL]]
    assign("PROBESET2GENE", PROBESET2GENE, envir=datacache)
}

checkProbeset <- function(probeset, datacache)
{
    if (is.null(probeset) || !is.character(probeset) || any(is.na(probeset)))
        stop("invalid first argument")
    PROBESET2GENE <- get("PROBESET2GENE", envir=datacache)
    not_found <- which(!(probeset %in% names(PROBESET2GENE)))
    if (length(not_found) != 0)
        stop("value for '", probeset[not_found[1]], "' not found")
}

GOtables <- function(all)
{
    tables <- c("go_bp", "go_cc", "go_mf")
    if (all)
        tables <- paste(tables, "_all", sep="")
    tables
}

normaliseSubmapKeys <- function(submap, keys)
{
    ## Old version, slow
    #ans <- submap[keys]; ans[sapply(ans, is.null)] <- NA;
    ## New version, slightly faster
    ans <- lapply(keys, function(x) {y <- submap[x][[1]]; if (is.null(y)) NA else y})
    names(ans) <- keys
    ans
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Classes representing annotation maps

setClass("AnnMap", representation(con="DBIConnection", datacache="environment"))

### Maps each probeset ID to an unnamed atomic vector (character or integer)
setClass(
    "AtomicAnnMap", contains="AnnMap",
    representation(
        mapTable="character",
        mapCol="character"
    )
)

setClass("ReverseAtomicAnnMap", contains="AtomicAnnMap")

### Data are "gene based" i.e. maps of class GeneBasedAtomicAnnMap have the
### following property:
###     if probeset1 and probeset2 have the same ENTREZID
###         then map[[probeset1]] == map[[probeset2]]
setClass("GeneBasedAtomicAnnMap", contains="AtomicAnnMap")

setClass("ReverseGeneBasedAtomicAnnMap", contains="ReverseAtomicAnnMap")

setClass("NamedGeneBasedAtomicAnnMap", contains="GeneBasedAtomicAnnMap", representation(namesCol="character"))

### Maps each probeset ID to a named list of GO nodes, each GO node being
### represented as a 3-element list of the form
###   list(GOID="GO:0006470" , Evidence="IEA" , Ontology="BP")
setClass("GeneBasedGOAnnMap", contains="AnnMap")

### Maps a GO term to a named character vector containing probeset IDs.
### Each element of the vector is named with the Evidence code.
setClass("ReverseGeneBasedGOAnnMap", contains="AnnMap", representation(all="logical"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "db" new generic

setGeneric("db", function(object) standardGeneric("db"))

setMethod("db", "AnnMap", function(object) object@con)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "length" generic

setMethod("length", "AnnMap",
    function(x)
    {
        length(get("PROBESET2GENE", envir=x@datacache))
    }
)

setMethod("length", "ReverseAtomicAnnMap",
    function(x)
    {
        countUniqueColValues(db(x), x@mapTable, x@mapCol)
    }
)

setMethod("length", "ReverseGeneBasedGOAnnMap",
    function(x)
    {
        mapTables <- GOtables(x@all)
        ## Our assumption is that a given go_id can only belong to 1 of the 3
        ## ontologies!
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
        names(get("PROBESET2GENE", envir=name@datacache))
    }
)

setMethod("ls", signature(name="ReverseAtomicAnnMap"),
    function(name, pos, envir, all.names, pattern)
    {
        uniqueColValues(db(name), name@mapTable, name@mapCol)
    }
)

setMethod("ls", signature(name="ReverseGeneBasedGOAnnMap"),
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
        cat("An object of class “", class(object), "”\n", sep="")
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Those functions are called by the "mget" and "as.list" methods for doing
### the real job.

subset.AtomicAnnMap <- function(map, subset=NULL)
{
    cols <- map@mapCol
    data <- subsetTable(db(map), map@mapTable, PROBESETID_COL, subset, cols)
    submap <- split(data[[map@mapCol]], data[[PROBESETID_COL]])
    if (is.null(subset))
        subset <- ls(map)
    normaliseSubmapKeys(submap, subset)
}

countMappedKeys.AtomicAnnMap <- function(map, subset=NULL)
{
    cols <- map@mapCol
    countUniqueSubsetsInSubsettedTable(db(map), map@mapTable, PROBESETID_COL, subset, cols)
}

subset.GeneBasedAtomicAnnMap <- function(map, subset=NULL)
{
    cols <- map@mapCol
    data <- subsetTable(db(map), map@mapTable, PROBESETID_COL, subset, cols, add_probes=TRUE)
    submap <- split(data[[map@mapCol]], data[[PROBESETID_COL]])
    if (is.null(subset))
        subset <- ls(map)
    normaliseSubmapKeys(submap, subset)
}

countMappedKeys.GeneBasedAtomicAnnMap <- function(map, subset=NULL)
{
    cols <- map@mapCol
    countUniqueSubsetsInSubsettedTable(db(map), map@mapTable, PROBESETID_COL, subset, cols, add_probes=TRUE)
}

subset.ReverseGeneBasedAtomicAnnMap <- function(map, subset=NULL)
{
    data <- fromValueToProbesetIDs(db(map), subset, map@mapTable, map@mapCol)
    submap <- split(data[[PROBESETID_COL]], data[[map@mapCol]])
    if (is.null(subset))
        subset <- ls(map)
    normaliseSubmapKeys(submap, subset)
}

countMappedKeys.ReverseGeneBasedAtomicAnnMap <- function(map, subset=NULL)
{
    cols <- PROBESETID_COL
    countUniqueSubsetsInSubsettedTable(db(map), map@mapTable, map@mapCol, subset, cols, add_probes=TRUE)
}

subset.NamedGeneBasedAtomicAnnMap <- function(map, subset=NULL)
{
    cols <- c(map@mapCol, map@namesCol)
    data <- subsetTable(db(map), map@mapTable, PROBESETID_COL, subset, cols, add_probes=TRUE)
    submap <- data[[map@mapCol]]
    names(submap) <- data[[map@namesCol]]
    submap <- split(submap, data[[PROBESETID_COL]])
    if (is.null(subset))
        subset <- ls(map)
    normaliseSubmapKeys(submap, subset)
}

subset.GeneBasedGOAnnMap <- function(map, subset=NULL)
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
        data <- subsetTable(db(map), table, PROBESETID_COL, subset, cols, add_probes=TRUE)
        if (nrow(data) == 0)
            return(list())
        GOIDs <- split(data[["go_id"]], data[[PROBESETID_COL]])
        Evidences <- split(data[["evidence"]], data[[PROBESETID_COL]])
        sapply(names(GOIDs), function(x) makeGONodeList(GOIDs[[x]], Evidences[[x]], Ontology))
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

countMappedKeys.GeneBasedGOAnnMap <- function(map, subset=NULL)
{
    getPartialSubmap <- function(table)
    {
        data <- subsetTable(db(map), table, PROBESETID_COL, subset, character(0), add_probes=TRUE)
        unique(data[[PROBESETID_COL]])
    }
    keys1 <- getMappedKeys("go_bp")
    keys2 <- getMappedKeys("go_cc")
    keys3 <- getMappedKeys("go_mf")
    length(intersect(keys1, intersect(keys2, keys3)))
}

subset.ReverseGeneBasedGOAnnMap <- function(map, subset=NULL)
{
    cols <- c(PROBESETID_COL, "evidence")
    getPartialSubmap <- function(table)
    {
        data <- subsetTable(db(map), table, "go_id", subset, cols, add_probes=TRUE)
        if (nrow(data) == 0)
            return(list())
        submap <- data[[PROBESETID_COL]]
        names(submap) <- data[["evidence"]]
        split(submap, data[["go_id"]])
    }
    mapTables <- GOtables(map@all)
    submap <- c(getPartialSubmap(mapTables[1]),
                getPartialSubmap(mapTables[2]),
                getPartialSubmap(mapTables[3]))
    if (is.null(subset))
        subset <- ls(map)
    normaliseSubmapKeys(submap, subset)
}

countMappedKeys.ReverseGeneBasedGOAnnMap <- function(map, subset=NULL)
{
    mapTables <- GOtables(map@all)
    count1 <- countUniqueSubsetsInSubsettedTable(db(map), mapTables[1],
                "go_id", subset, PROBESETID_COL, add_probes=TRUE)
    count2 <- countUniqueSubsetsInSubsettedTable(db(map), mapTables[2],
                "go_id", subset, PROBESETID_COL, add_probes=TRUE)
    count3 <- countUniqueSubsetsInSubsettedTable(db(map), mapTables[3],
                "go_id", subset, PROBESETID_COL, add_probes=TRUE)
    count1 + count2 + count3
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "mget" new generic

setMethod("mget", signature(envir="AtomicAnnMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        checkProbeset(x, envir@datacache)
        if (length(x) == 0)
            return(list())
        subset.AtomicAnnMap(envir, x)
    }
)

setMethod("mget", signature(envir="GeneBasedAtomicAnnMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        checkProbeset(x, envir@datacache)
        if (length(x) == 0)
            return(list())
        subset.GeneBasedAtomicAnnMap(envir, x)
    }
)

setMethod("mget", signature(envir="ReverseGeneBasedAtomicAnnMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        if (is.null(x) || !is.character(x) || any(is.na(x)))
            stop("invalid first argument")
        if (length(x) == 0)
            return(list())
        subset.ReverseGeneBasedAtomicAnnMap(envir, x)
    }
)

setMethod("mget", signature(envir="NamedGeneBasedAtomicAnnMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        checkProbeset(x, envir@datacache)
        if (length(x) == 0)
            return(list())
        subset.NamedGeneBasedAtomicAnnMap(envir, x)
    }
)

setMethod("mget", signature(envir="GeneBasedGOAnnMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        checkProbeset(x, envir@datacache)
        if (length(x) == 0)
            return(list())
        subset.GeneBasedGOAnnMap(envir, x)
    }
)

setMethod("mget", signature(envir="ReverseGeneBasedGOAnnMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        if (length(x) == 0)
            return(list())
        subset.ReverseGeneBasedGOAnnMap(envir, x)
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

setMethod("as.list", "GeneBasedAtomicAnnMap",
    function(x, ...)
    {
        subset.GeneBasedAtomicAnnMap(x)
    }
)

setMethod("as.list", "ReverseGeneBasedAtomicAnnMap",
    function(x, ...)
    {
        subset.ReverseGeneBasedAtomicAnnMap(x)
    }
)

setMethod("as.list", "NamedGeneBasedAtomicAnnMap",
    function(x, ...)
    {
        subset.NamedGeneBasedAtomicAnnMap(x)
    }
)

setMethod("as.list", "GeneBasedGOAnnMap",
    function(x, ...)
    {
        subset.GeneBasedGOAnnMap(x)
    }
)

setMethod("as.list", "ReverseGeneBasedGOAnnMap",
    function(x, ...)
    {
        subset.ReverseGeneBasedGOAnnMap(x)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "countMappedKeys" new generic.
### 'countMappedKeys(map)' is the number of keys that are mapped to a non-NA
### value. It could be defined by
###   sum(sapply(as.list(map), function(x) length(x)!=1 || !is.na(x)))
### but this would be too slow...

setGeneric("countMappedKeys", function(map) standardGeneric("countMappedKeys"))

setMethod("countMappedKeys", "AtomicAnnMap",
    function(map)
    {
        countMappedKeys.AtomicAnnMap(map)
    }
)

setMethod("countMappedKeys", "GeneBasedAtomicAnnMap",
    function(map)
    {
        countMappedKeys.GeneBasedAtomicAnnMap(map)
    }
)

setMethod("countMappedKeys", "ReverseGeneBasedAtomicAnnMap",
    function(map)
    {
        countMappedKeys.ReverseGeneBasedAtomicAnnMap(map)
    }
)

setMethod("countMappedKeys", "GeneBasedGOAnnMap",
    function(map)
    {
        countMappedKeys.GeneBasedGOAnnMap(map)
    }
)

setMethod("countMappedKeys", "ReverseGeneBasedGOAnnMap",
    function(map)
    {
        countMappedKeys.ReverseGeneBasedGOAnnMap(map)
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
### The "[[" generic

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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

createMAPCOUNTS <- function(con, chipname)
{
    data <- getTable(con, "qcdata")
    MAPCOUNTS <- data[["count"]]
    names(MAPCOUNTS) <- paste(chipname, data[["map_name"]], sep="")
    MAPCOUNTS[!(MAPCOUNTS %in% "TOTAL")]
}

