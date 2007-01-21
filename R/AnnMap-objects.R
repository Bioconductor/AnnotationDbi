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
### values are the keys (or names or symbols) of the map and are retrieved
### with the "names" or "ls" methods. The type, format and location in the
### DB of the right values depends on the particular subclass of the "AnnMap"
### object. For reverse "AnnMap" objects, the mapping is "right-to-left".
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

setClass("ReverseAtomicAnnMap", contains="AtomicAnnMap")

setClass("NamedAtomicAnnMap",
    contains="AtomicAnnMap",
    representation(
        rightNamesCol="character"
    )
)

### For a "GOAnnMap" object, the right values are named lists of GO nodes,
### each GO node being represented as a 3-element list of the form
###   list(GOID="GO:0006470" , Evidence="IEA" , Ontology="BP")
setClass("GOAnnMap", contains="AnnMap")

### Maps a GO term to a named character vector containing left values tagged
### with the Evidence code.
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
### The "as.data.frame" generic.
### Arguments 'row.names' and 'optional' are ignored.
### The 'subset' arg. can be one of the following:
###   - NULL: the entire map is converted (equivalent to passing
###     'subset=ls(x)' but more efficient).
###   - A NA-free character vector containing keys (symbols): only the rows
###     matching those keys are returned.
### Note that the 'subset' arg. is _not_ checked i.e. only NULL and NA-free
### character vectors are guaranted to work properly.

setMethod("as.data.frame", "AtomicAnnMap",
    function(x, row.names=NULL, optional=FALSE, subset=NULL)
    {
        cols <- x@rightCol
        subsetTable(db(x), x@rightTable, x@leftCol, subset, cols, x@baseJoins)
    }
)

setMethod("as.data.frame", "ReverseAtomicAnnMap",
    function(x, row.names=NULL, optional=FALSE, subset=NULL)
    {
        cols <- x@leftCol
        subsetTable(db(x), x@rightTable, x@rightCol, subset, cols, x@baseJoins)
    }
)

setMethod("as.data.frame", "NamedAtomicAnnMap",
    function(x, row.names=NULL, optional=FALSE, subset=NULL)
    {
        cols <- c(x@rightCol, x@rightNamesCol)
        subsetTable(db(x), x@rightTable, x@leftCol, subset, cols, x@baseJoins)
    }
)

setMethod("as.data.frame", "GOAnnMap",
    function(x, row.names=NULL, optional=FALSE, subset=NULL)
    {
        cols <- c("go_id", "evidence")
        getPartialSubmap <- function(Ontology)
        {
            table <- GOtables()[Ontology]
            data <- subsetTable(db(x), table, x@leftCol, subset, cols, x@baseJoins)
            data[["Ontology"]] <- Ontology
            data
        }
        rbind(getPartialSubmap("BP"),
              getPartialSubmap("CC"),
              getPartialSubmap("MF"))
    }
)

setMethod("as.data.frame", "ReverseGOAnnMap",
    function(x, row.names=NULL, optional=FALSE, subset=NULL)
    {
        cols <- c(x@leftCol, "evidence")
        getPartialSubmap <- function(Ontology)
        {
            table <- GOtables(x@all)[Ontology]
            data <- subsetTable(db(x), table, "go_id", subset, cols, x@baseJoins)
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
### Note that all the "as.list" methods below have an extra 'subset' arg.
### The 'subset' arg. can be one of the following:
###   - NULL: all map elements are extracted (equivalent to passing
###           'subset=ls(x)' but more efficient).
###   - A NA-free character vector: the names of the returned list will
###     be those passed in 'subset' in the same order. Names that are not
###     in the map are associated with NAs. Note that, unlike "mget",
###     "as.list" doesn't treat differently names that are not in the map
###     from names that are in the map but associated with NAs.
###   - A NA-free numeric vector: for conveniency 'as.list(x, 1:3)' is a
###     shortcut for 'as.list(x, ls(x)[1:3])'. It's identical to
###     'as.list(x)[1:3]' but _much_ more efficent of course.
### Note that the 'subset' arg. is _not_ checked i.e. only NULL, NA-free
### character vectors and NA-free numeric vectors are guaranted to work.

setMethod("as.list", "AtomicAnnMap",
    function(x, subset=NULL)
    {
        if (!is.null(subset) && length(subset) == 0)
            return(list())
        if (is.numeric(subset))
            subset <- ls(x)[subset]
        data <- as.data.frame(x, subset=subset)
        submap <- split(data[[x@rightCol]], data[[x@leftCol]])
        if (is.null(subset))
            subset <- ls(x)
        formatSubmap(submap, subset, x@rightColType, x@replace.single, x@replace.multiple)
    }
)

setMethod("as.list", "ReverseAtomicAnnMap",
    function(x, subset=NULL)
    {
        if (!is.null(subset) && length(subset) == 0)
            return(list())
        if (is.numeric(subset))
            subset <- ls(x)[subset]
        data <- as.data.frame(x, subset=subset)
        if (!is.null(subset)) {
            not_found <- which(!(subset %in% data[[x@rightCol]]))
            if (length(not_found) != 0)
                stop("value for '", subset[not_found[1]], "' not found")
        }
        submap <- split(data[[x@leftCol]], data[[x@rightCol]])
        if (is.null(subset))
            subset <- ls(x)
        formatSubmap(submap, subset, x@rightColType, x@replace.single, x@replace.multiple)
    }
)

setMethod("as.list", "NamedAtomicAnnMap",
    function(x, subset=NULL)
    {
        if (!is.null(subset) && length(subset) == 0)
            return(list())
        if (is.numeric(subset))
            subset <- ls(x)[subset]
        data <- as.data.frame(x, subset=subset)
        submap <- data[[x@rightCol]]
        names(submap) <- data[[x@rightNamesCol]]
        submap <- split(submap, data[[x@leftCol]])
        if (is.null(subset))
            subset <- ls(x)
        formatSubmap(submap, subset, x@rightColType, x@replace.single, x@replace.multiple)
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
    function(x, subset=NULL)
    {
        if (!is.null(subset) && length(subset) == 0)
            return(list())
        if (is.numeric(subset))
            subset <- ls(x)[subset]
        data <- as.data.frame(x, subset=subset)
        if (nrow(data) == 0)
            return(list())
        if (is.null(subset))
            subset <- ls(x)
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
        mapped_keys <- unique(data[[x@leftCol]])
        submap <- lapply(subset,
                         function(y)
                         {
                             if (!(y %in% mapped_keys))
                                 NA
                             else
                                 makeGONodeList(GOIDs[[y]], Evidences[[y]], Ontologies[[y]])
                         })
        names(submap) <- subset
        submap
    }
)

setMethod("as.list", "ReverseGOAnnMap",
    function(x, subset=NULL)
    {
        if (!is.null(subset) && length(subset) == 0)
            return(list())
        if (is.numeric(subset))
            subset <- ls(x)[subset]
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
        rightTables <- GOtables(x@all)
        submap <- c(getPartialSubmap(rightTables[1]),
                    getPartialSubmap(rightTables[2]),
                    getPartialSubmap(rightTables[3]))
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
        as.list(envir, subset=x)
    }
)

setMethod("mget", signature(envir="ReverseAtomicAnnMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        if (is.null(x) || !is.character(x) || any(is.na(x)))
            stop("invalid first argument")
        as.list(envir, subset=x)
    }
)

setMethod("mget", signature(envir="GOAnnMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        checkUserLeftIds(x, envir)
        as.list(envir, subset=x)
    }
)

setMethod("mget", signature(envir="ReverseGOAnnMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        if (is.null(x) || !is.character(x) || any(is.na(x)))
            stop("invalid first argument")
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
### The "countMappedKeys" new generic.
### 'countMappedKeys(map)' is the number of keys that are mapped to a non-NA
### value. It could be defined by
###   sum(sapply(as.list(map), function(x) length(x)!=1 || !is.na(x)))
### but this would be way too slow...
### Note that if map is a "reverse" map, then this would be the same as its
### length.

setGeneric("countMappedKeys", function(map) standardGeneric("countMappedKeys"))

### Ignore map@replace.single and map@replace.multiple, hence will give
### wrong results for maps that have one of those 2 fields with non-default
### values like silly maps ENTREZID and MULTIHIT in AGDB schema, but who
### cares, those maps are so silly anyway...
setMethod("countMappedKeys", "AtomicAnnMap",
    function(map)
    {
        cols <- map@rightCol
        countUniqueSubsetsInSubsettedTable(db(map), map@rightTable, map@leftCol,
                                           NULL, cols, map@baseJoins)
    }
)

setMethod("countMappedKeys", "ReverseAtomicAnnMap",
    function(map) length(map)
)

setMethod("countMappedKeys", "GOAnnMap",
    function(map)
    {
        cols <- character(0)
        getMappedKeys <- function(table)
        {
            data <- subsetTable(db(map), table, map@leftCol, NULL, cols, map@baseJoins)
            unique(data[[map@leftCol]])
        }
        keys1 <- getMappedKeys("go_bp")
        keys2 <- getMappedKeys("go_cc")
        keys3 <- getMappedKeys("go_mf")
        keys <- c(keys1, keys2, keys3)
        # Equivalent to length(unique(keys)) but slightly faster
        length(keys) - sum(duplicated(keys))
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

