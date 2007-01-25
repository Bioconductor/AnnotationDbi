### =========================================================================
### AnnMap objects
### --------------
###
### AnnMap objects are SQLite-based annotation maps.
###
### This file defines:
###
###   a) The "AnnMap" class and subclasses.
###
###   b) A low-level API for the "AnnMap" objects:
###        reverse
###        db
###        as.data.frame, nrow
###        left.names, right.names, names
###        left.length, right.length, length
###        show,
###        as.character, as.list
###        mapped.left.names, count.mapped.left.names
###        mapped.right.names, count.mapped.right.names
###        mapped.names, count.mapped.names
###        is.na
###
###      NB: "names", "length", "mapped.names" and "count.mapped.names" are
###      "oriented" methods, i.e. they give a different result on a map and
###      its "reverse" map (it depend on the orientation of the map).
###      For each of these methods, there are 2 "unoriented" methods: a left
###      method and a right method.
###
###   c) Some helper functions used by this low-level API.
###
### The environment-like API for AnnMap objects (ls, mget, etc...) is defined
### in the AnnMap-envirAPI.R file.
###
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### SQL helper functions.
###

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

dbMapToDataFrame <- function(con, table, join, left.col, left.names,
                             right.col, right.names, extra.cols, verbose=FALSE)
{
    cols <- c(left.col, right.col, extra.cols)
    sql <- paste("SELECT", paste(cols, collapse=","), "FROM", table)
    if (length(join) == 1) # will be FALSE for NULL or character(0)
        sql <- paste(sql, join)
    where1 <- toSQLWhere(left.col, left.names)
    where2 <- toSQLWhere(right.col, right.names)
    sql <- paste(sql, "WHERE", where1, "AND", where2)
    if (verbose)
        cat(sql, "\n", sep="")
    .dbGetQuery(con, sql)
}

dbCountDataFrameRows <- function(con, table, join, left.col, right.col)
{
    sql <- paste("SELECT COUNT(*) FROM", table)
    if (length(join) == 1) # will be FALSE for NULL or character(0)
        sql <- paste(sql, join)
    where1 <- toSQLWhere(left.col, NULL)
    where2 <- toSQLWhere(right.col, NULL)
    sql <- paste(sql, "WHERE", where1, "AND", where2)
    .dbGetQuery(con, sql)[[1]]
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### SQL helper functions with caching mechanism.

dbUniqueVals <- function(con, table, col, datacache=NULL)
{
    if (!is.null(datacache)) {
        objname <- paste("dbUniqueVals", table, col, sep=".")
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
dbCountUniqueVals <- function(con, table, col, datacache=NULL)
{
    if (!is.null(datacache)) {
        objname <- paste("dbUniqueVals", table, col, sep=".")
        if (exists(objname, envir=datacache, inherits=FALSE)) {
            count <- length(get(objname, envir=datacache, inherits=FALSE))
            return(count)
        }
    }
    sql <- paste("SELECT COUNT(DISTINCT ", col, ") FROM ", table, sep="")
    .dbGetQuery(con, sql)[[1]]
}

dbUniqueMappedVals <- function(con, table, join, from.col, to.col, datacache=NULL)
{
    if (!is.null(datacache)) {
        objname <- paste("dbUniqueMappedVals", table, from.col, to.col, sep=".")
        if (exists(objname, envir=datacache, inherits=FALSE)) {
            vals <- get(objname, envir=datacache, inherits=FALSE)
            return(vals)
        }
    }
    sql <- paste("SELECT DISTINCT", from.col, "FROM", table)
    if (length(join) == 1) # will be FALSE for NULL or character(0)
        sql <- paste(sql, join)
    sql <- paste(sql, "WHERE", from.col, "IS NOT NULL AND ", to.col, "IS NOT NULL")
    vals <- .dbGetQuery(con, sql)[[from.col]]
    if (!is.null(datacache)) {
        assign(objname, vals, envir=datacache, inherits=FALSE)
    }
    vals
}

### Read only caching!
dbCountUniqueMappedVals <- function(con, table, join, from.col, to.col, datacache=NULL)
{
    if (!is.null(datacache)) {
        objname <- paste("dbUniqueMappedVals", table, from.col, to.col, sep=".")
        if (exists(objname, envir=datacache, inherits=FALSE)) {
            count <- length(get(objname, envir=datacache, inherits=FALSE))
            return(count)
        }
    }
    sql <- paste("SELECT COUNT(DISTINCT ", from.col, ") FROM ", table, sep="")
    if (length(join) == 1) # will be FALSE for NULL or character(0)
        sql <- paste(sql, join)
    sql <- paste(sql, "WHERE", to.col, "IS NOT NULL")
    .dbGetQuery(con, sql)[[1]]
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Other helper functions

formatAsList <- function(lsubmap, names, type=NULL, replace.single=NULL, replace.multiple=NULL)
{
    formatVal <- function(key)
    {
        val <- lsubmap[key][[1]]
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
### Classes representing SQLite-based annotation maps.
###

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
        "VIRTUAL",
        join="character",
        leftCol="character",
        leftTable="character",
        con="DBIConnection",
        datacache="environment",
        chipShortname="character",
        mapName="character"
    )
)

setClass("ReverseAnnMap", representation("VIRTUAL"))

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
        ## set only if the right names are tagged
        tagsCol="character", 
        ## set only if the right names need coercion after extraction
        rightColType="character",
        replace.single="character",
        replace.multiple="character"
    )
)

### DON'T ADD ANY SLOT HERE! A given AnnMap subclass and its corresponding
### "reverse" class should always have exactly the same slots.
setClass("ReverseAtomicAnnMap", contains=c("ReverseAnnMap", "AtomicAnnMap"))

### For a "GOAnnMap" object, the right values are named lists of GO nodes,
### each GO node being represented as a 3-element list of the form
###   list(GOID="GO:0006470" , Evidence="IEA" , Ontology="BP")
setClass("GOAnnMap",
    contains="AnnMap",
    representation(all="logical")
)

### Maps a GO term to a named character vector containing left values tagged
### with the Evidence code.
### DON'T ADD ANY SLOT HERE! (Why? See "ReverseAtomicAnnMap" def above.)
setClass("ReverseGOAnnMap", contains=c("ReverseAnnMap", "GOAnnMap"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "revmap" new generic.
###
### Note that I'd like to use "reverse" instead of "revmap" but "reverse" is
### already defined as a generic in Biostrings and it seems that the second
### of the 2 packages to be loaded breaks the generic and attached methods
### defined in the first. Don't know how to deal with this situation :-/
### The "rev" generic defined in package:base doesn't work neither because
### we want to be able to use a different signature (2 args).

setGeneric("revmap", function(x, mapName=NULL) standardGeneric("revmap"))

setMethod("revmap", "AtomicAnnMap",
    function(x, mapName=NULL)
    {
        if (is.null(mapName))
            mapName <- paste("revmap(", x@mapName, ")", sep="")
        else
            mapName <- as.character(mapName)
        new("ReverseAtomicAnnMap", x, mapName=mapName)
    }
)
setMethod("revmap", "ReverseAtomicAnnMap",
    function(x, mapName=NULL)
    {
        stop("already a reverse map")
    }
)
setMethod("revmap", "GOAnnMap",
    function(x, mapName=NULL)
    {
        if (is.null(mapName))
            mapName <- paste("revmap(", x@mapName, ")", sep="")
        else
            mapName <- as.character(mapName)
        new("ReverseGOAnnMap", x, mapName=mapName)
    }
)
setMethod("revmap", "ReverseGOAnnMap",
    function(x, mapName=NULL)
    {
        stop("already a reverse map")
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "db" new generic.

setGeneric("db", function(object) standardGeneric("db"))

setMethod("db", "AnnMap", function(object) object@con)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "as.data.frame" generic.
###
### WORK IN PROGRESS!!! Experimenting a new as.data.frame interface. Use at
### your own risk!
###
### Note that because we use the same JOIN for a map and its corresponding
### "reverse" map (this is made possible thanks to an INNER JOIN), then the
### result returned by "as.data.frame" or "nrow" does not depend on the
### orientation (direct/reverse) of the map which is a nice property
### (e.g. 'nrow(map) == nrow(revmap(map))').
###
### Arguments 'row.names' and 'optional' are ignored.
### The 'left.names' and 'right.names' args can be one of the following:
###   - NULL: the arg is ignored.
###   - A NA-free character vector: only the rows with a "left name" (1st
###     field) matching one of the names in 'left.names' and a "right name"
###     (2nd field) matching one of the names in 'right.names' are
###     retrieved.
### Note that the 'left.names' and 'right.names' args are _not_ checked i.e.
### only NULL and NA-free character vectors are guaranted to work properly.

setMethod("as.data.frame", "AtomicAnnMap",
    function(x, row.names=NULL, optional=FALSE,
             left.names=NULL, right.names=NULL, extra.cols=NULL, verbose=FALSE)
    {
        if (missing(left.names))
            left.names <- row.names
        if (missing(right.names) && !identical(optional, FALSE))
            right.names <- optional
        if (length(x@tagsCol) == 1)
            extra.cols <- c(x@tagsCol, extra.cols)
        dbMapToDataFrame(db(x), x@rightTable, x@join,
                                x@leftCol, left.names,
                                x@rightCol, right.names,
                                extra.cols, verbose)
    }
)

setMethod("as.data.frame", "GOAnnMap",
    function(x, row.names=NULL, optional=FALSE,
             left.names=NULL, right.names=NULL, extra.cols=NULL, verbose=FALSE)
    {
        if (missing(left.names))
            left.names <- row.names
        if (missing(right.names) && !identical(optional, FALSE))
            right.names <- optional
        extra.cols <- c("evidence", extra.cols)
        getPartialSubmap <- function(Ontology)
        {
            table <- GOtables(x@all)[Ontology]
            data <- dbMapToDataFrame(db(x), table, x@join,
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "nrow" new generic.
###
### Conceptual definition (for AnnMap object x):
###     nrow(x) :== nrow(as.data.frame(x))
###
### Since "as.data.frame" is unoriented, then "nrow" is unoriented too.

setGeneric("nrow", function(x) standardGeneric("nrow"))

setMethod("nrow", "AtomicAnnMap",
    function(x)
    {
        dbCountDataFrameRows(db(x), x@rightTable, x@join, x@leftCol, x@rightCol)
    }
)

setMethod("nrow", "GOAnnMap",
    function(x)
    {
        countRows <- function(Ontology)
        {
            table <- GOtables(x@all)[Ontology]
            dbCountDataFrameRows(db(x), table, x@join, x@leftCol, "go_id")
        }
        countRows("BP") + countRows("CC") + countRows("MF")
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "left.names", "right.names" and "names" generics.
###

setGeneric("left.names", function(x) standardGeneric("left.names"))
setGeneric("right.names", function(x) standardGeneric("right.names"))

setMethod("left.names", "AnnMap",
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
            table <- GOtables(x@all)[Ontology]
            dbUniqueVals(db(x), table, "go_id", x@datacache)
        }
        ## Because a given go_id can only belong to 1 of the 3 ontologies...
        ## (if not, then apply unique to this result)
        c(getNames("BP"), getNames("CC"), getNames("MF"))
    }
)

setMethod("names", "AnnMap", function(x) left.names(x))
setMethod("names", "ReverseAnnMap", function(x) right.names(x))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "left.length", "right.length" and "length" generic.
###
### Conceptual definitions (for AnnMap object x):
###     left.length(x) :== length(left.names(x))
###     right.length(x) :== length(right.names(x))

setGeneric("left.length", function(x) standardGeneric("left.length"))
setGeneric("right.length", function(x) standardGeneric("right.length"))

### Will catch "AtomicAnnMap" and "GOAnnMap" objects.
setMethod("left.length", "AnnMap",
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
            table <- GOtables(x@all)[Ontology]
            dbCountUniqueVals(db(x), table, "go_id", x@datacache)
        }
        ## Because a given go_id can only belong to 1 of the 3 ontologies...
        countNames("BP") + countNames("CC") + countNames("MF")
    }
)

setMethod("length", "AnnMap", function(x) left.length(x))
setMethod("length", "ReverseAnnMap", function(x) right.length(x))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" generic.

setMethod("show", "AnnMap",
    function(object)
    {
        cat(object@mapName, " map for chip ", object@chipShortname,
            " (object of class \"", class(object), "\")\n", sep="")
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "as.character" generic.
###
### For untagged Reverse/AtomicAnnMap obects only!

### R doesn't let me add a 'names' arg here:
###  Error in rematchDefinition(definition, fdef, mnames, fnames, signature) :
###          methods can add arguments to the generic only if '...' is an argument to the generic
setMethod("as.character", "AtomicAnnMap",
    function(x)
    {
        if (length(x@tagsCol) == 1)
            stop("cannot coerce to character an AtomicAnnMap object with tags")
        data <- as.data.frame(x)
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
        if (length(x@tagsCol) == 1)
            stop("cannot coerce to character an AtomicAnnMap object with tags")
        data <- as.data.frame(x)
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
### The "as.list" generic.
###
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
###     shortcut for 'as.list(x, names(x)[1:3])'. This is identical to
###     'as.list(x)[1:3]' but of course much more efficent.
### Note that the 'names' arg. is _not_ checked i.e. only NULL, NA-free
### character vectors and NA-free numeric vectors are guaranted to work.

.checkNamesAreStrings <- function(names)
{
    if (is.null(names) || !is.character(names) || any(is.na(names)))
        stop("invalid first argument")
}

.checkNamesExist <- function(names, all.names)
{
    if (!is.null(names)) {
        not_found <- which(!(names %in% all.names))
        if (length(not_found) != 0)
            stop("value for '", names[not_found[1]], "' not found")
    }
}

setMethod("as.list", "AtomicAnnMap",
    function(x, names=NULL)
    {
        if (!is.null(names) && length(names) == 0)
            return(list())
        data <- as.data.frame(x, left.names=names)
        if (nrow(data) == 0)
            return(list())
        lsubmap <- data[[x@rightCol]]
        if (length(x@tagsCol) == 1)
            names(lsubmap) <- data[[x@tagsCol]]
        lsubmap <- split(lsubmap, data[[x@leftCol]])
        if (is.null(names))
            names <- names(x)
        formatAsList(lsubmap, names, x@rightColType, x@replace.single, x@replace.multiple)
    }
)

setMethod("as.list", "ReverseAtomicAnnMap",
    function(x, names=NULL)
    {
        if (!is.null(names) && length(names) == 0)
            return(list())
        data <- as.data.frame(x, right.names=names)
        if (!is.null(names) && !all(names %in% data[[x@rightCol]]))
            .checkNamesExist(names, names(x))
        if (nrow(data) == 0)
            return(list())
        lsubmap <- split(data[[x@leftCol]], data[[x@rightCol]])
        if (is.null(names))
            names <- names(x)
        formatAsList(lsubmap, names, x@rightColType, x@replace.single, x@replace.multiple)
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
        data <- as.data.frame(x, left.names=names)
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
        lsubmap <- lapply(names,
                         function(y)
                         {
                             if (!(y %in% mapped_names))
                                 NA
                             else
                                 makeGONodeList(GOIDs[[y]], Evidences[[y]], Ontologies[[y]])
                         })
        names(lsubmap) <- names
        lsubmap
    }
)

setMethod("as.list", "ReverseGOAnnMap",
    function(x, names=NULL)
    {
        if (!is.null(names) && length(names) == 0)
            return(list())
        data <- as.data.frame(x, right.names=names)
        if (!is.null(names) && !all(names %in% data[["go_id"]]))
            .checkNamesExist(names, names(x))
        if (nrow(data) == 0)
            return(list())
        lsubmap <- data[[x@leftCol]]
        names(lsubmap) <- data[["evidence"]]
        lsubmap <- split(lsubmap, data[["go_id"]])
        if (is.null(names))
            names <- names(x)
        formatAsList(lsubmap, names)
    }
)


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
        seed$Class <- "AtomicAnnMap"
        for (slot in names(seed0)) {
            if (is.null(seed[slot][[1]]))
                seed[[slot]] <- seed0[[slot]]
        }
        maps[[seed$mapName]] <- do.call("new", seed)
    }
    maps
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "mapped names" family of generics:
###   - mapped.left.names, count.mapped.left.names
###   - mapped.right.names, count.mapped.right.names
###   - mapped.names, count.mapped.names
###
### Conceptual definitions (for AnnMap object x):
###
###     mapped.left.names(x) :== unique values in left col (col 1) of
###                              as.data.frame(x)
###     count.mapped.left.names(x) :== length(mapped.left.names(x))
###
###     mapped.right.names(x) :== unique values in right col (col 2) of
###                               as.data.frame(x)
###     count.mapped.right.names(x) :== length(mapped.right.names(x))
###
### Note that all "right names" should be mapped to a "left name" hence
### mapped.right.names(x) should be the same as right.names(x) (something
### worth checking in a test unit).

setGeneric("mapped.left.names", function(x) standardGeneric("mapped.left.names"))
setGeneric("count.mapped.left.names", function(x) standardGeneric("count.mapped.left.names"))

setGeneric("mapped.right.names", function(x) standardGeneric("mapped.right.names"))
setGeneric("count.mapped.right.names", function(x) standardGeneric("count.mapped.right.names"))

setGeneric("mapped.names", function(x) standardGeneric("mapped.names"))
setGeneric("count.mapped.names", function(x) standardGeneric("count.mapped.names"))

### Ignore x@replace.single and x@replace.multiple, hence will give
### wrong results for maps that have one of those 2 fields with non-default
### values like silly maps ENTREZID and MULTIHIT in AGDB schema.
### But who cares, those maps are silly anyway...
setMethod("mapped.left.names", "AtomicAnnMap",
    function(x)
    {
        dbUniqueMappedVals(db(x), x@rightTable, x@join,
                           x@leftCol, x@rightCol, x@datacache)
    }
)
setMethod("count.mapped.left.names", "AtomicAnnMap",
    function(x)
    {
        dbCountUniqueMappedVals(db(x), x@rightTable, x@join,
                                x@leftCol, x@rightCol, x@datacache)
    }
)

setMethod("mapped.right.names", "AtomicAnnMap",
    function(x)
    {
        dbUniqueMappedVals(db(x), x@rightTable, x@join,
                           x@rightCol, x@leftCol, x@datacache)
    }
)
setMethod("count.mapped.right.names", "AtomicAnnMap",
    function(x)
    {
        dbCountUniqueMappedVals(db(x), x@rightTable, x@join,
                                x@rightCol, x@leftCol, x@datacache)
    }
)

setMethod("mapped.left.names", "GOAnnMap",
    function(x)
    {
        getMappedNames <- function(Ontology)
        {
            table <- GOtables(x@all)[Ontology]
            dbUniqueMappedVals(db(x), table, x@join,
                               x@leftCol, "go_id", x@datacache)
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
            table <- GOtables(x@all)[Ontology]
            dbUniqueMappedVals(db(x), table, x@join,
                               "go_id", x@leftCol, x@datacache)
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
            table <- GOtables(x@all)[Ontology]
            dbCountUniqueMappedVals(db(x), table, x@join,
                                    "go_id", x@leftCol, x@datacache)
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### For testing only (not exported).
###

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
        t1 <- system.time(count1 <- count.mapped.names(map))
        cat("  - count1 = ", count1, " (", t1[3], " s)\n", sep="")
        t2 <- system.time(count2 <- sum(sapply(as.list(map), function(x) length(x)!=1 || !is.na(x))))
        cat("  - count2 = ", count2, " (", t2[3], " s)\n", sep="")
        if (count1 != count0 || count2 != count0)
            stop("count0, count1 and count2 not the same")
    }
}

