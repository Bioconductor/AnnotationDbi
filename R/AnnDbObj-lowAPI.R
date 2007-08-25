### =========================================================================
### Low-level API for AnnDbObj objects
### ----------------------------------
###
### The "AnnDbObj" class is a general purpose container for SQLite-based
### annotation data (refer to AllClasses.R for the definition of the "AnnDbObj"
### class and its derived classes).
###
### This file defines and implements the low-level API for AnnDbObj objects.
### This API consists of the following set of generics:
###
###   1) Generics that do _not_ access the database:
###        db,
###        Ltablename, Rtablename,
###        Lfilter, Rfilter,
###        collabels,
###        colnames,
###        direction, revmap,
###        show
###
###   2) Generics that access the database:
###        links, count.links,
###        nrow,
###        Lkeys, Rkeys,
###        Llength, Rlength, 
###        mappedLkeys, mappedRkeys,
###        count.mappedLkeys, count.mappedRkeys,
###        subset,
###        flatten, toTable, dim,
###        as.character,
###        toLList, toRList,
###        is.na
###      NB: "keys", "length", "mappedkeys" and "count.mappedkeys" are
###      "directed" methods i.e. they give a different result on a map and its
###      "reverse" map (this result depends on the direction of the map).
###      For each of these methods, there are 2 "undirected" methods: a left
###      method and a right method.
###
### The environment-like API for AnnDbBimap objects (ls, mget, etc...) is defined
### in the AnnDbBimap-envirAPI.R file.
###
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Generics that return meta information about a given map:
###     db,
###     Ltablename, Rtablename,
###     Lcolname, Rcolname,
###     colnames,
###     Lfilter, Rfilter
###
### Note that these generics do _not_ query the database!
###

setMethod("db", "AnnDbObj", function(object) object@conn)

setMethod("Ltablename", "AnnDbBimap",
    function(x) L2Rchain.Ltablename(x@L2Rchain))
setMethod("Rtablename", "AnnDbBimap",
    function(x) L2Rchain.Rtablename(x@L2Rchain))
setMethod("Rtablename", "Go3AnnDbBimap", function(x) x@rightTables)

setMethod("Lfilter", "AnnDbBimap",
    function(x) L2Rchain.Lfilter(x@L2Rchain))
setMethod("Rfilter", "AnnDbBimap",
    function(x) L2Rchain.Rfilter(x@L2Rchain))



### =========================================================================
### Bimap methods for AnnDbBimap objects
### ------------------------------------
###
### Key property (Property0): if x is a AnnDbBimap object, f1 a Bimap
### method for FlatBimap objects and f2 the corresponding method for
### AnnDbBimap objects then f2(x) is expected to return exactly the same
### thing as f1(flatten(x)).
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "collabels" and "colnames" methods.
###

setMethod("collabels", "AnnDbBimap",
    function(x) L2Rchain.collabels(x@L2Rchain))

setMethod("colnames", "AnnDbBimap",
    function(x, do.NULL=TRUE, prefix="col")
        L2Rchain.colnames(x@L2Rchain))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "direction" and "revmap" methods.
###
### Conceptual definition:
###     revmap(x) is the reverse of map x i.e. the map that provides the
###     reverse lookup (or mapping)
###
### I would have liked to use "reverse" instead of "revmap" but "reverse" is
### already defined as a generic in Biostrings and it seems that the second
### of the 2 packages to be loaded breaks the generic and attached methods
### defined in the first. Don't know how to deal with this situation :-/
###
### The "rev" generic defined in package:base can't be used neither because
### it doesn't allow passing additional arguments to or from methods (i.e. it
### has no '...' arg) and we want to be able to pass the 'objName' arg.
### Other generics defined in package:base where having a '...' arg could be
### useful: "unlist", "t" and "scale" (just in case someone feels brave enough
### to request this on R-devel).
###
### Note that "revmap" does _not_ query the database!
###

setMethod("direction", "AnnDbBimap",
    function(x) x@direction)

setReplaceMethod("direction", "AnnDbBimap",
    function(x, value)
    {
        direction <- .normalize.direction(value)
        if (direction == 0)
            stop("undirected AnnDbBimap objects are not supported")
        if (direction != x@direction) {
            x@objName <- paste("revmap(", x@objName, ")", sep="")
            x@direction <- direction
        }
        x
    }
)

setReplaceMethod("direction", "AnnDbMap",
    function(x, value)
    {
        stop("changing the direction of an \"", class(x), "\" object is not supported")
    }
)

setMethod("revmap", "AnnDbBimap",
    function(x, objName=NULL, ...)
    {
        if (!is.null(objName))
            x@objName <- toString(objName)
        callNextMethod() # calls the method for Bimap
    }
)

setMethod("revmap", "environment",
    function(x, ...) l2e(reverseSplit(as.list(x)))
)
setMethod("revmap", "list",
    function(x, ...) reverseSplit(x)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "links" and "count.links" methods.
###

setMethod("links", "AnnDbBimap",
    function(x) dbGetMapLinks(db(x), x@L2Rchain))

setMethod("count.links", "AnnDbBimap",
    function(x) dbCountMapLinks(db(x), x@L2Rchain))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "nrow" methods.
###

### CURRENTLY BROKEN!
setMethod("nrow", "AnnDbTable",
    function(x)
    {
        dbCountRawAnnDbMapRows(db(x), Ltablename(x), Lcolname(x), NULL, NULL, x@from)
    }
)

setMethod("nrow", "AnnDbBimap",
    function(x)
    {
        dbCountRowsFromL2Rchain(db(x), x@L2Rchain, x@Lkeys, x@Rkeys)
    }
)


setMethod("nrow", "Go3AnnDbBimap",
    function(x)
    {
        countRows <- function(ontology)
        {
            tablename <- Rtablename(x)[ontology]
            L2Rchain <- makeGo3L2Rchain(x@L2Rchain, tablename, ontology)
            dbCountRowsFromL2Rchain(db(x), L2Rchain, x@Lkeys, x@Rkeys)
        }
        countRows("BP") + countRows("CC") + countRows("MF")
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "Lkeys", "Rkeys",  "Lkeys<-", "Rkeys<-"
### and "subset" generics.
###

.inslot.Lkeys <- function(x)
{
    length(x@Lkeys) != 1 || !is.na(x@Lkeys)
}
.inslot.Rkeys <- function(x)
{
    length(x@Rkeys) != 1 || !is.na(x@Rkeys)
}

setMethod("Lkeys", "AnnDbBimap",
    function(x)
    {
        if (.inslot.Lkeys(x))
            return(x@Lkeys)
        dbUniqueVals(db(x), Ltablename(x), Lcolname(x),
                            Lfilter(x), x@datacache)
    }
)

setMethod("Rkeys", "AnnDbBimap",
    function(x)
    {
        if (.inslot.Rkeys(x))
            return(x@Rkeys)
        dbUniqueVals(db(x), Rtablename(x), Rcolname(x),
                            Rfilter(x), x@datacache)
    }
)

setMethod("Rkeys", "Go3AnnDbBimap",
    function(x)
    {
        if (.inslot.Rkeys(x))
            return(x@Rkeys)
        getNames <- function(ontology)
        {
            tablename <- Rtablename(x)[ontology]
            dbUniqueVals(db(x), tablename, "go_id",
                                Rfilter(x), x@datacache)
        }
        ## Because a given go_id can only belong to 1 of the 3 ontologies...
        ## (if not, then apply unique to this result)
        c(getNames("BP"), getNames("CC"), getNames("MF"))
    }
)

setMethod("Rkeys", "AnnDbMap",
    function(x)
    {
        stop("Rkeys() is not supported for an \"", class(x), "\" object")
    }
)

setReplaceMethod("Lkeys", "AnnDbBimap",
    function(x, value)
    {
        if (!is.null(value)) {
            .checkKeys(value, Lkeys(x), x@ifnotfound)
            if (!is.null(names(value)))
                names(value) <- NULL
            x@Lkeys <- value
        }
        x
    }
)

setReplaceMethod("Rkeys", "AnnDbBimap",
    function(x, value)
    {
        if (!is.null(value)) {
            .checkKeys(value, Rkeys(x), x@ifnotfound)
            if (!is.null(names(value)))
                names(value) <- NULL
            x@Rkeys <- value
        }
        x
    }
)

setMethod("subset", "AnnDbBimap",
    function(x, Lkeys=NULL, Rkeys=NULL, objName=NULL, ...)
    {
        if (!is.null(objName))
            x@objName <- toString(objName)
        Lkeys(x) <- Lkeys
        Rkeys(x) <- Rkeys
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "Llength", "Rlength" and "length" generic.
###
### Conceptual definitions (for AnnDbBimap object x):
###     Llength(x) :== length(Lkeys(x))
###     Rlength(x) :== length(Rkeys(x))
###

setMethod("Llength", "AnnDbBimap",
    function(x)
    {
        if (.inslot.Lkeys(x))
            return(length(x@Lkeys))
        dbCountUniqueVals(db(x), Ltablename(x), Lcolname(x),
                                 Lfilter(x), x@datacache)
    }
)

setMethod("Rlength", "AnnDbBimap",
    function(x)
    {
        if (.inslot.Rkeys(x))
            return(length(x@Rkeys))
        dbCountUniqueVals(db(x), Rtablename(x), Rcolname(x),
                                 Rfilter(x), x@datacache)
    }
)

setMethod("Rlength", "Go3AnnDbBimap",
    function(x)
    {
        if (.inslot.Rkeys(x))
            return(length(x@Rkeys))
        countNames <- function(ontology)
        {
            tablename <- Rtablename(x)[ontology]
            dbCountUniqueVals(db(x), tablename, "go_id", Rfilter(x), x@datacache)
        }
        ## Because a given go_id can only belong to 1 of the 3 ontologies...
        countNames("BP") + countNames("CC") + countNames("MF")
    }
)

setMethod("Rlength", "AnnDbMap",
    function(x)
    {
        stop("Rlength() is not supported for an \"", class(x), "\" object")
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "[count.]mapped[L|R]keys" methods.
###
### Note that for some DB schemas like the HUMANCHIP_DB schema, all the "right
### objects" are expected to be mapped to a "left object" (and this is true
### for all the maps in the HUMANCHIP_DB-based ann package) hence
### mappedRkeys(x) should be the same as Rkeys(x) (maybe
### something worth checking in a test unit).
###

### For an AtomicAnnDbBimap, x@replace.single and x@replace.multiple will be
### ignored, hence will give wrong results if one of those 2 fields has a
### non-default value like silly maps ENTREZID and MULTIHIT in ARABIDOPSISCHIP_DB
### schema. But who cares, those maps are silly anyway...
setMethod("mappedLkeys", "AnnDbBimap",
    function(x)
    {
        dbUniqueMappedKeys(db(x), x@L2Rchain, x@Lkeys, x@Rkeys,
                                  1, x@datacache)
    }
)
setMethod("count.mappedLkeys", "AnnDbBimap",
    function(x)
    {
        dbCountUniqueMappedKeys(db(x), x@L2Rchain, x@Lkeys, x@Rkeys,
                                       1, x@datacache)
    }
)

setMethod("mappedRkeys", "AnnDbBimap",
    function(x)
    {
        dbUniqueMappedKeys(db(x), x@L2Rchain, x@Lkeys, x@Rkeys,
                                  -1, x@datacache)
    }
)
setMethod("count.mappedRkeys", "AnnDbBimap",
    function(x)
    {
        dbCountUniqueMappedKeys(db(x), x@L2Rchain, x@Lkeys, x@Rkeys,
                                       -1, x@datacache)
    }
)

setMethod("mappedLkeys", "Go3AnnDbBimap",
    function(x)
    {
        getMappedKeys <- function(ontology)
        {
            tablename <- Rtablename(x)[ontology]
            L2Rchain <- makeGo3L2Rchain(x@L2Rchain, tablename, ontology)
            dbUniqueMappedKeys(db(x), L2Rchain, x@Lkeys, x@Rkeys,
                                      1, x@datacache)
        }
        keys1 <- getMappedKeys("BP")
        keys2 <- getMappedKeys("CC")
        keys3 <- getMappedKeys("MF")
        unique(c(keys1, keys2, keys3))
    }
)
setMethod("count.mappedLkeys", "Go3AnnDbBimap",
    function(x) length(mappedLkeys(x))
)

setMethod("mappedRkeys", "Go3AnnDbBimap",
    function(x)
    {
        getMappedKeys <- function(ontology)
        {
            tablename <- Rtablename(x)[ontology]
            L2Rchain <- x@L2Rchain
            L2Rchain[[length(L2Rchain)]]@tablename <- tablename
            dbUniqueMappedKeys(db(x), L2Rchain, x@Lkeys, x@Rkeys,
                                      -1, x@datacache)
        }
        keys1 <- getMappedKeys("BP")
        keys2 <- getMappedKeys("CC")
        keys3 <- getMappedKeys("MF")
        ## Because a given go_id can only belong to 1 of the 3 ontologies...
        ## (if not, then apply unique to this result)
        c(keys1, keys2, keys3)
    }
)
setMethod("count.mappedRkeys", "Go3AnnDbBimap",
    function(x)
    {
        countMappedNames <- function(ontology)
        {
            tablename <- Rtablename(x)[ontology]
            L2Rchain <- makeGo3L2Rchain(x@L2Rchain, tablename, ontology)
            dbCountUniqueMappedKeys(db(x), L2Rchain, x@Lkeys, x@Rkeys,
                                           -1, x@datacache)
        }
        ## Because a given go_id can only belong to 1 of the 3 ontologies...
        countMappedNames("BP") + countMappedNames("CC") + countMappedNames("MF")
    }
)

setMethod("mappedRkeys", "AnnDbMap",
    function(x)
    {
        stop("mappedRkeys() is not supported for an \"", class(x), "\" object")
    }
)

setMethod("count.mappedRkeys", "AnnDbMap",
    function(x)
    {
        stop("count.mappedRkeys() is not supported for an \"", class(x), "\" object")
    }
)

### And for an environment or a list...
setMethod("mappedkeys", "environment",
    function(x)
    {
        is_na <- eapply(x, function(x) length(x) == 1 && is.na(x), all.names=TRUE)
        names(is_na)[!unlist(is_na, recursive=FALSE, use.names=FALSE)]
    }
)
setMethod("mappedkeys", "list",
    function(x)
    {
        is_na <- lapply(x, function(x) length(x) == 1 && is.na(x))
        names(is_na)[!unlist(is_na, recursive=FALSE, use.names=FALSE)]
    }
)
setMethod("count.mappedkeys", "ANY", function(x) length(mappedkeys(x)))



### =========================================================================
### The "flatten" methods
### ---------------------
###
### Note that because we use the same JOIN for a map and its corresponding
### "reverse" map (this is made possible thanks to the use of INNER joins),
### then the result returned by "flatten" or "nrow" does not depend on the
### direction (direct/reverse) of the map which is a nice property
### (e.g. 'nrow(map) == nrow(revmap(map))').
###
### The 'Lkeys' and 'Rkeys' args can be one of the following:
###   - NULL: the arg is ignored.
###   - A NA-free character vector: only the rows with a "left key" (1st
###     field) matching one of the keys in 'Lkeys' and a "right key"
###     (2nd field) matching one of the keys in 'Rkeys' are
###     retrieved.
### Note that the 'Lkeys' and 'Rkeys' args are _not_ checked i.e.
### only NULL and NA-free character vectors are guaranted to work properly.
###

### CURRENTLY BROKEN!
#setMethod("flatten", "AnnDbTable",
#    function(x)
#    {
#        dbRawAnnDbMapToTable(db(x), Ltablename(x), Lcolname(x), Lkeys,
#                                    NULL, NULL, NULL,
#                                    x@showCols, x@from)
#    }
#)

setMethod("flatten", "AnnDbBimap",
    function(x, fromKeys.only=FALSE)
    {
        data0 <- dbSelectFromL2Rchain(db(x), x@L2Rchain,
                                     x@Lkeys, x@Rkeys)
        Lkeys <- Rkeys <- as.character(NA)
        if ((direction(x) != 1) != fromKeys.only)
            Lkeys <- Lkeys(x)
        if ((direction(x) == 1) != fromKeys.only)
            Rkeys <- Rkeys(x)
        new("FlatBimap", collabels=collabels(x), direction=direction(x),
                         data=data0, Lkeys=Lkeys, Rkeys=Rkeys)
    }
)

### This method needs to retrieve and bind data from the 3 GO tables.
### Binding the results of the 3 SELECTs can be done early in SQLite with
### a UNION:
###   dbGetQuery("query1 UNION query2 UNION query3")
### or later in R with rbind():
###   rbind(dbGetQuery("query1"), dbGetQuery("query2"), dbGetQuery("query3"))
### Surprisingly the latter is almost twice faster than the former!
setMethod("flatten", "Go3AnnDbBimap",
    function(x, fromKeys.only=FALSE)
    {
        getPartialSubmap <- function(ontology)
        {
            tablename <- Rtablename(x)[ontology]
            L2Rchain <- makeGo3L2Rchain(x@L2Rchain, tablename, ontology)
            data <- dbSelectFromL2Rchain(db(x), L2Rchain,
                                        x@Lkeys, x@Rkeys)
            if (nrow(data) != 0)
                data[["Ontology"]] <- ontology
            data
        }
        data0 <- rbind(getPartialSubmap("BP"),
                       getPartialSubmap("CC"),
                       getPartialSubmap("MF"))
        Lkeys <- Rkeys <- as.character(NA)
        if ((direction(x) != 1) != fromKeys.only)
            Lkeys <- Lkeys(x)
        if ((direction(x) == 1) != fromKeys.only)
            Rkeys <- Rkeys(x)
        new("FlatBimap", collabels=collabels(x), direction=direction(x),
                         data=data0, Lkeys=Lkeys, Rkeys=Rkeys)
    }
)

setMethod("toTable", "AnnDbBimap",
    function(x, Lkeys=NULL, Rkeys=NULL)
    {
        x <- subset(x, Lkeys, Rkeys)
        flatten(x, fromKeys.only=TRUE)@data
    }
)



### =========================================================================
### Other convenience methods
### -------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" methods.
###
### Note that this generic does _not_ query the database!
###

.is.submap <- function(x)
{
    .inslot.Lkeys(x) || .inslot.Rkeys(x)
}

setMethod("show", "AnnDbTable",
    function(object)
    {
        cat(object@objName, " table for ", object@objTarget,
            " (object of class \"", class(object), "\")\n", sep="")
    }
)

setMethod("show", "AnnDbBimap",
    function(object)
    {
        map <- "map"
        if (.is.submap(object))
            map <- "submap"
        cat(object@objName, " ", map, " for ", object@objTarget,
            " (object of class \"", class(object), "\")\n", sep="")
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "as.character" generic.
###
### For untagged AtomicAnnDbBimap objects only!
###

### R doesn't let me add a 'keys' arg here:
###  Error in rematchDefinition(definition, fdef, mnames, fnames, signature) :
###          methods can add arguments to the generic only if '...' is an argument to the generic
setMethod("as.character", "AtomicAnnDbBimap",
    function(x)
    {
        if (ncol(x) > 2)
            stop("AtomicAnnDbBimap object with tags cannot be coerced to a character vector")
        data <- flatten(x, fromKeys.only=TRUE)@data
        if (direction(x) == 1)
            ans <- data[[2]] # could also use [[Rcolname(x)]]
        else
            ans <- data[[1]] # could also use [[Lcolname(x)]]
        if (!is.character(ans))
            ans <- as.character(ans)
        if (direction(x) == 1)
            names(ans) <- data[[1]] # could also use [[Lcolname(x)]]
        else
            names(ans) <- data[[2]] # could also use [[Rcolname(x)]]
        if (any(duplicated(names(ans))))
            warning("returned vector has duplicated names")
        ans
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "toLList" and "toRList" methods.
###
### Like the "toList" methods, the "toLList" and "toRList" methods
### have a 'keys' arg.
### The 'keys' arg. can be one of the following:
###   - NULL: all map elements are extracted (equivalent to passing
###           'keys=keys(x)' but more efficient).
###   - A NA-free character vector: the names of the returned list will
###     be those passed in 'keys' in the same order. Keys that are not
###     in the map are associated with NAs. Note that, unlike "mget",
###     "toList" doesn't treat differently keys that are not in the map
###     from keys that are in the map but associated with NAs.
###   - A NA-free numeric vector: for conveniency 'toList(x, 1:3)' is a
###     shortcut for 'toList(x, keys(x)[1:3])'. This is identical to
###     'toList(x)[1:3]' but much faster.
### Note that the 'keys' arg. is _not_ checked i.e. only NULL, NA-free
### character vectors and NA-free numeric vectors are guaranted to work.
###

setMethod("toLList", "AnnDbBimap",
    function(x, keys=NULL)
    {
        x <- subset(x, Lkeys=keys, Rkeys=NULL)
        toLList(flatten(x, fromKeys.only=TRUE))
    }
)

setMethod("toLList", "AnnDbMap",
    function(x, keys=NULL)
    {
        x <- subset(x, Lkeys=keys, Rkeys=NULL)
        y <- flatten(x, fromKeys.only=TRUE)
        if (length(x@rightColType) == 1
         && typeof(y@data[[2]]) != x@rightColType) {
                converter <- get(paste("as.", x@rightColType, sep=""))
                y@data[[2]] <- converter(y@data[[2]])
        }
        toLList(y)
    }
)

setMethod("toRList", "AnnDbBimap",
    function(x, keys=NULL)
    {
        x <- subset(x, Lkeys=NULL, Rkeys=keys)
        toRList(flatten(x, fromKeys.only=TRUE))
    }
)

setMethod("toRList", "AnnDbMap",
    function(x, keys=NULL)
    {
        stop("toRList() is not supported for an \"", class(x), "\" object")
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "is.na" generic.
###
### 'is.na(x)' is a named logical vector that associates each key in the map
### with TRUE except for those keys that are actually mapped to something
### (other than an NA).
###

setMethod("is.na", "AnnDbBimap",
    function(x)
    {
        mapped_keys <- mappedkeys(x)
        keys <- keys(x)
        ans <- !(keys %in% mapped_keys)
        names(ans) <- keys
        ans
    }
)

