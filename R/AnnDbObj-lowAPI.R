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
###     dbconn,
###     dbfile,
###     dbschema,
###     dbInfo,
###     Ltablename, Rtablename,
###     Lfilter, Rfilter,
###     flatten,
###     show
###
### The environment-like API for AnnDbBimap objects (ls, mget, etc...) is defined
### in the AnnDbBimap-envirAPI.R file.
###
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "dbconn" methods.
###

setMethod("dbconn", "environment", function(x) get("dbconn", envir=x))
setMethod("dbconn", "AnnDbObj", function(x) dbconn(x@datacache))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "dbfile" methods.
###

setMethod("dbfile", "environment", function(x) get("dbfile", envir=x))
setMethod("dbfile", "AnnDbObj", function(x) dbfile(x@datacache))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "dbschema" methods.
###

setMethod("dbschema", "DBIConnection",
    function(x, file="", show.indices=FALSE)
        cat("coming soon...\n", file=file))

setMethod("dbschema", "environment",
    function(x, file="", show.indices=FALSE)
        dbschema(dbconn(x), file=file, show.indices=show.indices))

setMethod("dbschema", "AnnDbObj",
    function(x, file="", show.indices=FALSE)
        dbschema(x@datacache, file=file, show.indices=show.indices))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "dbInfo" methods.
###

setMethod("dbInfo", "DBIConnection", function(x) cat("coming soon...\n"))
setMethod("dbInfo", "environment", function(x) dbInfo(dbconn(x)))
setMethod("dbInfo", "AnnDbObj", function(x) dbInfo(x@datacache))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "Ltablename", "Rtablename", "Lfilter" and "Rfilter" methods.
###
### They do NOT access the database!
###

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
#        dbRawAnnDbMapToTable(dbconn(x), Ltablename(x), Lkeyname(x), Lkeys,
#                                        NULL, NULL, NULL,
#                                        x@showCols, x@from)
#    }
#)

setMethod("flatten", "AnnDbBimap",
    function(x, fromKeys.only=FALSE)
    {
        data0 <- dbSelectFromL2Rchain(dbconn(x), x@L2Rchain, x@Lkeys, x@Rkeys)
        Lkeys <- Rkeys <- as.character(NA)
        if (!fromKeys.only || direction(x) ==  1)
            Lkeys <- Lkeys(x)
        if (!fromKeys.only || direction(x) == -1)
            Rkeys <- Rkeys(x)
        new("FlatBimap", colmetanames=colmetanames(x), direction=direction(x),
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
            dbSelectFromL2Rchain(dbconn(x), L2Rchain, x@Lkeys, x@Rkeys)
        }
        data0 <- rbind(getPartialSubmap("BP"),
                       getPartialSubmap("CC"),
                       getPartialSubmap("MF"))
        Lkeys <- Rkeys <- as.character(NA)
        if (!fromKeys.only || direction(x) ==  1)
            Lkeys <- Lkeys(x)
        if (!fromKeys.only || direction(x) == -1)
            Rkeys <- Rkeys(x)
        new("FlatBimap", colmetanames=colmetanames(x), direction=direction(x),
                         data=data0, Lkeys=Lkeys, Rkeys=Rkeys)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" method.
###

setMethod("show", "AnnDbTable",
    function(object)
    {
        cat(object@objName, " table for ", object@objTarget,
            " (object of class \"", class(object), "\")\n", sep="")
    }
)

