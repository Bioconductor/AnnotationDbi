### =========================================================================
### Low-level API for AnnDbObj objects
### ----------------------------------
###
### The "AnnDbObj" class is a general purpose container for SQLite-based
### annotation data (refer to AllClasses.R for the definition of the "AnnDbObj"
### class and its derived classes).
###
### This file defines and implements the low-level API for AnnDbObj objects.
### This API has the following regular function:
###     showQCData
### This API consists of the following set of generics:
###     dbconn,
###     dbfile,
###     dbmeta,
###     dbschema,
###     dbInfo,
###     Ltablename, Rtablename,
###     Lfilter, Rfilter,
###     flatten,
###     show
###
### The environment-like API for Bimap objects (ls, mget, etc...) is defined
### in the Bimap-envirAPI.R file.
###
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "showQCData" function.
###

## I need this function to see what mappings exist and only report if
## they DO exist.  But I DO want to use the mappings in addition to
## the map_counts table (because otherwise I have not verified that
## they exist and only looked that the map_counts expect them to
## exist).

showQCData <- function(prefix, datacache){
     cat(paste0("Quality control information for ", prefix, ":\n\n\n"))
     map_counts <- createMAPCOUNTS(dbconn(datacache), prefix)
     map_counts <- map_counts[!grepl('PFAM$', names(map_counts)) &
                              !grepl('PROSITE$', names(map_counts))]
     cat("This package has the following mappings:\n\n")
     for(i in seq_len(length(map_counts))){
         mapname <- names(map_counts)[i]
         map <- mget(mapname,inherits=TRUE,ifnotfound=NA)
         if(!is.na(map)[[1]]){
             cat(mapname, "has", map_counts[i], "mapped keys (of",
                 length(map[[1]]), "keys)\n")
         }
     }
     
     cat("\n\nAdditional Information about this package:\n\n")
     cat(paste0("DB schema: ",dbmeta(datacache, 'DBSCHEMA'), "\n"))
     cat(paste0("DB schema version: ",dbmeta(datacache, 'DBSCHEMAVERSION'), "\n"))
     ## Things to check for (may or may not be in some packages)
     meta = list(
       "ORGANISM" = "Organism: ",
       "EGSOURCEDATE" ="Date for NCBI data: ",
       "GOSOURCEDATE" = "Date for GO data: ",
       "KEGGSOURCEDATE" = "Date for KEGG data: ",
       "GPSOURCEDATE" = "Date for Golden Path data: ",
       ## "IPISOURCEDATE" = "Date for IPI data: ",
       "TAIRSOURCEDATE" = "Data for TAIR data: ",
       "YGSOURCEDATE" = "Date for SGD data: ",
       "FBSOURCEDATE" = "Date for Flybase data: ",
       "ENSOURCEDATE" = "Date for Ensembl data: "
     )

     for(i in 1:length(meta)){
         test = 0
         test = try( dbmeta(datacache, names(meta)[i]),silent=TRUE )
         if(inherits(test,"try-error")){ test = 0 }
         if(test != 0){ cat(meta[[i]], dbmeta(datacache, names(meta)[i]), "\n", sep="") }      
     }     
          
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "dbconn" methods.
###

setMethod("dbconn", "environment", function(x) get("dbconn", envir=x))
setMethod("dbconn", "AnnDbObj", function(x) dbconn(x@datacache))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "dbfile" methods.
###

setMethod("dbfile", "environment", function(x) get("dbfile", envir=x))
setMethod("dbfile", "AnnDbObj", function(x) dbfile(dbconn(x)))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "dbmeta" methods.
###

setMethod("dbmeta", "DBIConnection",
    function(x, name)
    {
        if (!is.character(name) || length(name) != 1 || is.na(name))
            stop("'name' must be a non-NA character string")
        name <- toSQLStringSet(name)
        SQL <- paste0("SELECT value FROM metadata WHERE name=", name)
        value <- dbQuery(x, SQL, 1)
        if (length(value) == 0)
            stop("meta ", name, " not found")
        if (length(value) != 1)
            stop("more than 1 meta found for ", name, " in metadata table (bad table)")
        value
    }
)

setMethod("dbmeta", "environment", function(x, name) dbmeta(dbconn(x), name))
setMethod("dbmeta", "AnnDbObj", function(x, name) dbmeta(x@datacahe, name))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "dbschema" methods.
###

setMethod("dbschema", "DBIConnection",
    function(x, file="", show.indices=FALSE)
    {
        schema <- dbmeta(x, "DBSCHEMA")
        version <- dbmeta(x, "DBSCHEMAVERSION")
        file <- system.file("DBschemas",
                            paste0("schemas_", version),
                            paste0(schema, ".sql"),
                            package="AnnotationDbi")
        lines <- readLines(file)
        if(schema == "INPARANOID_DB"){
            ## Remove the CREATE TABLE statement for the "central" organism
            ## (e.g. Homo sapiens for hom.Hs.inp.db) as well as the 2 CREATE
            ## INDEX statements for that table. That's 8 lines in total (6 for
            ## the CREATE TABLE statement and 1 per CREATE INDEX statement).
            ## The way this is done below is an ugly hack that is not robust
            ## at all!
            organism <- dbmeta(x, "ORGANISM")
            tableName <- sub(" ", "_", organism)
            ii <- grep(tableName, lines)
            if (length(ii) != 0L) {
                stopifnot(length(ii) == 3L)  # sanity check
                ## Not robust at all!
                ii <- c(ii[1]+1:5, ii)
                lines <- lines[-ii]  # this removes 8 lines
            }
        }
        if (!show.indices) {
            ## Remove the CREATE INDEX lines
            createIndexStart <- "CREATE INDEX"
            createIndexEnd <- ";"
            ii <- which(substr(lines, 1, nchar(createIndexStart)) == createIndexStart
                      & substr(lines, nchar(lines)-nchar(createIndexEnd)+1, nchar(lines)) == createIndexEnd)
            ii <- setdiff(ii, grep(createIndexEnd, substr(lines, 1, nchar(lines)-nchar(createIndexEnd)), fixed=TRUE))
            ## Remove comments preceding the CREATE INDEX blocks
            beforeLastBlock <- function(ii, i)
            {
                while ((i >= 1) && !(i %in% ii))
                    i <- i - 1
                while ((i >= 1) && (i %in% ii))
                    i <- i - 1
                i
            }
            i <- max(ii)
            while (i >= 1) {
                i <- beforeLastBlock(ii, i)
                while (i >= 1) {
                    if (substr(lines[i], 1, 2) != "--")
                        break
                    ii <- c(i, ii)
                    i <- i - 1
                }
            }
            lines <- lines[-ii]
        }
##         ## Remove empty trailing lines
##         ii <- integer(0)
##         i <- length(lines)
##         while ((i >= 1) && (lines[i] == "")) {
##             ii <- c(i, ii)
##             i <- i - 1
##         }
##         lines <- lines[-ii]
        cat(lines, sep="\n")
    }
)

setMethod("dbschema", "environment",
    function(x, file="", show.indices=FALSE)
        dbschema(dbconn(x), file=file, show.indices=show.indices))

setMethod("dbschema", "AnnDbObj",
    function(x, file="", show.indices=FALSE)
        dbschema(x@datacache, file=file, show.indices=show.indices))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "dbInfo" methods.
###

setMethod("dbInfo", "DBIConnection",
    function(x)
    {
        dbGetTable(x, "metadata")
    }
)
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

