### All the createAnnObjs.*_DB() functions currently support the same DB
### schema version (of course, each function support its own schema).
DBSCHEMAVERSION <- "0.9"

checkDBSCHEMA <- function(dbconn, DBSCHEMA)
{
    schema <- dbmeta(dbconn, "DBSCHEMA")
    if (schema != DBSCHEMA)
        stop("invalid DB schema (found ", schema, ", expected ", DBSCHEMA, ")")
    version <- dbmeta(dbconn, "DBSCHEMAVERSION")
    if (version != DBSCHEMAVERSION)
        stop("invalid DB schema version (found ", version, ", expected ", DBSCHEMAVERSION, ")")
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### 
###

Go3tablenames <- function(all=FALSE)
{
    tablenames <- c("go_bp", "go_cc", "go_mf")
    if (all)
        tablenames <- paste(tablenames, "_all", sep="")
    names(tablenames) <- c("BP", "CC", "MF")
    tablenames
}

makeGo3L2Rchain <- function(L2Rchain, tablename, ontology)
{
    chainlen <- length(L2Rchain)
    L2Rchain[[chainlen]]@tablename <- tablename
    L2Rchain[[chainlen]]@Rattribnames["Ontology"] <- paste("'", ontology, "'", sep="")
    L2Rchain
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
###

### If 'envir' is not NULL then the created objects are assigned to it.
### TODO: The function should check for name clashes
createAnnObjs <- function(class, seeds, seed0, envir=NULL)
{
    if (is.null(envir))
        envir <- new.env(hash=TRUE, parent=emptyenv())
    for (seed in seeds) {
        seed$Class <- class
        for (slot in names(seed0)) {
            if (is.null(seed[slot][[1]]))
                seed[[slot]] <- seed0[[slot]]
        }
        envir[[seed$objName]] <- do.call("new", seed)
    }
    envir
}

createAnnDbBimap <- function(seed, seed0)
{
    for (slot in names(seed0)) {
        if (is.null(seed[slot][[1]]))
            seed[[slot]] <- seed0[[slot]]
    }
    L2Rchain <- seed$L2Rchain
    seed$L2Rchain <- lapply(L2Rchain, function(L2Rlink) do.call("L2Rlink", L2Rlink))
    do.call("new", seed)
}

createAnnDbBimaps <- function(seeds, seed0, envir=NULL)
{
    if (is.null(envir))
        envir <- new.env(hash=TRUE, parent=emptyenv())
    for (seed in seeds)
        envir[[seed$objName]] <- createAnnDbBimap(seed, seed0)
    envir
}

### 3 special maps that are not AnnDbBimap objects (just named vectors).

createCHRLENGTHS <- function(dbconn)
{
    data <- dbGetTable(dbconn, "chrlengths")
    CHRLENGTHS <- data[["length"]]
    names(CHRLENGTHS) <- data[["chr"]]
    CHRLENGTHS
}

createREJECTORF <- function(dbconn)
{
    data <- dbGetTable(dbconn, "reject_orf")
    data[["systematic_name"]]
}

createMAPCOUNTS <- function(dbconn, prefix)
{
    data <- dbGetTable(dbconn, "qcdata", "WHERE map_name != 'TOTAL' ORDER BY map_name")
    MAPCOUNTS <- data[["count"]]
    names(MAPCOUNTS) <- paste(prefix, data[["map_name"]], sep="")
    MAPCOUNTS
}

### Rename all objects in the 'envir' environment by prefixing them
### with 'prefix'. The function is dumb i.e. it doesn't check for (neither
### doesn't try to avoid) possible name clashes. Note that those issues
### could be easily avoided by assigning the renamed objects to a separate
### environment but...
prefixAnnObjNames <- function(envir, prefix)
{
    keys <- ls(envir, all.names=TRUE)
    for (key in keys) {
        new_key <- paste(prefix, key, sep="")
        envir[[new_key]] <- envir[[key]]
    }
    remove(list=keys, envir=envir) # remove old keys
    envir
}

