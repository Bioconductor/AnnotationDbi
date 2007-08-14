### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### 
###

Go3tables <- function(all=FALSE)
{
    tables <- c("go_bp", "go_cc", "go_mf")
    if (all)
        tables <- paste(tables, "_all", sep="")
    names(tables) <- c("BP", "CC", "MF")
    tables
}

makeGo3L2Rpath <- function(L2Rpath, table, ontology)
{
    l <- length(L2Rpath)
    L2Rpath[[l]]@table <- table
    L2Rpath[[l]]@tagCols[2] <- paste("'", ontology, "'", sep="")
    L2Rpath
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

createAnnDbMap <- function(seed, seed0)
{
    for (slot in names(seed0)) {
        if (is.null(seed[slot][[1]]))
            seed[[slot]] <- seed0[[slot]]
    }
    L2Rpath <- seed$L2Rpath
    seed$L2Rpath <- lapply(L2Rpath, function(brick) do.call("L2Rbrick", brick))
    do.call("new", seed)
}

createAnnDbMaps <- function(seeds, seed0, envir=NULL)
{
    if (is.null(envir))
        envir <- new.env(hash=TRUE, parent=emptyenv())
    for (seed in seeds)
        envir[[seed$objName]] <- createAnnDbMap(seed, seed0)
    envir
}

### 3 special maps that are not AnnDbMap objects (just named vectors).

createCHRLENGTHS <- function(conn)
{
    data <- dbGetTable(conn, "chrlengths")
    CHRLENGTHS <- data[["length"]]
    names(CHRLENGTHS) <- data[["chr"]]
    CHRLENGTHS
}

createREJECTORF <- function(conn)
{
    data <- dbGetTable(conn, "reject_orf")
    data[["systematic_name"]]
}

createMAPCOUNTS <- function(conn, prefix)
{
    data <- dbGetTable(conn, "qcdata", "WHERE map_name != 'TOTAL' ORDER BY map_name")
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

