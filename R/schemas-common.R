### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
###

createAnnObjs <- function(class, seeds, seed0)
{
    maps <- list()
    for (seed in seeds) {
        seed$Class <- class
        for (slot in names(seed0)) {
            if (is.null(seed[slot][[1]]))
                seed[[slot]] <- seed0[[slot]]
        }
        maps[[seed$objName]] <- do.call("new", seed)
    }
    maps
}

### 2 special maps that are not AnnMap objects (just named integer vectors).

createCHRLENGTHS <- function(conn)
{
    data <- getTable(conn, "chrlengths")
    CHRLENGTHS <- data[["length"]]
    names(CHRLENGTHS) <- data[["chr"]]
    CHRLENGTHS
}

createMAPCOUNTS <- function(conn, prefix)
{
    data <- getTable(conn, "qcdata", "map_name != 'TOTAL'")
    MAPCOUNTS <- data[["count"]]
    names(MAPCOUNTS) <- paste(prefix, data[["map_name"]], sep="")
    MAPCOUNTS
}

