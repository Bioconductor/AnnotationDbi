### =========================================================================
### Create all data objects for an annotation data package
### with db schema LLMAPPINGS_DB
### -------------------------------------------------------------------------

### TODO: Everything!

LLMAPPINGS_DB_default_leftCol <- "gene_id"
LLMAPPINGS_DB_default_rightColType <- character(0)

### Mandatory fields: objName, rightTable and rightCol
LLMAPPINGS_DB_AtomicAnnMap_seeds <- list(
)

createAnnObjects.LLMAPPINGS_DB <- function(prefix, objTarget, conn, datacache)
{
    ## AtomicAnnMap objects
    seed0 <- list(
        objTarget=objTarget,
        conn=conn,
        datacache=datacache,
        rightColType=LLMAPPINGS_DB_default_rightColType,
        leftCol=LLMAPPINGS_DB_default_leftCol
    )
    maps <- createAtomicAnnMapObjects(LLMAPPINGS_DB_AtomicAnnMap_seeds, seed0)

    ## ReverseAtomicAnnMap objects

    ## GOAnnMap object

    ## ReverseGOAnnMap objects

    ## The MAPCOUNTS object (named integer vector)
    #maps$MAPCOUNTS <- createMAPCOUNTS(conn, prefix)

    names(maps) <- paste(prefix, names(maps), sep="")
    maps
}

compareAnnDataIn2Pkgs.LLMAPPINGS_DB <- function(pkgname1, pkgname2, prefix, probes=NULL, verbose=FALSE)
{
    direct_maps <- sapply(LLMAPPINGS_DB_AtomicAnnMap_seeds, function(x) x$objName)
    direct_maps <- c(direct_maps, "GO")
    reverse_maps <- c(
    )
    compareAnnDataIn2Pkgs(pkgname1, pkgname2, direct_maps, reverse_maps, prefix, probes, verbose)
}

