### =========================================================================
### Create all data objects for an annotation data package
### with db schema LLMAPPINGSDB
### -------------------------------------------------------------------------

### TODO: Everything!

LLMAPPINGSDB_default_leftCol <- "gene_id"
LLMAPPINGSDB_default_rightColType <- character(0)

### Mandatory fields: objName, rightTable and rightCol
LLMAPPINGSDB_AtomicAnnMap_seeds <- list(
)

createAnnObjects.LLMAPPINGSDB <- function(prefix, objTarget, conn, datacache)
{
    ## AtomicAnnMap objects
    seed0 <- list(
        objTarget=objTarget,
        conn=conn,
        datacache=datacache,
        rightColType=LLMAPPINGSDB_default_rightColType,
        leftCol=LLMAPPINGSDB_default_leftCol
    )
    maps <- createAtomicAnnMapObjects(LLMAPPINGSDB_AtomicAnnMap_seeds, seed0)

    ## ReverseAtomicAnnMap objects

    ## GOAnnMap object

    ## ReverseGOAnnMap objects

    ## The MAPCOUNTS object (named integer vector)
    #maps$MAPCOUNTS <- createMAPCOUNTS(conn, prefix)

    names(maps) <- paste(prefix, names(maps), sep="")
    maps
}

compareAnnDataIn2Pkgs.LLMAPPINGSDB <- function(pkgname1, pkgname2, prefix, probes=NULL, verbose=FALSE)
{
    direct_maps <- sapply(LLMAPPINGSDB_AtomicAnnMap_seeds, function(x) x$objName)
    direct_maps <- c(direct_maps, "GO")
    reverse_maps <- c(
    )
    compareAnnDataIn2Pkgs(pkgname1, pkgname2, direct_maps, reverse_maps, prefix, probes, verbose)
}

