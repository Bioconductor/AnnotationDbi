### =========================================================================
### Create all data objects for an annotation data package
### with db schema HUMAN_DB
### -------------------------------------------------------------------------

### TODO: Everything!

HUMAN_DB_default_leftCol <- "gene_id"
HUMAN_DB_default_rightColType <- character(0)

### Mandatory fields: objName, rightTable and rightCol
HUMAN_DB_AtomicAnnDbMap_seeds <- list(
)

createAnnObjs.HUMAN_DB <- function(prefix, objTarget, conn, datacache)
{
    ## AtomicAnnDbMap objects
    seed0 <- list(
        objTarget=objTarget,
        conn=conn,
        datacache=datacache,
        rightColType=HUMAN_DB_default_rightColType,
        leftCol=HUMAN_DB_default_leftCol
    )
    ann_objs <- createAnnObjs("AtomicAnnDbMap", HUMAN_DB_AtomicAnnDbMap_seeds, seed0)

    ## RevAtomicAnnDbMap objects

    ## Go3AnnDbMap object

    ## RevGo3AnnDbMap objects

    ## The MAPCOUNTS object (named integer vector)
    #ann_objs$MAPCOUNTS <- createMAPCOUNTS(conn, prefix)

    prefixAnnObjNames(ann_objs, prefix)
}

compareAnnDataIn2Pkgs.HUMAN_DB <- function(pkgname1, pkgname2, prefix, quick=FALSE, verbose=FALSE)
{
    direct_maps <- sapply(HUMAN_DB_AtomicAnnDbMap_seeds, function(x) x$objName)
    direct_maps <- c(direct_maps, "GO")
    reverse_maps <- c(
    )
    compareAnnDataIn2Pkgs(pkgname1, pkgname2, prefix, direct_maps, reverse_maps, quick, verbose)
}

