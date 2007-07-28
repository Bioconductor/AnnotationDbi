### =========================================================================
### Create all data objects for an annotation data package
### with db schema RODENT_DB
### -------------------------------------------------------------------------

### TODO: Everything!

RODENT_DB_default_leftCol <- "gene_id"
RODENT_DB_default_rightColType <- character(0)

### Mandatory fields: objName, rightTable and rightCol
RODENT_DB_AtomicAnnDbMap_seeds <- list(
)

createAnnObjs.RODENT_DB <- function(prefix, objTarget, conn, datacache)
{
    ## AtomicAnnDbMap objects
    seed0 <- list(
        objTarget=objTarget,
        conn=conn,
        datacache=datacache,
        rightColType=RODENT_DB_default_rightColType,
        leftCol=RODENT_DB_default_leftCol
    )
    ann_objs <- createAnnObjs("AtomicAnnDbMap", RODENT_DB_AtomicAnnDbMap_seeds, seed0)

    ## RevAtomicAnnDbMap objects

    ## Go3AnnDbMap object

    ## RevGo3AnnDbMap objects

    ## The MAPCOUNTS object (named integer vector)
    #ann_objs$MAPCOUNTS <- createMAPCOUNTS(conn, prefix)

    prefixAnnObjNames(ann_objs, prefix)
}

compareAnnDataIn2Pkgs.RODENT_DB <- function(pkgname1, pkgname2, prefix, quick=FALSE, verbose=FALSE)
{
    direct_maps <- sapply(RODENT_DB_AtomicAnnDbMap_seeds, function(x) x$objName)
    direct_maps <- c(direct_maps, "GO")
    reverse_maps <- c(
    )
    compareAnnDataIn2Pkgs(pkgname1, pkgname2, prefix, direct_maps, reverse_maps, quick, verbose)
}

