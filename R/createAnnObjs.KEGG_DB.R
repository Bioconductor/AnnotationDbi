### =========================================================================
### An SQLite-based ann data package (AnnDbPkg) provides a set of pre-defined
### AnnObj objects that are created at load-time. This set depends only on
### the underlying db schema i.e. all the SQLite-based ann data packages that
### share the same underlying db schema will provide the same set of AnnObj
### objects.
###
### This file describes the set of AnnObj objects provided by any
### KEGG_DB-based package i.e. any SQLite-based ann data package based
### on the KEGG_DB schema.
### The createAnnObjs.KEGG_DB() function is the main entry point for
### this file: it is called by any KEGG_DB-based package at load-time.
### -------------------------------------------------------------------------


### Mandatory fields: objName, Class and L2Rchain
KEGG_DB_AnnDbBimap_seeds <- list(
    list(
        objName="PATHID2NAME",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="pathway2name",
                Lcolname="path_id",
                Rcolname="path_name"
            )
        )
    ),
    list(
        objName="PATHID2EXTID",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="pathway2gene",
                Lcolname="pathway_id",
                Rcolname="gene_id"
            )
        )
    ),
    list(
        objName="ENZYMEID2GO",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="ec2go",
                Lcolname="ec_no",
                Rcolname="go_id"
            )
        )
    )
)

createAnnObjs.KEGG_DB <- function(prefix, objTarget, dbconn, datacache)
{
    checkDBSCHEMA(dbconn, "KEGG_DB")

    ## AnnDbBimap objects
    seed0 <- list(
        objTarget=objTarget,
        datacache=datacache
    )
    ann_objs <- createAnnDbBimaps(KEGG_DB_AnnDbBimap_seeds, seed0)

    ## Reverse maps
    ann_objs$PATHNAME2ID <- revmap(ann_objs$PATHID2NAME, objName="PATHNAME2ID")
    ann_objs$EXTID2PATHID <- revmap(ann_objs$PATHID2EXTID, objName="EXTID2PATHID")
    ann_objs$GO2ENZYMEID <- revmap(ann_objs$ENZYMEID2GO, objName="GO2ENZYMEID")

    ## 1 special map that is not an AnnDbBimap object (just a named integer vector)
    ann_objs$MAPCOUNTS <- createMAPCOUNTS(dbconn, prefix)

    prefixAnnObjNames(ann_objs, prefix)
}

