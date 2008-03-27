### =========================================================================
### An SQLite-based ann data package (AnnDbPkg) provides a set of pre-defined
### AnnObj objects that are created at load-time. This set depends only on
### the underlying db schema i.e. all the SQLite-based ann data packages that
### share the same underlying db schema will provide the same set of AnnObj
### objects.
###
### This file describes the set of AnnObj objects provided by any
### MALARIA_DB-based package i.e. any SQLite-based ann data package based
### on the MALARIA_DB schema.
### The createAnnObjs.MALARIA_DB() function is the main entry point for
### this file: it is called by any MALARIA_DB-based package at load-time.
### -------------------------------------------------------------------------


MALARIA_DB_L2Rlink1 <- list(tablename="genes", Lcolname="gene_id", Rcolname="_id")
 
### Mandatory fields: objName, Class and L2Rchain
MALARIA_DB_AnnDbBimap_seeds <- list(
    list(
        objName="ALIAS2ORF",
        Class="AnnDbBimap",
        L2Rchain=list(
            MALARIA_DB_L2Rlink1,
            list(
                tablename="alias",
                Lcolname="_id",
                Rcolname="alias_symbol"
            )
        ),
        direction=-1L
    ),
    list(
        objName="ENZYME",
        Class="AnnDbBimap",
        L2Rchain=list(
            MALARIA_DB_L2Rlink1,
            list(
                tablename="ec",
                Lcolname="_id",
                Rcolname="ec_number"
            )
        )
    ),
    list(
        objName="GENENAME",
        Class="AnnDbBimap",
        L2Rchain=list(
            MALARIA_DB_L2Rlink1,
            list(
                tablename="gene_info",
                Lcolname="_id",
                Rcolname="gene_name"
            )
        )
    ),
    list(
        objName="PATH",
        Class="AnnDbBimap",
        L2Rchain=list(
            MALARIA_DB_L2Rlink1,
            list(
                tablename="kegg",
                Lcolname="_id",
                Rcolname="path_id"
            )
        )
    ),
    list(
        objName="SYMBOL",
        Class="AnnDbBimap",
        L2Rchain=list(
            MALARIA_DB_L2Rlink1,
            list(
                tablename="gene_info",
                Lcolname="_id",
                Rcolname="symbol"
            )
        )
    ),
    list(
        objName="GO",
        Class="Go3AnnDbBimap",
        L2Rchain=list(
            MALARIA_DB_L2Rlink1,
            list(
                #tablename="go_term", # no rightmost table for a Go3AnnDbBimap
                Lcolname="_id",
                tagname=c(Evidence="{evidence}"),
                Rcolname="go_id",
                Rattribnames=c(Ontology="NULL")
            )
        ),
        rightTables=Go3tablenames()
    )
)

createAnnObjs.MALARIA_DB <- function(prefix, objTarget, dbconn, datacache)
{
    checkDBSCHEMA(dbconn, "MALARIA_DB")

    ## AnnDbBimap objects
    seed0 <- list(
        objTarget=objTarget,
        datacache=datacache
    )
    ann_objs <- createAnnDbBimaps(MALARIA_DB_AnnDbBimap_seeds, seed0)

    ## Reverse maps
    ann_objs$ENZYME2ORF <- revmap(ann_objs$ENZYME, objName="ENZYME2ORF")
    ann_objs$PATH2ORF <- revmap(ann_objs$PATH, objName="PATH2ORF")
    ann_objs$SYMBOL2ORF <- revmap(ann_objs$SYMBOL, objName="SYMBOL2ORF")
    ann_objs$GO2ORF <- revmap(ann_objs$GO, objName="GO2ORF")
    map <- ann_objs$GO2ORF
    map@rightTables <- Go3tablenames(all=TRUE)
    map@objName <- "GO2ALLORFS"
    ann_objs$GO2ALLORFS <- map

    ## 2 special maps that are not AnnDbBimap objects (just named integer vectors)
    ann_objs$MAPCOUNTS <- createMAPCOUNTS(dbconn, prefix)

    ## Some pre-caching
    Lkeys(ann_objs$GO)
    #mappedLkeys(ann_objs$GO)
    #Rkeys(ann_objs$GO2ORF)
    #mappedRkeys(ann_objs$GO2ORF)

    prefixAnnObjNames(ann_objs, prefix)
}

