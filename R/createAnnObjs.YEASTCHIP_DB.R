### =========================================================================
### An SQLite-based ann data package (AnnDbPkg) provides a set of pre-defined
### AnnObj objects that are created at load-time. This set depends only on
### the underlying db schema i.e. all the SQLite-based ann data packages that
### share the same underlying db schema will provide the same set of AnnObj
### objects.
###
### This file describes the set of AnnObj objects provided by any
### YEASTCHIP_DB-based package i.e. any SQLite-based ann data package based
### on the YEASTCHIP_DB schema.
### The createAnnObjs.YEASTCHIP_DB() function is the main entry point for
### this file: it is called by any YEASTCHIP_DB-based package at load-time.
### -------------------------------------------------------------------------


YEASTCHIP_DB_L2Rlink1 <- list(tablename="probes", Lcolname="probe_id", Rcolname="id")

### Mandatory fields: objName, Class and L2Rchain
YEASTCHIP_DB_AnnDbBimap_seeds <- list(
    list(
        objName="ALIAS",
        Class="AnnDbBimap",
        L2Rchain=list(
            YEASTCHIP_DB_L2Rlink1,
            list(
                tablename="gene2alias",
                Lcolname="id",
                Rcolname="alias"
            )
        )
    ),
    list(
        objName="CHR",
        Class="AnnDbBimap",
        L2Rchain=list(
            YEASTCHIP_DB_L2Rlink1,
            list(
                tablename="chromosome_features",
                Lcolname="id",
                Rcolname="chromosome"
            )
        )
    ),
    list(
        objName="CHRLOC",
        Class="AnnDbMap",
        L2Rchain=list(
            YEASTCHIP_DB_L2Rlink1,
            list(
                tablename="chromosome_features",
                Lcolname="id",
                tagname=c(Chromosome="{chromosome}"),
                Rcolname="start"
            )
        ),
        rightColType="integer"
    ),
    list(
        objName="DESCRIPTION",
        Class="AnnDbBimap",
        L2Rchain=list(
            YEASTCHIP_DB_L2Rlink1,
            list(
                tablename="chromosome_features",
                Lcolname="id",
                Rcolname="feature_description"
            )
        )
    ),
    list(
        objName="ENZYME",
        Class="AnnDbBimap",
        L2Rchain=list(
            YEASTCHIP_DB_L2Rlink1,
            list(
                tablename="ec",
                Lcolname="id",
                Rcolname="ec_number"
            )
        )
    ),
    list(
        objName="GENENAME",
        Class="AnnDbBimap",
        L2Rchain=list(
            YEASTCHIP_DB_L2Rlink1,
            list(
                tablename="sgd",
                Lcolname="id",
                Rcolname="gene_name"
            )
        )
    ),
    list(
        objName="PATH",
        Class="AnnDbBimap",
        L2Rchain=list(
            YEASTCHIP_DB_L2Rlink1,
            list(
                tablename="kegg",
                Lcolname="id",
                Rcolname="kegg_id"
            )
        )
    ),
    list(
        objName="PMID",
        Class="AnnDbBimap",
        L2Rchain=list(
            YEASTCHIP_DB_L2Rlink1,
            list(
                tablename="pubmed",
                Lcolname="id",
                Rcolname="pubmed_id"
            )
        )
    ),
    list(
        objName="GO",
        Class="Go3AnnDbBimap",
        L2Rchain=list(
            YEASTCHIP_DB_L2Rlink1,
            list(
                #tablename="go_term", # no rightmost table for a Go3AnnDbBimap
                Lcolname="id",
                tagname=c(Evidence="{evidence}"),
                Rcolname="go_id",
                Rattribnames=c(Ontology="NULL")
            )
        ),
        rightTables=Go3tablenames()
    ),
    list(
        objName="ORF",
        Class="AnnDbBimap",
        L2Rchain=list(
            YEASTCHIP_DB_L2Rlink1,
            list(
                tablename="sgd",
                Lcolname="id",
                Rcolname="systematic_name"
            )
        )
    )
)

createAnnObjs.YEASTCHIP_DB <- function(prefix, objTarget, dbconn, datacache)
{
    checkDBSCHEMA(dbconn, "YEASTCHIP_DB")

    ## AnnDbBimap objects
    seed0 <- list(
        objTarget=objTarget,
        datacache=datacache
    )
    ann_objs <- createAnnDbBimaps(YEASTCHIP_DB_AnnDbBimap_seeds, seed0)

    ## Reverse maps
    ann_objs$ENZYME2PROBE <- revmap(ann_objs$ENZYME, objName="ENZYME2PROBE")
    ann_objs$PATH2PROBE <- revmap(ann_objs$PATH, objName="PATH2PROBE")
    ann_objs$PMID2PROBE <- revmap(ann_objs$PMID, objName="PMID2PROBE")
    ann_objs$GO2PROBE <- revmap(ann_objs$GO, objName="GO2PROBE")
    map <- ann_objs$GO2PROBE
    map@rightTables <- Go3tablenames(all=TRUE)
    map@objName <- "GO2ALLPROBES"
    ann_objs$GO2ALLPROBES <- map

    ## 2 special maps that are not AnnDbBimap objects (just named integer vectors)
    ann_objs$CHRLENGTHS <- createCHRLENGTHS(dbconn)
    ann_objs$MAPCOUNTS <- createMAPCOUNTS(dbconn, prefix)

    ## Some pre-caching
    Lkeys(ann_objs$GO)

    prefixAnnObjNames(ann_objs, prefix)
}

