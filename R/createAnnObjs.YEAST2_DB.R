### =========================================================================
### An SQLite-based ann data package (AnnDbPkg) provides a set of pre-defined
### AnnObj objects that are created at load-time. This set depends only on
### the underlying db schema i.e. all the SQLite-based ann data packages that
### share the same underlying db schema will provide the same set of AnnObj
### objects.
###
### This file describes the set of AnnObj objects provided by any
### YEAST2_DB-based package i.e. any SQLite-based ann data package based
### on the YEAST2_DB schema.
### The createAnnObjs.YEAST2_DB() function is the main entry point for
### this file: it is called by any YEAST2_DB-based package at load-time.
### -------------------------------------------------------------------------


YEAST2_DB_L2Rbrick1 <- list(table="probes", Lcolname="probe_id", Rcolname="id")

### Mandatory fields: objName, Class and L2Rpath
YEAST2_DB_AnnDbMap_seeds <- list(
    list(
        objName="ALIAS",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            YEAST2_DB_L2Rbrick1,
            list(
                table="gene2alias",
                Lcolname="id",
                Rcolname="alias"
            )
        )
    ),
    list(
        objName="CHR",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            YEAST2_DB_L2Rbrick1,
            list(
                table="chromosome_features",
                Lcolname="id",
                Rcolname="chromosome"
            )
        )
    ),
    list(
        objName="CHRLOC",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            YEAST2_DB_L2Rbrick1,
            list(
                table="chromosome_features",
                Lcolname="id",
                Rcolname="start",
                tagCols=c(Chromosome="{chromosome}")
            )
        ),
        rightColType="integer"
    ),
    list(
        objName="DESCRIPTION",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            YEAST2_DB_L2Rbrick1,
            list(
                table="chromosome_features",
                Lcolname="id",
                Rcolname="feature_description"
            )
        )
    ),
    list(
        objName="ENZYME",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            YEAST2_DB_L2Rbrick1,
            list(
                table="ec",
                Lcolname="id",
                Rcolname="ec_number"
            )
        )
    ),
    list(
        objName="GENENAME",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            YEAST2_DB_L2Rbrick1,
            list(
                table="sgd",
                Lcolname="id",
                Rcolname="gene_name"
            )
        )
    ),
    list(
        objName="PATH",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            YEAST2_DB_L2Rbrick1,
            list(
                table="kegg",
                Lcolname="id",
                Rcolname="kegg_id"
            )
        )
    ),
    list(
        objName="PMID",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            YEAST2_DB_L2Rbrick1,
            list(
                table="pubmed",
                Lcolname="id",
                Rcolname="pubmed_id"
            )
        )
    ),
    list(
        objName="GO",
        Class="Go3AnnDbMap",
        L2Rpath=list(
            YEAST2_DB_L2Rbrick1,
            list(
                #table="go_term", # no rightmost table for a Go3AnnDbMap
                Lcolname="id",
                Rcolname="go_id",
                tagCols=c(Evidence="{evidence}", Ontology="NULL")
            )
        ),
        rightTables=Go3tables()
    ),
    list(
        objName="ORF",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            YEAST2_DB_L2Rbrick1,
            list(
                table="sgd",
                Lcolname="id",
                Rcolname="systematic_name"
            )
        )
    )
)

createAnnObjs.YEAST2_DB <- function(prefix, objTarget, conn, datacache)
{
    ## AnnDbMap objects
    seed0 <- list(
        objTarget=objTarget,
        datacache=datacache,
        conn=conn
    )
    ann_objs <- createAnnDbMaps(YEAST2_DB_AnnDbMap_seeds, seed0)

    ## RevAtomicAnnDbMap objects
    ann_objs$ENZYME2PROBE <- revmap(ann_objs$ENZYME, objName="ENZYME2PROBE")
    ann_objs$PATH2PROBE <- revmap(ann_objs$PATH, objName="PATH2PROBE")
    ann_objs$PMID2PROBE <- revmap(ann_objs$PMID, objName="PMID2PROBE")

    ## RevGo3AnnDbMap objects
    ann_objs$GO2PROBE <- revmap(ann_objs$GO, objName="GO2PROBE")
    map <- ann_objs$GO2PROBE; map@rightTables <- Go3tables(all=TRUE)
    ann_objs$GO2ALLPROBES <- map

    ## 2 special maps that are not AnnDbMap objects (just named integer vectors)
    ann_objs$CHRLENGTHS <- createCHRLENGTHS(conn)
    ann_objs$MAPCOUNTS <- createMAPCOUNTS(conn, prefix)

    ## Some pre-caching
    left.names(ann_objs$GO)

    prefixAnnObjNames(ann_objs, prefix)
}

compareAnnDataIn2Pkgs.YEAST2_DB <- function(pkgname1, pkgname2, prefix, quick=FALSE, verbose=FALSE)
{
    direct_maps <- sapply(YEAST2_DB_AnnDbMap_seeds, function(x) x$objName)
    reverse_maps <- c(
        "ENZYME2PROBE",
        "PATH2PROBE",
        "PMID2PROBE",
        "GO2PROBE",
        "GO2ALLPROBES"
    )
    compareAnnDataIn2Pkgs(pkgname1, pkgname2, prefix, direct_maps, reverse_maps, quick, verbose)
}

