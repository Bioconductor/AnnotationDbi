### =========================================================================
### An SQLite-based ann data package (AnnDbPkg) provides a set of pre-defined
### AnnObj objects that are created at load-time. This set depends only on
### the underlying db schema i.e. all the SQLite-based ann data packages that
### share the same underlying db schema will provide the same set of AnnObj
### objects.
###
### This file describes the set of AnnObj objects provided by any
### ARABIDOPSISCHIP_DB-based package i.e. any SQLite-based ann data package based
### on the ARABIDOPSISCHIP_DB schema.
### The createAnnObjs.ARABIDOPSISCHIP_DB() function is the main entry point for
### this file: it is called by any ARABIDOPSISCHIP_DB-based package at load-time.
### -------------------------------------------------------------------------

ARABIDOPSISCHIP_DB_L2Rbrick1 <- list(table="probes", Lcolname="probe_id", Rcolname="id")

### Mandatory fields: objName, Class and L2Rpath
ARABIDOPSISCHIP_DB_AnnDbMap_seeds <- list(
    list(
        objName="ACCNUM",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            ARABIDOPSISCHIP_DB_L2Rbrick1,
            list(
                table="genes",
                Lcolname="id",
                Rcolname="gene_id"
            )
        ),
        replace.multiple="multiple"
    ),
    list(
        objName="ARACYC",
        Class="AtomicAnnDbBimap",
        L2Rpath=list(
            ARABIDOPSISCHIP_DB_L2Rbrick1,
            list(
                table="aracyc",
                Lcolname="id",
                Rcolname="pathway_name"
            )
        )
    ),
    list(
        objName="CHR",
        Class="AtomicAnnDbBimap",
        L2Rpath=list(
            ARABIDOPSISCHIP_DB_L2Rbrick1,
            list(
                table="gene_info",
                Lcolname="id",
                Rcolname="chromosome"
            )
        )
    ),
    list(
        objName="ENZYME",
        Class="AtomicAnnDbBimap",
        L2Rpath=list(
            ARABIDOPSISCHIP_DB_L2Rbrick1,
            list(
                table="enzyme",
                Lcolname="id",
                Rcolname="ec_name"
            )
        )
    ),
    list(
        objName="GENENAME",
        Class="AtomicAnnDbBimap",
        L2Rpath=list(
            ARABIDOPSISCHIP_DB_L2Rbrick1,
            list(
                table="gene_info",
                Lcolname="id",
                Rcolname="gene_name"
            )
        )
    ),
    list(
        objName="MULTIHIT",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            ARABIDOPSISCHIP_DB_L2Rbrick1,
            list(
                table="genes",
                Lcolname="id",
                Rcolname="gene_id"
            )
        ),
        replace.single=as.character(NA)
    ),
    list(
        objName="PATH",
        Class="AtomicAnnDbBimap",
        L2Rpath=list(
            ARABIDOPSISCHIP_DB_L2Rbrick1,
            list(
                table="kegg",
                Lcolname="id",
                Rcolname="kegg_id"
            )
        )
    ),
    list(
        objName="PMID",
        Class="AtomicAnnDbBimap",
        L2Rpath=list(
            ARABIDOPSISCHIP_DB_L2Rbrick1,
            list(
                table="pubmed",
                Lcolname="id",
                Rcolname="pubmed_id"
            )
        )
    ),
    list(
        objName="SYMBOL",
        Class="AtomicAnnDbBimap",
        L2Rpath=list(
            ARABIDOPSISCHIP_DB_L2Rbrick1,
            list(
                table="gene_info",
                Lcolname="id",
                Rcolname="symbol"
            )
        )
    ),
    list(
        objName="CHRLOC",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            ARABIDOPSISCHIP_DB_L2Rbrick1,
            list(
                table="chromosome_locations",
                Lcolname="id",
                Rcolname="start_location",
                tagCols=c(Chromosome="{chromosome}")
            )
        ),
        rightColType="integer"
    ),
    list(
        objName="GO",
        Class="Go3AnnDbMap",
        L2Rpath=list(
            ARABIDOPSISCHIP_DB_L2Rbrick1,
            list(
                #table="go_term", # no rightmost table for a Go3AnnDbMap
                Lcolname="id",
                Rcolname="go_id",
                tagCols=c(Evidence="{evidence}", Ontology="NULL")
            )
        ),
        rightTables=Go3tables()
    )
)

createAnnObjs.ARABIDOPSISCHIP_DB <- function(prefix, objTarget, conn, datacache)
{
    ## AnnDbMap objects
    seed0 <- list(
        objTarget=objTarget,
        datacache=datacache,
        conn=conn
    )
    ann_objs <- createAnnDbMaps(ARABIDOPSISCHIP_DB_AnnDbMap_seeds, seed0)

    ## Reverse maps
    ann_objs$ENZYME2PROBE <- revmap(ann_objs$ENZYME, objName="ENZYME2PROBE")
    ann_objs$PATH2PROBE <- revmap(ann_objs$PATH, objName="PATH2PROBE")
    ann_objs$PMID2PROBE <- revmap(ann_objs$PMID, objName="PMID2PROBE")

    ## RevGo3AnnDbMap objects
    ann_objs$GO2PROBE <- revmap(ann_objs$GO, objName="GO2PROBE")
    map <- ann_objs$GO2PROBE; map@rightTables <- Go3tables(all=TRUE)
    ann_objs$GO2ALLPROBES <- map

    ## Aliases for AnnDbMap objects
    ann_objs$ENTREZID <- ann_objs$ACCNUM

    ## 1 special map that is not an AnnDbMap object (just a named integer vector)
    ann_objs$MAPCOUNTS <- createMAPCOUNTS(conn, prefix)

    ## Some pre-caching
    left.keys(ann_objs$GO)

    prefixAnnObjNames(ann_objs, prefix)
}

compareAnnDataIn2Pkgs.ARABIDOPSISCHIP_DB <- function(pkgname1, pkgname2, prefix, quick=FALSE, verbose=FALSE)
{
    direct_maps <- sapply(ARABIDOPSISCHIP_DB_AnnDbMap_seeds, function(x) x$objName)
    reverse_maps <- c(
        "ENZYME2PROBE",
        "PATH2PROBE",
        "PMID2PROBE",
        "GO2PROBE",
        "GO2ALLPROBES"
    )
    compareAnnDataIn2Pkgs(pkgname1, pkgname2, prefix, direct_maps, reverse_maps, quick, verbose)
}

