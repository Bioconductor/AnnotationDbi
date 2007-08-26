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

ARABIDOPSISCHIP_DB_L2Rlink1 <- list(tablename="probes", Lkeyname="probe_id", Rkeyname="id")

### Mandatory fields: objName, Class and L2Rchain
ARABIDOPSISCHIP_DB_AnnDbBimap_seeds <- list(
    list(
        objName="ACCNUM",
        Class="AgiAnnDbMap",
        L2Rchain=list(
            ARABIDOPSISCHIP_DB_L2Rlink1,
            list(
                tablename="genes",
                Lkeyname="id",
                Rkeyname="gene_id"
            )
        ),
        replace.multiple="multiple"
    ),
    list(
        objName="ARACYC",
        Class="AtomicAnnDbBimap",
        L2Rchain=list(
            ARABIDOPSISCHIP_DB_L2Rlink1,
            list(
                tablename="aracyc",
                Lkeyname="id",
                Rkeyname="pathway_name"
            )
        )
    ),
    list(
        objName="CHR",
        Class="AtomicAnnDbBimap",
        L2Rchain=list(
            ARABIDOPSISCHIP_DB_L2Rlink1,
            list(
                tablename="gene_info",
                Lkeyname="id",
                Rkeyname="chromosome"
            )
        )
    ),
    list(
        objName="ENZYME",
        Class="AtomicAnnDbBimap",
        L2Rchain=list(
            ARABIDOPSISCHIP_DB_L2Rlink1,
            list(
                tablename="enzyme",
                Lkeyname="id",
                Rkeyname="ec_name"
            )
        )
    ),
    list(
        objName="GENENAME",
        Class="AtomicAnnDbBimap",
        L2Rchain=list(
            ARABIDOPSISCHIP_DB_L2Rlink1,
            list(
                tablename="gene_info",
                Lkeyname="id",
                Rkeyname="gene_name"
            )
        )
    ),
    list(
        objName="MULTIHIT",
        Class="AgiAnnDbMap",
        L2Rchain=list(
            ARABIDOPSISCHIP_DB_L2Rlink1,
            list(
                tablename="genes",
                Lkeyname="id",
                Rkeyname="gene_id"
            )
        ),
        replace.single=as.character(NA)
    ),
    list(
        objName="PATH",
        Class="AtomicAnnDbBimap",
        L2Rchain=list(
            ARABIDOPSISCHIP_DB_L2Rlink1,
            list(
                tablename="kegg",
                Lkeyname="id",
                Rkeyname="kegg_id"
            )
        )
    ),
    list(
        objName="PMID",
        Class="AtomicAnnDbBimap",
        L2Rchain=list(
            ARABIDOPSISCHIP_DB_L2Rlink1,
            list(
                tablename="pubmed",
                Lkeyname="id",
                Rkeyname="pubmed_id"
            )
        )
    ),
    list(
        objName="SYMBOL",
        Class="AtomicAnnDbBimap",
        L2Rchain=list(
            ARABIDOPSISCHIP_DB_L2Rlink1,
            list(
                tablename="gene_info",
                Lkeyname="id",
                Rkeyname="symbol"
            )
        )
    ),
    list(
        objName="CHRLOC",
        Class="AnnDbMap",
        L2Rchain=list(
            ARABIDOPSISCHIP_DB_L2Rlink1,
            list(
                tablename="chromosome_locations",
                Lkeyname="id",
                tagname=c(Chromosome="{chromosome}"),
                Rkeyname="start_location"
            )
        ),
        rightColType="integer"
    ),
    list(
        objName="GO",
        Class="Go3AnnDbBimap",
        L2Rchain=list(
            ARABIDOPSISCHIP_DB_L2Rlink1,
            list(
                #tablename="go_term", # no rightmost table for a Go3AnnDbBimap
                Lkeyname="id",
                tagname=c(Evidence="{evidence}"),
                Rkeyname="go_id",
                Rattribnames=c(Ontology="NULL")
            )
        ),
        rightTables=Go3tablenames()
    )
)

createAnnObjs.ARABIDOPSISCHIP_DB <- function(prefix, objTarget, conn, datacache)
{
    ## AnnDbBimap objects
    seed0 <- list(
        objTarget=objTarget,
        datacache=datacache,
        conn=conn
    )
    ann_objs <- createAnnDbBimaps(ARABIDOPSISCHIP_DB_AnnDbBimap_seeds, seed0)

    ## Reverse maps
    ann_objs$ENZYME2PROBE <- revmap(ann_objs$ENZYME, objName="ENZYME2PROBE")
    ann_objs$PATH2PROBE <- revmap(ann_objs$PATH, objName="PATH2PROBE")
    ann_objs$PMID2PROBE <- revmap(ann_objs$PMID, objName="PMID2PROBE")
    ann_objs$GO2PROBE <- revmap(ann_objs$GO, objName="GO2PROBE")
    map <- ann_objs$GO2PROBE
    map@rightTables <- Go3tablenames(all=TRUE)
    map@objName <- "GO2ALLPROBES"
    ann_objs$GO2ALLPROBES <- map

    ## Some map aliases
    ann_objs$ENTREZID <- ann_objs$ACCNUM

    ## 1 special map that is not an AnnDbBimap object (just a named integer vector)
    ann_objs$MAPCOUNTS <- createMAPCOUNTS(conn, prefix)

    ## Some pre-caching
    Lkeys(ann_objs$GO)

    prefixAnnObjNames(ann_objs, prefix)
}

compareAnnDataIn2Pkgs.ARABIDOPSISCHIP_DB <- function(pkgname1, pkgname2, prefix, quick=FALSE, verbose=FALSE)
{
    direct_maps <- sapply(ARABIDOPSISCHIP_DB_AnnDbBimap_seeds, function(x) x$objName)
    reverse_maps <- c(
        "ENZYME2PROBE",
        "PATH2PROBE",
        "PMID2PROBE",
        "GO2PROBE",
        "GO2ALLPROBES"
    )
    compareAnnDataIn2Pkgs(pkgname1, pkgname2, prefix, direct_maps, reverse_maps, quick, verbose)
}

