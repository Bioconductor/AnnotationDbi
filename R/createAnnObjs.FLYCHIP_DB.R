### =========================================================================
### An SQLite-based ann data package (AnnDbPkg) provides a set of pre-defined
### AnnObj objects that are created at load-time. This set depends only on
### the underlying db schema i.e. all the SQLite-based ann data packages that
### share the same underlying db schema will provide the same set of AnnObj
### objects.
###
### This file describes the set of AnnObj objects provided by any
### FLYCHIP_DB-based package i.e. any SQLite-based ann data package based
### on the FLYCHIP_DB schema.
### The createAnnObjs.FLYCHIP_DB() function is the main entry point for
### this file: it is called by any FLYCHIP_DB-based package at load-time.
### -------------------------------------------------------------------------


FLYCHIP_DB_L2Rbrick1 <- list(tablename="probes", Lcolname="probe_id", Rcolname="id")

### Mandatory fields: objName, Class and L2Rpath
FLYCHIP_DB_AnnDbBimap_seeds <- list(
    list(
        objName="ACCNUM",
        Class="AtomicAnnDbBimap",
        L2Rpath=list(
            list(
                tablename="probes",
                Lcolname="probe_id",
                Rcolname="accession"
            )
        )
    ),
    list(
        objName="ALIAS2PROBE",
        Class="AtomicAnnDbBimap",
        L2Rpath=list(
            FLYCHIP_DB_L2Rbrick1,
            list(
                tablename="alias",
                Lcolname="id",
                Rcolname="alias_symbol"
            )
        ),
        direction=-1L
    ),
    list(
        objName="CHR",
        Class="AtomicAnnDbBimap",
        L2Rpath=list(
            FLYCHIP_DB_L2Rbrick1,
            list(
                tablename="chromosomes",
                Lcolname="id",
                Rcolname="chromosome"
            )
        )
    ),
    list(
        objName="ENTREZID",
        Class="AtomicAnnDbBimap",
        L2Rpath=list(
            FLYCHIP_DB_L2Rbrick1,
            list(
                tablename="genes",
                Lcolname="id",
                Rcolname="gene_id"
            )
        )
    ),
    list(
        objName="ENZYME",
        Class="AtomicAnnDbBimap",
        L2Rpath=list(
            FLYCHIP_DB_L2Rbrick1,
            list(
                tablename="ec",
                Lcolname="id",
                Rcolname="ec_number"
            )
        )
    ),
    list(
        objName="GENENAME",
        Class="AtomicAnnDbBimap",
        L2Rpath=list(
            FLYCHIP_DB_L2Rbrick1,
            list(
                tablename="gene_info",
                Lcolname="id",
                Rcolname="gene_name"
            )
        )
    ),
    list(
        objName="MAP",
        Class="AtomicAnnDbBimap",
        L2Rpath=list(
            FLYCHIP_DB_L2Rbrick1,
            list(
                tablename="cytogenetic_locations",
                Lcolname="id",
                Rcolname="cytogenetic_location"
            )
        )
    ),
    list(
        objName="PATH",
        Class="AtomicAnnDbBimap",
        L2Rpath=list(
            FLYCHIP_DB_L2Rbrick1,
            list(
                tablename="kegg",
                Lcolname="id",
                Rcolname="kegg_id"
            )
        )
    ),
    list(
        objName="PMID",
        Class="AtomicAnnDbBimap",
        L2Rpath=list(
            FLYCHIP_DB_L2Rbrick1,
            list(
                tablename="pubmed",
                Lcolname="id",
                Rcolname="pubmed_id"
            )
        )
    ),
    list(
        objName="REFSEQ",
        Class="AtomicAnnDbBimap",
        L2Rpath=list(
            FLYCHIP_DB_L2Rbrick1,
            list(
                tablename="refseq",
                Lcolname="id",
                Rcolname="accession"
            )
        )
    ),
    list(
        objName="SYMBOL",
        Class="AtomicAnnDbBimap",
        L2Rpath=list(
            FLYCHIP_DB_L2Rbrick1,
            list(
                tablename="gene_info",
                Lcolname="id",
                Rcolname="symbol"
            )
        )
    ),
    list(
        objName="UNIGENE",
        Class="AtomicAnnDbBimap",
        L2Rpath=list(
            FLYCHIP_DB_L2Rbrick1,
            list(
                tablename="unigene",
                Lcolname="id",
                Rcolname="unigene_id"
            )
        )
    ),
    list(
        objName="CHRLOC",
        Class="AnnDbMap",
        L2Rpath=list(
            FLYCHIP_DB_L2Rbrick1,
            list(
                tablename="chromosome_locations",
                Lcolname="id",
                Rcolname="start_location",
                tagCols=c(Chromosome="{chromosome}")
            )
        ),
        rightColType="integer"
    ),
    list(
        objName="FLYBASE",
        Class="AtomicAnnDbBimap",
        L2Rpath=list(
            FLYCHIP_DB_L2Rbrick1,
            list(
                tablename="flybase",
                Lcolname="id",
                Rcolname="FB_id"
            )
        )
    ),
    list(
        objName="GO",
        Class="Go3AnnDbBimap",
        L2Rpath=list(
            FLYCHIP_DB_L2Rbrick1,
            list(
                #tablename="go_term", # no rightmost table for a Go3AnnDbBimap
                Lcolname="id",
                Rcolname="go_id",
                tagCols=c(Evidence="{evidence}", Ontology="NULL")
            )
        ),
        rightTables=Go3tablenames()
    )
)

createAnnObjs.FLYCHIP_DB <- function(prefix, objTarget, conn, datacache)
{
    ## AnnDbBimap objects
    seed0 <- list(
        objTarget=objTarget,
        datacache=datacache,
        conn=conn
    )
    ann_objs <- createAnnDbBimaps(FLYCHIP_DB_AnnDbBimap_seeds, seed0)

    ## Reverse maps
    ann_objs$ENZYME2PROBE <- revmap(ann_objs$ENZYME, objName="ENZYME2PROBE")
    ann_objs$PATH2PROBE <- revmap(ann_objs$PATH, objName="PATH2PROBE")
    ann_objs$PMID2PROBE <- revmap(ann_objs$PMID, objName="PMID2PROBE")
    ann_objs$GO2PROBE <- revmap(ann_objs$GO, objName="GO2PROBE")
    map <- ann_objs$GO2PROBE; map@rightTables <- Go3tablenames(all=TRUE)
    ann_objs$GO2ALLPROBES <- map

    ## 2 special maps that are not AnnDbBimap objects (just named integer vectors)
    ann_objs$CHRLENGTHS <- createCHRLENGTHS(conn)
    ann_objs$MAPCOUNTS <- createMAPCOUNTS(conn, prefix)

    ## Some pre-caching
    Lkeys(ann_objs$GO)
    #mappedLkeys(ann_objs$GO)
    #Rkeys(ann_objs$GO2PROBE)
    #mappedRkeys(ann_objs$GO2PROBE)
    #Rkeys(ann_objs$GO2ALLPROBES)
    #mappedRkeys(ann_objs$GO2ALLPROBES)

    prefixAnnObjNames(ann_objs, prefix)
}

compareAnnDataIn2Pkgs.FLYCHIP_DB <- function(pkgname1, pkgname2, prefix, quick=FALSE, verbose=FALSE)
{
    direct_maps <- sapply(FLYCHIP_DB_AnnDbBimap_seeds, function(x) x$objName)
    reverse_maps <- c(
        "ALIAS2PROBE",
        "ENZYME2PROBE",
        "PATH2PROBE",
        "PMID2PROBE",
        "GO2PROBE",
        "GO2ALLPROBES"
    )
    compareAnnDataIn2Pkgs(pkgname1, pkgname2, prefix, direct_maps, reverse_maps, quick, verbose)
}

