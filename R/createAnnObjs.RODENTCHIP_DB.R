### =========================================================================
### An SQLite-based ann data package (AnnDbPkg) provides a set of pre-defined
### AnnObj objects that are created at load-time. This set depends only on
### the underlying db schema i.e. all the SQLite-based ann data packages that
### share the same underlying db schema will provide the same set of AnnObj
### objects.
###
### This file describes the set of AnnObj objects provided by any
### RODENTCHIP_DB-based package i.e. any SQLite-based ann data package based
### on the RODENTCHIP_DB schema.
### The createAnnObjs.RODENTCHIP_DB() function is the main entry point for
### this file: it is called by any RODENTCHIP_DB-based package at load-time.
### -------------------------------------------------------------------------


RODENTCHIP_DB_L2Rlink1 <- list(tablename="probes", Lkeyname="probe_id", Rkeyname="id")

### Mandatory fields: objName, Class and L2Rchain
RODENTCHIP_DB_AnnDbBimap_seeds <- list(
    list(
        objName="ACCNUM",
        Class="AtomicAnnDbBimap",
        L2Rchain=list(
            list(
                tablename="probes",
                Lkeyname="probe_id",
                Rkeyname="accession"
            )
        )
    ),
    list(
        objName="ALIAS2PROBE",
        Class="AtomicAnnDbBimap",
        L2Rchain=list(
            RODENTCHIP_DB_L2Rlink1,
            list(
                tablename="alias",
                Lkeyname="id",
                Rkeyname="alias_symbol"
            )
        ),
        direction=-1L
    ),
    list(
        objName="CHR",
        Class="AtomicAnnDbBimap",
        L2Rchain=list(
            RODENTCHIP_DB_L2Rlink1,
            list(
                tablename="chromosomes",
                Lkeyname="id",
                Rkeyname="chromosome"
            )
        )
    ),
    list(
        objName="ENTREZID",
        Class="AtomicAnnDbBimap",
        L2Rchain=list(
            RODENTCHIP_DB_L2Rlink1,
            list(
                tablename="genes",
                Lkeyname="id",
                Rkeyname="gene_id"
            )
        )
    ),
    list(
        objName="ENZYME",
        Class="AtomicAnnDbBimap",
        L2Rchain=list(
            RODENTCHIP_DB_L2Rlink1,
            list(
                tablename="ec",
                Lkeyname="id",
                Rkeyname="ec_number"
            )
        )
    ),
    list(
        objName="GENENAME",
        Class="AtomicAnnDbBimap",
        L2Rchain=list(
            RODENTCHIP_DB_L2Rlink1,
            list(
                tablename="gene_info",
                Lkeyname="id",
                Rkeyname="gene_name"
            )
        )
    ),
    list(
        objName="MAP",
        Class="AtomicAnnDbBimap",
        L2Rchain=list(
            RODENTCHIP_DB_L2Rlink1,
            list(
                tablename="cytogenetic_locations",
                Lkeyname="id",
                Rkeyname="cytogenetic_location"
            )
        )
    ),
    list(
        objName="PATH",
        Class="AtomicAnnDbBimap",
        L2Rchain=list(
            RODENTCHIP_DB_L2Rlink1,
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
            RODENTCHIP_DB_L2Rlink1,
            list(
                tablename="pubmed",
                Lkeyname="id",
                Rkeyname="pubmed_id"
            )
        )
    ),
    list(
        objName="REFSEQ",
        Class="AtomicAnnDbBimap",
        L2Rchain=list(
            RODENTCHIP_DB_L2Rlink1,
            list(
                tablename="refseq",
                Lkeyname="id",
                Rkeyname="accession"
            )
        )
    ),
    list(
        objName="SYMBOL",
        Class="AtomicAnnDbBimap",
        L2Rchain=list(
            RODENTCHIP_DB_L2Rlink1,
            list(
                tablename="gene_info",
                Lkeyname="id",
                Rkeyname="symbol"
            )
        )
    ),
    list(
        objName="UNIGENE",
        Class="AtomicAnnDbBimap",
        L2Rchain=list(
            RODENTCHIP_DB_L2Rlink1,
            list(
                tablename="unigene",
                Lkeyname="id",
                Rkeyname="unigene_id"
            )
        )
    ),
    list(
        objName="CHRLOC",
        Class="AnnDbMap",
        L2Rchain=list(
            RODENTCHIP_DB_L2Rlink1,
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
        objName="PFAM",
        Class="IpiAnnDbMap",
        L2Rchain=list(
            RODENTCHIP_DB_L2Rlink1,
            list(
                tablename="pfam",
                Lkeyname="id",
                Rkeyname="ipi_id",
                Rattribnames=c(PfamId="{pfam_id}")
            )
        )
    ),
    list(
        objName="PROSITE",
        Class="IpiAnnDbMap",
        L2Rchain=list(
            RODENTCHIP_DB_L2Rlink1,
            list(
                tablename="prosite",
                Lkeyname="id",
                Rkeyname="ipi_id",
                Rattribnames=c(PrositeId="{prosite_id}")
            )
        )
    ),
    list(
        objName="GO",
        Class="Go3AnnDbBimap",
        L2Rchain=list(
            RODENTCHIP_DB_L2Rlink1,
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

createAnnObjs.RODENTCHIP_DB <- function(prefix, objTarget, conn, datacache)
{
    ## AnnDbBimap objects
    seed0 <- list(
        objTarget=objTarget,
        datacache=datacache,
        conn=conn
    )
    ann_objs <- createAnnDbBimaps(RODENTCHIP_DB_AnnDbBimap_seeds, seed0)

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

compareAnnDataIn2Pkgs.RODENTCHIP_DB <- function(pkgname1, pkgname2, prefix, quick=FALSE, verbose=FALSE)
{
    direct_maps <- sapply(RODENTCHIP_DB_AnnDbBimap_seeds, function(x) x$objName)
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

