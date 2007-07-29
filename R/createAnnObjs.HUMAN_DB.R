### =========================================================================
### An SQLite-based ann data package (AnnDbPkg) provides a set of pre-defined
### AnnObj objects that are created at load-time. This set depends only on
### the underlying db schema i.e. all the SQLite-based ann data packages that
### share the same underlying db schema will provide the same set of AnnObj
### objects.
###
### This file describes the set of AnnObj objects provided by any
### HUMAN_DB-based package i.e. any SQLite-based ann data package based
### on the HUMAN_DB schema.
### The createAnnObjs.HUMAN_DB() function is the main entry point for
### this file: it is called by any HUMAN_DB-based package at load-time.
### -------------------------------------------------------------------------


HUMAN_DB_L2Rbrick1 <- list(table="genes", Lcolname="gene_id", Rcolname="id")

### Mandatory fields: objName, Class and L2Rpath
HUMAN_DB_AnnDbMap_seeds <- list(
    list(
        objName="ACCNUM",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            HUMAN_DB_L2Rbrick1,
            list(
                table="accessions",
                Lcolname="id",
                Rcolname="accession"
            )
        )
    ),
    list(
        objName="CHR",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            HUMAN_DB_L2Rbrick1,
            list(
                table="chromosomes",
                Lcolname="id",
                Rcolname="chromosome"
            )
        )
    ),
    list(
        objName="ENZYME",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            HUMAN_DB_L2Rbrick1,
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
            HUMAN_DB_L2Rbrick1,
            list(
                table="gene_info",
                Lcolname="id",
                Rcolname="gene_name"
            )
        )
    ),
    list(
        objName="MAP",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            HUMAN_DB_L2Rbrick1,
            list(
                table="cytogenetic_locations",
                Lcolname="id",
                Rcolname="cytogenetic_location"
            )
        )
    ),
    list(
        objName="OMIM",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            HUMAN_DB_L2Rbrick1,
            list(
                table="omim",
                Lcolname="id",
                Rcolname="omim_id"
            )
        )
    ),
    list(
        objName="PATH",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            HUMAN_DB_L2Rbrick1,
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
            HUMAN_DB_L2Rbrick1,
            list(
                table="pubmed",
                Lcolname="id",
                Rcolname="pubmed_id"
            )
        )
    ),
    list(
        objName="REFSEQ",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            HUMAN_DB_L2Rbrick1,
            list(
                table="refseq",
                Lcolname="id",
                Rcolname="accession"
            )
        )
    ),
    list(
        objName="SYMBOL",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            HUMAN_DB_L2Rbrick1,
            list(
                table="gene_info",
                Lcolname="id",
                Rcolname="symbol"
            )
        )
    ),
    list(
        objName="UNIGENE",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            HUMAN_DB_L2Rbrick1,
            list(
                table="unigene",
                Lcolname="id",
                Rcolname="unigene_id"
            )
        )
    ),
    list(
        objName="CHRLOC",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            HUMAN_DB_L2Rbrick1,
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
        objName="PFAM",
        Class="IpiAnnDbMap",
        L2Rpath=list(
            HUMAN_DB_L2Rbrick1,
            list(
                table="pfam",
                Lcolname="id",
                Rcolname="ipi_id",
                tagCols=c(PfamId="{pfam_id}")
            )
        )
    ),
    list(
        objName="PROSITE",
        Class="IpiAnnDbMap",
        L2Rpath=list(
            HUMAN_DB_L2Rbrick1,
            list(
                table="prosite",
                Lcolname="id",
                Rcolname="ipi_id",
                tagCols=c(PrositeId="{prosite_id}")
            )
        )
    ),
    list(
        objName="GO",
        Class="Go3AnnDbMap",
        L2Rpath=list(
            HUMAN_DB_L2Rbrick1,
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

createAnnObjs.HUMAN_DB <- function(prefix, objTarget, conn, datacache)
{
    ## AnnDbMap objects
    seed0 <- list(
        objTarget=objTarget,
        datacache=datacache,
        conn=conn
    )
    ann_objs <- createAnnDbMaps(HUMAN_DB_AnnDbMap_seeds, seed0)

    ## RevAtomicAnnDbMap objects
    ann_objs$ACCNUM2EG <- revmap(ann_objs$ACCNUM, objName="ACCNUM2EG")
    ann_objs$ENZYME2EG <- revmap(ann_objs$ENZYME, objName="ENZYME2EG")
    ann_objs$GENENAME2EG <- revmap(ann_objs$GENENAME, objName="GENENAME2EG")
    ann_objs$MAP2EG <- revmap(ann_objs$MAP, objName="MAP2EG")
    ann_objs$OMIM2EG <- revmap(ann_objs$OMIM, objName="OMIM2EG")
    ann_objs$PATH2EG <- revmap(ann_objs$PATH, objName="PATH2EG")
    ann_objs$PMID2EG <- revmap(ann_objs$PMID, objName="PMID2EG")
    ann_objs$REFSEQ2EG <- revmap(ann_objs$REFSEQ, objName="REFSEQ2EG")
    ann_objs$SYMBOL2EG <- revmap(ann_objs$SYMBOL, objName="SYMBOL2EG")
    ann_objs$UNIGENE2EG <- revmap(ann_objs$UNIGENE, objName="UNIGENE2EG")
    ann_objs$PFAM2EG <- revmap(ann_objs$PFAM, objName="PFAM2EG")
    ann_objs$PROSITE2EG <- revmap(ann_objs$PROSITE, objName="PROSITE2EG")

    ## RevGo3AnnDbMap objects
    ann_objs$GO2EG <- revmap(ann_objs$GO, objName="GO2EG")

    ## 2 special maps that are not AnnDbMap objects (just named integer vectors)
    ann_objs$CHRLENGTHS <- createCHRLENGTHS(conn)
    ann_objs$MAPCOUNTS <- createMAPCOUNTS(conn, prefix)

    ## Some pre-caching
    left.names(ann_objs$GO)
    #left.mappedNames(ann_objs$GO)
    #right.names(ann_objs$GO2EG)
    #right.mappedNames(ann_objs$GO2EG)

    prefixAnnObjNames(ann_objs, prefix)
}

compareAnnDataIn2Pkgs.HUMAN_DB <- function(pkgname1, pkgname2, prefix, quick=FALSE, verbose=FALSE)
{
    direct_maps <- sapply(HUMAN_DB_AnnDbMap_seeds, function(x) x$objName)
    reverse_maps <- c(
        "ACCNUM2EG",
        "ENZYME2EG",
        "GENENAME2EG",
        "MAP2EG",
        "OMIM2EG",
        "PATH2EG",
        "PMID2EG",
        "REFSEQ2EG",
        "SYMBOL2EG",
        "UNIGENE2EG",
        "PFAM2EG",
        "PROSITE2EG"
        "GO2EG"
    )
    compareAnnDataIn2Pkgs(pkgname1, pkgname2, prefix, direct_maps, reverse_maps, quick, verbose)
}

