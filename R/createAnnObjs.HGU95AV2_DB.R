### =========================================================================
### An SQLite-based ann data package (AnnDbPkg) provides a set of pre-defined
### AnnObj objects that are created at load-time. This set depends only on
### the underlying db schema i.e. all the SQLite-based ann data packages that
### share the same underlying db schema will provide the same set of AnnObj
### objects.
###
### This file describes the set of AnnObj objects provided by any
### HGU95AV2_DB-based package i.e. any SQLite-based ann data package based
### on the HGU95AV2_DB schema.
### The createAnnObjs.HGU95AV2_DB() function is the main entry point for
### this file: it is called by any HGU95AV2_DB-based package at load-time.
### -------------------------------------------------------------------------


HGU95AV2_DB_L2Rbrick1 <- list(table="probes", Lcolname="probe_id", Rcolname="id")

### Mandatory fields: objName, Class and L2Rpath
HGU95AV2_DB_AnnDbMap_seeds <- list(
    list(
        objName="ACCNUM",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            list(
                table="probes",
                Lcolname="probe_id",
                Rcolname="accession"
            )
        )
    ),
    list(
        objName="CHR",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            HGU95AV2_DB_L2Rbrick1,
            list(
                table="chromosomes",
                Lcolname="id",
                Rcolname="chromosome"
            )
        )
    ),
    list(
        objName="ENTREZID",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            HGU95AV2_DB_L2Rbrick1,
            list(
                table="genes",
                Lcolname="id",
                Rcolname="gene_id"
            )
        ),
        rightColType="integer"
    ),
    list(
        objName="ENZYME",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            HGU95AV2_DB_L2Rbrick1,
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
            HGU95AV2_DB_L2Rbrick1,
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
            HGU95AV2_DB_L2Rbrick1,
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
            HGU95AV2_DB_L2Rbrick1,
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
            HGU95AV2_DB_L2Rbrick1,
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
            HGU95AV2_DB_L2Rbrick1,
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
            HGU95AV2_DB_L2Rbrick1,
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
            HGU95AV2_DB_L2Rbrick1,
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
            HGU95AV2_DB_L2Rbrick1,
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
            HGU95AV2_DB_L2Rbrick1,
            list(
                table="chromosome_locations",
                Lcolname="id",
                Rcolname="start_location",
                attribCols=c(Chromosome="{chromosome}")
            )
        ),
        rightColType="integer"
    ),
    list(
        objName="PFAM",
        Class="IpiAnnDbMap",
        L2Rpath=list(
            HGU95AV2_DB_L2Rbrick1,
            list(
                table="pfam",
                Lcolname="id",
                Rcolname="ipi_id",
                attribCols=c(PfamId="{pfam_id}")
            )
        )
    ),
    list(
        objName="PROSITE",
        Class="IpiAnnDbMap",
        L2Rpath=list(
            HGU95AV2_DB_L2Rbrick1,
            list(
                table="prosite",
                Lcolname="id",
                Rcolname="ipi_id",
                attribCols=c(PrositeId="{prosite_id}")
            )
        )
    ),
    list(
        objName="GO",
        Class="Go3AnnDbMap",
        L2Rpath=list(
            HGU95AV2_DB_L2Rbrick1,
            list(
                #table="go_term", # no rightmost table for a Go3AnnDbMap
                Lcolname="id",
                Rcolname="go_id",
                attribCols=c(Evidence="{evidence}")
            )
        ),
        rightTables=GOtables()
    )
)

createAnnObjs.HGU95AV2_DB <- function(prefix, objTarget, conn, datacache)
{
    ## AnnDbMap objects
    seed0 <- list(
        objTarget=objTarget,
        datacache=datacache,
        conn=conn
    )
    ann_objs <- createAnnDbMaps(HGU95AV2_DB_AnnDbMap_seeds, seed0)

    ## RevAtomicAnnDbMap objects
    ann_objs$ENZYME2PROBE <- revmap(ann_objs$ENZYME, objName="ENZYME2PROBE")
    ann_objs$PATH2PROBE <- revmap(ann_objs$PATH, objName="PATH2PROBE")
    ann_objs$PMID2PROBE <- revmap(ann_objs$PMID, objName="PMID2PROBE")

    ## RevGo3AnnDbMap objects
    ann_objs$GO2PROBE <- revmap(ann_objs$GO, objName="GO2PROBE")
    map <- ann_objs$GO2PROBE; map@rightTables <- GOtables(all=TRUE)
    ann_objs$GO2ALLPROBES <- map

    ## 2 special maps that are not AnnDbMap objects (just named integer vectors)
    ann_objs$CHRLENGTHS <- createCHRLENGTHS(conn)
    ann_objs$MAPCOUNTS <- createMAPCOUNTS(conn, prefix)

    ## Some pre-caching
    left.names(ann_objs$GO)
    #mapped.left.names(ann_objs$GO)
    #right.names(ann_objs$GO2PROBE)
    #mapped.right.names(ann_objs$GO2PROBE)
    #right.names(ann_objs$GO2ALLPROBES)
    #mapped.right.names(ann_objs$GO2ALLPROBES)

    prefixAnnObjNames(ann_objs, prefix)
}

compareAnnDataIn2Pkgs.HGU95AV2_DB <- function(pkgname1, pkgname2, prefix, quick=FALSE, verbose=FALSE)
{
    direct_maps <- sapply(HGU95AV2_DB_AtomicAnnDbMap_seeds, function(x) x$objName)
    direct_maps <- c(direct_maps, sapply(HGU95AV2_DB_IpiAnnDbMap_seeds, function(x) x$objName))
    direct_maps <- c(direct_maps, "GO")
    reverse_maps <- c(
        "ENZYME2PROBE",
        "PATH2PROBE",
        "PMID2PROBE",
        "GO2PROBE",
        "GO2ALLPROBES"
    )
    compareAnnDataIn2Pkgs(pkgname1, pkgname2, prefix, direct_maps, reverse_maps, quick, verbose)
}

