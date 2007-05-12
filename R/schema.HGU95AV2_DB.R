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


HGU95AV2_DB_default_leftTable <- "probes"
HGU95AV2_DB_default_leftCol <- "probe_id"
HGU95AV2_DB_default_join <- "INNER JOIN probes USING (id)"
HGU95AV2_DB_default_rightColType <- character(0)

### Mandatory fields: objName, rightTable and rightCol
HGU95AV2_DB_AtomicAnnDbMap_seeds <- list(
    list(
        objName="ACCNUM",
        rightTable="probes",
        rightCol="accession",
        join=character(0) # not the default join!
    ),
    list(
        objName="CHR",
        rightTable="chromosomes",
        rightCol="chromosome"
    ),
    list(
        objName="ENTREZID",
        rightTable="genes",
        rightCol="gene_id",
        rightColType="integer"
    ),
    list(
        objName="ENZYME",
        rightTable="ec",
        rightCol="ec_number"
    ),
    list(
        objName="GENENAME",
        rightTable="gene_info",
        rightCol="gene_name"
    ),
    list(
        objName="MAP",
        rightTable="cytogenetic_locations",
        rightCol="cytogenetic_location"
    ),
    list(
        objName="OMIM",
        rightTable="omim",
        rightCol="omim_id"
    ),
    list(
        objName="PATH",
        rightTable="kegg",
        rightCol="kegg_id"
    ),
    list(
        objName="PMID",
        rightTable="pubmed",
        rightCol="pubmed_id"
    ),
    list(
        objName="REFSEQ",
        rightTable="refseq",
        rightCol="accession"
    ),
    list(
        objName="SYMBOL",
        rightTable="gene_info",
        rightCol="symbol"
    ),
    list(
        objName="UNIGENE",
        rightTable="unigene",
        rightCol="unigene_id"
    ),
    list(
        objName="CHRLOC",
        rightTable="chromosome_locations",
        rightCol="start_location",
        rightColType="integer",
        tagCols="chromosome"
    )
)

HGU95AV2_DB_IpiAnnDbMap_seeds <- list(
    list(
        objName="PFAM",
        rightTable="pfam",
        rightCol="ipi_id",
        tagCols="pfam_id"
    ),
    list(
        objName="PROSITE",
        rightTable="prosite",
        rightCol="ipi_id",
        tagCols="prosite_id"
    )
)

createAnnObjs.HGU95AV2_DB <- function(prefix, objTarget, conn, datacache)
{
    ## AtomicAnnDbMap and IpiAnnDbMap objects
    seed0 <- list(
        objTarget=objTarget,
        datacache=datacache,
        conn=conn,
        leftTable=HGU95AV2_DB_default_leftTable,
        leftCol=HGU95AV2_DB_default_leftCol,
        rightColType=HGU95AV2_DB_default_rightColType,
        join=HGU95AV2_DB_default_join
    )
    ann_objs <- createAnnObjs("AtomicAnnDbMap", HGU95AV2_DB_AtomicAnnDbMap_seeds, seed0)
    createAnnObjs("IpiAnnDbMap", HGU95AV2_DB_IpiAnnDbMap_seeds, seed0, ann_objs)

    ## RevAtomicAnnDbMap objects
    ann_objs$ENZYME2PROBE <- revmap(ann_objs$ENZYME, objName="ENZYME2PROBE")
    ann_objs$PATH2PROBE <- revmap(ann_objs$PATH, objName="PATH2PROBE")
    ann_objs$PMID2PROBE <- revmap(ann_objs$PMID, objName="PMID2PROBE")

    ## GoAnnDbMap object
    ann_objs$GO <- new("GoAnnDbMap",
        objName="GO",
        objTarget=objTarget,
        datacache=datacache,
        conn=conn,
        leftTable=HGU95AV2_DB_default_leftTable,
        leftCol=HGU95AV2_DB_default_leftCol,
        rightTable=GOtables(),
        rightCol="go_id",
        tagCols=c("evidence", "Ontology"),
        join=HGU95AV2_DB_default_join
    )

    ## RevGoAnnDbMap objects
    ann_objs$GO2PROBE <- revmap(ann_objs$GO, objName="GO2PROBE")
    ann_objs$GO2ALLPROBES <- new("RevGoAnnDbMap", ann_objs$GO,
                                objName="GO2ALLPROBES", rightTable=GOtables(all=TRUE))

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

