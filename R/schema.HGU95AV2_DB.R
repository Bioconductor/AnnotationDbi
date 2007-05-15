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


### Mandatory fields: objName and L2Rpath
HGU95AV2_DB_AtomicAnnDbMap_seeds <- list(
    list(
        objName="ACCNUM",
        L2Rpath=list(probes=c("probe_id","accession"))
    ),
    list(
        objName="CHR",
        L2Rpath=list(probes=c("probe_id","id"),
                     chromosomes=c("id","chromosome"))
    ),
    list(
        objName="ENTREZID",
        L2Rpath=list(probes=c("probe_id","id"),
                     genes=c("id","gene_id")),
        rightColType="integer"
    ),
    list(
        objName="ENZYME",
        L2Rpath=list(probes=c("probe_id","id"),
                     ec=c("id","ec_number"))
    ),
    list(
        objName="GENENAME",
        L2Rpath=list(probes=c("probe_id","id"),
                     gene_info=c("id","gene_name"))
    ),
    list(
        objName="MAP",
        L2Rpath=list(probes=c("probe_id","id"),
                     cytogenetic_locations=c("id","cytogenetic_location"))
    ),
    list(
        objName="OMIM",
        L2Rpath=list(probes=c("probe_id","id"),
                     omim=c("id","omim_id"))
    ),
    list(
        objName="PATH",
        L2Rpath=list(probes=c("probe_id","id"),
                     kegg=c("id","kegg_id"))
    ),
    list(
        objName="PMID",
        L2Rpath=list(probes=c("probe_id","id"),
                     pubmed=c("id","pubmed_id"))
    ),
    list(
        objName="REFSEQ",
        L2Rpath=list(probes=c("probe_id","id"),
                     refseq=c("id","accession"))
    ),
    list(
        objName="SYMBOL",
        L2Rpath=list(probes=c("probe_id","id"),
                     gene_info=c("id","symbol"))
    ),
    list(
        objName="UNIGENE",
        L2Rpath=list(probes=c("probe_id","id"),
                     unigene=c("id","unigene_id"))
    ),
    list(
        objName="CHRLOC",
        L2Rpath=list(probes=c("probe_id","id"),
                     chromosome_locations=c("id","start_location")),
        rightColType="integer",
        tagCols="chromosome"
    )
)

HGU95AV2_DB_IpiAnnDbMap_seeds <- list(
    list(
        objName="PFAM",
        L2Rpath=list(probes=c("probe_id","id"),
                     pfam=c("id","ipi_id")),
        tagCols="pfam_id"
    ),
    list(
        objName="PROSITE",
        L2Rpath=list(probes=c("probe_id","id"),
                     prosite=c("id","ipi_id")),
        tagCols="prosite_id"
    )
)

createAnnObjs.HGU95AV2_DB <- function(prefix, objTarget, conn, datacache)
{
    ## AtomicAnnDbMap and IpiAnnDbMap objects
    seed0 <- list(
        objTarget=objTarget,
        datacache=datacache,
        conn=conn
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
        L2Rpath=list(probes=c("probe_id","id"),
                     c("id", "go_id")), # unnamed element
        rightTables=GOtables(),
        tagCols=c("evidence", "Ontology")
    )

    ## RevGoAnnDbMap objects
    ann_objs$GO2PROBE <- revmap(ann_objs$GO, objName="GO2PROBE")
    ann_objs$GO2ALLPROBES <- new("RevGoAnnDbMap", ann_objs$GO,
                                 objName="GO2ALLPROBES", rightTables=GOtables(all=TRUE))

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

