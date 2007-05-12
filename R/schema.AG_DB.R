### =========================================================================
### Create all data objects for an annotation data package
### with db schema AG_DB
### -------------------------------------------------------------------------

AG_DB_default_leftTable <- "probes"
AG_DB_default_leftCol <- "probe_id"
AG_DB_default_rightColType <- character(0)
AG_DB_default_join <- "INNER JOIN probes USING (id)"

### Mandatory fields: objName, rightTable and rightCol
AG_DB_AtomicAnnDbMap_seeds <- list(
    #list(
    #    objName="ACCNUM",
    #    rightTable="accessions",
    #    rightCol="accession",
    #    join="INNER JOIN probes USING (probe_id)" # not the default join!
    #),
    list(
        objName="ARACYC",
        rightTable="aracyc",
        rightCol="pathway_name"
    ),
    list(
        objName="CHR",
        rightTable="gene_info",
        rightCol="chromosome"
    ),
    list(
        objName="ENTREZID",
        rightTable="genes",
        rightCol="gene_id",
        replace.multiple="multiple"
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
        objName="MULTIHIT",
        rightTable="genes",
        rightCol="gene_id",
        replace.single=as.character(NA)
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
        objName="SYMBOL",
        rightTable="gene_info",
        rightCol="symbol"
    ),
    list(
        objName="CHRLOC",
        rightTable="chromosome_locations",
        rightCol="start_location",
        rightColType="integer",
        tagCol="chromosome"
    )
)

createAnnObjs.AG_DB <- function(prefix, objTarget, conn, datacache)
{
    ## AtomicAnnDbMap objects
    seed0 <- list(
        objTarget=objTarget,
        conn=conn,
        datacache=datacache,
        leftTable=AG_DB_default_leftTable,
        leftCol=AG_DB_default_leftCol,
        join=AG_DB_default_join,
        rightColType=AG_DB_default_rightColType
    )
    ann_objs <- createAnnObjs("AtomicAnnDbMap", AG_DB_AtomicAnnDbMap_seeds, seed0)

    ## RevAtomicAnnDbMap objects
    ann_objs$ENZYME2PROBE <- revmap(ann_objs$ENZYME, objName="ENZYME2PROBE")
    ann_objs$PATH2PROBE <- revmap(ann_objs$PATH, objName="PATH2PROBE")
    ann_objs$PMID2PROBE <- revmap(ann_objs$PMID, objName="PMID2PROBE")

    ## GoAnnDbMap object
    ann_objs$GO <- new("GoAnnDbMap",
        objTarget=objTarget,
        conn=conn,
        datacache=datacache,
        objName="GO",
        leftTable=AG_DB_default_leftTable,
        leftCol=AG_DB_default_leftCol,
        join=AG_DB_default_join,
        all=FALSE
    )

    ## RevGoAnnDbMap objects
    ann_objs$GO2PROBE <- revmap(ann_objs$GO, objName="GO2PROBE")
    ann_objs$GO2ALLPROBES <- new("RevGoAnnDbMap", ann_objs$GO, objName="GO2ALLPROBES", all=TRUE)

    ## Some pre-caching
    left.names(ann_objs$GO)

    ## The MAPCOUNTS object (named integer vector)
    #ann_objs$MAPCOUNTS <- createMAPCOUNTS(conn, prefix)

    prefixAnnObjNames(ann_objs, prefix)
}

compareAnnDataIn2Pkgs.AG_DB <- function(pkgname1, pkgname2, prefix, quick=FALSE, verbose=FALSE)
{
    direct_maps <- sapply(AG_DB_AtomicAnnDbMap_seeds, function(x) x$objName)
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

