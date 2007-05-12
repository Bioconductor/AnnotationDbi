### =========================================================================
### Create all data objects for an annotation data package
### with db schema YEAST_DB
### -------------------------------------------------------------------------

### TODO: The following maps are missing for now:
###   miscellaneous maps: CHRLENGTHS, REJECTORF

YEAST_DB_default_leftTable <- "sgd"
YEAST_DB_default_leftCol <- "systematic_name"
YEAST_DB_default_join <- "INNER JOIN sgd using (id)"
YEAST_DB_default_rightColType <- character(0)

### Mandatory fields: objName, rightTable and rightCol
YEAST_DB_AtomicAnnDbMap_seeds <- list(
    list(
        objName="ALIAS",
        rightTable="gene2alias",
        rightCol="alias"
    ),
    list(
        objName="CHR",
        rightTable="chromosome_features",
        rightCol="chromosome"
    ),
    list(
        objName="DESCRIPTION",
        rightTable="chromosome_features",
        rightCol="feature_description"
    ),
    list(
        objName="ENZYME",
        rightTable="ec",
        rightCol="ec_number"
    ),
    list(
        objName="GENENAME",
        rightTable="sgd",
        rightCol="gene_name",
        join=character(0)
    ),
    list(
        objName="INTERPRO",
        rightTable="interpro",
        rightCol="interpro_id"
    ),
    list(
        objName="PATH",
        rightTable="kegg",
        rightCol="kegg_id"
    ),
    list(
        objName="PFAM",
        rightTable="pfam",
        rightCol="pfam_id"
    ),
    list(
        objName="PMID",
        rightTable="pubmed",
        rightCol="pubmed_id"
    ),
    list(
        objName="SMART",
        rightTable="smart",
        rightCol="smart_id"
    ),
    list(
        objName="CHRLOC",
        rightTable="chromosome_features",
        rightCol="start",
        rightColType="integer",
        tagCol="chromosome"
    )
)

createAnnObjs.YEAST_DB <- function(prefix, objTarget, conn, datacache)
{
    ## AtomicAnnDbMap objects
    seed0 <- list(
        objTarget=objTarget,
        conn=conn,
        datacache=datacache,
        leftTable=YEAST_DB_default_leftTable,
        leftCol=YEAST_DB_default_leftCol,
        join=YEAST_DB_default_join,
        rightColType=YEAST_DB_default_rightColType
    )
    ann_objs <- createAnnObjs("AtomicAnnDbMap", YEAST_DB_AtomicAnnDbMap_seeds, seed0)

    ## RevAtomicAnnDbMap objects
    ann_objs$COMMON2SYSTEMATIC <- revmap(ann_objs$GENENAME, objName="COMMON2SYSTEMATIC")
    ann_objs$ENZYME2PROBE <- revmap(ann_objs$ENZYME, objName="ENZYME2PROBE")
    ann_objs$PATH2PROBE <- revmap(ann_objs$PATH, objName="PATH2PROBE")
    ann_objs$PMID2PROBE <- revmap(ann_objs$PMID, objName="PMID2PROBE")

    ## GoAnnDbMap object
    ann_objs$GO <- new("GoAnnDbMap",
        objTarget=objTarget,
        conn=conn,
        datacache=datacache,
        objName="GO",
        leftTable=YEAST_DB_default_leftTable,
        leftCol=YEAST_DB_default_leftCol,
        join=YEAST_DB_default_join,
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

compareAnnDataIn2Pkgs.YEAST_DB <- function(pkgname1, pkgname2, prefix, quick=FALSE, verbose=FALSE)
{
    direct_maps <- sapply(YEAST_DB_AtomicAnnDbMap_seeds, function(x) x$objName)
    direct_maps <- c(direct_maps, "GO")
    reverse_maps <- c(
        "COMMON2SYSTEMATIC",
        "ENZYME2PROBE",
        "PATH2PROBE",
        "PMID2PROBE",
        "GO2PROBE",
        "GO2ALLPROBES"
    )
    compareAnnDataIn2Pkgs(pkgname1, pkgname2, prefix, direct_maps, reverse_maps, quick, verbose)
}

