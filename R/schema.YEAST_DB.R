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
YEAST_DB_AtomicAnnMap_seeds <- list(
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

createAnnObjects.YEAST_DB <- function(prefix, objTarget, conn, datacache)
{
    ## AtomicAnnMap objects
    seed0 <- list(
        objTarget=objTarget,
        conn=conn,
        datacache=datacache,
        leftTable=YEAST_DB_default_leftTable,
        leftCol=YEAST_DB_default_leftCol,
        join=YEAST_DB_default_join,
        rightColType=YEAST_DB_default_rightColType
    )
    maps <- createAnnObjects("AtomicAnnMap", YEAST_DB_AtomicAnnMap_seeds, seed0)

    ## ReverseAtomicAnnMap objects
    maps$COMMON2SYSTEMATIC <- revmap(maps$GENENAME, objName="COMMON2SYSTEMATIC")
    maps$ENZYME2PROBE <- revmap(maps$ENZYME, objName="ENZYME2PROBE")
    maps$PATH2PROBE <- revmap(maps$PATH, objName="PATH2PROBE")
    maps$PMID2PROBE <- revmap(maps$PMID, objName="PMID2PROBE")

    ## GOAnnMap object
    maps$GO <- new("GOAnnMap",
            objTarget=objTarget,
            conn=conn,
            datacache=datacache,
            objName="GO",
            leftTable=YEAST_DB_default_leftTable,
            leftCol=YEAST_DB_default_leftCol,
            join=YEAST_DB_default_join,
            all=FALSE)

    ## ReverseGOAnnMap objects
    maps$GO2PROBE <- revmap(maps$GO, objName="GO2PROBE")
    maps$GO2ALLPROBES <- new("ReverseGOAnnMap", maps$GO, objName="GO2ALLPROBES", all=TRUE)

    ## Some pre-caching
    left.names(maps$GO)

    ## The MAPCOUNTS object (named integer vector)
    #maps$MAPCOUNTS <- createMAPCOUNTS(conn, prefix)

    names(maps) <- paste(prefix, names(maps), sep="")
    maps
}

compareAnnDataIn2Pkgs.YEAST_DB <- function(pkgname1, pkgname2, prefix, probes=NULL, verbose=FALSE)
{
    direct_maps <- sapply(YEAST_DB_AtomicAnnMap_seeds, function(x) x$objName)
    direct_maps <- c(direct_maps, "GO")
    reverse_maps <- c(
        "COMMON2SYSTEMATIC",
        "ENZYME2PROBE",
        "PATH2PROBE",
        "PMID2PROBE",
        "GO2PROBE",
        "GO2ALLPROBES"
    )
    compareAnnDataIn2Pkgs(pkgname1, pkgname2, direct_maps, reverse_maps, prefix, probes, verbose)
}

