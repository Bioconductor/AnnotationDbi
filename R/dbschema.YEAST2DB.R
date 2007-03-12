### =========================================================================
### Create all data objects for an annotation data package
### with db schema YEAST2DB
### -------------------------------------------------------------------------

### TODO: The following maps are missing for now:
###   miscellaneous maps: CHRLENGTHS

YEAST2DB_default_leftTable <- "probes"
YEAST2DB_default_leftCol <- "probe_id"
YEAST2DB_short_join <- "INNER JOIN probes USING (systematic_name)"
YEAST2DB_default_join <- paste("INNER JOIN sgd USING (id)", YEAST2DB_short_join)
YEAST2DB_default_rightColType <- character(0)

### Mandatory fields: objName, rightTable and rightCol
YEAST2DB_AtomicAnnMap_seeds <- list(
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
                join=YEAST2DB_short_join
        ),
        list(
                objName="ORF",
                rightTable="probes",
                rightCol="systematic_name",
                join=character(0)
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
                objName="CHRLOC",
                rightTable="chromosome_features",
                rightCol="start",
                rightColType="integer",
                tagCol="chromosome"
        )
)

createAnnObjects.YEAST2DB <- function(prefix, objTarget, conn, datacache)
{
    ## AtomicAnnMap objects
    seed0 <- list(
        objTarget=objTarget,
        conn=conn,
        datacache=datacache,
        rightColType=YEAST2DB_default_rightColType,
        leftTable=YEAST2DB_default_leftTable,
        leftCol=YEAST2DB_default_leftCol,
        join=YEAST2DB_default_join
    )
    maps <- createAtomicAnnMapObjects(YEAST2DB_AtomicAnnMap_seeds, seed0)

    ## ReverseAtomicAnnMap objects
    maps$ENZYME2PROBE <- revmap(maps$ENZYME, objName="ENZYME2PROBE")
    maps$PATH2PROBE <- revmap(maps$PATH, objName="PATH2PROBE")
    maps$PMID2PROBE <- revmap(maps$PMID, objName="PMID2PROBE")

    ## GOAnnMap object
    maps$GO <- new("GOAnnMap",
            objTarget=objTarget,
            conn=conn,
            datacache=datacache,
            objName="GO",
            leftTable=YEAST2DB_default_leftTable,
            leftCol=YEAST2DB_default_leftCol,
            join=YEAST2DB_default_join,
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

compareAnnDataIn2Pkgs.YEAST2DB <- function(pkgname1, pkgname2, prefix, probes=NULL, verbose=FALSE)
{
    direct_maps <- sapply(YEAST2DB_AtomicAnnMap_seeds, function(x) x$objName)
    direct_maps <- c(direct_maps, "GO")
    reverse_maps <- c(
        "ENZYME2PROBE",
        "PATH2PROBE",
        "PMID2PROBE",
        "GO2PROBE",
        "GO2ALLPROBES"
    )
    compareAnnDataIn2Pkgs(pkgname1, pkgname2, direct_maps, reverse_maps, prefix, probes, verbose)
}

