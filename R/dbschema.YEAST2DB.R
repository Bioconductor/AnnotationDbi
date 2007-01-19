### =========================================================================
### Create all data objects for an annotation data package
### with db schema YEAST2DB
### -------------------------------------------------------------------------

### TODO: The following maps are missing for now:
###   miscellaneous maps: CHRLENGTHS

YEAST2DB_short_join <- "INNER JOIN probes USING (systematic_name)"
YEAST2DB_default_baseJoins <- paste("INNER JOIN sgd USING (id)", YEAST2DB_short_join)
YEAST2DB_baseCol <- "probe_id"
YEAST2DB_default_mapColType <- character(0)

### Mandatory fields: mapName, mapTable and mapCol
YEAST2DB_AtomicAnnMap_seeds <- list(

    ## AtomicAnnMap objects
        list(
                mapName="ALIAS",
                mapTable="gene2alias",
                mapCol="alias"
        ),
        list(
                mapName="CHR",
                mapTable="chromosome_features",
                mapCol="chromosome"
        ),
        list(
                mapName="DESCRIPTION",
                mapTable="chromosome_features",
                mapCol="feature_description"
        ),
        list(
                mapName="ENZYME",
                mapTable="ec",
                mapCol="ec_number"
        ),
        list(
                mapName="GENENAME",
                mapTable="sgd",
                mapCol="gene_name",
                baseJoins=YEAST2DB_short_join
        ),
        list(
                mapName="ORF",
                mapTable="probes",
                mapCol="systematic_name"
        ),
        list(
                mapName="PATH",
                mapTable="kegg",
                mapCol="kegg_id"
        ),
        list(
                mapName="PMID",
                mapTable="pubmed",
                mapCol="pubmed_id"
        ),

    ## NamedAtomicAnnMap objects
        list(
                mapName="CHRLOC",
                mapTable="chromosome_features",
                mapCol="start",
                mapColType="integer",
                namesCol="chromosome"
        )
)

createAnnDataObjects.YEAST2DB <- function(chipShortname, con, datacache)
{
    cacheBASEIDS(con, "sgd", YEAST2DB_baseCol, datacache)

    ## AtomicAnnMap objects
    seed0 <- list(
        mapColType=YEAST2DB_default_mapColType,
        chipShortname=chipShortname,
        baseJoins=YEAST2DB_default_baseJoins,
        baseCol=YEAST2DB_baseCol,
        con=con,
        datacache=datacache
    )
    maps <- createAtomicAnnMapObjects(YEAST2DB_AtomicAnnMap_seeds, seed0)

    ## ReverseAtomicAnnMap objects
    maps$ENZYME2PROBE <- new("ReverseAtomicAnnMap", mapName="ENZYME2PROBE", maps$ENZYME)
    maps$PATH2PROBE <- new("ReverseAtomicAnnMap", mapName="PATH2PROBE", maps$PATH)
    maps$PMID2PROBE <- new("ReverseAtomicAnnMap", mapName="PMID2PROBE", maps$PMID)

    ## GOAnnMap objects
    maps$GO <- new("GOAnnMap",
            mapName="GO",
            chipShortname=chipShortname,
            baseJoins=YEAST2DB_default_baseJoins,
            baseCol=YEAST2DB_baseCol,
            con=con,
            datacache=datacache)

    ## ReverseGOAnnMap objects
    maps$GO2PROBE <- new("ReverseGOAnnMap", mapName="GO2PROBE", maps$GO, all=FALSE)
    maps$GO2ALLPROBES <- new("ReverseGOAnnMap", mapName="GO2ALLPROBES", maps$GO, all=TRUE)

    ## The MAPCOUNTS object (named integer vector)
    #maps$MAPCOUNTS <- createMAPCOUNTS(con, chipShortname)

    names(maps) <- paste(chipShortname, names(maps), sep="")
    maps
}

compareAnnDataIn2Pkgs.YEAST2DB <- function(pkgname1, pkgname2, mapprefix, probes=NULL, verbose=FALSE)
{
    direct_maps <- sapply(YEAST2DB_AtomicAnnMap_seeds, function(x) x$mapName)
    direct_maps <- c(direct_maps, "GO")
    reverse_maps <- c(
        "GO2PROBE",
        "GO2ALLPROBES",
        "ENZYME2PROBE",
        "PATH2PROBE",
        "PMID2PROBE"
    )
    compareAnnDataIn2Pkgs(pkgname1, pkgname2, direct_maps, reverse_maps, mapprefix, probes, verbose)
}

