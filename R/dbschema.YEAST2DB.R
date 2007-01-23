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

### Mandatory fields: mapName, rightTable and rightCol
YEAST2DB_AtomicAnnMap_seeds <- list(
        list(
                mapName="ALIAS",
                rightTable="gene2alias",
                rightCol="alias"
        ),
        list(
                mapName="CHR",
                rightTable="chromosome_features",
                rightCol="chromosome"
        ),
        list(
                mapName="DESCRIPTION",
                rightTable="chromosome_features",
                rightCol="feature_description"
        ),
        list(
                mapName="ENZYME",
                rightTable="ec",
                rightCol="ec_number"
        ),
        list(
                mapName="GENENAME",
                rightTable="sgd",
                rightCol="gene_name",
                join=YEAST2DB_short_join
        ),
        list(
                mapName="ORF",
                rightTable="probes",
                rightCol="systematic_name",
                join=character(0)
        ),
        list(
                mapName="PATH",
                rightTable="kegg",
                rightCol="kegg_id"
        ),
        list(
                mapName="PMID",
                rightTable="pubmed",
                rightCol="pubmed_id"
        ),
        list(
                mapName="CHRLOC",
                rightTable="chromosome_features",
                rightCol="start",
                rightColType="integer",
                tagsCol="chromosome"
        )
)

createAnnDataObjects.YEAST2DB <- function(chipShortname, con, datacache)
{
    ## AtomicAnnMap objects
    seed0 <- list(
        chipShortname=chipShortname,
        con=con,
        datacache=datacache,
        rightColType=YEAST2DB_default_rightColType,
        leftTable=YEAST2DB_default_leftTable,
        leftCol=YEAST2DB_default_leftCol,
        join=YEAST2DB_default_join
    )
    maps <- createAtomicAnnMapObjects(YEAST2DB_AtomicAnnMap_seeds, seed0)

    ## ReverseAtomicAnnMap objects
    maps$ENZYME2PROBE <- revmap(maps$ENZYME, mapName="ENZYME2PROBE")
    maps$PATH2PROBE <- revmap(maps$PATH, mapName="PATH2PROBE")
    maps$PMID2PROBE <- revmap(maps$PMID, mapName="PMID2PROBE")

    ## GOAnnMap object
    maps$GO <- new("GOAnnMap",
            chipShortname=chipShortname,
            con=con,
            datacache=datacache,
            mapName="GO",
            leftTable=YEAST2DB_default_leftTable,
            leftCol=YEAST2DB_default_leftCol,
            join=YEAST2DB_default_join,
            all=FALSE)

    ## ReverseGOAnnMap objects
    maps$GO2PROBE <- revmap(maps$GO, mapName="GO2PROBE")
    maps$GO2ALLPROBES <- new("ReverseGOAnnMap", maps$GO, mapName="GO2ALLPROBES", all=TRUE)

    ## Some pre-caching
    left.names(maps$GO)

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
        "ENZYME2PROBE",
        "PATH2PROBE",
        "PMID2PROBE",
        "GO2PROBE",
        "GO2ALLPROBES"
    )
    compareAnnDataIn2Pkgs(pkgname1, pkgname2, direct_maps, reverse_maps, mapprefix, probes, verbose)
}

