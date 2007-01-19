### =========================================================================
### Create all data objects for an annotation data package
### with db schema YEASTDB
### -------------------------------------------------------------------------

### TODO: The following maps are missing for now:
###   miscellaneous maps: CHRLENGTHS, REJECTORF

YEASTDB_default_baseJoins <- "INNER JOIN sgd using (id)"
YEASTDB_baseCol <- "systematic_name"
YEASTDB_default_mapColType <- character(0)

### Mandatory fields: mapName, mapTable and mapCol
YEASTDB_AtomicAnnMap_seeds <- list(

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
                baseJoins=character(0)
        ),
        list(
                mapName="INTERPRO",
                mapTable="interpro",
                mapCol="interpro_id"
        ),
        list(
                mapName="PATH",
                mapTable="kegg",
                mapCol="kegg_id"
        ),
        list(
                mapName="PFAM",
                mapTable="pfam",
                mapCol="pfam_id"
        ),
        list(
                mapName="PMID",
                mapTable="pubmed",
                mapCol="pubmed_id"
        ),
        list(
                mapName="SMART",
                mapTable="smart",
                mapCol="smart_id"
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

createAnnDataObjects.YEASTDB <- function(chipShortname, con, datacache)
{
    cacheBASEIDS(con, "sgd", YEASTDB_baseCol, datacache)

    ## AtomicAnnMap objects
    seed0 <- list(
        mapColType=YEASTDB_default_mapColType,
        chipShortname=chipShortname,
        baseJoins=YEASTDB_default_baseJoins,
        baseCol=YEASTDB_baseCol,
        con=con,
        datacache=datacache
    )
    maps <- createAtomicAnnMapObjects(YEASTDB_AtomicAnnMap_seeds, seed0)

    ## ReverseAtomicAnnMap objects
    maps$COMMON2SYSTEMATIC <- new("ReverseAtomicAnnMap", mapName="COMMON2SYSTEMATIC", maps$GENENAME)
    maps$ENZYME2PROBE <- new("ReverseAtomicAnnMap", mapName="ENZYME2PROBE", maps$ENZYME)
    maps$PATH2PROBE <- new("ReverseAtomicAnnMap", mapName="PATH2PROBE", maps$PATH)
    maps$PMID2PROBE <- new("ReverseAtomicAnnMap", mapName="PMID2PROBE", maps$PMID)

    ## GOAnnMap objects
    maps$GO <- new("GOAnnMap",
            mapName="GO",
            chipShortname=chipShortname,
            baseJoins=YEASTDB_default_baseJoins,
            baseCol=YEASTDB_baseCol,
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

compareAnnDataIn2Pkgs.YEASTDB <- function(pkgname1, pkgname2, mapprefix, probes=NULL, verbose=FALSE)
{
    direct_maps <- sapply(YEASTDB_AtomicAnnMap_seeds, function(x) x$mapName)
    direct_maps <- c(direct_maps, "GO")
    reverse_maps <- c(
        "COMMON2SYSTEMATIC",
        "ENZYME2PROBE",
        "PATH2PROBE",
        "PMID2PROBE",
        "GO2PROBE",
        "GO2ALLPROBES"
    )
    compareAnnDataIn2Pkgs(pkgname1, pkgname2, direct_maps, reverse_maps, mapprefix, probes, verbose)
}

