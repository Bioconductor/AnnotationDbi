### =========================================================================
### Create all data objects for an annotation data package
### with db schema YEASTDB
### -------------------------------------------------------------------------

### TODO: The following maps are missing for now:
###   miscellaneous maps: CHRLENGTHS, REJECTORF

YEASTDB_default_leftTable <- "sgd"
YEASTDB_default_leftCol <- "systematic_name"
YEASTDB_default_join <- "INNER JOIN sgd using (id)"
YEASTDB_default_rightColType <- character(0)

### Mandatory fields: mapName, rightTable and rightCol
YEASTDB_AtomicAnnMap_seeds <- list(
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
                join=character(0)
        ),
        list(
                mapName="INTERPRO",
                rightTable="interpro",
                rightCol="interpro_id"
        ),
        list(
                mapName="PATH",
                rightTable="kegg",
                rightCol="kegg_id"
        ),
        list(
                mapName="PFAM",
                rightTable="pfam",
                rightCol="pfam_id"
        ),
        list(
                mapName="PMID",
                rightTable="pubmed",
                rightCol="pubmed_id"
        ),
        list(
                mapName="SMART",
                rightTable="smart",
                rightCol="smart_id"
        ),
        list(
                mapName="CHRLOC",
                rightTable="chromosome_features",
                rightCol="start",
                rightColType="integer",
                tagsCol="chromosome"
        )
)

createAnnDataObjects.YEASTDB <- function(chipShortname, con, datacache)
{
    ## AtomicAnnMap objects
    seed0 <- list(
        chipShortname=chipShortname,
        con=con,
        datacache=datacache,
        leftTable=YEASTDB_default_leftTable,
        leftCol=YEASTDB_default_leftCol,
        join=YEASTDB_default_join,
        rightColType=YEASTDB_default_rightColType
    )
    maps <- createAtomicAnnMapObjects(YEASTDB_AtomicAnnMap_seeds, seed0)

    ## ReverseAtomicAnnMap objects
    maps$COMMON2SYSTEMATIC <- revmap(maps$GENENAME, mapName="COMMON2SYSTEMATIC")
    maps$ENZYME2PROBE <- revmap(maps$ENZYME, mapName="ENZYME2PROBE")
    maps$PATH2PROBE <- revmap(maps$PATH, mapName="PATH2PROBE")
    maps$PMID2PROBE <- revmap(maps$PMID, mapName="PMID2PROBE")

    ## GOAnnMap object
    maps$GO <- new("GOAnnMap",
            chipShortname=chipShortname,
            con=con,
            datacache=datacache,
            mapName="GO",
            leftTable=YEASTDB_default_leftTable,
            leftCol=YEASTDB_default_leftCol,
            join=YEASTDB_default_join,
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

