### =========================================================================
### Create all data objects for an annotation data package
### with db schema AGDB
### -------------------------------------------------------------------------

AGDB_default_leftTable <- "probes"
AGDB_default_leftCol <- "probe_id"
AGDB_default_rightColType <- character(0)
AGDB_default_join <- "INNER JOIN probes USING (id)"

### Mandatory fields: mapName, rightTable and rightCol
AGDB_AtomicAnnMap_seeds <- list(

    ## AtomicAnnMap objects
        #list(
        #        mapName="ACCNUM",
        #        rightTable="accessions",
        #        rightCol="accession",
        #        join=character(0) # no join for this map
        #),
        list(
                mapName="ARACYC",
                rightTable="aracyc",
                rightCol="pathway_name"
        ),
        list(
                mapName="CHR",
                rightTable="gene_info",
                rightCol="chromosome"
        ),
        list(
                mapName="ENTREZID",
                rightTable="genes",
                rightCol="gene_id",
                replace.multiple="multiple"
        ),
        list(
                mapName="ENZYME",
                rightTable="ec",
                rightCol="ec_number"
        ),
        list(
                mapName="GENENAME",
                rightTable="gene_info",
                rightCol="gene_name"
        ),
        list(
                mapName="MULTIHIT",
                rightTable="genes",
                rightCol="gene_id",
                replace.single=as.character(NA)
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
                mapName="SYMBOL",
                rightTable="gene_info",
                rightCol="symbol"
        ),

    ## NamedAtomicAnnMap objects
        list(
                mapName="CHRLOC",
                rightTable="chromosome_locations",
                rightCol="start_location",
                rightColType="integer",
                rightNamesCol="chromosome"
        )
)

createAnnDataObjects.AGDB <- function(chipShortname, con, datacache)
{
    ## The side effect of this is to cache the probeset ids.
    dbUniqueColVals(con, AGDB_default_leftTable,
                    AGDB_default_leftCol, datacache)

    ## AtomicAnnMap objects
    seed0 <- list(
        chipShortname=chipShortname,
        con=con,
        datacache=datacache,
        leftTable=AGDB_default_leftTable,
        leftCol=AGDB_default_leftCol,
        join=AGDB_default_join,
        rightColType=AGDB_default_rightColType
    )
    maps <- createAtomicAnnMapObjects(AGDB_AtomicAnnMap_seeds, seed0)

    ## ReverseAtomicAnnMap objects
    maps$ENZYME2PROBE <- new("ReverseAtomicAnnMap", mapName="ENZYME2PROBE", maps$ENZYME)
    maps$PATH2PROBE <- new("ReverseAtomicAnnMap", mapName="PATH2PROBE", maps$PATH)
    maps$PMID2PROBE <- new("ReverseAtomicAnnMap", mapName="PMID2PROBE", maps$PMID)

    ## GOAnnMap object
    maps$GO <- new("GOAnnMap",
            chipShortname=chipShortname,
            con=con,
            datacache=datacache,
            mapName="GO",
            leftTable=AGDB_default_leftTable,
            leftCol=AGDB_default_leftCol,
            join=AGDB_default_join)

    ## ReverseGOAnnMap objects
    maps$GO2PROBE <- new("ReverseGOAnnMap", mapName="GO2PROBE", maps$GO, all=FALSE)
    maps$GO2ALLPROBES <- new("ReverseGOAnnMap", mapName="GO2ALLPROBES", maps$GO, all=TRUE)

    ## The MAPCOUNTS object (named integer vector)
    #maps$MAPCOUNTS <- createMAPCOUNTS(con, chipShortname)

    names(maps) <- paste(chipShortname, names(maps), sep="")
    maps
}

compareAnnDataIn2Pkgs.AGDB <- function(pkgname1, pkgname2, mapprefix, probes=NULL, verbose=FALSE)
{
    direct_maps <- sapply(AGDB_AtomicAnnMap_seeds, function(x) x$mapName)
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

