### =========================================================================
### Create all data objects for an annotation data package
### with db schema AGDB
### -------------------------------------------------------------------------

AGDB_default_joins <- "INNER JOIN probes USING (id)"
AGDB_default_mapColType <- character(0)

### Mandatory fields: mapName, mapTable and mapCol
AGDB_AtomicAnnMap_seeds <- list(

    ## AtomicAnnMap objects
        #list(
        #        mapName="ACCNUM",
        #        mapTable="accessions",
        #        mapCol="accession",
        #        joins=character(0) # no join for this map
        #),
        list(
                mapName="ARACYC",
                mapTable="aracyc",
                mapCol="pathway_name"
        ),
        list(
                mapName="CHR",
                mapTable="gene_info",
                mapCol="chromosome"
        ),
        list(
                mapName="ENTREZID",
                mapTable="genes",
                mapCol="gene_id",
                replace.multiple="multiple"
        ),
        list(
                mapName="ENZYME",
                mapTable="ec",
                mapCol="ec_number"
        ),
        list(
                mapName="GENENAME",
                mapTable="gene_info",
                mapCol="gene_name"
        ),
        list(
                mapName="MULTIHIT",
                mapTable="genes",
                mapCol="gene_id",
                replace.single=as.character(NA)
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
        list(
                mapName="SYMBOL",
                mapTable="gene_info",
                mapCol="symbol"
        ),

    ## NamedAtomicAnnMap objects
        list(
                mapName="CHRLOC",
                mapTable="chromosome_locations",
                mapCol="start_location",
                mapColType="integer",
                namesCol="chromosome"
        )
)

createAnnDataObjects.AGDB <- function(chipShortname, con, datacache)
{
    cachePROBESET2GENE(con, "probes", NULL, datacache)

    ## AtomicAnnMap objects
    seed0 <- list(
        joins=AGDB_default_joins,
        mapColType=AGDB_default_mapColType,
        chipShortname=chipShortname,
        con=con,
        datacache=datacache
    )
    maps <- createAtomicAnnMapObjects(AGDB_AtomicAnnMap_seeds, seed0)

    ## ReverseAtomicAnnMap objects
    maps$ENZYME2PROBE <- new("ReverseAtomicAnnMap", mapName="ENZYME2PROBE", maps$ENZYME)
    maps$PATH2PROBE <- new("ReverseAtomicAnnMap", mapName="PATH2PROBE", maps$PATH)
    maps$PMID2PROBE <- new("ReverseAtomicAnnMap", mapName="PMID2PROBE", maps$PMID)

    ## GOAnnMap objects
    maps$GO <- new("GOAnnMap",
            mapName="GO",
            joins=AGDB_default_joins,
            chipShortname=chipShortname,
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

compareAnnDataIn2Pkgs.AGDB <- function(pkgname1, pkgname2, mapprefix, probes=NULL, verbose=FALSE)
{
    direct_maps <- sapply(AGDB_AtomicAnnMap_seeds, function(x) x$mapName)
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

