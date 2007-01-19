### =========================================================================
### Create all data objects for an annotation data package
### with db schema HGU95AV2DB
### -------------------------------------------------------------------------

### TODO: The following maps are missing for now:
###   AtomicAnnMap: SUMFUNC
###   miscellaneous maps: CHRLENGTHS

HGU95AV2DB_default_baseJoins <- "INNER JOIN probes USING (id)"
HGU95AV2DB_baseCol <- "probe_id"
HGU95AV2DB_default_mapColType <- character(0)

### Mandatory fields: mapName, mapTable and mapCol
HGU95AV2DB_AtomicAnnMap_seeds <- list(

    ## AtomicAnnMap objects
        list(
                mapName="ACCNUM",
                mapTable="accessions",
                mapCol="accession",
                baseJoins=character(0) # no join for this map
        ),
        list(
                mapName="CHR",
                mapTable="chromosomes",
                mapCol="chromosome"
        ),
        list(
                mapName="ENTREZID",
                mapTable="genes",
                mapCol="gene_id",
                mapColType="integer"
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
                mapName="MAP",
                mapTable="cytogenetic_locations",
                mapCol="cytogenetic_location"
        ),
        list(
                mapName="OMIM",
                mapTable="omim",
                mapCol="omim_id"
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
                mapName="REFSEQ",
                mapTable="refseq",
                mapCol="accession"
        ),
        list(
                mapName="SYMBOL",
                mapTable="gene_info",
                mapCol="symbol"
        ),
        list(
                mapName="UNIGENE",
                mapTable="unigene",
                mapCol="unigene_id"
        ),

    ## NamedAtomicAnnMap objects
        list(
                mapName="CHRLOC",
                mapTable="chromosome_locations",
                mapCol="start_location",
                mapColType="integer",
                namesCol="chromosome"
        ),
        list(
                mapName="PFAM",
                mapTable="pfam",
                mapCol="pfam_id",
                namesCol="ipi_id"
        ),
        list(
                mapName="PROSITE",
                mapTable="prosite",
                mapCol="prosite_id",
                namesCol="ipi_id"
        )
)

createAnnDataObjects.HGU95AV2DB <- function(chipShortname, con, datacache)
{
    cacheBASEIDS(con, "probes", HGU95AV2DB_baseCol, datacache)

    ## AtomicAnnMap objects
    seed0 <- list(
        mapColType=HGU95AV2DB_default_mapColType,
        chipShortname=chipShortname,
        baseJoins=HGU95AV2DB_default_baseJoins,
        baseCol=HGU95AV2DB_baseCol,
        con=con,
        datacache=datacache
    )
    maps <- createAtomicAnnMapObjects(HGU95AV2DB_AtomicAnnMap_seeds, seed0)

    ## ReverseAtomicAnnMap objects
    maps$ENZYME2PROBE <- new("ReverseAtomicAnnMap", mapName="ENZYME2PROBE", maps$ENZYME)
    maps$PATH2PROBE <- new("ReverseAtomicAnnMap", mapName="PATH2PROBE", maps$PATH)
    maps$PMID2PROBE <- new("ReverseAtomicAnnMap", mapName="PMID2PROBE", maps$PMID)

    ## GOAnnMap objects
    maps$GO <- new("GOAnnMap",
            mapName="GO",
            chipShortname=chipShortname,
            baseJoins=HGU95AV2DB_default_baseJoins,
            baseCol=HGU95AV2DB_baseCol,
            con=con,
            datacache=datacache)

    ## ReverseGOAnnMap objects
    maps$GO2PROBE <- new("ReverseGOAnnMap", mapName="GO2PROBE", maps$GO, all=FALSE)
    maps$GO2ALLPROBES <- new("ReverseGOAnnMap", mapName="GO2ALLPROBES", maps$GO, all=TRUE)

    ## The MAPCOUNTS object (named integer vector)
    maps$MAPCOUNTS <- createMAPCOUNTS(con, chipShortname)

    names(maps) <- paste(chipShortname, names(maps), sep="")
    maps
}

compareAnnDataIn2Pkgs.HGU95AV2DB <- function(pkgname1, pkgname2, mapprefix, probes=NULL, verbose=FALSE)
{
    direct_maps <- sapply(HGU95AV2DB_AtomicAnnMap_seeds, function(x) x$mapName)
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

