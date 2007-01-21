### =========================================================================
### Create all data objects for an annotation data package
### with db schema HGU95AV2DB
### -------------------------------------------------------------------------

### TODO: The following maps are missing for now:
###   AtomicAnnMap: SUMFUNC
###   miscellaneous maps: CHRLENGTHS

HGU95AV2DB_default_leftTable <- "probes"
HGU95AV2DB_default_leftCol <- "probe_id"
HGU95AV2DB_default_join <- "INNER JOIN probes USING (id)"
HGU95AV2DB_default_rightColType <- character(0)

### Mandatory fields: mapName, rightTable and rightCol
HGU95AV2DB_AtomicAnnMap_seeds <- list(

    ## AtomicAnnMap objects
        list(
                mapName="ACCNUM",
                rightTable="accessions",
                rightCol="accession",
                join=character(0) # no join for this map
        ),
        list(
                mapName="CHR",
                rightTable="chromosomes",
                rightCol="chromosome"
        ),
        list(
                mapName="ENTREZID",
                rightTable="genes",
                rightCol="gene_id",
                rightColType="integer"
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
                mapName="MAP",
                rightTable="cytogenetic_locations",
                rightCol="cytogenetic_location"
        ),
        list(
                mapName="OMIM",
                rightTable="omim",
                rightCol="omim_id"
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
                mapName="REFSEQ",
                rightTable="refseq",
                rightCol="accession"
        ),
        list(
                mapName="SYMBOL",
                rightTable="gene_info",
                rightCol="symbol"
        ),
        list(
                mapName="UNIGENE",
                rightTable="unigene",
                rightCol="unigene_id"
        ),

    ## NamedAtomicAnnMap objects
        list(
                mapName="CHRLOC",
                rightTable="chromosome_locations",
                rightCol="start_location",
                rightColType="integer",
                rightNamesCol="chromosome"
        ),
        list(
                mapName="PFAM",
                rightTable="pfam",
                rightCol="pfam_id",
                rightNamesCol="ipi_id"
        ),
        list(
                mapName="PROSITE",
                rightTable="prosite",
                rightCol="prosite_id",
                rightNamesCol="ipi_id"
        )
)

createAnnDataObjects.HGU95AV2DB <- function(chipShortname, con, datacache)
{
    ## The side effect of this is to cache the probeset ids.
    dbUniqueColVals(con, HGU95AV2DB_default_leftTable,
                    HGU95AV2DB_default_leftCol, datacache)

    ## AtomicAnnMap objects
    seed0 <- list(
        chipShortname=chipShortname,
        con=con,
        datacache=datacache,
        leftTable=HGU95AV2DB_default_leftTable,
        leftCol=HGU95AV2DB_default_leftCol,
        join=HGU95AV2DB_default_join,
        rightColType=HGU95AV2DB_default_rightColType
    )
    maps <- createAtomicAnnMapObjects(HGU95AV2DB_AtomicAnnMap_seeds, seed0)

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
            leftTable=HGU95AV2DB_default_leftTable,
            leftCol=HGU95AV2DB_default_leftCol,
            join=HGU95AV2DB_default_join)

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

