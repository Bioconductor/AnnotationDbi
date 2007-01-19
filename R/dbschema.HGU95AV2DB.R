### =========================================================================
### Create all data objects for an annotation data package
### with db schema HGU95AV2DB
### -------------------------------------------------------------------------

### TODO: The following maps are missing for now:
###   AtomicAnnMap: SUMFUNC
###   misceallenous maps: CHRLENGTHS

HGU95AV2DB_default_joins <- "INNER JOIN probes USING (id)"
HGU95AV2DB_default_mapColType <- character(0)

### Mandatory fields: mapName, mapTable and mapCol
HGU95AV2DB_atomic_ann_maps <- list(
        list(
                mapName="ACCNUM",
                mapTable="accessions",
                mapCol="accession",
                joins=character(0) # no join for this map
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
        )
)

### Mandatory fields: mapName, mapTable, mapCol and namesCol
HGU95AV2DB_named_atomic_ann_maps <- list(
        list(
                mapName="CHRLOC",
                mapTable="chromosome_locations",
                mapCol="start_location",
                namesCol="chromosome",
                mapColType="integer"
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
    cachePROBESET2GENE(con, "probes", NULL, datacache)
    maps <- list()
    completeSeed <- function(seed, Class)
    {
        seed$Class <- Class
        seed$chipShortname <- chipShortname
        seed$con <- con
        seed$datacache <- datacache
        if (is.null(seed["joins"][[1]]))
            seed$joins <- HGU95AV2DB_default_joins
        if (is.null(seed["mapColType"][[1]]))
            seed$mapColType <- HGU95AV2DB_default_mapColType
        seed
    }
    for (seed in HGU95AV2DB_atomic_ann_maps) {
        seed <- completeSeed(seed, "AtomicAnnMap")
        str(seed)
        maps[[seed$mapName]] <- do.call("new", seed)
    }
    for (seed in HGU95AV2DB_named_atomic_ann_maps) {
        seed <- completeSeed(seed, "NamedAtomicAnnMap")
        str(seed)
        maps[[seed$mapName]] <- do.call("new", seed)
    }
    maps$GO <- new("GOAnnMap",
            mapName="GO",
            chipShortname=chipShortname,
            joins=HGU95AV2DB_default_joins,
            con=con,
            datacache=datacache)
    maps$GO2PROBE <- new("ReverseGOAnnMap", mapName="GO2PROBE", maps$GO, all=FALSE)
    maps$GO2ALLPROBES <- new("ReverseGOAnnMap", mapName="GO2ALLPROBES", maps$GO, all=TRUE)
    maps$ENZYME2PROBE <- new("ReverseAtomicAnnMap", mapName="ENZYME2PROBE", maps$ENZYME)
    maps$PATH2PROBE <- new("ReverseAtomicAnnMap", mapName="PATH2PROBE", maps$PATH)
    maps$PMID2PROBE <- new("ReverseAtomicAnnMap", mapName="PMID2PROBE", maps$PMID)
    maps$MAPCOUNTS <- createMAPCOUNTS(con, chipShortname)
    names(maps) <- paste(chipShortname, names(maps), sep="")
    maps
}

compareAnnDataIn2Pkgs.HGU95AV2DB <- function(pkgname1, pkgname2, mapprefix, probes=NULL, verbose=FALSE)
{
    direct_maps <- c(HGU95AV2DB_atomic_ann_maps, HGU95AV2DB_named_atomic_ann_maps)
    direct_maps <- sapply(direct_maps, function(x) x$mapName)
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

