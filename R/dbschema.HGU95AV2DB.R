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
        list(
                mapName="ACCNUM",
                rightTable="accessions",
                rightCol="accession",
                join="INNER JOIN probes USING (probe_id)" # not the default join!
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
        list(
                mapName="CHRLOC",
                rightTable="chromosome_locations",
                rightCol="start_location",
                rightColType="integer",
                tagsCol="chromosome"
        ),
        list(
                mapName="PFAM",
                rightTable="pfam",
                rightCol="pfam_id",
                tagsCol="ipi_id"
        ),
        list(
                mapName="PROSITE",
                rightTable="prosite",
                rightCol="prosite_id",
                tagsCol="ipi_id"
        )
)

createAnnDataObjects.HGU95AV2DB <- function(prefix, mapTarget, con, datacache)
{
    ## AtomicAnnMap objects
    seed0 <- list(
        mapTarget=mapTarget,
        con=con,
        datacache=datacache,
        leftTable=HGU95AV2DB_default_leftTable,
        leftCol=HGU95AV2DB_default_leftCol,
        join=HGU95AV2DB_default_join,
        rightColType=HGU95AV2DB_default_rightColType
    )
    maps <- createAtomicAnnMapObjects(HGU95AV2DB_AtomicAnnMap_seeds, seed0)

    ## ReverseAtomicAnnMap objects
    maps$ENZYME2PROBE <- revmap(maps$ENZYME, mapName="ENZYME2PROBE")
    maps$PATH2PROBE <- revmap(maps$PATH, mapName="PATH2PROBE")
    maps$PMID2PROBE <- revmap(maps$PMID, mapName="PMID2PROBE")

    ## GOAnnMap object
    maps$GO <- new("GOAnnMap",
            mapTarget=mapTarget,
            con=con,
            datacache=datacache,
            mapName="GO",
            leftTable=HGU95AV2DB_default_leftTable,
            leftCol=HGU95AV2DB_default_leftCol,
            join=HGU95AV2DB_default_join,
            all=FALSE)

    ## ReverseGOAnnMap objects
    maps$GO2PROBE <- revmap(maps$GO, mapName="GO2PROBE")
    maps$GO2ALLPROBES <- new("ReverseGOAnnMap", maps$GO, mapName="GO2ALLPROBES", all=TRUE)

    ## Some pre-caching
    left.names(maps$GO)
    #mapped.left.names(maps$GO)
    #right.names(maps$GO2PROBE)
    #mapped.right.names(maps$GO2PROBE)
    #right.names(maps$GO2ALLPROBES)
    #mapped.right.names(maps$GO2ALLPROBES)

    ## The MAPCOUNTS object (named integer vector)
    maps$MAPCOUNTS <- createMAPCOUNTS(con, prefix)

    names(maps) <- paste(prefix, names(maps), sep="")
    maps
}

compareAnnDataIn2Pkgs.HGU95AV2DB <- function(pkgname1, pkgname2, prefix, probes=NULL, verbose=FALSE)
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
    compareAnnDataIn2Pkgs(pkgname1, pkgname2, direct_maps, reverse_maps, prefix, probes, verbose)
}

