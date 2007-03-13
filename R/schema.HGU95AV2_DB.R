### =========================================================================
### Create all data objects for an annotation data package
### with db schema HGU95AV2_DB
### -------------------------------------------------------------------------

### TODO: The following maps are missing for now:
###   AtomicAnnMap: SUMFUNC
###   miscellaneous maps: CHRLENGTHS

HGU95AV2_DB_default_leftTable <- "probes"
HGU95AV2_DB_default_leftCol <- "probe_id"
HGU95AV2_DB_default_join <- "INNER JOIN probes USING (id)"
HGU95AV2_DB_default_rightColType <- character(0)

### Mandatory fields: objName, rightTable and rightCol
HGU95AV2_DB_AtomicAnnMap_seeds <- list(
        list(
                objName="ACCNUM",
                rightTable="accessions",
                rightCol="accession",
                join="INNER JOIN probes USING (probe_id)" # not the default join!
        ),
        list(
                objName="CHR",
                rightTable="chromosomes",
                rightCol="chromosome"
        ),
        list(
                objName="ENTREZID",
                rightTable="genes",
                rightCol="gene_id",
                rightColType="integer"
        ),
        list(
                objName="ENZYME",
                rightTable="ec",
                rightCol="ec_number"
        ),
        list(
                objName="GENENAME",
                rightTable="gene_info",
                rightCol="gene_name"
        ),
        list(
                objName="MAP",
                rightTable="cytogenetic_locations",
                rightCol="cytogenetic_location"
        ),
        list(
                objName="OMIM",
                rightTable="omim",
                rightCol="omim_id"
        ),
        list(
                objName="PATH",
                rightTable="kegg",
                rightCol="kegg_id"
        ),
        list(
                objName="PMID",
                rightTable="pubmed",
                rightCol="pubmed_id"
        ),
        list(
                objName="REFSEQ",
                rightTable="refseq",
                rightCol="accession"
        ),
        list(
                objName="SYMBOL",
                rightTable="gene_info",
                rightCol="symbol"
        ),
        list(
                objName="UNIGENE",
                rightTable="unigene",
                rightCol="unigene_id"
        ),
        list(
                objName="CHRLOC",
                rightTable="chromosome_locations",
                rightCol="start_location",
                rightColType="integer",
                tagCol="chromosome"
        ),
        list(
                objName="PFAM",
                rightTable="pfam",
                rightCol="pfam_id",
                tagCol="ipi_id"
        ),
        list(
                objName="PROSITE",
                rightTable="prosite",
                rightCol="prosite_id",
                tagCol="ipi_id"
        )
)

createAnnObjects.HGU95AV2_DB <- function(prefix, objTarget, conn, datacache)
{
    ## AtomicAnnMap objects
    seed0 <- list(
        objTarget=objTarget,
        conn=conn,
        datacache=datacache,
        leftTable=HGU95AV2_DB_default_leftTable,
        leftCol=HGU95AV2_DB_default_leftCol,
        join=HGU95AV2_DB_default_join,
        rightColType=HGU95AV2_DB_default_rightColType
    )
    maps <- createAnnObjects("AtomicAnnMap", HGU95AV2_DB_AtomicAnnMap_seeds, seed0)

    ## ReverseAtomicAnnMap objects
    maps$ENZYME2PROBE <- revmap(maps$ENZYME, objName="ENZYME2PROBE")
    maps$PATH2PROBE <- revmap(maps$PATH, objName="PATH2PROBE")
    maps$PMID2PROBE <- revmap(maps$PMID, objName="PMID2PROBE")

    ## GOAnnMap object
    maps$GO <- new("GOAnnMap",
            objTarget=objTarget,
            conn=conn,
            datacache=datacache,
            objName="GO",
            leftTable=HGU95AV2_DB_default_leftTable,
            leftCol=HGU95AV2_DB_default_leftCol,
            join=HGU95AV2_DB_default_join,
            all=FALSE)

    ## ReverseGOAnnMap objects
    maps$GO2PROBE <- revmap(maps$GO, objName="GO2PROBE")
    maps$GO2ALLPROBES <- new("ReverseGOAnnMap", maps$GO, objName="GO2ALLPROBES", all=TRUE)

    ## Some pre-caching
    left.names(maps$GO)
    #mapped.left.names(maps$GO)
    #right.names(maps$GO2PROBE)
    #mapped.right.names(maps$GO2PROBE)
    #right.names(maps$GO2ALLPROBES)
    #mapped.right.names(maps$GO2ALLPROBES)

    ## The MAPCOUNTS object (named integer vector)
    maps$MAPCOUNTS <- createMAPCOUNTS(conn, prefix)

    names(maps) <- paste(prefix, names(maps), sep="")
    maps
}

compareAnnDataIn2Pkgs.HGU95AV2_DB <- function(pkgname1, pkgname2, prefix, probes=NULL, verbose=FALSE)
{
    direct_maps <- sapply(HGU95AV2_DB_AtomicAnnMap_seeds, function(x) x$objName)
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

