### =========================================================================
### Create all data objects for an annotation data package
### with db schema YEAST2DB
### -------------------------------------------------------------------------

### TODO: The following maps are missing for now:
###   misceallenous maps: CHRLENGTHS

### 'chipname' is the chip "shortname" e.g. "yeast2" for the yeast2db package
createAnnDataObjects.YEAST2DB <- function(chipname, con, datacache)
{
    joins1 <- "INNER JOIN probes USING (systematic_name)"
    cachePROBESET2GENE(con, "sgd", joins1, datacache)
    joins2 <- paste("INNER JOIN sgd USING (id)", joins1)
    maps <- list(
            ALIAS=new("AtomicAnnMap",
                mapTable="gene2alias",
                mapCol="alias",
                joins=joins2, con=con, datacache=datacache),
            CHR=new("AtomicAnnMap",
                mapTable="chromosome_features",
                mapCol="chromosome",
                joins=joins2, con=con, datacache=datacache),
            CHRLOC=new("NamedAtomicAnnMap",
                mapTable="chromosome_features",
                mapCol="start",
                mapColType="integer",
                namesCol="chromosome",
                joins=joins2, con=con, datacache=datacache),
            DESCRIPTION=new("AtomicAnnMap",
                mapTable="chromosome_features",
                mapCol="feature_description",
                joins=joins2, con=con, datacache=datacache),
            ENZYME=new("AtomicAnnMap",
                mapTable="ec",
                mapCol="ec_number",
                joins=joins2, con=con, datacache=datacache),
            GENENAME=new("AtomicAnnMap",
                mapTable="sgd",
                mapCol="gene_name",
                joins=joins1, con=con, datacache=datacache),
            GO=new("GOAnnMap",
                joins=joins2, con=con, datacache=datacache),
            GO2ALLPROBES=new("ReverseGOAnnMap",
                all=TRUE,
                joins=joins2, con=con, datacache=datacache),
            GO2PROBE=new("ReverseGOAnnMap",
                all=FALSE,
                joins=joins2, con=con, datacache=datacache),
            ORF=new("AtomicAnnMap",
                mapTable="probes",
                mapCol="systematic_name",
                con=con, datacache=datacache),
            PATH=new("AtomicAnnMap",
                mapTable="kegg",
                mapCol="kegg_id",
                joins=joins2, con=con, datacache=datacache),
            PMID=new("AtomicAnnMap",
                mapTable="pubmed",
                mapCol="pubmed_id",
                joins=joins2, con=con, datacache=datacache)
    )
    maps$ENZYME2PROBE <- new("ReverseAtomicAnnMap", maps$ENZYME)
    maps$PATH2PROBE <- new("ReverseAtomicAnnMap", maps$PATH)
    maps$PMID2PROBE <- new("ReverseAtomicAnnMap", maps$PMID)
    #maps$MAPCOUNTS <- createMAPCOUNTS(con, chipname)
    names(maps) <- paste(chipname, names(maps), sep="")
    maps
}

compareAnnDataIn2Pkgs.YEAST2DB <- function(pkgname1, pkgname2, mapprefix, probes=NULL, verbose=FALSE)
{
    direct_maps <- c(
        "ALIAS",
        "CHR",
        "CHRLOC",
        "DESCRIPTION",
        "ENZYME",
        "GENENAME",
        "GO",
        "ORF",
        "PATH",
        "PMID"
    )
    reverse_maps <- c(
        "GO2ALLPROBES",
        "GO2PROBE",
        "ENZYME2PROBE",
        "PATH2PROBE",
        "PMID2PROBE"
    )
    compareAnnDataIn2Pkgs(pkgname1, pkgname2, direct_maps, reverse_maps, mapprefix, probes, verbose)
}

