### =========================================================================
### Create all data objects for an annotation data package
### with db schema HGU95AV2DB
### -------------------------------------------------------------------------

### TODO: The following maps are missing for now:
###   AtomicAnnMap: SUMFUNC
###   misceallenous maps: CHRLENGTHS

### 'chipname' is the chip "shortname" e.g. "hgu95av2" for the hgu95av2db package
createAnnDataObjects.HGU95AV2DB <- function(chipname, con, datacache)
{
    cachePROBESET2GENE(con, "probes", NULL, datacache)
    joins1 <- "INNER JOIN probes USING (id)"
    maps <- list(
            ACCNUM=new("AtomicAnnMap",
                mapTable="accessions",
                mapCol="accession",
                con=con, datacache=datacache),
            CHR=new("AtomicAnnMap",
                mapTable="chromosomes",
                mapCol="chromosome",
                joins=joins1, con=con, datacache=datacache),
            CHRLOC=new("NamedAtomicAnnMap",
                mapTable="chromosome_locations",
                mapCol="start_location",
                namesCol="chromosome",
                joins=joins1, con=con, datacache=datacache),
            ENTREZID=new("AtomicAnnMap",
                mapTable="genes",
                mapCol="gene_id",
                joins=joins1, con=con, datacache=datacache),
            ENZYME=new("AtomicAnnMap",
                mapTable="ec",
                mapCol="ec_number",
                joins=joins1, con=con, datacache=datacache),
            GENENAME=new("AtomicAnnMap",
                mapTable="gene_info",
                mapCol="gene_name",
                joins=joins1, con=con, datacache=datacache),
            GO=new("GOAnnMap",
                joins=joins1, con=con, datacache=datacache),
            GO2ALLPROBES=new("ReverseGOAnnMap",
                all=TRUE,
                joins=joins1, con=con, datacache=datacache),
            GO2PROBE=new("ReverseGOAnnMap",
                all=FALSE,
                joins=joins1, con=con, datacache=datacache),
            MAP=new("AtomicAnnMap",
                mapTable="cytogenetic_locations",
                mapCol="cytogenetic_location",
                joins=joins1, con=con, datacache=datacache),
            OMIM=new("AtomicAnnMap",
                mapTable="omim",
                mapCol="omim_id",
                joins=joins1, con=con, datacache=datacache),
            PATH=new("AtomicAnnMap",
                mapTable="kegg",
                mapCol="kegg_id",
                joins=joins1, con=con, datacache=datacache),
            PFAM=new("NamedAtomicAnnMap",
                mapTable="pfam",
                mapCol="pfam_id",
                namesCol="ipi_id",
                joins=joins1, con=con, datacache=datacache),
            PMID=new("AtomicAnnMap",
                mapTable="pubmed",
                mapCol="pubmed_id",
                joins=joins1, con=con, datacache=datacache),
            PROSITE=new("NamedAtomicAnnMap",
                mapTable="prosite",
                mapCol="prosite_id",
                namesCol="ipi_id",
                joins=joins1, con=con, datacache=datacache),
            REFSEQ=new("AtomicAnnMap",
                mapTable="refseq",
                mapCol="accession",
                joins=joins1, con=con, datacache=datacache),
            SYMBOL=new("AtomicAnnMap",
                mapTable="gene_info",
                mapCol="symbol",
                joins=joins1, con=con, datacache=datacache),
            UNIGENE=new("AtomicAnnMap",
                mapTable="unigene",
                mapCol="unigene_id",
                joins=joins1, con=con, datacache=datacache)
    )
    maps$ENZYME2PROBE <- new("ReverseAtomicAnnMap", maps$ENZYME)
    maps$PATH2PROBE <- new("ReverseAtomicAnnMap", maps$PATH)
    maps$PMID2PROBE <- new("ReverseAtomicAnnMap", maps$PMID)
    maps$MAPCOUNTS <- createMAPCOUNTS(con, chipname)
    names(maps) <- paste(chipname, names(maps), sep="")
    maps
}

compareAnnDataIn2Pkgs.HGU95AV2DB <- function(pkgname1, pkgname2, mapprefix, probes=NULL)
{
    direct_maps <- c(
        "ACCNUM",
        "CHR",
        "CHRLOC",
        "ENTREZID",
        "ENZYME",
        "GENENAME",
        "GO",
        "MAP",
        "OMIM",
        "PATH",
        "PFAM",
        "PMID",
        "PROSITE",
        "REFSEQ",
        "SYMBOL",
        "UNIGENE"
    )
    reverse_maps <- c(
        "GO2ALLPROBES",
        "GO2PROBE",
        "ENZYME2PROBE",
        "PATH2PROBE",
        "PMID2PROBE"
    )
    compareAnnDataIn2Pkgs(pkgname1, pkgname2, direct_maps, reverse_maps, mapprefix, probes)
}

