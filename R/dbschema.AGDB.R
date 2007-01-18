### =========================================================================
### Create all data objects for an annotation data package
### with db schema AGDB
### -------------------------------------------------------------------------

### 'chipname' is the chip "shortname" e.g. "ag" for the agdb package
createAnnDataObjects.AGDB <- function(chipname, con, datacache)
{
    cachePROBESET2GENE(con, "probes", NULL, datacache)
    joins1 <- "INNER JOIN probes USING (id)"
    maps <- list(
            #ACCNUM=new("AtomicAnnMap",
            #    mapTable="accessions",
            #    mapCol="accession",
            #    con=con, datacache=datacache),
            ARACYC=new("AtomicAnnMap",
                mapTable="aracyc",
                mapCol="pathway_name",
                joins=joins1, con=con, datacache=datacache),
            CHR=new("AtomicAnnMap",
                mapTable="gene_info",
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
                replace.multiple="multiple",
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
            MULTIHIT=new("AtomicAnnMap",
                mapTable="genes",
                mapCol="gene_id",
                replace.single=as.character(NA),
                joins=joins1, con=con, datacache=datacache),
            PATH=new("AtomicAnnMap",
                mapTable="kegg",
                mapCol="kegg_id",
                joins=joins1, con=con, datacache=datacache),
            PMID=new("AtomicAnnMap",
                mapTable="pubmed",
                mapCol="pubmed_id",
                joins=joins1, con=con, datacache=datacache),
            SYMBOL=new("AtomicAnnMap",
                mapTable="gene_info",
                mapCol="symbol",
                joins=joins1, con=con, datacache=datacache)
    )
    maps$ENZYME2PROBE <- new("ReverseAtomicAnnMap", maps$ENZYME)
    maps$PATH2PROBE <- new("ReverseAtomicAnnMap", maps$PATH)
    maps$PMID2PROBE <- new("ReverseAtomicAnnMap", maps$PMID)
    #maps$MAPCOUNTS <- createMAPCOUNTS(con, chipname)
    names(maps) <- paste(chipname, names(maps), sep="")
    maps
}

compareAnnPackages.AGDB <- function(pkgname1, pkgname2, mapprefix, probes=NULL)
{
    direct_maps <- c(
        #"ACCNUM",
        "ARACYC",
        "CHR",
        "CHRLOC",
        "ENTREZID",
        "ENZYME",
        "GENENAME",
        "GO",
        "MULTIHIT",
        "PATH",
        "PMID",
        "SYMBOL"
    )
    reverse_maps <- c(
        "GO2ALLPROBES",
        "GO2PROBE",
        "ENZYME2PROBE",
        "PATH2PROBE",
        "PMID2PROBE"
    )
    compareAnnPackages(pkgname1, pkgname2, direct_maps, reverse_maps, mapprefix, probes)
}

