### =========================================================================
### Create all data objects for a given annotation data package
### -------------------------------------------------------------------------

### TODO: The following maps are missing for now:
###   GeneBasedAtomicAnnMap: SUMFUNC
###   misceallenous maps: CHRLENGTHS
### 'chipname' is this chip "shortname" e.g. "hgu95av2" for the hgu95av2db package
initAnnDataObjects <- function(chipname, con, datacache)
{
    cachePROBESET2GENE(con, datacache)
    maps <- list(
                 ACCNUM=new("AtomicAnnMap",
                   mapTable="accessions", mapCol="accession", con=con, datacache=datacache),
                 CHR=new("GeneBasedAtomicAnnMap",
                   mapTable="chromosomes", mapCol="chromosome", con=con, datacache=datacache),
                 CHRLOC=new("NamedGeneBasedAtomicAnnMap",
                   mapTable="chromosome_locations", mapCol="start_location",
                   namesCol="chromosome", con=con, datacache=datacache),
                 ENTREZID=new("GeneBasedAtomicAnnMap",
                   mapTable="genes", mapCol="gene_id", con=con, datacache=datacache),
                 ENZYME=new("GeneBasedAtomicAnnMap",
                   mapTable="ec", mapCol="ec_number", con=con, datacache=datacache),
                 GENENAME=new("GeneBasedAtomicAnnMap",
                   mapTable="gene_info", mapCol="gene_name", con=con, datacache=datacache),
                 GO=new("GeneBasedGOAnnMap",
                   con=con, datacache=datacache),
                 GO2ALLPROBES=new("ReverseGeneBasedGOAnnMap",
                   all=TRUE, con=con, datacache=datacache),
                 GO2PROBE=new("ReverseGeneBasedGOAnnMap",
                   all=FALSE, con=con, datacache=datacache),
                 MAP=new("GeneBasedAtomicAnnMap",
                   mapTable="cytogenetic_locations", mapCol="cytogenetic_location", con=con, datacache=datacache),
                 OMIM=new("GeneBasedAtomicAnnMap",
                   mapTable="omim", mapCol="omim_id", con=con, datacache=datacache),
                 PATH=new("GeneBasedAtomicAnnMap",
                   mapTable="kegg", mapCol="kegg_id", con=con, datacache=datacache),
                 PFAM=new("NamedGeneBasedAtomicAnnMap",
                   mapTable="pfam", mapCol="pfam_id", namesCol="ipi_id", con=con, datacache=datacache),
                 PMID=new("GeneBasedAtomicAnnMap",
                   mapTable="pubmed", mapCol="pubmed_id", con=con, datacache=datacache),
                 PROSITE=new("NamedGeneBasedAtomicAnnMap",
                   mapTable="prosite", mapCol="prosite_id", namesCol="ipi_id", con=con, datacache=datacache),
                 REFSEQ=new("GeneBasedAtomicAnnMap",
                   mapTable="refseq", mapCol="accession", con=con, datacache=datacache),
                 SYMBOL=new("GeneBasedAtomicAnnMap",
                   mapTable="gene_info", mapCol="symbol", con=con, datacache=datacache),
                 UNIGENE=new("GeneBasedAtomicAnnMap",
                   mapTable="unigene", mapCol="unigene_id", con=con, datacache=datacache)
                 )
    maps$ENZYME2PROBE <- new("ReverseGeneBasedAtomicAnnMap", maps$ENZYME)
    maps$PATH2PROBE <- new("ReverseGeneBasedAtomicAnnMap", maps$PATH)
    maps$PMID2PROBE <- new("ReverseGeneBasedAtomicAnnMap", maps$PMID)
    names(maps) <- paste(chipname, names(maps), sep="")
    maps[[paste(chipname, "MAPCOUNTS", sep="")]] <- createMAPCOUNTS(maps)
    maps
}

benchmarks <- function(pkgname)
{
}

