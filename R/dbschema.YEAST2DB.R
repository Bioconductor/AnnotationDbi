### =========================================================================
### Create all data objects for an annotation data package
### with db schema YEAST2DB
### -------------------------------------------------------------------------

### TODO: The following maps are missing for now:
###   GeneBasedAtomicAnnMap: SUMFUNC
###   misceallenous maps: CHRLENGTHS
### 'chipname' is this chip "shortname" e.g. "yeast2" for the yeast2db package
createAnnDataObjects.YEAST2DB <- function(chipname, con, datacache)
{
    cachePROBESET2GENE("sgd INNER JOIN probes USING(systematic_name)", con, datacache)
    joins <- "INNER JOIN sgd USING (id) INNER JOIN probes USING (systematic_name)"
    maps <- list(
            ALIAS=new("GeneBasedAtomicAnnMap",
                mapTable="gene2alias",
                mapCol="alias",
                joins=joins, con=con, datacache=datacache),
            CHR=new("GeneBasedAtomicAnnMap",
                mapTable="chromosome_features",
                mapCol="chromosome",
                joins=joins, con=con, datacache=datacache),
            CHRLOC=new("NamedGeneBasedAtomicAnnMap",
                mapTable="chromosome_features",
                mapCol="start",
                namesCol="chromosome",
                joins=joins, con=con, datacache=datacache),
            DESCRIPTION=new("GeneBasedAtomicAnnMap",
                mapTable="chromosome_features",
                mapCol="feature_description",
                joins=joins, con=con, datacache=datacache),
            ENZYME=new("GeneBasedAtomicAnnMap",
                mapTable="ec",
                mapCol="ec_number",
                joins=joins, con=con, datacache=datacache),
            PATH=new("GeneBasedAtomicAnnMap",
                mapTable="kegg",
                mapCol="kegg_id",
                joins=joins, con=con, datacache=datacache)
    )
    #maps$MAPCOUNTS <- createMAPCOUNTS(con, chipname)
    names(maps) <- paste(chipname, names(maps), sep="")
    maps
}

