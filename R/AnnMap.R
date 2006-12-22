### =========================================================================
### Environment-like annotation maps
### -------------------------------------------------------------------------


PROBESETID_COL <- "probe_id"

extractPROBESET2GENE <- function(con)
{
    sql <- paste("SELECT ", PROBESETID_COL, ",id FROM probes", sep="")
    data <- dbGetQuery(con, sql)
    PROBESET2GENE <- data[["id"]]
    names(PROBESET2GENE) <- data[[PROBESETID_COL]]
    PROBESET2GENE
}

extractMapRightValues <- function(con, mapTable, mapCol)
{
    sql <- paste("SELECT DISTINCT ", mapCol, " FROM ", mapTable, sep="")
    data <- dbGetQuery(con, sql)
    data[[mapCol]]
}

checkProbeset <- function(probeset, datacache)
{
    if (!is.character(probeset) || any(is.na(probeset)))
        stop("invalid first argument")
    PROBESET2GENE <- get("PROBESET2GENE", envir=datacache)
    not_found <- which(!(probeset %in% names(PROBESET2GENE)))
    if (length(not_found) != 0)
        stop("value for '", probeset[not_found[1]], "' not found")
}

fromProbesetIDToValues1  <- function(con, probeset, mapTable, mapCol)
{
    sql <- paste("SELECT ", PROBESETID_COL, ",", mapCol, sep="")
    sql <- paste(sql, " FROM ", mapTable, sep="")
    sql <- paste(sql, " WHERE ", PROBESETID_COL, " IN ('", paste(probeset, collapse="','"), "')", sep="")
    dbGetQuery(con, sql)
}

getSQLJoin <- function(table1, table2, joinCol, joinType="LEFT")
{
    paste(table1, " ", joinType, " JOIN ", table2, " USING (", joinCol, ")", sep="")
}

fromProbesetIDToValues2  <- function(con, probeset, mapTable, mapCols, isLeft=TRUE)
{
    sql <- paste("SELECT ", PROBESETID_COL, ",", paste(mapCols, collapse=","), sep="")
    if (isLeft) joinType <- "LEFT" else joinType <- "INNER"
    sql <- paste(sql, " FROM ", getSQLJoin("probes", mapTable, "id", joinType), sep="")
    sql <- paste(sql, " WHERE ", PROBESETID_COL, " IN ('", paste(probeset, collapse="','"), "')", sep="")
    #cat(sql, "\n")
    dbGetQuery(con, sql)
}

fromValueToProbesetIDs  <- function(con, value, mapTable, mapCol)
{
    if (!is.character(value) || any(is.na(value)))
        stop("invalid first argument")
    sql <- paste("SELECT ", mapCol, ",", PROBESETID_COL, sep="")
    sql <- paste(sql, " FROM ", getSQLJoin(mapTable, "probes", "id"), sep="")
    sql <- paste(sql, " WHERE ", mapCol, " IN ('", paste(value, collapse="','"), "')", sep="")
    data <- dbGetQuery(con, sql)
    not_found <- which(!(value %in% data[[mapCol]]))
    if (length(not_found) != 0)
        stop("value for '", value[not_found[1]], "' not found")
    data
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Classes representing annotation maps

setClass("AnnMap", representation(con="DBIConnection", datacache="environment"))

### Maps each probeset ID to an unnamed atomic vector (character or integer)
setClass(
    "AtomicAnnMap", contains="AnnMap",
    representation(
        mapTable="character",
        mapCol="character"
    )
)

setClass("ReverseAtomicAnnMap", contains="AtomicAnnMap")

### Data are "gene based" i.e. maps of class GeneBasedAtomicAnnMap have the
### following property:
###     if probeset1 and probeset2 have the same ENTREZID
###         then map[[probeset1]] == map[[probeset2]]
setClass("GeneBasedAtomicAnnMap", contains="AtomicAnnMap")

setClass("ReverseGeneBasedAtomicAnnMap", contains="ReverseAtomicAnnMap")

setClass("NamedGeneBasedAtomicAnnMap", contains="GeneBasedAtomicAnnMap", representation(namesCol="character"))

### Maps each probeset ID to a named list of GO nodes, each GO node being
### represented as a 3-element list of the form
###   list(GOID="GO:0006470" , Evidence="IEA" , Ontology="BP")
setClass("GeneBasedGOAnnMap", contains="AnnMap")

setClass("ReverseGeneBasedGOAnnMap", contains="AnnMap", representation(all="logical")))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "db" new generic

setGeneric("db", function(object) standardGeneric("db"))

setMethod("db", "AnnMap", function(object) object@con)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "ls" new generic

setMethod("ls", signature(name="AnnMap"),
    function(name, pos, envir, all.names, pattern)
    {
        names(get("PROBESET2GENE", envir=name@datacache))
    }
)

setMethod("ls", signature(name="ReverseAtomicAnnMap"),
    function(name, pos, envir, all.names, pattern)
    {
        extractMapRightValues(db(name), name@mapTable, name@mapCol)
    }
)

setMethod("ls", signature(name="ReverseGeneBasedGOAnnMap"),
    function(name, pos, envir, all.names, pattern)
    {
        if (name@all) {
            c(extractMapRightValues(db(name), "go_bp_all", "go_id"),
              extractMapRightValues(db(name), "go_cc_all", "go_id"),
              extractMapRightValues(db(name), "go_mf_all", "go_id"))
        } else {
            c(extractMapRightValues(db(name), "go_bp", "go_id"),
              extractMapRightValues(db(name), "go_cc", "go_id"),
              extractMapRightValues(db(name), "go_mf", "go_id"))
        }
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "mget" new generic

setMethod("mget", signature(envir="AtomicAnnMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        if (length(x) == 0)
            return(list())
        checkProbeset(x, envir@datacache)
        data <- fromProbesetIDToValues1(db(envir), x, envir@mapTable, envir@mapCol)
        split(data[[envir@mapCol]], data[[PROBESETID_COL]])
    }
)

setMethod("mget", signature(envir="GeneBasedAtomicAnnMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        if (length(x) == 0)
            return(list())
        checkProbeset(x, envir@datacache)
        data <- fromProbesetIDToValues2(db(envir), x, envir@mapTable, envir@mapCol)
        split(data[[envir@mapCol]], data[[PROBESETID_COL]])
    }
)

setMethod("mget", signature(envir="ReverseGeneBasedAtomicAnnMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        if (length(x) == 0)
            return(list())
        data <- fromValueToProbesetIDs(db(envir), x, envir@mapTable, envir@mapCol)
        split(data[[PROBESETID_COL]], data[[envir@mapCol]])
    }
)

setMethod("mget", signature(envir="NamedGeneBasedAtomicAnnMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        if (length(x) == 0)
            return(list())
        checkProbeset(x, envir@datacache)
        data <- fromProbesetIDToValues2(db(envir), x, envir@mapTable, c(envir@mapCol, envir@namesCol))
        submap <- data[[envir@mapCol]]
        names(submap) <- data[[envir@namesCol]]
        split(submap, data[[PROBESETID_COL]])
    }
)

setMethod("mget", signature(envir="GeneBasedGOAnnMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        makeGONodeList <- function(GOIDs, Evidences, Ontology)
        {
            ans <- lapply(1:length(GOIDs), function(x) list(GOID=GOIDs[x], Evidence=Evidences[x], Ontology=Ontology))
            names(ans) <- GOIDs
            ans
        }
        getProbesetBasedGoMap <- function(probeset, table, Ontology)
        {
            data <- fromProbesetIDToValues2(db(envir), probeset, table, c("go_id", "evidence"), isLeft=FALSE)
            if (nrow(data) == 0)
                return(list())
            GOIDs <- split(data$go_id, data$probe_id)
            Evidences <- split(data$evidence, data$probe_id)
            sapply(names(GOIDs), function(x) makeGONodeList(GOIDs[[x]], Evidences[[x]], Ontology))
        }
        if (length(x) == 0)
            return(list())
        checkProbeset(x, envir@datacache)
        submap1 <- getProbesetBasedGoMap(x, "go_bp", "BP")
        submap2 <- getProbesetBasedGoMap(x, "go_cc", "CC")
        submap3 <- getProbesetBasedGoMap(x, "go_mf", "MF")
        ## submap1[xx][[1]] is a trick to ensure _exact_ matching! (we don't want partial matching)
        submap <- lapply(x, function(xx)
                            {
                                y <- c(submap1[xx][[1]], submap2[xx][[1]], submap3[xx][[1]])
                                if (length(y) == 0) NA else y
                            }
                        )
        names(submap) <- x
        submap
    }
)

setMethod("mget", signature(envir="ReverseGeneBasedGOAnnMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        if (length(x) == 0)
            return(list())
        cat("coming soon\n")
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "get" new generic

setMethod("get", signature(envir="AnnMap"),
    function(x, pos, envir, mode, inherits)
    {
        mget(x[1], envir)[[1]]
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### add more generics and methods here, like "length", "[[", "as.list",
### "eapply", etc...


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

### TODO: The following maps are missing for now:
###   GeneBasedAtomicAnnMap: SUMFUNC
###   misceallenous maps: CHRLENGTHS, MAPCOUNTS
allAnnMaps <- function(con, datacache)
{
    assign("PROBESET2GENE", extractPROBESET2GENE(con), envir=datacache)
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
    maps
}

benchmarks <- function(pkgname)
{
}

