PROBESETID_COL <- "probe_id"

extractProbesetBasedSubmap  <- function(con, probeset, mapTable, mapCols, isLeft=TRUE)
{
    if (!is.character(probeset) || any(is.na(probeset)))
        stop("invalid first argument")
    sql <- paste("SELECT ", PROBESETID_COL, ",", paste(mapCols, collapse=","), sep="")
    if (isLeft) joinType <- "LEFT" else joinType <- "INNER"
    sql <- paste(sql, " FROM probes ", joinType, " JOIN ", mapTable, sep="")
    sql <- paste(sql, " ON (probes.id = ", mapTable, ".id)", sep="")
    sql <- paste(sql, " WHERE ", PROBESETID_COL, " IN ('", paste(probeset, collapse="','"), "')", sep="")
    #cat(sql, "\n")
    data <- dbGetQuery(con, sql)
    if (isLeft) {
        not_found <- which(!(probeset %in% data[[PROBESETID_COL]]))
        if (length(not_found) != 0)
            stop("value for '", probeset[not_found[1]], "' not found")
    }
    data
}

getProbesetBasedReverseSubmap  <- function(con, value, mapTable, mapCol)
{
    if (!is.character(value) || any(is.na(value)))
        stop("invalid first argument")
    sql <- paste("SELECT ", mapCol, ",", PROBESETID_COL, sep="")
    sql <- paste(sql, " FROM ", mapTable, " LEFT JOIN probes", sep="")
    sql <- paste(sql, " ON (", mapTable, ".id = probes.id)", sep="")
    sql <- paste(sql, " WHERE ", mapCol, " IN ('", paste(value, collapse="','"), "')", sep="")
    data <- dbGetQuery(con, sql)
    not_found <- which(!(value %in% data[[mapCol]]))
    if (length(not_found) != 0)
        stop("value for '", value[not_found[1]], "' not found")
    data
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

setClass("ProbesetBasedVirtualMap" , representation(con="SQLiteConnection"))

setClass(
    "ProbesetBasedMap", contains="ProbesetBasedVirtualMap",
    representation(
        mapTable="character",
        mapCol="character"
    )
)

setClass("ProbesetBasedNamedMap", contains="ProbesetBasedMap", representation(namesCol="character"))

setClass("ProbesetBasedGOMap", contains="ProbesetBasedVirtualMap")

setClass("ProbesetBasedReverseMap", contains="ProbesetBasedMap")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

setGeneric("db", function(object) standardGeneric("db"))

setMethod("db", "ProbesetBasedVirtualMap",
          function(object) object@con)

setGeneric("mget", function(x, envir, mode, ifnotfound, inherits) standardGeneric("mget"))

setMethod("mget", signature(envir="ProbesetBasedMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        if (length(x) == 0)
            return(list())
        data <- extractProbesetBasedSubmap(db(envir), x, envir@mapTable, envir@mapCol)
        split(data[[envir@mapCol]], data[[PROBESETID_COL]])
    }
)

setMethod("mget", signature(envir="ProbesetBasedNamedMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        if (length(x) == 0)
            return(list())
        data <- extractProbesetBasedSubmap(db(envir), x, envir@mapTable, c(envir@mapCol, envir@namesCol))
        submap <- data[[envir@mapCol]]
        names(submap) <- data[[envir@namesCol]]
        split(submap, data[[PROBESETID_COL]])
    }
)

setMethod("mget", signature(envir="ProbesetBasedGOMap"),
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
            data <- extractProbesetBasedSubmap(db(envir), probeset, table, c("go_id", "evidence"), isLeft=FALSE)
            if (nrow(data) == 0)
                return(list())
            GOIDs <- split(data$go_id, data$probe_id)
            Evidences <- split(data$evidence, data$probe_id)
            sapply(names(GOIDs), function(x) makeGONodeList(GOIDs[[x]], Evidences[[x]], Ontology))
        }
        if (length(x) == 0)
            return(list())
        submap1 <- getProbesetBasedGoMap(x, "go_bp", "BP")
        submap2 <- getProbesetBasedGoMap(x, "go_cc", "CC")
        submap3 <- getProbesetBasedGoMap(x, "go_mf", "MF")
        ## submap1[x][[1]] is a trick to ensure _exact_ matching! (we don't want partial matching)
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

setMethod("mget", signature(envir="ProbesetBasedReverseMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        if (length(x) == 0)
            return(list())
        data <- getProbesetBasedReverseSubmap(db(envir), x, envir@mapTable, envir@mapCol)
        split(data[[PROBESETID_COL]], data[[envir@mapCol]])
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

setGeneric("get", function(x, pos, envir, mode, inherits) standardGeneric("get"))

setMethod("get", signature(envir="ProbesetBasedVirtualMap"),
    function(x, pos, envir, mode, inherits)
    {
        mget(x[1], envir)[[1]]
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### add more generics and methods here, like "length", "[[", "ls", "as.list",
### "eapply", etc...


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

### TODO: The following maps are missing for now:
###   ProbesetBasedMap: SUMFUNC
###   ProbesetBasedGOReverseMap: GO2ALLPROBES, GO2PROBE
###   misceallenous maps: CHRLENGTHS, MAPCOUNTS
allMaps <- function(con)
{
    maps <- list(
                 ACCNUM=new("ProbesetBasedMap",
                   mapTable="accessions", mapCol="accession", con=con),
                 CHR=new("ProbesetBasedMap",
                   mapTable="chromosomes", mapCol="chromosome", con=con),
                 CHRLOC=new("ProbesetBasedNamedMap",
                   mapTable="chromosome_locations", mapCol="start_location",
                   namesCol="chromosome", con=con),
                 ENTREZID=new("ProbesetBasedMap",
                   mapTable="genes", mapCol="gene_id", con=con),
                 ENZYME=new("ProbesetBasedMap",
                   mapTable="ec", mapCol="ec_number", con=con),
                 GENENAME=new("ProbesetBasedMap",
                   mapTable="gene_info", mapCol="gene_name", con=con),
                 GO=new("ProbesetBasedGOMap",
                   con=con),
                 MAP=new("ProbesetBasedMap",
                   mapTable="cytogenetic_locations", mapCol="cytogenetic_location", con=con),
                 OMIM=new("ProbesetBasedMap",
                   mapTable="omim", mapCol="omim_id", con=con),
                 PATH=new("ProbesetBasedMap",
                   mapTable="kegg", mapCol="kegg_id", con=con),
                 PFAM=new("ProbesetBasedNamedMap",
                   mapTable="pfam", mapCol="pfam_id", namesCol="ipi_id", con=con),
                 PMID=new("ProbesetBasedMap",
                   mapTable="pubmed", mapCol="pubmed_id", con=con),
                 PROSITE=new("ProbesetBasedNamedMap",
                   mapTable="prosite", mapCol="prosite_id", namesCol="ipi_id", con=con),
                 REFSEQ=new("ProbesetBasedMap",
                   mapTable="refseq", mapCol="accession", con=con),
                 SYMBOL=new("ProbesetBasedMap",
                   mapTable="gene_info", mapCol="symbol", con=con),
                 UNIGENE=new("ProbesetBasedMap",
                   mapTable="unigene", mapCol="unigene_id", con=con)
                 )
    maps$ENZYME2PROBE <- new("ProbesetBasedReverseMap", maps$ENZYME)
    maps$PATH2PROBE <- new("ProbesetBasedReverseMap", maps$PATH)
    maps$PMID2PROBE <- new("ProbesetBasedReverseMap", maps$PMID)
    maps
}

