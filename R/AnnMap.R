### =========================================================================
### Environment-like annotation maps
### -------------------------------------------------------------------------


PROBESETID_COL <- "probe_id"


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### SQL helper functions

getTable <- function(con, table)
{
    sql <- paste("SELECT * FROM ", table, sep="")
    dbGetQuery(con, sql)
}

countUniqueColValues <- function(con, table, col)
{
    sql <- paste("SELECT COUNT(DISTINCT ", col, ") FROM ", table, sep="")
    dbGetQuery(con, sql)[[1]]
}

uniqueColValues <- function(con, table, col)
{
    sql <- paste("SELECT DISTINCT ", col, " FROM ", table, sep="")
    dbGetQuery(con, sql)[[col]]
}

sqlJoin <- function(table1, table2, using, joinType="LEFT")
{
    paste(table1, " ", joinType, " JOIN ", table2, " USING (", using, ")", sep="")
}

subsetTable <- function(con, table, index, subset, cols, add_probes=FALSE)
{
    cols <- append(index, cols)
    if (add_probes) {
        if (!(PROBESETID_COL %in% cols))
            cols <- append(cols, PROBESETID_COL)
        table <- sqlJoin(table, "probes", "id", "INNER")
    }
    sql <- paste("SELECT ", paste(cols, collapse=","),
                 " FROM ", table,
                 " WHERE ", index, " IN ('", paste(subset, collapse="','"), "')",
                 sep="")
    dbGetQuery(con, sql)
}

fromValueToProbesetIDs  <- function(con, value, mapTable, mapCol)
{
    if (!is.character(value) || any(is.na(value)))
        stop("invalid first argument")
    sql <- paste("SELECT ", mapCol, ",", PROBESETID_COL, sep="")
    sql <- paste(sql, " FROM ", sqlJoin(mapTable, "probes", "id"), sep="")
    sql <- paste(sql, " WHERE ", mapCol, " IN ('", paste(value, collapse="','"), "')", sep="")
    data <- dbGetQuery(con, sql)
    not_found <- which(!(value %in% data[[mapCol]]))
    if (length(not_found) != 0)
        stop("value for '", value[not_found[1]], "' not found")
    data
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Other helper functions

cachePROBESET2GENE <- function(con, datacache)
{
    data <- getTable(con, "probes")
    PROBESET2GENE <- data[["id"]]
    names(PROBESET2GENE) <- data[[PROBESETID_COL]]
    assign("PROBESET2GENE", PROBESET2GENE, envir=datacache)
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

GOtables <- function(all)
{
    tables <- c("go_bp", "go_cc", "go_mf")
    if (all)
        tables <- paste(tables, "_all", sep="")
    tables
}

normaliseSubmapKeys <- function(submap, keys)
{
    ## Old version, slow
    #ans <- submap[keys]; ans[sapply(ans, is.null)] <- NA;
    ## New version, slightly faster
    ans <- lapply(keys, function(x) {y <- submap[x][[1]]; if (is.null(y)) NA else y})
    names(ans) <- keys
    ans
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

### Maps a GO term to a named character vector containing probeset IDs.
### Each element of the vector is named with the Evidence code.
setClass("ReverseGeneBasedGOAnnMap", contains="AnnMap", representation(all="logical"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "db" new generic

setGeneric("db", function(object) standardGeneric("db"))

setMethod("db", "AnnMap", function(object) object@con)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "length" generic

setMethod("length", "AnnMap",
    function(x)
    {
        length(get("PROBESET2GENE", envir=x@datacache))
    }
)

setMethod("length", "ReverseAtomicAnnMap",
    function(x)
    {
        countUniqueColValues(db(x), x@mapTable, x@mapCol)
    }
)

setMethod("length", "ReverseGeneBasedGOAnnMap",
    function(x)
    {
        mapTables <- GOtables(x@all)
        ## Our assumption is that a given go_id can only belong to 1 of the 3
        ## ontologies!
        countUniqueColValues(db(x), mapTables[1], "go_id") +
            countUniqueColValues(db(x), mapTables[2], "go_id") +
            countUniqueColValues(db(x), mapTables[3], "go_id")
    }
)


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
        uniqueColValues(db(name), name@mapTable, name@mapCol)
    }
)

setMethod("ls", signature(name="ReverseGeneBasedGOAnnMap"),
    function(name, pos, envir, all.names, pattern)
    {
        mapTables <- GOtables(name@all)
        ## Our assumption is that a given go_id can only belong to 1 of the 3
        ## ontologies! If we are wrong, then we should apply unique() to the
        ## result of this concantenation and fix the "length" method above.
        c(uniqueColValues(db(name), mapTables[1], "go_id"),
          uniqueColValues(db(name), mapTables[2], "go_id"),
          uniqueColValues(db(name), mapTables[3], "go_id"))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" generic

setMethod("show", "AnnMap",
    function(object)
    {
        cat("An object of class “", class(object), "”\n", sep="")
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
        cols <- envir@mapCol
        data <- subsetTable(db(envir), envir@mapTable, PROBESETID_COL, x, cols)
        submap <- split(data[[envir@mapCol]], data[[PROBESETID_COL]])
        normaliseSubmapKeys(submap, x)
    }
)

setMethod("mget", signature(envir="GeneBasedAtomicAnnMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        if (length(x) == 0)
            return(list())
        checkProbeset(x, envir@datacache)
        cols <- envir@mapCol
        data <- subsetTable(db(envir), envir@mapTable, PROBESETID_COL, x, cols, add_probes=TRUE)
        submap <- split(data[[envir@mapCol]], data[[PROBESETID_COL]])
        normaliseSubmapKeys(submap, x)
    }
)

setMethod("mget", signature(envir="ReverseGeneBasedAtomicAnnMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        if (length(x) == 0)
            return(list())
        data <- fromValueToProbesetIDs(db(envir), x, envir@mapTable, envir@mapCol)
        submap <- split(data[[PROBESETID_COL]], data[[envir@mapCol]])
        normaliseSubmapKeys(submap, x)
    }
)

setMethod("mget", signature(envir="NamedGeneBasedAtomicAnnMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        if (length(x) == 0)
            return(list())
        checkProbeset(x, envir@datacache)
        cols <- c(envir@mapCol, envir@namesCol)
        data <- subsetTable(db(envir), envir@mapTable, PROBESETID_COL, x, cols, add_probes=TRUE)
        submap <- data[[envir@mapCol]]
        names(submap) <- data[[envir@namesCol]]
        submap <- split(submap, data[[PROBESETID_COL]])
        normaliseSubmapKeys(submap, x)
    }
)

setMethod("mget", signature(envir="GeneBasedGOAnnMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        if (length(x) == 0)
            return(list())
        checkProbeset(x, envir@datacache)
        makeGONodeList <- function(GOIDs, Evidences, Ontology)
        {
            ans <- lapply(1:length(GOIDs), function(x) list(GOID=GOIDs[x], Evidence=Evidences[x], Ontology=Ontology))
            names(ans) <- GOIDs
            ans
        }
        cols <- c("go_id", "evidence")
        getPartialSubmap <- function(table, Ontology)
        {
            data <- subsetTable(db(envir), table, PROBESETID_COL, x, cols, add_probes=TRUE)
            if (nrow(data) == 0)
                return(list())
            GOIDs <- split(data[["go_id"]], data[[PROBESETID_COL]])
            Evidences <- split(data[["evidence"]], data[[PROBESETID_COL]])
            sapply(names(GOIDs), function(x) makeGONodeList(GOIDs[[x]], Evidences[[x]], Ontology))
        }
        submap1 <- getPartialSubmap("go_bp", "BP")
        submap2 <- getPartialSubmap("go_cc", "CC")
        submap3 <- getPartialSubmap("go_mf", "MF")
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
        cols <- "evidence"
        getPartialSubmap <- function(table)
        {
            data <- subsetTable(db(envir), table, "go_id", x, cols, add_probes=TRUE)
            if (nrow(data) == 0)
                return(list())
            submap <- data[[PROBESETID_COL]]
            names(submap) <- data[["evidence"]]
            split(submap, data[["go_id"]])
        }
        mapTables <- GOtables(envir@all)
        submap <- c(getPartialSubmap(mapTables[1]),
                    getPartialSubmap(mapTables[2]),
                    getPartialSubmap(mapTables[3]))
        normaliseSubmapKeys(submap, x)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "as.list" generic

setMethod("as.list", "AtomicAnnMap",
    function(x, ...)
    {
        cat("coming soon...\n")
    }
)

setMethod("as.list", "GeneBasedAtomicAnnMap",
    function(x, ...)
    {
        cat("coming soon...\n")
    }
)

setMethod("as.list", "ReverseGeneBasedAtomicAnnMap",
    function(x, ...)
    {
        cat("coming soon...\n")
    }
)

setMethod("as.list", "NamedGeneBasedAtomicAnnMap",
    function(x, ...)
    {
        cat("coming soon...\n")
    }
)

setMethod("as.list", "GeneBasedGOAnnMap",
    function(x, ...)
    {
        cat("coming soon...\n")
    }
)

setMethod("as.list", "ReverseGeneBasedGOAnnMap",
    function(x, ...)
    {
        cat("coming soon...\n")
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
### The "[[" generic

setMethod("[[", "AnnMap",
    function(x, i, j, ...)
    {
        # 'x' is guaranteed to be a "AnnMap" object (if it's not, then the
        # method dispatch algo will not call this method in the first place),
        # so nargs() is guaranteed to be >= 1
        if (nargs() >= 3)
            stop("too many subscripts")
        subscripts <- list(...)
        if (!missing(i))
            subscripts$i <- i
        if (!missing(j))
            subscripts$j <- j
        # At this point, 'subscripts' should be guaranteed
        # to be of length <= 1
        if (length(subscripts) == 0)
            stop("no index specified")
        i <- subscripts[[1]]
        if (length(i) < 1)
            stop("attempt to select less than one element")
        if (length(i) > 1)
            stop("attempt to select more than one element")
        if (!is.character(i) || is.na(i))
            stop("wrong argument for subsetting an object of class “", class(x), "“")
        get(i, envir=x)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### add more generics and methods here, like "eapply", etc...


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

createMAPCOUNTS <- function(maps)
{
    sapply(maps, length)
}

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

