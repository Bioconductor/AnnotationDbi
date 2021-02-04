AnnotationDb <-
    function(conn,  ...)
{
    .AnnotationDb$new(conn=conn,  ...)
}

## setMethod(show, "AnnotationDb",
##    function(object)
## {
##     cat("class:", class(object), "\n")
## })

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Low-level accessor (not exported).
###


## These two things should just go away and be replaced with:x$conn
## .getConn <- function(envir) get("conn", envir=envir, inherits=FALSE)
## txdbConn <- function(txdb) .getConn(txdb@envir)


# ## On the fence about whether or not to export dbconn(), it could be useful,
# ## but it is also quite similar to say: org.Hs.eg_dbconn(), so if we export
# ## it, it will have to deprecate the older ways.  For now, I will just call it
# ## using AnnotationDbi:::dbconn(db), Also AnnDbBiMaps have something called
# ## dbconn() (lowercase "c") - blergh...
# setMethod("dbconn", "AnnotationDb",
#     function(x) x$conn
# )

## Accesor for packageName
setMethod("packageName", "AnnotationDb",
    function(x) x$packageName
)

## Overload metadata for AnnotationDb
setMethod("metadata", "AnnotationDb",
    function(x) dbReadTable(dbconn(x), "metadata")
)


## Get the species AnnotationDb
setMethod("species", "AnnotationDb",
    function(object){
      res <- as.character(dbEasyQuery(dbconn(object),
        "SELECT value FROM metadata WHERE name='ORGANISM'"))
      if(res == "character(0)"){ ## then try again.
        res <- as.character(dbEasyQuery(dbconn(object),
         "SELECT value FROM metadata WHERE name='Organism'"))
      }
      if(res == "character(0)"){ ## then try again.
        res <- as.character(dbEasyQuery(dbconn(object),
         "SELECT value FROM metadata WHERE name='Genus and Species'"))
      }
      res
    }
)


## Try to make this generic
setMethod("show", "AnnotationDb",
    function(object)
    {
        cat(class(object), "object:\n")
        metadata <- metadata(object)
        for (i in seq_len(nrow(metadata))) {
            cat("| ", metadata[i, "name"], ": ", metadata[i, "value"],
                "\n", sep="")
        }
        message("\n","Please see: help('select') for usage information", sep="")
    }
)

setMethod("saveDb", "AnnotationDb",
    function(x, file)
    {
        if (!isSingleString(file))
          stop("'file' must be a single string")
        sqliteCopyDatabase(dbconn(x), file)
        return(x) ## return the thing you just saved.
    }
)

setMethod("columns", "AnnotationDb",
    function(x) 
    {
        tables <- dbListTables(x$conn)
        sapply(tables, dbListFields, conn=x$conn)
    }
)

cols <- function(x)
    .Deprecated("columns")

.selectWarnReact <- function(x, keys, columns, keytype, ...){
    extraArgs <- list(...)
    if(missing(keytype)){
        .selectReact(x, keys, columns, keytype=extraArgs[["kt"]])
    }else{
        .selectReact(x, keys, columns, keytype)
    }
}

## library(AnnotationDbi)
## library(RSQLite)
## library(GenomicFeatures); fl = system.file("extdata", "UCSC_knownGene_sample.sqlite", package="GenomicFeatures")
## conn = dbConnect(SQLite(), fl)
## loadDb(fl)                                 ## bug
## loadFeatures(fl)                           ## other bug
## GenomicFeatures:::TxDb(conn)               ## bug
## AnnotationDbi:::loadDb(fl)                 ## other bug


## TODO: add option to replace multi-matches with NAs or to just remove them.
## To cleanly handle having 'multiVals' being EITHER a FUN or something else:
## DO like: if(is.function(multiVals)){}else{match.arg(multiVals)}

###############################################################################
## New method just to make it easier to access the sqlite file (and thus make 
## it easier to make use of great new stuff like dplyr)

## Basically I want this thing to return the sqlite file name path as a string

## make dbconn() and dbfile() methods for AnnotationDb objects (and export it)
setMethod("dbconn", "AnnotationDb", function(x) x$conn)
setMethod("dbfile", "AnnotationDb", function(x) dbfile(dbconn(x)))
