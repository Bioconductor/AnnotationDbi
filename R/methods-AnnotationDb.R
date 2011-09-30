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


## On the fence about whether or not to export dbConn(), it could be useful,
## but it is also quite similar to say: org.Hs.eg_dbconn(), so if we export
## it, it will have to deprecate the older ways.  For now, I will just call it
## using AnnotationDbi:::dbConn(db), Also AnnDbBiMaps have something called
## dbconn() (lowercase "c") - blergh...
setMethod("dbConn", "AnnotationDb",
    function(x) x$conn
)

## Accesor for packageName
setMethod("packageName", "AnnotationDb",
    function(x) x$packageName
)

## Overload metadata for AnnotationDb
setMethod("metadata", "AnnotationDb",
    function(x) dbReadTable(dbConn(x), "metadata")
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
    }
)

setMethod("saveDb", "AnnotationDb",
    function(x, file)
    {
        if (!isSingleString(file))
          stop("'file' must be a single string")
        sqliteCopyDatabase(dbConn(x), file)
    }
)

setMethod("cols", "AnnotationDb",
    function(x) 
    {
        tables <- dbListTables(x$conn)
        sapply(tables, dbListFields, conn=x$conn)
    }
)

setMethod("names", "AnnotationDb",
    function(x) 
    {
        dbListTables(x$conn)
    }
)

## library(AnnotationDbi)
## library(RSQLite)
## library(GenomicFeatures); fl = system.file("extdata", "UCSC_knownGene_sample.sqlite", package="GenomicFeatures")
## conn = dbConnect(SQLite(), fl)
## loadDb(fl)                                 ## bug
## loadFeatures(fl)                           ## other bug
## GenomicFeatures:::TranscriptDb(conn)       ## bug
## AnnotationDbi:::loadDb(fl)                 ## other bug

