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


## Get the species AnnotationDb
setMethod("species", "AnnotationDb",
    function(x){
      res <- as.character(dbEasyQuery(dbConn(x),
        "SELECT value FROM metadata WHERE name='ORGANISM'"))
      if(res == "character(0)"){ ## then try again.
        res <- as.character(dbEasyQuery(dbConn(x),
         "SELECT value FROM metadata WHERE name='Organism'"))
      }
      if(res == "character(0)"){ ## then try again.
        res <- as.character(dbEasyQuery(dbConn(x),
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

setMethod("columns", "AnnotationDb",
    function(x) 
    {
        tables <- dbListTables(x$conn)
        sapply(tables, dbListFields, conn=x$conn)
    }
)

## Remove this special cols function after 2.13 has released
cols <- function(x){
    ## deprecation method
    msg <- "'cols' has been deprecated and replaced by 'columns' for versions of Bioc that are higher than 2.13.  Please use 'columns' anywhere that you previously used 'cols'"
    warning(paste(strwrap(msg, exdent=2), collapse="\n"))
    
    ## then call columns
    columns(x)
}


## Remove this special warning function after 2.13 has released
.colsArgumentWarning <- function(){
    ## deprecation method
    msg <- "The 'cols' argument has been deprecated and replaced by 'columns' for versions of Bioc that are higher than 2.13.  Please use the 'columns' argument anywhere that you previously used 'cols'"
    warning(paste(strwrap(msg, exdent=2), collapse="\n"))
}

##  Remove this select warning function after 2.13 has released
.selectWarnJT <- function(x, keys, columns, keytype, ...){
    
    ## remove condition after 2.13
    extraArgs <- list(...)
    if("cols" %in% names(extraArgs)){
        ## warn the user about the old argument
        .colsArgumentWarning()
        ## then call it using cols in place of columns
        ## in this case columns will hold the value meant for keytype?
        if(missing(keytype)){
            if(missing(columns)){
                .select(x, keys, extraArgs[["cols"]], keytype=extraArgs[["kt"]],
                        jointype=extraArgs[["jointype"]] )
            }else{
                .select(x, keys, extraArgs[["cols"]], keytype = columns,
                        jointype=extraArgs[["jointype"]] )
            }
        }else{
            if(missing(keytype)){
                .select(x, keys, extraArgs[["cols"]], keytype=extraArgs[["kt"]],
                        jointype=extraArgs[["jointype"]] )
            }else{
                .select(x, keys, extraArgs[["cols"]], keytype = keytype,
                        jointype=extraArgs[["jointype"]] )
            }
        }
    }else{
        if(missing(keytype)){
            .select(x, keys, columns, keytype=extraArgs[["kt"]],
                    jointype=jointype)
        }else{
            .select(x, keys, columns, keytype, jointype=jointype)            
        }
    }
}

##  Remove this select warning function after 2.13 has released
.selectWarnInp <- function(x, keys, columns, keytype, ...){
    
    ## remove condition after 2.13
    extraArgs <- list(...)
    if("cols" %in% names(extraArgs)){
        ## warn the user about the old argument
        .colsArgumentWarning()
        ## then call it using cols in place of columns
        if(missing(keytype)){
            if(missing(columns)){
                .selectInp(x, keys, extraArgs[["cols"]],
                           keytype=extraArgs[["kt"]], ... )
            }else{
                .selectInp(x, keys, extraArgs[["cols"]], keytype = columns,
                           ... )
            }              
        }else{
            if(missing(keytype)){
                .selectInp(x, keys, extraArgs[["cols"]],
                           keytype=extraArgs[["kt"]], ... )
            }else{
                .selectInp(x, keys, extraArgs[["cols"]], keytype = keytype,
                           ... )             
            }
        }
    }else{
        if(missing(keytype)){
            .selectInp(x, keys, columns, keytype=extraArgs[["kt"]])
        }else{
            .selectInp(x, keys, columns, keytype)
        }
    }
}
    
##  Remove this select warning function after 2.13 has released
.selectWarnReact <- function(x, keys, columns, keytype, ...){
    
    ## remove condition after 2.13
    extraArgs <- list(...)
    if("cols" %in% names(extraArgs)){
        ## warn the user about the old argument
        .colsArgumentWarning()
        ## then call it using cols in place of columns  
        if(missing(keytype)){
            if(missing(columns)){
                .selectReact(x, keys, extraArgs[["cols"]],
                             keytype=extraArgs[["kt"]], ... )
            }else{
                .selectReact(x, keys, extraArgs[["cols"]], keytype = columns,
                             ... )
            }
        }else{
            if(missing(keytype)){
                .selectReact(x, keys, extraArgs[["cols"]],
                             keytype=extraArgs[["kt"]], ... )
            }else{
                .selectReact(x, keys, extraArgs[["cols"]], keytype=keytype,
                             ... )
            }
        }
    }else{
        if(missing(keytype)){
            .selectReact(x, keys, columns, keytype=extraArgs[["kt"]])
        }else{
            .selectReact(x, keys, columns, keytype)
        }
    }
}


## library(AnnotationDbi)
## library(RSQLite)
## library(GenomicFeatures); fl = system.file("extdata", "UCSC_knownGene_sample.sqlite", package="GenomicFeatures")
## conn = dbConnect(SQLite(), fl)
## loadDb(fl)                                 ## bug
## loadFeatures(fl)                           ## other bug
## GenomicFeatures:::TranscriptDb(conn)       ## bug
## AnnotationDbi:::loadDb(fl)                 ## other bug

