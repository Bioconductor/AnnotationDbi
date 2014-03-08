### The functions below are shared across all SQLite-based ann data packages.
### They're centralized here and used in the templates of the ann data
### packages instead of being redefined over and over in every templates.

### Used at load time (in .onLoad).
dbFileConnect <- function(dbfile)
{
    ## This is a protection against dbConnect() working even with non-existing
    ## files (for our use case, the .sqlite file _must_ exist):
    if (!file.exists(dbfile))
        stop("DB file '", dbfile, "' not found")
    ## We should not need to explicitly library(RSQLite) because it's in
    ## Depends and Imports but this seems to make 'R CMD check hgu95av2.db' happier.
    library(RSQLite)
    dbConnect(SQLite(), dbname=dbfile, cache_size=64000, synchronous=0,
              flags=SQLITE_RO)
}

### Used at unload time (in .onUnload).
dbFileDisconnect <- function(dbconn)
{
    dbDisconnect(dbconn)
}



    

### Used at load time (in .onLoad) by the SQLite-based ann data package to
### dynamically add exported symbols to its namespace environment.

addToNamespaceAndExport <- function(x, value, pkgname)
{
    prefix <- sub('.db','',pkgname)    
    dc <- eval(parse(text=paste0(pkgname,":::datacache")))
    warnIf <- function(){
        if(grepl("PFAM",x)){
            bimapName <- paste0(prefix,"PFAM")
        }else{
             bimapName <- paste0(prefix,"PROSITE")
        }
        x <- dc[[bimapName]]
        message(bimapName,
 " is deprecated because up to date IPI IDs are no longer available.\n",
 "Please use select() if you need access to PFAM or PROSITE accessions. \n")   
        x
    }
    ns <- asNamespace(pkgname)    
    ## If they are 'PFAM' or 'PROSITE'
    if(any(grepl("PFAM",x), grepl("PROSITE",x))){
        assign(x, value, envir=dc) ## stash it, for later retrieval
        makeActiveBinding(sym=x, fun=warnIf, env=ns)
    }else{
        assign(x, value, envir=ns) ## Older way of doing this.
    }
    namespaceExport(ns, x)
}


mergeToNamespaceAndExport <- function(envir, pkgname)
{
    keys <- ls(envir, all.names=TRUE)
    for (key in keys)
        addToNamespaceAndExport(key, envir[[key]], pkgname)
}


## debug(AnnotationDbi:::mergeToNamespaceAndExport); 
## debug(AnnotationDbi:::addToNamespaceAndExport);
## library(org.Hs.eg.db)
## I am stuck on how to use makeActiveBinding() correctly

## It's clear I need to use it INSTEAD of assign().  But the problem
## is that x is currently pointing to a character vector and no value is available for 

## So basically, I think that warnIf needs to re-run the code that
## produces the symbols (so as to re-create them on the fly). BUT: I
## can't do that without a mountain of refactoring (templating system
## is in way).

## SO: I need a way to stash the initial envs so that I can read them
## in later on... (Save the initial env as well as the key names).

## BUT: if I have to do that, how can I hide the envir with the
## goodies in it?  Anonymous env?
## new.env(parent=emptyenv())

## Herve reminded me that we have this datacache env already.  So I will just use that...

##
