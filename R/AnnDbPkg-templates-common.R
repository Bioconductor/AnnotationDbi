### The functions below are shared across all SQLite-based ann data packages.
### They're centralized here and used in the templates of the ann data
### packages instead of being redefined over and over in every templates.

### Used at load time (in .onLoad) by the SQLite-based ann data package to
### dynamically add exported symbols to its namespace environment.

.kosherPkg <- function(pkgname){
    if(pkgname %in% c('PFAM.db','mpedbarray.db','pedbarrayv9.db',
                      'pedbarrayv10.db','humanCHRLOC','mouseCHRLOC',
                      'ratCHRLOC')){
        return(FALSE)
    }else{
        return(TRUE)
    }
}

addToNamespaceAndExport <- function(x, value, pkgname)
{
    prefix <- sub('.db','',pkgname)    
    dc <- eval(parse(text=paste0(pkgname,":::datacache")))
    warnIfDep <- function(){
        if(grepl("CHR$",x)){
            bimapName <- paste0(prefix,"CHR")
        }else if(grepl("CHRLENGTHS$",x)){
            bimapName <- paste0(prefix,"CHRLENGTHS")
        }else if(grepl("CHRLOC$",x)){
            bimapName <- paste0(prefix,"CHRLOC")
        }else if(grepl("CHRLOCEND$",x)){
            bimapName <- paste0(prefix,"CHRLOCEND")
        }
        x <- dc[[bimapName]]
        x
    }
    warnIfDef <- function(){
        if(grepl("PFAM",x)){
            bimapName <- paste0(prefix,"PFAM")
        }else{
             bimapName <- paste0(prefix,"PROSITE")
        }
        x <- dc[[bimapName]]
        msg = wmsg(paste0(bimapName,
 " is defunct. ",
 "Please use select() if you need access to PFAM or PROSITE accessions. \n"))
        if(interactive()){
            .Defunct(msg=msg)
        }
    }
    ns <- asNamespace(pkgname)
    ## Uncomment after the release
    if(grepl("CHR",x) &&  .kosherPkg(pkgname)){
        assign(x, value, envir=dc) ## stash it, for later retrieval
        makeActiveBinding(sym=x, fun=warnIfDep, env=ns)
    }else
    if(any(grepl("PFAM",x), grepl("PROSITE",x)) &&  .kosherPkg(pkgname)){
        assign(x, value, envir=dc) ## stash it
        makeActiveBinding(sym=x, fun=warnIfDef, env=ns)
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
