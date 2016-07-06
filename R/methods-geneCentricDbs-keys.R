### =========================================================================
### keys() and keytype() - related functions for gene-centric Dbs
### -------------------------------------------------------------------------


## Three helpers for deprecating keytypes
## One to just list the bum keytypes
.listDeprecatedKeytypes <- function(){
   c('CHR','CHRLOC','CHRLOCEND') ## Uncomment after the release
}
## Another for keytypes to remove unwanted keytypes
.filterDeprecatedKeytypes <- function(keytypes){
    keytypes[!(keytypes %in% .listDeprecatedKeytypes())]
}

## this is a 'standard' warning for people who try to use keys or cols
## that are no longer valid due to being deprecated
.deprecatedColsMessage <- function(){
    depCols <- paste(.listDeprecatedKeytypes(), collapse="','")
    warning(wmsg(paste0("Accessing gene location information via '",
                        depCols,"' is deprecated. Please use a range ",
                        "based accessor like genes(), or select() with ",
                        "columns values like TXCHROM and TXSTART ",
                        "on a TxDb or OrganismDb object instead.\n")))
}

## And one for keys and select to warn if the user tries to use them
.checkForDeprecatedKeytype <- function(keytype){
    if(any(.listDeprecatedKeytypes() %in% keytype )){
        .deprecatedColsMessage()
    }
}

## Need an accessor for getting the central ID for a DB (when appropriate)
.getCentralID <- function(x){
  as.character(dbQuery(dbconn(x),
                       "SELECT value FROM metadata WHERE name='CENTRALID'"))
}

## Sometimes we need to translate a centralID into a central keytype.
chooseCentralOrgPkgSymbol <- function(x){
  centralID <- .getCentralID(x)
  keytype <- switch(EXPR = centralID,
                    "EG" = "ENTREZID",
                    "TAIR" = "TAIR",
                    "ORF" = "ORF",
                    "GID" = "GID")
  keytype
}


## keys methods return the possible primary keys.  So for EG based packages,
## this will be the viable entrez gene IDs.
## Must use SELECT DISTINCT for now because some packages like ag.db
## (Arabidopsis) have repeated probe ids in the probes table (those are the
## probe ids that hit multiple genes).
## TODO: When 'x' has the new slot containing the package name, use
## dbUniqueVals() (defined in SQL.R) and pass pkgname:::datacache to it.
## dbUniqueVals() is what's used behind the scene by the Lkeys/Rkeys/keys
## methods for AnnDbBimap objects so the "keys" methods below will give a
## consistent answer (and will take advantage of the cache).
## helper to get keys
.queryForKeys <- function(x, keytype){
  if(class(x)=="ChipDb"){
    x <- .getOrgPkg(x)
  }
  table <- .getDBLocs(x, keytype)
  field <- .getDBLocs(x, keytype, value="field")
  sql <- paste("SELECT DISTINCT",field,"FROM",table)
  res <- dbQuery(dbconn(x), sql)
  t(res)
}


.legacyKeys <- function(x, keytype){
  ## have to swap keytype
  ## keytype <- .swapSymbolExceptions(x, keytype)
  keytype <- .simplifyCols(x, keytype)
  
  ## Some org packages may have entrez genes in weird places...
  centralID <- .getCentralID(x)
  EGgeneTable <- character()
  if(centralID == "EG" || centralID == "ORF"){
    EGgeneTable <- "genes"
  }else if(centralID == "TAIR"){
    EGgeneTable <- "entrez_genes"
  }
  ## now decide
  if(class(x) == "OrgDb" && species(x) != "Plasmodium falciparum"){
    res <- switch(EXPR = keytype,
                  "ENTREZID" = dbQuery(dbconn(x),
                    paste("SELECT gene_id FROM", EGgeneTable), 1L),
                  "TAIR" = dbQuery(dbconn(x),
                    "SELECT gene_id FROM genes", 1L),
                  "ORF" = dbQuery(dbconn(x),
                    "SELECT systematic_name FROM sgd", 1L),
                  "PROBEID" =
                     stop("PROBEID is not supported for Organism packages"),
                  .queryForKeys(x, keytype))
  }
  if(class(x) == "OrgDb" && species(x) == "Plasmodium falciparum"){
    res <-  switch(EXPR = keytype,
                   "ORF" = dbQuery(dbconn(x),
                     paste("SELECT gene_id FROM", EGgeneTable), 1L),
                   .queryForKeys(x, keytype))
  }
  if(class(x) == "ChipDb"){
    res <- switch(EXPR = keytype,
                  "ENTREZID" = dbQuery(dbconn(x),
                    "SELECT gene_id FROM probes", 1L),
                  "PROBEID" =  dbQuery(dbconn(x),
                    "SELECT DISTINCT probe_id FROM probes", 1L),
                  .queryForKeys(x, keytype))
  }
  if(class(x) == "GODb"){
    res <- switch(EXPR = keytype,
                  "GOID" =  dbQuery(dbconn(x),
                    "SELECT DISTINCT go_id FROM go_term", 1L),
                  .queryForKeys(x, keytype))
  }
  as.character(res[!is.na(res)])
}


## special functions for newer NOSCHEMA_DB's
.deriveTableNameFromField <- function(field, x){
    if(class(x)=="ChipDb"){
        y <- x ## Switcheroo
        x <- .getOrgPkg(x)
        try(.attachDB(x,y), silent=TRUE) ## not a disaster if we fail
    }
    con <- dbconn(x)
    tables <- .getDataTables(con)
    if(exists("y", inherits=FALSE)){
        tables <- c("c.probes", tables)
    }
    colTabs <- sapply(tables, FUN=dbListFields, con=con)
    m <- unlist2(sapply(colTabs, match, field))  ## cannot ever be repeated
    tab <- names(m)[!is.na(m)]
    if(length(tab)!=1){stop("Two fields in the source DB have the same name.")}
    tab
}

.noSchemaKeys <- function(x, keytype){
    tab <- .deriveTableNameFromField(field=keytype, x)
    ## So now we know table name (tab) and field (keytype)
    if(class(x)=="ChipDb"){
        y <- x ## Switcheroo
        x <- .getOrgPkg(x)
        try(.attachDB(x,y), silent=TRUE) ## not a disaster if we fail
    }
    sql <- paste("SELECT",keytype,"FROM",tab)
    res <- dbQuery(dbconn(x), sql, 1L)
    as.character(res[!is.na(res)])
}


## general keys function
.keys <- function(x, keytype){
    .checkForDeprecatedKeytype(keytype)
    testForValidKeytype(x, keytype)
    schema <- metadata(x)[metadata(x)$name=="DBSCHEMA",]$value
    if(schema=="NOSCHEMA_DB" || schema=="NOCHIPSCHEMA_DB"){
        .noSchemaKeys(x, keytype)
    }else{
        .legacyKeys(x, keytype)
    }
}


####################################################################
## So the new idea is that each place where I want to "enhance" keys,
## I should just be able to use a helper to wrap up the actual keys
## method...


## And we need a master helper to tie it all together
smartKeys <-
    function(x, keytype, ..., pattern, column, fuzzy=FALSE, FUN)
{
    ## check args, then...

    ## FUN is the base keys method
    .keys <- FUN
    
    ## So 1st we need helpers for other "keys" situations
    ## keys0 is for when we have a pattern we want to match in the keys
    .keys0 <- function(x, keytype, ..., pattern, fuzzy=FALSE)
        {   ## assumes 'pattern' present
            FUN <- if (fuzzy) agrep else grep
            FUN(pattern, .keys(x, keytype), value=TRUE, ...)
        }
    ## keys1 is for when we have a column but no pattern
    ## so we want to filter by column
    .keys1 <- function(x, keytype, ..., column)
        {   ## column acts as filter
            k <- suppressWarnings(select(x, as.character(.keys(x, keytype)),
                                         column, keytype))
            k[[keytype]][ !is.na(k[[column]]) ]
        }
    ## keys2 is for when we have a column, and a pattern to match on that
    ## column, and we want all the keys of a particular keytype that match
    ## that column.
    .keys2 <- function(x, keytype, ..., pattern, column, fuzzy=FALSE)
        {   ## assumes 'pattern', 'column' present
            FUN <- if (fuzzy) agrep else grep
            k <- suppressWarnings(select(x, as.character(.keys(x, keytype)),
                                         column, keytype))
            k[[keytype]][ FUN(pattern, k[[column]], ...) ]
        }

    ## Now decide which function to call...
    if (missing(pattern) && missing(column))
        k <- .keys(x, keytype)
    else if (missing(column))
        k <- .keys0(x, keytype, ..., pattern=pattern, fuzzy=fuzzy)
    else if (missing(pattern))
        k <- .keys1(x, keytype, ..., column=column)
    else
        k <- .keys2(x, keytype, ..., pattern=pattern, column=column,
                    fuzzy=fuzzy)
    
    unique(k)
}



## TODO: don't fail to document all the new arguments (pattern, column and fuzzy)
setMethod("keys", "OrgDb",
    function(x, keytype, ...){
      if(missing(keytype)){
        keytype <- chooseCentralOrgPkgSymbol(x)
      }
      smartKeys(x=x, keytype=keytype, ..., FUN=.keys)
  }
)

setMethod("keys", "ChipDb",
    function(x, keytype, ...){
      if(missing(keytype)) keytype <- "PROBEID"
      smartKeys(x=x, keytype=keytype, ..., FUN=.keys)
  }
)

setMethod("keys", "GODb",
    function(x, keytype, ...){
      if(missing(keytype)) keytype <- "GOID"
      smartKeys(x=x, keytype=keytype, ..., FUN=.keys)
  }
)


## new uses for keys:
## now TERM is a real key? (TODO: someone tell the keytypes)
## head(keys(GO.db, keytype="TERM"))



## get TERM keys that match a particular pattern
## head(keys(GO.db, keytype="TERM", pattern="mitochondrion"))

## get GOIDs where a TERM exists.
## head(keys(GO.db, keytype="GOID", column="TERM"))



## get keys of type GOID that go with a pattern match in TERM
## head(keys(GO.db, keytype="GOID", pattern="mitochondrion", column="TERM"))
## select(GO.db, keys =head(keys(GO.db, keytype="GOID", pattern="mitochondrion", column="TERM")), cols=c("GOID","TERM"))


## do the above but use fuzzy matching
## head(keys(GO.db, keytype="GOID", pattern="mitochondrion", column="TERM", fuzzy=TRUE))
## select(GO.db, keys = head(keys(GO.db, keytype="GOID", pattern="mitochondrion", column="TERM", fuzzy=TRUE)), cols=c("GOID","TERM"))



## Can just get keys (straight up)
## head(keys(org.Hs.eg.db, keytype="SYMBOL"))

## keys1 situation works fine (and smartKeys is called twice.)
## Can filter by column (only return keys where there is a value for "PATH"
## length(keys(org.Hs.eg.db, keytype="ENTREZID", column="PATH"))
## is shorter than:
## length(keys(org.Hs.eg.db, keytype="ENTREZID"))


## debug(AnnotationDbi:::smartKeys)


## Can just get keys that match a pattern
## keys(org.Hs.eg.db, keytype="SYMBOL", pattern="BRCA")


## Can get a key that matches a pattern on some other column
## head(keys(org.Hs.eg.db,keytype="ENTREZID",pattern="MSX",column="SYMBOL"))







## keytypes method is to allow the user to specify what kind of keytype is
## passed in to either keys or the select methods.
## temporarily:this method will be VERY unsophisticated.

## TODO: would like to find a way to restore these blacklisted types to being
## able to be used, but I need a way around the lack of an Rkeys() method etc.

## keytypesBlackList <- c("CHRLOCEND","CHRLOC","PFAM","PROSITE",
##                        "DESCRIPTION", "GENENAME")
## .filterKeytypes <- function(x, baseType, keytypesBlackList){
##   res <- .cols(x, baseType=baseType)
##   res <- res[!res %in% keytypesBlackList]
##   ## append the centralID (if not already present)
##   centralID <- .getCentralID(x)
##   if(centralID == "EG"){ centralID <- "ENTREZID" }
##   res <- c(res, centralID)
##   unique(res)
## }

setMethod("keytypes", "OrgDb",
    ## function(x) .filterKeytypes(x, baseType="ENTREZID", keytypesBlackList)
    function(x){ 
        kts <- .cols(x, baseType="ENTREZID")
        .filterDeprecatedKeytypes(kts)
    }
)

setMethod("keytypes", "ChipDb",
    ## function(x) .filterKeytypes(x, baseType="PROBEID", keytypesBlackList) 
    function(x){ 
        kts <- .cols(x, baseType="ENTREZID")
        .filterDeprecatedKeytypes(kts)
    }          
)

setMethod("keytypes", "GODb",
    function(x) return(c("GOID","TERM","ONTOLOGY","DEFINITION")) ## only one type makes sense
)

## Marc's TODO:
##X .5) make keytype so that it uses the mapping names instead of internal stuff
##X 1) make keytypes so that it returns all possible keytypes
##X 2) make keys() so that it gets correct keys for correct keytypes
##X 3) make select() so that it is more efficient (pre-filter)
##X 4) make select() so that it uses keytypes to initially map in to the correct thing and then call internal funcs.
## 4.5) Make sure this thing is sorting correctly!
## 5) document all this stuff.





#############################
## TEST CODE:
## library(org.Hs.eg.db)
## ls(2)

## con = AnnotationDbi:::dbconn(org.Hs.eg.db)
## keys = head(keys(org.Hs.egCHR))


## debug(AnnotationDbi:::.queryForKeys)
## debug(AnnotationDbi:::.keys)

## example of keys that uses keytype
## keys = keys(org.Hs.eg.db, keytype="ALIAS2EG")[1:4]

## example of keys that does not use keytype
## keys = keys(org.Hs.eg.db)[1:5]



## default keytype example
## keys = keys(org.Hs.eg.db)[1:5]
## cols = c("SYMBOL", "UNIPROT")
## select(org.Hs.eg.db, keys, cols)

## idType = "gene_id"



## debug(AnnotationDbi:::resort_base)

## debug(AnnotationDbi:::.mergeBimaps)

## debug(AnnotationDbi:::.select)


#############################
## keytype example

## library(hgu95av2.db); columns(hgu95av2.db); columns(org.Hs.eg.db); head(keys(org.Hs.eg.db, "ALIAS")); keys(org.Hs.eg.db, keytype="PROBEID")## should be an error

## library(org.Hs.eg.db); keys2 = head(keys(org.Hs.eg.db, "ALIAS"));cols = c("SYMBOL", "GO");res <- select(org.Hs.eg.db, keys2, cols, keytype="ALIAS"); head(res); dim(res)

## debug(AnnotationDbi:::.select)

## library(hgu95av2.db); keys2 = head(keys(hgu95av2.db, "ALIAS"));cols = c("SYMBOL", "GO");res <- select(hgu95av2.db, keys2, cols, keytype="ALIAS"); head(res); dim(res)


## works now
## library(org.Hs.eg.db);keys2 = head(Rkeys(org.Hs.egALIAS2EG));cols = c("SYMBOL","ENTREZID", "GO");res <- select(org.Hs.eg.db, keys2, cols, keytype="ALIAS")

## works now
## keys = head(keys(org.Hs.eg.db)); cols = c("SYMBOL","ENTREZID", "GO");res <- select(org.Hs.eg.db, keys, cols, keytype="ENTREZID")

## also works now
## library(hgu95av2.db); keys = head(keys(hgu95av2.db)); cols = c("SYMBOL","ENTREZID", "GO", "PROBEID"); res <- select(hgu95av2.db, keys, cols, keytype="PROBEID"); head(res)


## This shouldn't work - wrong keytype):
## keys = head(keys(hgu95av2.db)); cols = c("SYMBOL","ENTREZID", "GO"); res <- select(hgu95av2.db, keys, cols, keytype="ENTREZID")

## This does work (and should):
## library(hgu95av2.db); keys = head(keys(hgu95av2.db)); cols = c("SYMBOL","ENTREZID", "GO"); res <- select(hgu95av2.db, keys, cols, keytype="PROBEID"); head(res)



## library(GO.db); select(GO.db, keys(GO.db)[1:4], c("TERM","SYNONYM"))

## library(hgu95av2.db); okeys = keys(hgu95av2.db,keytype="OMIM")[1:4]; cols = c("SYMBOL", "UNIPROT", "PATH"); select(hgu95av2.db, okeys, cols, keytype="OMIM")




## TODO Bugs/refinements:

## TODO: this one should produce an output... - FIXED
## keys = head(keys(hgu95av2.db, "ENTREZID")); cols = c("PROBEID","SYMBOL","ENTREZID", "GO"); res <- select(hgu95av2.db, keys, cols, keytype="ENTREZID"); head(res)

## 3) Add a NEWS page with info. about these changes.




## strange bug: - killed
## 4) Putting "ENTREZID" in for keytype and then giving probe IDs as keys should NOT work for hgu95av2.db: but it does... (it only seems to allow this with the one kind of key) 








## also need to roll back the removal of the keytype from the columns.
## library(org.Hs.eg.db);keys2 = head(Rkeys(org.Hs.egALIAS2EG));cols = c("SYMBOL","ENTREZID", "GO");res <- select(org.Hs.eg.db, keys2, cols, keytype="ALIAS");







## Strange bug:
## the following all work:
## foo = select(org.Hs.eg.db, keys=head(keys(org.Hs.eg.db),n=2),cols="REFSEQ"); head(foo)
## foo = select(org.Hs.eg.db, keys=head(keys(org.Hs.eg.db),n=2),cols=c("REFSEQ","ACCNUM")); head(foo)
## foo = select(org.Hs.eg.db, keys=head(keys(org.Hs.eg.db),n=2),cols=c("ACCNUM")); head(foo)

## But this does NOT work (fixed):
## foo = select(org.Hs.eg.db, keys=head(keys(org.Hs.eg.db),n=2),cols=head(columns(org.Hs.eg.db))); head(foo); head(columns(org.Hs.eg.db))

## debug(AnnotationDbi:::.nameExceptions)
## debug(AnnotationDbi:::.addNAsInPlace)




## Requirements for having a select method that works and plays well with
## others:
## 1) Use the same arguments for the method (obvious)
## 2) remove dulicated columns.





## Martins slow select example.  It takes advantage of the fact that for
## simple cases, like the one below, our select method has to gather each
## piece and then merge them together which costs a lot of time (both to merge
## and also because we don't pre-subset).
## Also our code is doing more post-processing (returning prettier results in
## particular order etc.)
## Also because our code is blind to what the user wants out, we move ALL of
## each bimap through memory and don't pare them down till we merge them
## together and this is ultimately inefficient.

## If the code knew (as Martin did in this case) the relationships between
## these different elements (perhaps it could learn that graph from the DB),
## then it could make smarter decisions about how to query.

## library(org.Hs.eg.db)
## sym <- "ITGA7"
## system.time(res0 <- toTable(org.Hs.egPFAM[ org.Hs.egALIAS2EG[[sym]] ]))
## system.time(res1 <- select(org.Hs.eg.db, sym, "PFAM", "ALIAS"))
## system.time(res3 <- toTable(org.Hs.egGO[ org.Hs.egALIAS2EG[[sym]] ]))
## system.time(res4 <- select(org.Hs.eg.db, sym, "GO", "ALIAS"))


## ALSO: there is something to be said for the notion that we need a general
## solution to this problem that does NOT involve a Bimap.  Bimaps are nice,
## but we don't normally have them for a new resource and we might want a
## faster way to handle these sorts of manipulations when we don't have them.


## Basically, I think that I want to use a graph here, but not require one
## from the user, I need to 1) be able to infer the graph from SQL, 2) be able
## to path-find through the graph such that all the keys requested are
## hit. and 3) be able to construct a sensible query from that graph.  Tall
## order, but a fun problem.


## Reasons for generalizing this: 1) I need to be able to do this in ALL
## databases (not just bimap ones). and 2) We are moving away from bimaps and
## 3) I want to be able to add mappings to existing bimap based data resources
## that are actually not available as a bimap (reactome) and 4) I would really
## like to be able to transparently pull data from another resource and just
## have it appear to be in one place.  Sort of like we currently do for
## microRNAs with TxDbs


## Really radical thoughts:
## What if discovery functions just reported based on which databases were
## installed (instead of just what was in a package?).

## What if select searched across all of these databases to make joins on the
## fly as appropriate by already knowing how to connect the dots?

## What if we could have select work out how things connect based on the type
## of package, and some internal information about how those would be joined?





## THE R CMD build bug:
## For AnnotationDbi:
## R --vanilla
## utils::Sweave("IntroToAnnotationPackages.Rnw") ## runs no problem
## BUT:
## R --vanilla
## utils::Sweave("AnnotationDbi.Rnw")
## utils::Sweave("IntroToAnnotationPackages.Rnw") ## FAILBOAT
## This failure is happening because the values that are left littered in the
## global namespace are allowed to leak down into the scope of the functions
## being called...

## Bug was able to happen this way:
## y = "foo"
## source("AnnotationDbi/inst/doc/IntroToAnnotationPackages.R")

## was caused by overly grabby exists() calls combined with the sloppy way
## that R CMD check leaves variables all over the place when it runs R CMD
## check.  exists() calls are no longer grabby.
## ALSO, lexical scoping meant that the exists() call being falsely tripped
## led to a call of species(y) actually being executed when I should never
## have been






## fieldNames <- c("gene_id","accession","accession")
## expectedCols <- c("ENTREZID","ACCNUM","REFSEQ")
## type <- 



## the na bug
## sym <- "ITGA7"
## select(org.Hs.eg.db,sym,"PFAM",keytype="ALIAS");

## Problem: our code that filters rows needs to drop the keytype column before filtering NAs
