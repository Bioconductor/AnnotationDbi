## Select Methods return the results from looking up things (cols) that match
## the keys provided.  cols is a character vector to specify columns the user
## wants back and keys are the keys to look up.

.getObjList <- function(x){
  meta <- metadata(x) 
  schema <- meta[meta["name"] == "DBSCHEMA","value"]
  eval(parse(text=paste("AnnotationDbi:::",schema,
                  "_AnnDbBimap_seeds",sep="")))  
}

## a helper to make the strings to objects
.makeBimapsFromStrings <- function(x, cols){
  pkgname <- sub(".db$","", AnnotationDbi:::packageName(x))
  lapply(cols, function(x){
    eval(parse(text=paste(pkgname, x, sep="")))
  })
}

## another helper to merge
## Be sure to use all.x=TRUE (and all.y=TRUE), then filter on keys in a later
## step
## 1st a helper to remove any pre-existing col duplicates from bimaps.
.toTableAndCleanCols <- function(map){
  tab <- toTable(map)
  tab[,!duplicated(colnames(tab))]
}

.mergeBimaps <- function(objs, keys, jointype){
  for(i in seq_len(length(objs))){
    if(i==1){
      finTab <- .toTableAndCleanCols(objs[[1]])
      finTab <- finTab[finTab[[jointype]] %in% keys,]
    }else{
      nextTab <- .toTableAndCleanCols(objs[[i]])
      nextTab <- nextTab[nextTab[[jointype]] %in% keys,]
      finTab <- merge(finTab, nextTab,
                      by=jointype, all=TRUE, suffixes = c("",""))
    }
  }
  finTab
}

## slower alternate for when we cannot pre-filter.
.mergeBimapsPostFilt <- function(objs, keys, jointype){
  for(i in seq_len(length(objs))){
    if(i==1){
      finTab <- .toTableAndCleanCols(objs[[1]])
    }else{
      nextTab <- .toTableAndCleanCols(objs[[i]])
      finTab <- merge(finTab, nextTab,
                      by=jointype, all=TRUE, suffixes = c("",""))
    }
  }
  finTab[finTab[[jointype]] %in% keys,]
  finTab
}

## helper to get a rightColNames from the keyTypes
.getRKeyName <- function(x, keytype){
  objList <- .getObjList(x)
  names <- unlist(lapply(objList, function(x){x$objName}))  
  obj <- objList[[grep(paste("^",keytype,"$",sep=""),names,perl=TRUE)]]
  objChain <- obj$L2Rchain
  finElem <- objChain[[length(objChain)]]
  finElem$Rcolname
}
## a vectorized version of the above helper.
.getRKeyNames <- function(x, keytypes){
  unlist(lapply(keytypes, FUN=.getRKeyName, x=x))
}

## Helper for matching the short names of mappings with the salient table cols
.makeColAbbrs <- function(x){
  objList <- .getObjList(x)
  cols <- unlist(lapply(objList, function(x){x$objName}))
  names(cols) <- .getRKeyNames(x, cols)
  cols
}

## Need a helper to allow bimap symbols to be remapped on the fly
## if the string you pass into this is one thing, it gets the other
## ... and vice versa
.swapOneSymbolException <- function(x, str){
  ## 1st we define a vector of things we want to be able to "flip"
  if(length(str) > 1){
    stop(".swapOneSymbolException can only process one string at a time")
  }
  ## list of things that we will exchange (can be as long as we need,
  ## but there can be one list here for each class of "x").
  if(class(x)=="ChipDb"){
    swpNames <- c(ALIAS="ALIAS2PROBE")
  }else if(class(x)=="OrgDb"){
    swpNames <- c(ALIAS="ALIAS2EG")
  }else{
    swpNames <- c()
  }
  if(str %in% names(swpNames)){
    res <- unlist(swpNames[grep(str, names(swpNames), fixed=TRUE)])
  }else if(str %in% swpNames){
    res <- names(swpNames)[grep(str, swpNames, fixed=TRUE)]
  }else{
    res <- str
  }
  names(res) <- NULL
  res
}
## here is the vectorized version 
.swapSymbolExceptions <- function(x, strings){
  unlist(lapply(strings, .swapOneSymbolException, x=x))
}

  
## Another Helper for getting all possible short mapping names for salient cols
.getAllColAbbrs <- function(x){
  cols <- .makeColAbbrs(x)## unique strips off the name so we loop.  :(
  maybeMissing = c(probe_id="PROBEID", gene_id="ENTREZID")
  for(i in seq_len(length(maybeMissing))){
    if(!maybeMissing[i] %in% cols){
       cols <- c(cols,maybeMissing[i])
    }
  }
  cols
}

## look for exceptions, BUT the logic of the loop used by this helper strictly
## requires that the names and cols be of the same length. Therefore, only
## primaryNames that are NOT NA can be passed down to here
.nameExceptions <- function(names, cols){
  if(length(names) != length(cols)){ return(names) }else{
    newNames <- character(length(names))
    for(i in seq_len(length(cols))){
      newNames[[i]] <- switch(EXPR = names(names)[[i]],
                    "go_id" = "GO",
                    "ipi_id" = ifelse(cols[[i]]=="PFAM","PFAM","PROSITE"),
                    "accession" = ifelse(cols[[i]]=="ACCNUM","ACCNUM","REFSEQ"),
                    names[[i]])
    }
    names(newNames) <- names(names)
    return(newNames)
  }
}

.addNAsInPlace <- function(names, cols){
  res <- character(length(names))
  ## loop needs two counters
  colsPlace <- 1
  for(i in seq_len(length(names))){
    if(!is.na(names)[i]){
      res[i] <- cols[colsPlace]
      colsPlace <- colsPlace + 1
    }else{
      res[i] <- names[i]
    }
  }
  res
}

.selectivelyMatchNameExceptions <- function(names, cols){
  ## 1st we ADD NAs to the cols (in the same places as the oriNames)
  modCols <- .addNAsInPlace(names, cols)
  ## Then call method to selectively replace names with original col names.
  .nameExceptions(names, modCols)
}

.renameColumnsWithRepectForExtras <- function(x, res, oriCols){
  colnames(res) <- gsub(".1","",colnames(res))  ## Removes duplicate suffixes.
  fcNames <- .getAllColAbbrs(x)
  fcNames <- .swapSymbolExceptions(x, fcNames)
  secondaryNames <- colnames(res)
  primaryNames <- fcNames[match(colnames(res), names(fcNames))]
  ## selectively replace certain problematic secondaryNames 
  primaryNames <- .selectivelyMatchNameExceptions(primaryNames, oriCols)
  
  ## merge two name types giving preference to primary
  colNames <- character()
  if(length(secondaryNames) == length(primaryNames)){
    for(i in seq_len(length(primaryNames))){
      if(!is.na(primaryNames[i])){
        colNames[i] <- primaryNames[i]
      }else{
        colNames[i] <- secondaryNames[i]
      }
    }
  }else{stop("primaryNames and secondaryNames must be same length.")}
  colNames
}

## Remove unwanted ID cols  TODO: make this code a helper
## We only want to drop columns that really are "adds"
.cleanOutUnwantedCols <- function(x, res, keytype, oriCols){
  blackList <- unique(c(keytype, "ENTREZID","PROBEID"))
  blackList <- blackList[!(blackList %in% oriCols)]
  fcNames <- .getAllColAbbrs(x)
  smBlackList <- names(fcNames)[fcNames %in% blackList]
  res <- res[,!(colnames(res) %in% smBlackList),drop=FALSE]
  res
}

## overhead is caused by the fact that on rare occasions cols will
## contain things that cannot really be made into bimaps
.cleanupBaseTypesFromCols <- function(x, cols){
  if(class(x)=="OrgDb"){
    cols <- cols[!(cols %in% "ENTREZID")]
  }
  if(class(x)=="ChipDb"){
    cols <- cols[!(cols %in% "PROBEID")]
  }
  cols
}

## Helper for tidying up the final table.
## .resort drops unwanted rows, rearanges cols and puts things into order that
## the keys were initially
.resort <- function(tab, keys, jointype){
  ## 1st of all jointype MUST be in the colnames of tab
  tab <- unique(tab)  ## make sure no rows are duplicated
  rownames(tab) <- NULL ## reset the rownames (for matching below)
  if(jointype %in% colnames(tab)){
    ## This row-level uniqueness is required for match() below
    ## first find keys that will never match up and add rows for them
    noMatchKeys <- keys[!(keys %in% tab[[jointype]])]
    for(i in seq_len(length(noMatchKeys))){
      row <- rep(NA, dim(tab)[2])
      row[colnames(tab) %in% jointype] <- noMatchKeys[i]
      tab <- rbind(tab,row)
    }
  
    ## match up and filter out rows that don't match.
    ind = match(tab[[jointype]],keys)
    names(ind) = as.numeric(rownames(tab)) ## step REQUIRES good rownames
    tab <- tab[as.numeric(names(sort(ind))),,drop=FALSE]
    
    ## rearrange to make sure our jointab col is on the left
    cnames <- c(jointype, colnames(tab)[!(colnames(tab) %in% jointype)])
    tab <- data.frame(tab[[jointype]],
                      tab[,!(colnames(tab) %in% jointype)])
    ## reset the table rownames and colnames
    colnames(tab) <- cnames
  }
  rownames(tab) <- NULL
  tab
}

## Fresh start.  I need to NOT do this as a double merge
.select <- function(x, keys=NULL, cols=NULL, keytype, jointype){
  if(is.null(keys)) keys <- keys(x) ## if no keys provided: use them all
  if(is.null(cols)) cols <- cols(x) ## if no cols provided: use them all
  ## check that the keytype matches the keys
  ktKeys = keys(x, keytype=keytype)
  if(!(any(ktKeys %in% keys))){
    stop("keys must be of the same keytype as the actual keytype")
  }  
  ## translate any cute colnames or keytype names back to bimaps
  cols <- .swapSymbolExceptions(x, cols)
  keytype <- .swapSymbolExceptions(x, keytype)
  ## oriCols is a snapshot of col requests needed for column filter below
  oriCols <- c(keytype, cols) 

  ## now drop a value from cols before we try to make any bimaps
  cols <- .cleanupBaseTypesFromCols(x, cols)
  ## then filter any NAs from the keys
  keys <- keys[!is.na(keys)]

  if(keytype %in% c("ENTREZID","PROBEID","GOID") &&
     !(keytype %in% "ENTREZID" && class(x)=="ChipDb")){
    objs <- .makeBimapsFromStrings(x, cols)
    res <-.mergeBimaps(objs, keys, jointype=jointype)
  }else{ ## not a central ID, so an extra col is required
    if(!(keytype %in% cols)){ cols <- unique(c( keytype, cols)) }
    objs <- .makeBimapsFromStrings(x, cols)
    ## merge using the base joinType (based on primary key)
    res <- .mergeBimapsPostFilt(objs, keys, jointype=jointype)
    ## deduce NEW jointype from the keytype (do NOT use the default one!)
    ## This jointype is for filtering (which happens next)
    jointype <- .getRKeyName(x, keytype)
  }
  
  ## .resort will resort the rows relative to the jointype etc.
  if(dim(res)[1]>0){
    res <- .resort(res, keys, jointype)
  }
  
  ## this takes a black list approach for cleaning out unwanted cols
  res <- .cleanOutUnwantedCols(x, res, keytype, oriCols)
  
  ## rename col headers, BUT if they are not returned by cols, then we have to
  ## still keep the column name (but adjust it)
  colnames(res) <- .renameColumnsWithRepectForExtras(x, res, oriCols)
  res
}





setMethod("select", "OrgDb",
    function(x, keys, cols, keytype) {
          if (missing(keytype)) keytype <- "ENTREZID"
          .select(x, keys, cols, keytype, jointype="gene_id")
        }
)

setMethod("select", "ChipDb",
    function(x, keys, cols, keytype){
          if (missing(keytype)) keytype <- "PROBEID"
          .select(x, keys, cols, keytype, jointype="probe_id")
        }
)

setMethod("select", "GODb",
    function(x, keys, cols, keytype){
          if (missing(keytype)) keytype <- "GOID"
          .select(x, keys, cols, keytype, jointype="go_id")
        }
)

## setMethod("select", "InparanoidDb",
##     function(x, keys, cols, keytype="gene_id"){
##            .select(x, keys, cols, keytype)}
## )






#############################
## Internally we want to reconstruct these guys so we can merge() on them
## c <- cols(GO.db)[7]
## prefix = "GO"
## foo = paste(prefix,c,sep="")
## bar = eval(parse(text=foo))



##############################################################################
## cols methods return the list of things that users can ask for.  This can be
## just the table names, or it might be a list of mappings

.cols <- function(x, baseType){
  cols <- .makeColAbbrs(x)
  if(!missing(baseType)){
    cols <- c(baseType, cols)
  }
  ## translate relevant short bimap names to "cute" names
  cols <- .swapSymbolExceptions(x, cols) 

  ## .cols does not care about your names
  names(cols) <- NULL
  cols
}


setMethod("cols", "OrgDb",
    function(x) .cols(x, baseType="ENTREZID")
)

setMethod("cols", "ChipDb",
    function(x) .cols(x, baseType="PROBEID")
)

setMethod("cols", "GODb",
    function(x) .cols(x) ## does not have a missing baseType
)

## something more tricky required for Inparanoid since a single template
## exists for all (basically I need to do.call() a specific function
## setMethod("cols", "InparanoidDb",
##     function(x) .cols(x)
## )










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
.getKeysFromString <- function(x, keytype){
  if(length(keytype) > 1) stop("There can be only one keytype.")
  ## make a bimap from the keytype
  map <- .makeBimapsFromStrings(x, keytype)[[1]] ## there is only ever one.
  ## then get the Rkeys
  Rkeys(map) 
}

.makeKeytypeChoice <- function(x, keytype){
  ## have to swap keytype
  keytype <- .swapSymbolExceptions(x, keytype)
  if(class(x) == "OrgDb"){
    res <- switch(EXPR = keytype,
                  "ENTREZID" = dbQuery(dbConn(x),
                    "SELECT gene_id FROM genes", 1L),
                  "PROBEID" =
                     stop("PROBEID is not supported for Organism packages"),
                  .getKeysFromString(x, keytype))
  }
  if(class(x) == "ChipDb"){
    res <- switch(EXPR = keytype,
                  "ENTREZID" = dbQuery(dbConn(x),
                    "SELECT gene_id FROM probes", 1L),
                  "PROBEID" =  dbQuery(dbConn(x),
                    "SELECT DISTINCT probe_id FROM probes", 1L),
                  .getKeysFromString(x, keytype))
  }
  res
}


## TODO: swap initial SQL query for an Lkeys() call for OrgDb and ChipDb??
setMethod("keys", "OrgDb",
    function(x, keytype){
      if (missing(keytype)) keytype <- "ENTREZID"
      .makeKeytypeChoice(x, keytype)
    }
)

setMethod("keys", "ChipDb",
    function(x, keytype){
      if (missing(keytype)) keytype <- "PROBEID"
      .makeKeytypeChoice(x, keytype)
    }
)

setMethod("keys", "GODb",
    function(x, keytype){
      if (missing(keytype)) keytype <- "GOID"
      dbQuery(dbConn(x), "SELECT go_id FROM go_term", 1L)
    }
)

## for Inparanoid, we want to select keys carefully (depeninding on the
## organism - which is in the metadata)
## setMethod("keys", "InparanoidDb",
##     function(x) as.character(t(dbGetQuery(dbConn(x),
##                                            "SELECT gene_id FROM genes")))
## )








## keytypes method is to allow the user to specify what kind of keytype is
## passed in to either keys or the select methods.
## temporarily:this method will be VERY unsophisticated.

setMethod("keytypes", "OrgDb",
    function(x) .cols(x, baseType="ENTREZID")
)

setMethod("keytypes", "ChipDb",
    function(x) .cols(x, baseType="PROBEID") 
)

setMethod("keytypes", "GODb",
    function(x) return("GOID") ## only one type makes sense
)





## TODO:
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

## con = AnnotationDbi:::dbConn(org.Hs.eg.db)
## keys = head(keys(org.Hs.egCHR))


## debug(AnnotationDbi:::.getKeysFromString)
## debug(AnnotationDbi:::.makeKeytypeChoice)

## example of keys that uses keytype
## keys = keys(org.Hs.eg.db, keytype="ALIAS2EG")[1:4]

## example of keys that does not use keytype
## keys = keys(org.Hs.eg.db)[1:5]



## default keytype example
## keys = keys(org.Hs.eg.db)[1:5]
## cols = c("SYMBOL", "UNIPROT")
## select(org.Hs.eg.db, keys, cols)

## idType = "gene_id"



## debug(AnnotationDbi:::.resort)

## debug(AnnotationDbi:::.mergeBimaps)

## debug(AnnotationDbi:::.select)


#############################
## keytype example

## library(hgu95av2.db); cols(hgu95av2.db); cols(org.Hs.eg.db); head(keys(org.Hs.eg.db, "ALIAS")); keys(org.Hs.eg.db, keytype="PROBEID")## should be an error

## library(org.Hs.eg.db); keys2 = head(keys(org.Hs.eg.db, "ALIAS"));cols = c("SYMBOL", "GO");res <- select(org.Hs.eg.db, keys2, cols, keytype="ALIAS"); head(res); dim(res)

## debug(AnnotationDbi:::.select)

## library(hgu95av2.db); keys2 = head(keys(hgu95av2.db, "ALIAS"));cols = c("SYMBOL", "GO");res <- select(hgu95av2.db, keys2, cols, keytype="ALIAS"); head(res); dim(res)


## works now
## library(org.Hs.eg.db);keys2 = head(Rkeys(org.Hs.egALIAS2EG));cols = c("SYMBOL","ENTREZID", "GO");res <- select(org.Hs.eg.db, keys2, cols, keytype="ALIAS");

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
