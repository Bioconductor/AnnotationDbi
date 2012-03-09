## Need an accessor for getting the central ID for a DB (when appropriate)
.getCentralID <- function(x){
  as.character(dbQuery(dbConn(x),
                       "SELECT value FROM metadata WHERE name='CENTRALID'"))
}

## Sometimes we need to translate a centralID into a central keytype.
.chooseCentralOrgPkgSymbol <- function(x){
  centralID <- .getCentralID(x)
  keytype <- switch(EXPR = centralID,
                    "EG" = "ENTREZID",
                    "TAIR" = "TAIR",
                    "ORF" = "ORF")
  keytype
}

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
.toTableAndCleanCols <- function(x, map){
  tab <- toTable(map)
  if(map@objName == "ENTREZID" && .getCentralID(x) == "TAIR"){
    ## in this one strange case, we have to rename the 2nd column
    colnames(tab)[2] <- c("ENTREZID") 
  }else{
    tab <- tab[,!duplicated(colnames(tab)),drop=FALSE]
  }
  tab
}

.filterSuffixes <- function(tab){
  ## clean up .x and .y extensions?
  colnames(tab) <- gsub("\\.x","",colnames(tab))
  colnames(tab) <- gsub("\\.y","",colnames(tab))
  ## clean up .1's
  colnames(tab) <- gsub("\\.1","",colnames(tab))  
  tab
}

.mergeBimaps <- function(x, objs, keys, jointype){
  for(i in seq_len(length(objs))){
    if(i==1){
      finTab <- .toTableAndCleanCols(x, objs[[1]])
      finTab <- finTab[finTab[[jointype]] %in% keys,]
    }else{
      nextTab <- .toTableAndCleanCols(x, objs[[i]])
      nextTab <- nextTab[nextTab[[jointype]] %in% keys,]
      finTab <- merge(finTab, nextTab,
                      by=jointype, all=TRUE)
    }
  }
  finTab <- .filterSuffixes(finTab)
  finTab
}

## slower alternate for when we cannot pre-filter.
.mergeBimapsPostFilt <- function(x, objs, keys, jointype){
  for(i in seq_len(length(objs))){
    if(i==1){
      finTab <- .toTableAndCleanCols(x, objs[[1]])
    }else{
      nextTab <- .toTableAndCleanCols(x, objs[[i]])
      finTab <- merge(finTab, nextTab,
                      by=jointype, all=TRUE)
    }
  }
  ## We do NOT want to to any actual row-filtering inside this method. It all
  ## has to be done later, and after we have merged together requested data.
  ## post filter means we filter LATER on and not while we are merging.
  finTab <- .filterSuffixes(finTab)
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
  maybeMissing <- c(probe_id="PROBEID", gene_id="ENTREZID",
                   systematic_name="ORF")
  ## if we have a tair DB, add tair to the list, but otherwise do not.
  ## reason is b/c it creates a duplicate key situation with 'gene_id'
  if(.getCentralID(x) == "TAIR"){
    maybeMissing <- c(c(gene_id="TAIR"), maybeMissing)
  }
  for(i in seq_len(length(maybeMissing))){
    if(!maybeMissing[i] %in% cols){
       cols <- c(cols,maybeMissing[i])
    }
  }
  cols
}

## look for exceptions, BUT the logic of the loop used by this helper strictly
## requires that the names and cols be of the same length AND IN THE SAME
## ORDER!. Therefore, only primaryNames that are NOT NA can be passed down to
## here, and the order of names and cols must be consistent beforehand.
.nameExceptions <- function(names, cols){
  if(length(names) != length(cols)){
    warning("cols could not be renamed because length(names) != length(cols)")
    return(names)
  }else{
    newNames <- character(length(names))
    for(i in seq_len(length(cols))){
      newNames[[i]] <- switch(EXPR = names(names)[[i]],
              "go_id" = "GO",
              "systematic_name" = "ORF",
              "ipi_id" = ifelse(cols[[i]]=="PFAM","IPI","IPI"),
              "accession" = ifelse(cols[[i]]=="ACCNUM","ACCNUM","REFSEQ"),
              "gene_id" = ifelse(cols[[i]]=="ENTREZID","ENTREZID","TAIR"),
                    names[[i]])
    }
    names(newNames) <- names(names)
    return(newNames)
  }
}

## this just loops through the cols and discovers which ones need extra
## padding then it makes a new "cols" that has NAs where they will be needed.
## If I use this, replace .makeBimapsFromStrings with a one row version of
## same for speedup
.addNAsInPlace <- function(x, names, cols){
  res <- character()
  for(i in seq_len(length(cols))){
    ## 1st get the number of cols associated
    if(!cols[i] %in%  c("ENTREZID","GOID","PROBEID","TAIR","ORF")){
      obj <- .makeBimapsFromStrings(x, cols[i])[[1]]
      colLen <- dim(toTable(obj[1]))[2] #fast
      localCNs <- colnames(toTable(obj[1]))
      duplevel <- length(localCNs) - length(unique(localCNs))
      ## Check to see if any colnames are repeating?
      if(colLen > 2 && duplevel==0){
        ## then add some NAs
        res <- c(res, cols[i], rep(NA, times=colLen-2))
      }
      else if(colLen > 2 && duplevel>0){
        ## then add fewer NAs
        res <- c(res, cols[i], rep(NA, times=(colLen-2-duplevel)))
      }else{## don't add NAs
        res <- c(res, cols[i])
      }
    }else{
      res <- c(res, cols[i])
    }
  }
  res
}

.selectivelyMatchNameExceptions <- function(x, names, cols){
  ## 1st we ADD NAs to the cols (in the same places as the oriNames)
  modCols <- .addNAsInPlace(x, names, unique(cols))
  ## Then call method to selectively replace names with original col names.
  .nameExceptions(names, modCols)
}

## used to rename the cols (where appropriate) with all caps labels
.renameColumnsWithRepectForExtras <- function(x, res, oriCols){  
  res <- .filterSuffixes(res) ## Removes duplicate suffixes
  uncleanedfcNames <- .getAllColAbbrs(x)
  ## THEN clean up symbol exceptions  
  fcNames <- .swapSymbolExceptions(x, uncleanedfcNames)
  primaryNames <- fcNames[match(colnames(res), names(fcNames))]
  primaryNames <- .selectivelyMatchNameExceptions(x, primaryNames, oriCols)
  ## secondary names are just the table names.
  secondaryNames <- colnames(res)

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

## Remove unwanted ID cols  
## We only want to drop columns that really are "adds"
.cleanOutUnwantedCols <- function(x, res, keytype, oriCols){
  centralID <- .getCentralID(x)
  blackList <- switch(EXPR = centralID,
                      "EG" = unique(c(keytype, "ENTREZID","PROBEID")),
                      "ENTREZID" = unique(c(keytype, "ENTREZID","PROBEID")),
                      "ORF" = unique(c(keytype, "ORF","PROBEID")),
                      "TAIR" = unique(c(keytype, "TAIR","PROBEID")))
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
    centralSymbol <- .chooseCentralOrgPkgSymbol(x)
    cols <- cols[!(cols %in% centralSymbol)]
  }
  if(class(x)=="ChipDb"){
    cols <- cols[!(cols %in% "PROBEID")]
  }
  cols
}


###############################################################################
## Helpers for tidying up the final table.
## .resort drops unwanted rows, rearanges cols and puts things into order that
## the keys were initially

## drop rows that don't match
.dropUnwantedRows <- function(tab, keys, jointype){
  ## 1st of all jointype MUST be in the colnames of tab
  tab <- unique(tab)  ## make sure no rows are duplicated
  rownames(tab) <- NULL ## reset the rownames (for matching below)
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
  tab
}

## resort the Column Names
.resortColumns <- function(tab, jointype, reqCols){
  tab <- .filterSuffixes(tab) ## Removes duplicate suffixes
  if(all(colnames(tab) %in% reqCols)){  ## this might be too stringent...
    cnames <- c(jointype, reqCols[!(reqCols %in% jointype)])
    indc <- match(cnames, colnames(tab))
    tab <- tab[,indc,drop=FALSE]
    colnames(tab) <- cnames
  }else{stop("Some of the reqCols are not in the table (colnames(tab)).")}
  tab
}

## helper to remove any columns that are true duplicates
.dropDuplicatedCols <- function(tab){
  cols <- colnames(tab)
  cols <- cols[!duplicated(cols)]  
  tab <- tab[,cols]
  tab
}


## Create extra rows
## TODO: there are still problems here.
## I have issues where I drop rows from tables that have extra rows of real
## info. while tring to match to the keys.
.generateExtraRows <- function(tab, keys, jointype){
  ## 4 possibilities
  ## if there are not dups, then we skip this function.
  ## if(any(duplicated(keys)) ## then we have to expand the keys
  ## if(any(duplicated(tab[[jointype]]))) ## then we have to expand the table...
  ## AND if they are BOTH redundant how do I decide which row to expand?
  ## I think that I have to throw a warning and NOT do this step in that case?
  keyTest <- any(duplicated(keys))
  rowTest <-  any(duplicated(tab[[jointype]]))         
  if(!keyTest && !rowTest){ ## already the same - nothing to do
    tab<-tab
  }else if(keyTest && !rowTest){ ## Need to account for row dups
    ind = match(keys, tab[[jointype]])
    tab <- tab[ind,,drop=FALSE]
    rownames(tab) <- NULL
  }else if(!keyTest && !rowTest){ ## Need to account for data dups
    warning("The data you have requested can only be expressed by duplicating some of the keys you requested.  Some of your keys may appear multiple times in the output")
    tab<-tab
  }else if(keyTest && rowTest){ ## Hands in air. - User will get data "as is"
    warning("The data you have requested can only be expressed by duplicating some of the keys you requested.  Furthermore, it also appears that you have given us some of your keys multiple times.  Normally we would duplicate those rows for you, but this time we can't because of the existing redundancy in the data you have requested.")
    tab<-tab
  }
  tab
}

## .resort is the main function for cleaning up a table so that results look
## formatted the way we want them to.
.resort <- function(tab, keys, jointype, reqCols){
  if(jointype %in% colnames(tab)){
    tab <- .dropUnwantedRows(tab, keys, jointype)
    ## rearrange to make sure cols are in correct order
    tab <- .resortColumns(tab, jointype, reqCols)
  }
  ## Duplicate any rows as appropriate (based on those keys)
  tab <- .generateExtraRows(tab, keys, jointype)
  tab
}


## ## Helper to make sure that oriTabCols is in same order as oriCols
.resortOriTabCols <- function(oriTabCols, oriCols, x, res){
  ## One strange exception caused by another "ENTREZID" exception upstream
  CNAMES <- c(.getAllColAbbrs(x), ENTREZID="ENTREZID")
  oriTabNames <- CNAMES[match(oriTabCols,names(CNAMES))]
  names(oriTabNames) <- oriTabCols
  ## need to split this vector into the correct "pieces" (split by "not an NA")
  len <- length(oriTabNames)
  chunks <- list()
  resInd <- 1 ## index of next chunks
  prevInd <- 0 ## index of prev chunks
  for(i in seq_len(len)){
    cur <- oriTabNames[i]    
    if(!is.na(cur)){
      chunks[[resInd]] <- cur
      prevInd <- resInd
      resInd <- resInd + 1
    }else{
      chunks[[prevInd]] <- c(chunks[[prevInd]], cur)
    }
  }  
  ## get the short names of oriCols (which gives the desired order)
  CNAMES <- .getAllColAbbrs(x)
  oriNames <- CNAMES[match(oriCols, CNAMES)]
  ## helper to look up stuff in chunks (What chunk has the ID?)
  chunkIdx <- function(str){
    res <- integer(length(chunks))
    for(i in seq_len(length(chunks))){
      if(str %in% chunks[[i]]){
        res[i] <- i
      }else{
        res[i] <- NA
      }
    }
    res <- res[!is.na(res)]
    res[1] ## return the 1st match...
  }
  ## Now I just need to make another loop and reassemble things in order of
  ## oriNames
  len <- length(oriNames)
  result <- character()
  for(i in seq_len(len)){
    str <- oriNames[i]
    idx <- chunkIdx(str)
    if(!is.na(idx)){
      result <- c(result, chunks[[idx]]) ## try to deduce the chunk
    }else{
      result <- c(result, oriNames[i]) ## if you fail, fallback to oriCols
    }
  }
  ## TAIR exception. 
  if("ENTREZID" %in% oriCols && "TAIR" %in% oriCols &&
     .getCentralID(x) == "TAIR"){
    ## means that we have tair and ENTREZID
    names(result)[match("ENTREZID",result)] <- "ENTREZID"
  }
  ## return the names:
  names(result)
}


## the core of the select method for GO org and chip packages.
.select <- function(x, keys=NULL, cols=NULL, keytype, jointype){
  ## if asked for what they have, just return that.
  if(all(cols %in% keytype)  && length(cols)==1){
    res <- data.frame(keys=keys)
    colnames(res) <- cols
    return(res)
  }
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
  oriCols <- unique(c(keytype, cols))

  ## All this because I need to account for the cols that "expand"
  ## I really do need a new helper here
  ## oriTabCols <- .getDBHeaderCols(x, oriCols)
  
  ## now drop a value from cols before we try to make any bimaps
  cols <- .cleanupBaseTypesFromCols(x, cols)
  ## keys should NOT be NAs, but if they are, warn and then filter them.
  if(length(keys) != length(keys[!is.na(keys)])){
    warning(paste("You cannot really use NA values as keys.",
                  "All such keys will be dropped for you and the",
                  "results will be correspondingly smaller."))}
  keys <- keys[!is.na(keys)]

  if(keytype %in% c("ENTREZID","PROBEID","GOID","TAIR","ORF") &&
     !(keytype %in% "ENTREZID" && class(x)=="ChipDb") &&
     !(keytype %in% "ENTREZID" && .getCentralID(x)=="TAIR") &&
     !(keytype %in% "ENTREZID" && .getCentralID(x)=="ORF")){
    objs <- .makeBimapsFromStrings(x, cols)
    res <-.mergeBimaps(x, objs, keys, jointype=jointype)
  }else{ ## not a central ID, so an extra col is required
    if(!(keytype %in% cols)){ cols <- unique(c( keytype, cols)) }
    objs <- .makeBimapsFromStrings(x, cols)
    ## merge using the base joinType (based on primary key)
    res <- .mergeBimapsPostFilt(x, objs, keys, jointype=jointype)
    ## deduce NEW jointype from the keytype (do NOT use the default one!)
    ## This jointype is for filtering (which happens next)
    jointype <- .getRKeyName(x, keytype)
  }

  ## this takes a black list approach for cleaning out unwanted cols
  res <- .cleanOutUnwantedCols(x, res, keytype, oriCols)
  
  ## now is the time to collect the column names that we expect from the DB
  res <- .filterSuffixes(res)
  oriTabCols <- colnames(res)
  ## It's important that these are in the same order as oriCols.
  oriTabCols <- .resortOriTabCols(oriTabCols, oriCols, x, res)
  
  ## .resort will resort the rows relative to the jointype etc.
  if(dim(res)[1]>0){
    res <- .resort(res, keys, jointype, oriTabCols)
  }
  
  ## rename col headers, BUT if they are not returned by cols, then we have to
  ## still keep the column name (but adjust it)
  colnames(res) <- .renameColumnsWithRepectForExtras(x, res, oriCols)

  ## This last step removes unwanted column duplicates.  As much as I would like
  ## to, I CANNOT do this step inside of .resort(), because .resort() is
  ## dealing only with the actual db-style column names that will sometimes
  ## have (at least for AnnotationDbi) have legitimate duplications that we do
  ## NOT want to remove. 
  res <- .dropDuplicatedCols(res)
  rownames(res) <- NULL
  res
}

## Helper for setting the jointype to an appropriate default
.chooseJointType  <- function(x){
  if(.getCentralID(x) == "ORF"){
    jointype <- "systematic_name"
  }else{
    jointype <- "gene_id"
  }
  jointype
}



setMethod("select", "OrgDb",
    function(x, keys, cols, keytype) {
          if (missing(keytype)){
            keytype <- .chooseCentralOrgPkgSymbol(x)
          }
          jointype <- .chooseJointType(x)
          .select(x, keys, cols, keytype, jointype=jointype)
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
  unique(cols)
}


setMethod("cols", "OrgDb",
    function(x){
      baseType <- .chooseCentralOrgPkgSymbol(x)
      .cols(x, baseType)
    }
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
  ## Some org packages may have entrez genes in weird places...
  centralID <- .getCentralID(x)
  EGgeneTable <- character()
  if(centralID == "EG" || centralID == "ORF"){
    EGgeneTable <- "genes"
  }else if(centralID == "TAIR"){
    EGgeneTable <- "entrez_genes"
  }
  ## now decide
  if(class(x) == "OrgDb"){
    res <- switch(EXPR = keytype,
                  "ENTREZID" = dbQuery(dbConn(x),
                    paste("SELECT gene_id FROM", EGgeneTable), 1L),
                  "TAIR" = dbQuery(dbConn(x),
                    "SELECT gene_id FROM genes", 1L),
                  "ORF" = dbQuery(dbConn(x),
                    "SELECT systematic_name FROM sgd", 1L),
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
  res[!is.na(res)]
}


## TODO: swap initial SQL query for an Lkeys() call for OrgDb and ChipDb??
setMethod("keys", "OrgDb",
    function(x, keytype){
      if(missing(keytype)){
        keytype <- .chooseCentralOrgPkgSymbol(x)
      }
      .makeKeytypeChoice(x, keytype)
    }
)

setMethod("keys", "ChipDb",
    function(x, keytype){
      if(missing(keytype)) keytype <- "PROBEID"
      .makeKeytypeChoice(x, keytype)
    }
)

setMethod("keys", "GODb",
    function(x, keytype){
      if(missing(keytype)) keytype <- "GOID"
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

## TODO: would like to find a way to restore these blacklisted types to being
## able to be used, but I need a way around the lack of an Rkeys() method etc.

keytypesBlackList <- c("CHRLOCEND","CHRLOC","PFAM","PROSITE",
                       "DESCRIPTION", "GENENAME")
.filterKeytypes <- function(x, baseType, keytypesBlackList){
  res <- .cols(x, baseType=baseType)
  res <- res[!res %in% keytypesBlackList]
  ## append the centralID (if not already present)
  centralID <- .getCentralID(x)
  if(centralID == "EG"){ centralID <- "ENTREZID" }
  res <- c(res, centralID)
  unique(res)
}

setMethod("keytypes", "OrgDb",
    function(x) .filterKeytypes(x, baseType="ENTREZID", keytypesBlackList)
)

setMethod("keytypes", "ChipDb",
    function(x) .filterKeytypes(x, baseType="PROBEID", keytypesBlackList) 
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
## foo = select(org.Hs.eg.db, keys=head(keys(org.Hs.eg.db),n=2),cols=head(cols(org.Hs.eg.db))); head(foo); head(cols(org.Hs.eg.db))

## debug(AnnotationDbi:::.nameExceptions)
## debug(AnnotationDbi:::.addNAsInPlace)




## Requirements for having a select method that works and plays well with
## others:
## 1) Use the same arguments for the method (obvious)
## 2) remove dulicated columns.
