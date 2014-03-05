## Inparanoid select methods:

.getBaseSpecies8 <- function(x){
  meta <- metadata(x)  
  ## TODO: switch to helper function:
  baseSpecies <- sub(" ","_",meta[meta$name=="ORGANISM","value"])
  baseSpecies
}


## match a "set" of cols
.getInpTables8 <- function(x, cols){
  res <- .getTableNames8(x)
  res <- res[res %in% cols]
  unique(res)
}


## replace the above with
.makeShortNameMapping <- function(){
    data <- read.delim(system.file('extdata','Inp_species_mapping',
                                  package='AnnotationDbi'),
                      sep="\t", header=TRUE, stringsAsFactors=FALSE)
    res <- data$inparanoidSpecies
    names(res) <- data$tableNames
    res
}

## replace the above with
.getBaseShortCode <- function(baseSpecies){
    shortMap <- .makeShortNameMapping()
    shortMap[names(shortMap) %in% baseSpecies]
}

## helpers
.getLCcolnames8 <- function(x){
  con <- AnnotationDbi:::dbConn(x)
  list <- dbListTables(con)
  ## drop unwanted tables
  unwanted <- c("metadata")
  list <- list[!list %in% unwanted]
  ## Add baseSpecies to the front...
  list <- c(.getBaseSpecies8(x), list)
}

.inpCols8 <- function(x, type="all"){
  list <- .getLCcolnames8(x)
  ## Then just to format things in the usual way
  list <- toupper(list)
  if(type=="noBase"){
    baseSpecies <- .getBaseSpecies8(x)  
    list <- list[!(list %in% toupper(baseSpecies))] 
  }
  list
}


setMethod("columns", "Inparanoid8Db", function(x){.inpCols8(x)})

## currently, I think the base type CAN be a valid keytype.
setMethod("keytypes", "Inparanoid8Db", function(x){.inpCols8(x)})
## setMethod("keytypes", "Inparanoid8Db", function(x){.inpCols8(x,type="noBase")})
          


## new helper to map Table names to UC names used by cols and keytypes
.getTableNames8 <- function(x){
  LC <- .getLCcolnames8(x)
  UC <- .inpCols8(x)
  names(UC) <- LC
  UC
}

.keysInp8 <- function(x, keytype){
  ## argument checking
  .testForValidKeytype(x, keytype)
  ## translate keytype back to table name
  tabNames <- .getTableNames8(x)
  lckeytype <- names(tabNames[tabNames %in% keytype])
  ## get the baseSpecies
  baseSpecies <- .getBaseSpecies8(x)
  baseShortCode <- .getBaseShortCode(baseSpecies)
  ## get connection to DB
  con <- dbConn(x)
  if(baseSpecies != lckeytype){
    sql <- paste("SELECT inp_id FROM", lckeytype,
                 paste0("WHERE species!='",baseShortCode,"'"))
    res <- dbGetQuery(con, sql)
    res <- as.vector(t(res))
  }else{
    res <- character()
 shortMap <- .makeShortNameMapping()
    ## remove the baseSpecies from shortMap names to get all real tables
    tables <- names(shortMap)[!(names(shortMap) %in% baseSpecies)]  
    for(i in seq_len(length(tables))){
      sql <- paste("SELECT inp_id FROM", tables[i],
                   paste0("WHERE species=='",baseShortCode,"'"))
      rs <- dbGetQuery(con, sql)
      rs <- as.vector(t(rs))
      res <- unique(c(res, rs)) ## should not be too bad
    }
  }
  as.character(res)
}

setMethod("keys", "Inparanoid8Db",
    function(x, keytype, ...){
      if(missing(keytype)){stop("Please supply a keytype argument.")}
      smartKeys(x=x, keytype=keytype, ..., FUN=.keysInp8)
    }
)
## usage:
## head(keys(hom.Hs.inp.db, keytype="HOMO_SAPIENS"))
## head(keys(hom.Hs.inp.db, keytype="HOMO_SAPIENS", pattern="4$"))
## k = head(keys(hom.Hs.inp.db, keytype="HOMO_SAPIENS", pattern="4$", column="RATTUS_NORVEGICUS"))
## select(hom.Hs.inp.db, keys=k, cols=c("HOMO_SAPIENS","RATTUS_NORVEGICUS"), keytype="HOMO_SAPIENS")
## length(keys(hom.Hs.inp.db, keytype="HOMO_SAPIENS", column="APIS_MELLIFERA"))
## length(keys(hom.Hs.inp.db, keytype="HOMO_SAPIENS"))

##############################################################################
## Select is more complicated, but I should be able to implement it similar to
## how I did it for Reactome.db

## .UCToStandard <- function(strVec){
##   strVec <- tolower(strVec)
##   firstLett <- toupper(substr(strVec,1,1))
##   rest <- substr(strVec,2,nchar(strVec))
##   paste0(firstLett, rest)
## }




###########################################################################
## New idea:
## It might actually be faster to just get the subqueries for each in another
## set of helpers, and then to merge them in R on the clust_ids...
## This is indeed about a billion times faster.

.extractWithSimpleInpQuery8 <- function(x, table, keys, keytype,
                                       baseShortCode, shortMap){
    shortCode <- shortMap[names(shortMap) %in% table]

  ## Base query for Alt portion
  subQueryAlt <- paste0("SELECT * FROM ", table,
                        " AS alt WHERE species='",shortCode,
                        "' AND seed_status='100%'")

  ## Base query for base portion
  subQueryBase <- paste0("SELECT * FROM ", table,
                         " AS base WHERE species='",baseShortCode,
                         "' AND seed_status='100%'")
  
  
  ## Clause to append to whichever of these matches the keytype
  inClause <- paste( "AND inp_id IN",               
               paste0("('",paste(keys, collapse="','"),"')"))
  ## base or not?  WHO gets the inClause?
  shortMap <- .makeShortNameMapping()
  if(keytype==names(shortMap)[shortMap %in% baseShortCode]){
    subQueryBase <- paste(subQueryBase, inClause)
  }else{
    subQueryAlt <- paste(subQueryAlt, inClause)
  }
  
  ## then extract
  resBase <- dbQuery(dbConn(x), subQueryBase)
  resAlt <- dbQuery(dbConn(x), subQueryAlt)
  ## then merge as an inner join on clust_id for each
  res <- merge(resBase, resAlt, by.x="clust_id", by.y="clust_id")
  res <- res[,c("inp_id.x","inp_id.y")]
  colnames(res) <- c("base.inp_id",shortCode)
  res
}



## This calls .extractWithSimpleInpQuery8 for each table and merges results
.collateInpQueryResults8 <- function(x, tables, keys, keytype, shortMap,
                                    baseShortCode, baseSpecies){  
  mergeID <- "base.inp_id"
  res <- data.frame()
  for(i in seq_len(length(tables))){
    if(i==1){
      if(tables[i]==baseSpecies){
        ## This means that my keytype == baseSpecies
        res <- keys(x, keytype=toupper(baseSpecies))
        res <- res[res %in% keys]
        res <- as.data.frame(res)
        colnames(res) <- mergeID
      }else{ ## Otherwise, we must query the DB
        res <- .extractWithSimpleInpQuery8(x, tables[i], keys, keytype,
                                          baseShortCode, shortMap)
      }
    }else{
      if(i==2){ ## on 2nd pass, set up these vals
        mergeKeys <- res[[mergeID]] 
        mkeytype <- baseSpecies
      }
      if(tables[i] == baseSpecies){
        ## This means that one of later cols == baseSpecies
        res <- cbind(res, res[mergeID])
        colnames(res)[dim(res)[2]] <- baseShortCode
      }else{
        res <- merge(res,
                     .extractWithSimpleInpQuery8(x, tables[i], mergeKeys,
                                                mkeytype, baseShortCode,
                                                shortMap),
                     by.x=mergeID, by.y=mergeID,
                     all.x=TRUE, all.y=TRUE)
      }
    }
  }
  ## last thing is to put baseShortCode instead of "base.inp_id"
  colnames(res)[colnames(res) == "base.inp_id"] <- baseShortCode
  res
}


## function for making select happen
.selectInp8 <- function(x, keys, cols, keytype){
  ## Some argument checking
  .testSelectArgs(x, keys=keys, cols=cols, keytype=keytype)

  ## filter out keys that are not legit (just from the DB query)
  ktKeys = keys(x, keytype=keytype)
  qkeys <- keys[keys %in% ktKeys]
  
  ## now I need to go through each table, and for each I want to extract the
  ## missing piece with a SIMPLE query (basically get ONE thing), and then
  ## append it onto the results
  baseSpecies <- .getBaseSpecies8(x)  
  shortMap <- .makeShortNameMapping()
  baseShortCode <- .getBaseShortCode(baseSpecies)

  ## collate possible types (type must ALWAYS be in front)
  tables <-unique(c(keytype,cols[!(cols %in% keytype)]))
  
  res <- .collateInpQueryResults8(x, .UCToStandard(tables), qkeys, keytype,
                                 shortMap, baseShortCode, baseSpecies)

  ## Setup to call .resort
  ## reqCols must have exactly the same stuff as in tables, but in same format
  ## as header from res
  reqCols <- shortMap[toupper(names(shortMap)) %in% tables]

  ## colType is the table abbreviation that matches the initial keytype.
  colType <- shortMap[toupper(names(shortMap)) %in% keytype]
  
  ## now drop any unrequested cols
  res <- res[,reqCols,drop=FALSE]
  ## And then resort/tidy etc.
  res <- .resort(res, keys, jointype=colType, reqCols=reqCols)

  ## Then match to the colnames
  colnames(res) <- toupper(names(shortMap)[match(colnames(res),shortMap)])
 
  ## return results
  res            
}


## select method
setMethod("select", "Inparanoid8Db",
          function(x, keys, columns, keytype, ...){
              if (missing(keytype)) keytype <- "ENTREZID"
              .selectInp8(x, keys, columns, keytype)
          }
)






## test:
## library(hom.Hs.inp.db); i <- hom.Hs.inp.db;  cols(i); keytypes(i); k= head(keys(i, "MUS_MUSCULUS"));

## select(i, keys=k, cols=c("APIS_MELLIFERA","AEDES_AEGYPTI"), keytype="MUS_MUSCULUS")

## debug(AnnotationDbi:::.selectInp8)

## debug(AnnotationDbi:::.extractWithSimpleInpQuery8)

## debug(AnnotationDbi:::.collateInpQueryResults8)

## working on keys - fixed
## debug(AnnotationDbi:::.keysInp)
## res2 <- head(keys(i, keytype="HOMO_SAPIENS"))



## TODO: make a slew of unit tests similar to those for ReactomeDb





## this still no worky (and maybe it really shouldn't - because the use of
## humans as a keytype means I have to start with the table (in their list)
## that has the most human keys mapped and then go to the next and the next
## etc.  In that case, the path I choose for them would influence the output.
## I don't think we want that kind of responsibility...  For other keys, it's
## OK, because it is a human centered DB, and the human IDs are therefore
## natural as a universal key.  But when human IDs are the central ID And ALSO
## the keytype - this creates a problem because that 1st step can change all
## of the results...
## So I am pretty confident that I want to ban the baseSpecies from the keytypes.
## So TODO?: drop baseSpecies from the keytypes?
## PROBLEM with this idea: HOMO_SAPIENS is a valid keytype for keys()! (and
## really should be)
## For now, I think it's OK if this works for keys (the answer is legit).  But
## it will still not be listed as a legit keytype (even while one of the
## methods works).

## As for cols, I should be able to still have baseSpecies be a valid value
## for cols.







## library(hom.Hs.inp.db); i <- hom.Hs.inp.db;  cols(i); keytypes(i); k= head(keys(i, "MUS_MUSCULUS"));


## now this works
## select(i, keys=k, cols=c("APIS_MELLIFERA","HOMO_SAPIENS"), keytype="MUS_MUSCULUS")



## And this too.
## hk <- head(head(keys(i, keytype="HOMO_SAPIENS")))
## select(i, keys=hk, cols=c("APIS_MELLIFERA","MUS_MUSCULUS"), keytype="HOMO_SAPIENS")



