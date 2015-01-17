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
  con <- dbconn(x)
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
  con <- dbconn(x)
  if(baseSpecies != lckeytype){
    sql <- paste("SELECT inp_id FROM", lckeytype,
                 paste0("WHERE species!='",baseShortCode,"'"))
    res <- dbGetQuery(con, sql)
    res <- as.vector(t(res))
  }else{
    res <- character()
    tables <- dbListTables(con)
    tables <- tables[!(tables %in% c(baseSpecies,'metadata'))]
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
  resBase <- dbQuery(dbconn(x), subQueryBase)
  resAlt <- dbQuery(dbconn(x), subQueryAlt)
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




## Then from the DB you can do this
## library(AnnotationDbi); hom.Homo_sapiens.inp8.db <- loadDb('hom.Homo_sapiens.inp8.sqlite')


## test keytypes and columns()
## keytypes(hom.Homo_sapiens.inp8.db)
## columns(hom.Homo_sapiens.inp8.db)
##  Results are OK...

## This *appears* to work
## kp = head(keys(hom.Homo_sapiens.inp8.db, keytype="PONGO_ABELII"))
## This will take longer but should work now.
## k = head(keys(hom.Homo_sapiens.inp8.db, keytype="HOMO_SAPIENS"))


## This doesn't work right (in part, because of 5 letter code holdover stuff)
## I need to make some simpler methods for inparanoid8 stuff...
## select(hom.Homo_sapiens.inp8.db, keys=k, columns="MUS_MUSCULUS", keytype="HOMO_SAPIENS")


## select(hom.Homo_sapiens.inp8.db, keys=kp, columns="MUS_MUSCULUS", keytype="PONGO_ABELII")

## Seems to work!

