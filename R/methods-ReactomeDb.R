## 1st the legacy stuff:
### Mandatory fields: objName, Class and L2Rchain
reactome_AnnDbBimap_seeds <- list(
    list(
        objName="PATHID2NAME",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="pathway2name",
                Lcolname="DB_ID",
                Rcolname="path_name"
            )
        )
    ),
    list(
        objName="PATHID2EXTID",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="pathway2gene",
                Lcolname="DB_ID",
                Rcolname="gene_id"
            )
        )
    ),
    list(
        objName="REACTOMEID2GO",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="reactome2go",
                Lcolname="DB_ID",
                Rcolname="go_id"
            )
        )
    )
)

createAnnObjs.reactome <- function(prefix, objTarget, dbconn, datacache)
{
    ##checkDBSCHEMA(dbconn, "reactome")

    ## AnnDbBimap objects
    seed0 <- list(
        objTarget=objTarget,
        datacache=datacache
    )
    ann_objs <- createAnnDbBimaps(reactome_AnnDbBimap_seeds, seed0)

    ## Reverse maps
    ann_objs$PATHNAME2ID <- revmap(ann_objs$PATHID2NAME, objName="PATHNAME2ID")
    ann_objs$EXTID2PATHID <- revmap(ann_objs$PATHID2EXTID, objName="EXTID2PATHID")
    ann_objs$GO2REACTOMEID <- revmap(ann_objs$REACTOMEID2GO, objName="GO2REACTOMEID")

    ## 1 special map that is not an AnnDbBimap object (just a named integer vector)
    ann_objs$MAPCOUNTS <- createMAPCOUNTS(dbconn, prefix)

    prefixAnnObjNames(ann_objs, prefix)
}



## newer select methods:
setMethod("keytypes", "ReactomeDb",
    function(x) c("ENTREZID","GO","PATHNAME","PATHID","REACTOMEID")
)

setMethod("cols", "ReactomeDb",
    function(x) c("ENTREZID","GO","PATHNAME","PATHID","REACTOMEID")
)

.keysReact <- function(x, keytype){
  sql <- switch(keytype,
                "ENTREZID" = "SELECT gene_id FROM pathway2gene",
                "GO" = "SELECT go_id FROM reactome2go",
                "PATHNAME" = "SELECT path_name FROM pathway2name",
                "PATHID" = "SELECT DB_ID FROM pathway2name",
                ## need proper reactome tables for this one:
                "REACTOMEID" = "SELECT DB_ID FROM DatabaseObject", 
                stop("No keytype specified for .keys"))
  unique(dbQuery(dbConn(x), sql, 1L))
}

setMethod("keys", "ReactomeDb",
    function(x, keytype){
      if(missing(keytype)) keytype <- "ENTREZID"
      .keysReact(x, keytype)
    }
)


## I think the way to go is to generate the SQL based on table associations.
## pathways_ids are DB_IDs (a subset)
## DatabaseObject and all standard reactome tables also have DB_IDs as keys...
## So to get stuff, I just need to make an outer join across all affected tables, joining on DB_IDs etc as appropriate.  Then at the end, I need to go to uppercase, and drop any unwanted columns.  While we are in here, I should probably clean up the schema a bit (change reactome_id over to DB_ID on the supplemental tables - DONE)

## This could all get really complicated if we start to expand this a lot...


## match a col value to appropriate table
## Can ALSO match to a subquery (when appropriate)
## every thing that this function returns must contain the central ID.
## TODO: this needs to actually match up with TWO things (more complex):
## 1) the table/query (done).
## 2) the colnames
.getTable <- function(col, retVal="table"){
  res <- switch(col,
                "ENTREZID" = c("pathway2gene","gene_id"),
                "GO" = c("reactome2go","go_id"),
                "PATHNAME" = c("pathway2name","path_name"),
                "PATHID" = c("pathway2name","DB_ID"),
                ## need proper reactome tables
                "REACTOMEID" = c("DatabaseObject","DB_ID"), 
                stop("No col specified for .keys"))
  ## Then test and return appropriate records.
  if(retVal=="table"){
    res <- res[[1]]
  }else if(retVal=="colname"){
    res <- res[[2]]
  }
  res
}

## match all cols
.getTables <- function(cols, retVal="table"){
  res <- character(length(cols))
  for(i in seq_len(length(cols))){
      res[i] <- .getTable(cols[i], retVal=retVal)
  }
  names(res) <- cols 
  unique(res)
}




## ## FUTURE TODO: if we have more than one table per type (and we will), then we
## ## need to have a way to get all those together and do that join as a
## ## subquery, but then treat it as a single table for the rest of the way...

## ## match a pair of things to a join
## .makeJoinWhereClause <- function(table1, table2){
##   joins <- character()
##   if(!missing(table1) && !missing(table2)){
##     ## Two tables that need to be joined.
##     joins <- paste(c(paste0(table1,".DB_ID"),
##                      paste0(table2,".DB_ID")),
##                    collapse="=") 
##   }else{
##     ## Then there is only one table involved (no join needed)
##   }
##   joins
## }
## ## .makeJoinWhereClause("pathway2gene","pathway2name")


## ## match a pair of things to a join
## .makeJoinRelationship <- function(table1, table2){
##   joins <- character()
##   if(!missing(table1) && !missing(table2)){
##     ## Two tables that need to be joined.
##     joins <- paste(table1,"LEFT OUTER JOIN",table2, "ON") 
##   }else{
##     ## Then there is only one table involved (no join needed)
##   }
##   joins
## }
## ## .makeJoinRelationship("pathway2gene","pathway2name")


## ## match all pairs for a given vector.
## ## Initially I thought to make use of this to solve the problem.
## ## combn(LETTERS[1:4], 2L)
## ## But no, because that it way too many joins.  All I really need is one join
## ## for each link in the chain.  So if I have a, b, c.  I just need a-b and
## ## b-c.  I don't care about a-c etc.  Number of links will always be n-1
## ## (where n is the number of types.  what I really need to use here is
## ## split...  n <- length(types) - 1 a b c a b b c So remove 1st and last, Then
## ## rep(middleVals, each=2) then append 1st and last back on.  Then split by 2s

## ## needed to properly rep the tables vector.
## .stutter <- function(tables){
##   if(length(tables>2)){
##     start <- tables[1]
##     n <- length(tables)
##     end <- tables[n]
##     tablesTrunc <- tables[2:(n-1)]
##     tablesReps <- rep(tablesTrunc, each=2)
##     res <- c(start, tablesReps, end) 
##   }else{
##     stop("To stutter you need at least 3 tables")
##   }
##     split(repTables, rep(1:(n-1), each=2))
## }


## ## Needed to process all the joins
## .makeJoins <- function(tables){
##   n <- length(tables)
##   res <- character()
##   if(length(tables)==2){
##     res[1] <- paste(.makeJoinRelationship(tables[1],tables[2]),
##                     .makeJoinWhereClause(tables[1],tables[2]), "UNION",
##                     .makeJoinRelationship(tables[2],tables[1]),
##                     .makeJoinWhereClause(tables[2],tables[1]))
##   }else{
##     repChunks <- .stutter(tables) 
##     ## Then 
##     for(i in seq_len(length(repChunks))){
##       res[i] <-paste(.makeJoinRelationship(repChunks[[i]][1],repChunks[[i]][2]),
##                      .makeJoinWhereClause(repChunks[[i]][1],repChunks[[i]][2]),
##                      "UNION",
##                      .makeJoinRelationship(repChunks[[i]][2],repChunks[[i]][1]),
##                      .makeJoinWhereClause(repChunks[[i]][2],repChunks[[i]][1]))
      
##     }
##   }
##   unique(res)
## }
## types = c("ENTREZID", "PATHNAME")
## types = c("ENTREZID", "PATHNAME", "GO")
## tables = .getTables(types)
## .makeJoins(tables)

## This is for making simple queries
.extractWithSimpleQuery <- function(x, table, colType, keys){
  ## generate a simple query for each table
  sql <- paste("SELECT * FROM", table, "WHERE", colType,"IN",
               paste0('("',paste(keys, collapse='","'),'")') )
  ## then extract it
  dbQuery(dbConn(x), sql)
}

## this calls .extractWithSimpleQuery for each table and merges results
.collateQueryResults <- function(x, tables, colType, keys, mergeID="DB_ID"){
  res <- data.frame()
  mergeKeys <- character()
  for(i in seq_len(length(tables))){
    if(i==1){
      res <- .extractWithSimpleQuery(x, tables[i], colType, keys)
    }else{
      if(i==2){ mergeKeys <- res[[mergeID]] } ## cond. for speed
      res <- merge(res,
                   .extractWithSimpleQuery(x, tables[i],
                                           colType=mergeID,
                                           keys=mergeKeys),
                   by.x=mergeID, by.y=mergeID,
                   all.x=TRUE, all.y=TRUE)
    }
  }
  res
}




## function for making select happen
.selectReact <- function(x, keys, cols, keytype){
  ## check that the keys are of the correct keytype.
  .testIfKeysAreOfProposedKeytype(x, keys, keytype)
  ## filter out keys that are not legit (just from the DB query)
  ktKeys = keys(x, keytype=keytype)
  qkeys <- keys[keys %in% ktKeys]
  
  ## collate possible types (type must ALWAYS be in front)
  types <-unique(c(keytype,cols)) ## this was not enough

  ## translate to relevant "tables" (some virtual) that I need stuff from
  tables <- .getTables(types)

  ## need to know the colType that goes with the INITIAL keytype..
  colType <- .getTable(keytype, retVal="colname")
  
  ## now I need to go through each table, and for each I want to extract the
  ## missing piece with a SIMPLE query (basically get ONE thing), and then
  ## append it onto the results
  res <- .collateQueryResults(x, tables, colType, qkeys, mergeID="DB_ID")


  ## .resort just needs:
  ## the results,
  ## ALL of the keys,
  ## the colname that matches the keys (jointype or colType) and
  ## the reqCols (which means the column names expected to be in the results
  ## at this point,
  ## primary object (i.e., TxDb, OrgDb etc.))
  
  reqCols <- .getTables(types, retVal="colname")
  ## now drop any unrequested cols
  res <- res[,reqCols,drop=FALSE]
  ## And then resort/tidy etc.
  res <- .resort(res, keys, jointype=colType, reqCols=reqCols)

  ## Capture relationship between uc and lc names
  names(reqCols) <- types
  ## Then match to the colnames
  colnames(res) <- names(reqCols)[match(colnames(res),reqCols)]
  
  ## OLDE (delete later):
  ## joins <- .makeJoins(tables)
  ## create a join for these tables.
  ## TODO: add keys to this query.
  ## sql <- paste("SELECT * FROM",joins, collapse=" UNION ")  
  ## res <- dbQuery(dbConn(x), sql, 1L)

  
  ## TODO: sort the results
  res            
}


## select method
setMethod("select", "ReactomeDb",
    function(x, keys, cols, keytype){
          if (missing(keytype)) keytype <- "ENTREZID"
          .selectReact(x, keys, cols, keytype)
        }
)



## TODO:
## Change this to be either full outer joins, or devise a new strategy???
## Remove repeated cols? (maybe done for me by .resort?)
## .resort()
## translate names (needs some new functions.
## Add a WHERE IN clause to the end for filtering out the keys...


## Full outer joins are just not going to work.  I need a different approach.
## I need instead to just write a nibbler.  Something that will just nibble at
## the SQL DB as needed and get just what is needed one slice at a time (just
## not practical to generate big ass queries all the time).

## The code will know
## 1) what tables have the data for each cols value
## 2) for each nibble (bite) it can call a simpler select function that just
## "knows" how to make VERY simple SQL queries based on the .getTables
## function idea (including the original keys).  For more hopeless queries,
## subqueries can be placed in this function, but the point is that this
## function will only know how to get one kind of thing at a time.  
## 3) then the results of each nibble can be merged into place in R

## This above part is mostly "done" (except I have to pass keys in!)

## Next comes the part where I remap col names and re-order/resort the results.
## 4) Use our method (.resort) to clean up
## 5) rename the columns (this should be mapped up near where we specify what tables/queries are needed to extract based on COLs)

## WHY DO IT THIS WAY?  Because if I write/generate a SQL query I end up
## pulling WAY more stuff back than I will ever need.  This is slow and
## painful and the queries are all going to have to start as full outer joins,
## and I don't even know a priori what order they should be put in...  In
## actual fact, I only want the records that go with my keys.  So I will look
## up matches one tiny query at a time, and then merge the results together
## for a hybrid approach that I hope will be more performant.


## So to pull this off, I will need to deal with the fact that sometimes the
## keys I need will change mid-job.  So I may start with GO IDs, so fine, I
## look up the reactome IDs (easy), but then I also need Entrez IDs, my code
## needs to be smart enough to use reactome IDs next after it fails to be able
## to nibble out the Entrez IDs using just the GO IDs.  And it needs to have a
## logical set of fall back keys to use...








## TODO:
## 1) make this work when the initial keys don't all match. - filtering these in .reactSelect before calling to the DB is probably sufficient.... - DONE

## 2) find an example where the dim(res) is not the same number every time.  IOW, verify that merge nibbling is not the equivalent of an inner join... - hint: just test with other IDs like DB_IDs - DONE

## 3) implement filtering and column renaming. - almost done, still have to do renaming and vectorize the colnames identifing (this latter step should take care of #4 below) - DONE
## 4) remove columns that are not requested from the results - DONE

## Bugs:
## 6) Do I have a .resort bug with keys becoming duplicated???  - fixed?

## 7) Add documentation aliases





## 5) Finish up unit tests. (all here - but they get their own page)


