## Select Methods return the results from looking up things (cols) that match
## the keys provided.  cols is a character vector to specify columns the user
## wants back and keys are the keys to look up.

## .select <- function(db, keys, cols, idType){
##   con <- dbConn(db)
##   sql <- paste("SELECT * FROM ",
##                paste(cols, collapse=","),
##                " WHERE ", idType ," IN ('",
##                paste(keys, collapse="','"), "') ",
##                sep="")
##   dbGetQuery(con, sql)
## }

## a helper to make the strings to objects
.makeBimapsFromStrings <- function(db, cols){
  pkgname <- sub(".db$","", AnnotationDbi:::packageName(db))
  lapply(cols, function(x){
    eval(parse(text=paste(pkgname, x, sep="")))
  })
}

## another helper to merge
## Be sure to use all.x=TRUE (and all.y=TRUE), then filter on keys in a later
## step
.mergeBimaps <- function(objs, keys, keyType){
  for(i in seq_len(length(objs))){
    if(i==1){
      finTab <- toTable(objs[[1]])
    }else{
      finTab <- merge(finTab, toTable(objs[[i]]),
                      by=keyType, all.x=TRUE, all.y=TRUE)
    }
  }
  finTab[finTab[[keyType]] %in% keys,]
}

                         
## Select uses merge to combine bimaps.  It needs to know the keyType
## expected, which will vary with the database (and hence the method).
.select <- function(db, keys=NULL, cols, keyType){
  if(is.null(keys)) keys <- keys(db) ## if no keys provided: use them all
  objs <- .makeBimapsFromStrings(db, cols)
  .mergeBimaps(objs, keys, keyType=keyType)
}



setMethod("select", "OrgDb",
    function(db, keys, cols) .select(db, keys, cols, "gene_id")
)

setMethod("select", "ChipDb",
    function(db, keys, cols) .select(db, keys, cols, "probe_id")
)

setMethod("select", "GODb",
    function(db, keys, cols) .select(db, keys, cols, "go_id")
)

## setMethod("select", "InparanoidDb",
##     function(db, keys, cols) .select(db, keys, cols, "gene_id")
## )



## TEST CODE:
## library(org.Hs.eg.db)
## ls(2)

## con = AnnotationDbi:::dbConn(org.Hs.eg.db)
## keys = head(keys(org.Hs.egCHR))
## keys = keys(org.Hs.eg.db)[1:5]
## cols = c("SYMBOL", "UNIPROT")
## select(org.Hs.eg.db, keys, cols)

## idType = "gene_id"



#############################
## Internally we want to reconstruct these guys so we can merge() on them
## c <- cols(GO.db)[7]
## prefix = "GO"
## foo = paste(prefix,c,sep="")
## bar = eval(parse(text=foo))






##############################################################################
## Cols methods return the list of things that users can ask for.  This can be
## just the table names, or it might be a list of mappings


.cols <- function(db){
  meta <- metadata(db) ##bug I need to get7 the package name!
  schema <- meta[meta["name"] == "DBSCHEMA","value"]
  objList <- eval(parse(text=paste("AnnotationDbi:::",schema,
                  "_AnnDbBimap_seeds",sep="")))  
  unlist(lapply(objList, function(x){x$objName}))
}


setMethod("cols", "OrgDb",
    function(x) .cols(x)
)

setMethod("cols", "ChipDb",
    function(x) .cols(x)
)

setMethod("cols", "GODb",
    function(x) .cols(x)
)

## something more tricky required for Inparanoid since a single template
## exists for all (basically I need to do.call() a specific function
## setMethod("cols", "InparanoidDb",
##     function(x) .cols(x)
## )













## Keys methods return the possible primary keys.  So for EG based packages,
## this will be the viable entrez gene IDs.
## Must use SELECT DISTINCT for now because some packages like ag.db
## (Arabidopsis) have repeated probe ids in the probes table (those are the
## probe ids that hit multiple genes).
## TODO: When 'x' has the new slot containing the package name, use
## dbUniqueVals() (defined in SQL.R) and pass pkgname:::datacache to it.
## dbUniqueVals() is what's used behind the scene by the Lkeys/Rkeys/keys
## methods for AnnDbBimap objects so the "keys" methods below will give a
## consistent answer (and will take advantage of the cache).
setMethod("keys", "OrgDb",
    function(x) dbQuery(dbConn(x), "SELECT gene_id FROM genes", 1L)
)

setMethod("keys", "ChipDb",
    function(x) dbQuery(dbConn(x), "SELECT DISTINCT probe_id FROM probes", 1L)
)

setMethod("keys", "GODb",
    function(x) dbQuery(dbConn(x), "SELECT go_id FROM go_term", 1L)
)

## for Inparanoid, we want to select keys carefully (depeninding on the
## organism - which is in the metadata)
## setMethod("keys", "InparanoidDb",
##     function(x) as.character(t(dbGetQuery(dbConn(x),
##                                            "SELECT gene_id FROM genes")))
## )



