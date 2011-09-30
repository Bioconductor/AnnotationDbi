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

## ## lets just have it merge() bimaps
.select <- function(db, keys, cols, idType){  
##  con <- dbConn(db)
  
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

## keys = keys(org.Hs.eg.db)[1:5]
## cols= "genes"
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

setMethod("keys", "OrgDb",
    function(x) as.character(t(dbGetQuery(dbConn(x),
                                           "SELECT gene_id FROM genes"))) 
)

setMethod("keys", "ChipDb",
    function(x) as.character(t(dbGetQuery(dbConn(x),
                                           "SELECT probe_id FROM probes")))
)

setMethod("keys", "GODb",
    function(x) as.character(t(dbGetQuery(dbConn(x),
                                           "SELECT go_id FROM go_term")))
)

## for Inparanoid, we want to select keys carefully (depeninding on the
## organism - which is in the metadata)
## setMethod("keys", "InparanoidDb",
##     function(x) as.character(t(dbGetQuery(dbConn(x),
##                                            "SELECT gene_id FROM genes")))
## )



