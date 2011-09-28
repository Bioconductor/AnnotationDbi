## Select Methods return the results from looking up things (cols) that match
## the keys provided.  cols is a character vector to specify columns the user
## wants back and keys are the keys to look up.

.select <- function(con, keys, cols, idType){  
  sql <- paste("SELECT * FROM ",
               paste(cols, collapse=","),
               " WHERE ", idType ," IN ('",
               paste(keys, collapse="','"), "') ",
               sep="")
  dbGetQuery(con, sql)
}

## ## lets just have it merge() bimaps
## .select <- function(con, keys, cols, idType){  

## }



setMethod("select", "OrgDb",
    function(db, keys, cols) .select(dbConn(db), keys, cols, "gene_id")
)


setMethod("select", "ChipDb",
    function(db, keys, cols) .select(dbConn(db), keys, cols, "probe_id")
)


## setMethod("select", "InparanoidDb",
##     function(db, keys, cols) .select(dbConn(db), keys, cols, "gene_id")
## )


setMethod("select", "GODb",
    function(db, keys, cols) .select(dbConn(db), keys, cols, "go_id")
)


## library(org.Tguttata.eg.db)
## ls(2)
## select(Tguttata_eg_OrgDb)



## Cols methods return the list of things that users can ask for.  This can be
## just the table names, or it might be a list of mappings

.cols <- function(con){
  dbListTables(con)## TEMP: Not what we actually want
}


setMethod("cols", "OrgDb",
    function(x) .cols(dbConn(x))
)


setMethod("cols", "ChipDb",
    function(x) .cols(dbConn(x))
)


## setMethod("cols", "InparanoidDb",
##     function(x) .cols(dbConn(x))
## )


setMethod("cols", "GODb",
    function(x) .cols(dbConn(x))
)



## Keys methods return the possible primary keys.  So for EG based packages,
## this will be the viable entrez gene IDs.

## .keys <- function(con){
##   as.character(t(dbGetQuery(con, "SELECT gene_id FROM genes")))
## }

setMethod("keys", "OrgDb",
    function(x) as.character(t(dbGetQuery(dbConn(x),
                                           "SELECT gene_id FROM genes"))) 
)


setMethod("keys", "ChipDb",
    function(x) as.character(t(dbGetQuery(dbConn(x),
                                           "SELECT probe_id FROM probes")))
)


## setMethod("keys", "InparanoidDb",
##     function(x) as.character(t(dbGetQuery(dbConn(x),
##                                            "SELECT gene_id FROM genes")))
## )


setMethod("keys", "GODb",
    function(x) as.character(t(dbGetQuery(dbConn(x),
                                           "SELECT go_id FROM go_term")))
)




## library(org.Hs.eg.db)
## con = AnnotationDbi:::dbConn(org.Hs.eg.db)
## keys = keys(org.Hs.eg.db)[1:5]
## cols= "genes"
## select(org.Hs.eg.db, keys, cols)

## idType = "gene_id"
