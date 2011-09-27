## Select Methods

.select <- function(con, keys, cols){
  dbGetQuery(con, "SELECT * FROM genes")
}

setMethod("select", "OrgDb",
    function(db, keys, cols) .select(dbConn(db), keys, cols)
)


setMethod("select", "ChipDb",
    function(db, keys, cols) .select(dbConn(db), keys, cols)
)


setMethod("select", "InparanoidDb",
    function(db, keys, cols) .select(dbConn(db), keys, cols)
)


## library(org.Tguttata.eg.db)
## ls(2)
## select(Tguttata_eg_OrgDb)
