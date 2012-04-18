###############################################################################
## Code to access the special chromosome name conventions DB.

supportedSeqnameStyles <- function(){
  require("RSQLite")
  require("seqnames.db")
  ## get a connection to this
  db <- system.file("extdata","seqnames.sqlite",package="seqnames.db")
  con <- dbConnect("SQLite", dbname=db)
  tables <- dbListTables(con)
  getFields <- function(table, con){
    fields <- dbListFields(con, table)
  }
  fields <- lapply(tables, getFields, con)
  names(fields) <- tables
  fields
}
