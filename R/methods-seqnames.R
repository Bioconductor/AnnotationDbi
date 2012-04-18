###############################################################################
## Code to access the special chromosome name conventions DB.

## setup for accessing the seqnames DB
.seqnamesSetup <- function(){
  require("RSQLite")
  require("seqnames.db")
  ## get a connection to this
  db <- system.file("extdata","seqnames.sqlite",package="seqnames.db")
  con <- dbConnect("SQLite", dbname=db)
  con
}

## A discovery method for users to learn the supported seqname styles
supportedSeqnameStyles <- function(){
  con <- .seqnamesSetup()
  tables <- dbListTables(con)
  getFields <- function(table, con){
    fields <- dbListFields(con, table)
  }
  fields <- lapply(tables, getFields, con)
  names(fields) <- tables
  fields
}


## A function for retrieving one type of data
## usage: extractSeqnameSet("NCBI", "Rattus norvegicus")
extractSeqnameSet <- function(style="UCSC", species="Homo sapiens"){
  oriSpecies <- species
  species <- sub(" ", "_", species)
  con <- .seqnamesSetup()
  ## quick test:
  supNames <- supportedSeqnameStyles()
  if(any(style %in% supNames[[species]])){
    sql <- paste("SELECT",style,"FROM", species)
    result <- as.character(t(dbGetQuery(con,sql)))
  }else{stop("The style specified by '",style,
             "' does not have a compatible entry for the species ",oriSpecies)}
  result
}


## A function for testing if a vector of data is the correct thing
## "TRUE" Usage example
## testSeqnames(seqnames=c(1:20,"X","MT"),"NCBI","Rattus norvegicus")
## "FALSE" Usage example
## testSeqnames(seqnames=c(1:22,"X","Y","MT"),"NCBI","Rattus norvegicus")
testSeqnames <- function(seqnames, style="ensembl", species="Homo sapiens"){
  trueSeq <- extractSeqnameSet(style=style,species=species)
  identical(seqnames, trueSeq)
}

