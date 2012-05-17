###############################################################################
## Code to access the special chromosome name conventions DB.
## The methods in this file are all about access to the seqnames.db database

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

## check whether or not a value is really a supported seqnameStyle
isSupportedSeqnamesStyle <- function(species, value){
  species <- sub(" ", "_", species)
  possible <- supportedSeqnameStyles()
  availStyles <- possible[[species]]
  value %in% availStyles
}


## Tests:
## library(TxDb.Athaliana.BioMart.plantsmart12); txdb = TxDb.Athaliana.BioMart.plantsmart12; isSupportedSeqnamesStyle(species, "NCBI")
## isSupportedSeqnamesStyle(species, "UCSC")



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
## .testSeqnames("NCBI",seqnames=c(1:20,"X","MT"),"Rattus norvegicus")
## "FALSE" Usage example
## .testSeqnames("NCBI",seqnames=c(1:22,"X","Y","MT"),"Rattus norvegicus")
.testSeqnames <- function(style, seqnames, species){
  ## trueSeq is expected to be the shorter (definitive) list.
  trueSeq <- extractSeqnameSet(style=style,species=species)
  all(trueSeq %in% seqnames)
}

## vectorized for multiple styles
## examples
## testSeqnames(c("ensembl","NCBI"),seqnames=c(1:22,"X","Y","MT"),"Rattus norvegicus")
testSeqnames <- function(styles=c("ensembl", "UCSC"), seqnames, species="Homo sapiens"){
  unlist(lapply(styles, .testSeqnames, seqnames=seqnames, species=species))
}


## Testing:
## seqnames = as.character(c(1:5,"Pt","Mt"))
## styles = c("NCBI","ensembl")
## species = "Arabidopsis thaliana"
## testSeqnames(styles,seqnames, species)

## seqnames = as.character(c(paste("chr",1:22,sep=""),paste("chr",c("M","Y","X"),sep=""),"chr6_apd_hap1"))
## styles = c("UCSC","NCBI","ensembl")
## species = "Homo sapiens"
## testSeqnames(styles,seqnames, species)

## seqnames2 = (paste("chr",1:22, sep=""))



## This helper returns a full listing of all style listings in the entire DB.
## NOTE: this code can probably be made a lot faster if I am willing for it to
## be less pretty.
listAllSupportedStylesBySpecies <- function(species){
  styles <- supportedSeqnameStyles()[[species]]
  res <- lapply(styles,extractSeqnameSet,species)
  names(res) <- paste(rep(species, length(styles)),"__" ,styles, sep="")
  res
}


listAllSupportedSeqnameStyles <- function(){
  species <- names(supportedSeqnameStyles())
  ## then call extractSeqnameSet() for each style.
  unlist(lapply(species,listAllSupportedStylesBySpecies), recursive=FALSE)
}


## This helper takes no arguments and just returns all the possible seqnames in the whole DB (in no particular order, just the unique set).
supportedSeqnames <- function(){
  res <- unique(unlist(listAllSupportedSeqnameStyles()))
  names(res) <- NULL
  res
}












###############################################################################
###############################################################################
###############################################################################
###                   Code for generating seqnames.db.                      ### 
###############################################################################
###############################################################################
###############################################################################

###############################################################################
## organisms to start with:
## arabidopsis
## celegans
## drosophila
## human
## mouse
## rat
## yeast


###############################################################################
## To generate a fresh DB simply:
## 1) place any new csv files into the extdata/dataFiles dir and then:
##    R CMD INSTALL AnnotationDbi
## 2) Call generateSeqnames.db()
##



###############################################################################
## function to help create and populate a seqnames DB from csv files.
## 'specString' is species name (must match a file), and
## 'where' is path to location for the DB

writeTable <- function(specString, con){
  tables <- dbListTables(con)
  if(any(specString %in% tables)){
    stop(paste("there is already a",specString,"table"))
  }
  df <- system.file("seqnames-template","inst","extdata","dataFiles",
                     package="AnnotationDbi")
  data <- read.csv(file.path(df, paste(specString,".csv",sep="")))
  message("Creating table: ",specString)
  sqliteWriteTable(con, specString, value = data, row.names = FALSE)
}



###############################################################################
## regenDB() is for 1) creating the tables, 2) reading in the appropriate
## files, and populating the DB.
## 'where' defines the path to the created DB.

regenDb <- function(where){
  require(RSQLite)
  con <- dbConnect("SQLite", dbname=file.path(where,"seqnames.sqlite"))
  ## list files in the extData/tabfiles dir
  dfs <- system.file("seqnames-template","inst","extdata","dataFiles",
                     package="AnnotationDbi")
  files <- gsub(".csv","",dir(dfs))
  ## Then for each file, call makeTable
  lapply(files, writeTable, con=con)
}

## usage example:
## regenDb(where=".")




## A function to create the a new DB from the template.
generateSeqnames.db <- function(version, outdir="."){
  require(RSQLite)
  ## use creatPackage (from Biobase)
  symbolValues <- list(VERSION=version,
                       ANNOTATIONDBIVERSION = "1.19.4")
  message("Creating the package directories")
  createPackage(pkgname="seqnames.db",
                destinationDir = outdir,
                originDir = system.file("seqnames-template",
                  package="AnnotationDbi"),
                symbolValues = symbolValues,
                unlink = TRUE,
                quiet = TRUE)
  
  message("Creating the database")
  ## now generate the DBs
  regenDb(where=file.path(getwd(),"seqnames.db","inst","extdata"))
}


## require(AnnotationDbi); AnnotationDbi:::generateSeqnames.db(version="1.1.0")


## TODO: export generateSeqnames.db and write a manual page for it.
