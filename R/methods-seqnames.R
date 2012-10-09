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

supportedSeqnameMappings <- function(){
  con <- .seqnamesSetup()
  tables <- dbListTables(con)
  names(tables) <- tables
  lapply(tables, function(table) dbReadTable(con, table))
}

findSequenceRenamingMaps <- function(seqnames, style,
                                     best.only=TRUE, drop=TRUE){
  if (!is.character(seqnames))
    stop("'seqnames' must be a character vector")
  if (!isSingleString(style))
    stop("the supplied seqname style must be a single string")
  if (!isTRUEorFALSE(best.only))
    stop("'best.only' must be TRUE or FALSE")
  if (!isTRUEorFALSE(drop))
    stop("'drop' must be TRUE or FALSE")
  supported_styles <- supportedSeqnameStyles()
  ## Get the target species (.e. species supporting the supplied style).
  tmp <- unlist(supported_styles, use.names=FALSE)
  compatible_species <- rep.int(names(supported_styles),
                                elementLengths(supported_styles))
  compatible_species <- compatible_species[tolower(tmp) == tolower(style)]
  if (length(compatible_species) == 0L)
    stop("supplied seqname style \"", style, "\" is not supported")
  ## Walk all the seqname mappings to find the sequence renaming maps
  ## compatible with the specified style.
  seqname_mappings <- supportedSeqnameMappings()
  ans <- lapply(compatible_species,
    function(species) {
      mapping <- seqname_mappings[[species]]
      names(mapping) <- tolower(names(mapping))
      to_seqnames <- mapping[[tolower(style)]]
      lapply(mapping,
        function(from_seqnames)
          to_seqnames[match(seqnames, from_seqnames)])
    })
  ans_ncol <- length(seqnames)
  ans <- matrix(unlist(ans, use.names=FALSE), ncol=ans_ncol, byrow=TRUE)
  colnames(ans) <- seqnames
  score <- rowSums(!is.na(ans))
  idx <- score != 0L
  if (best.only)
    idx <- idx & (score == max(score))  # keep only "best" rows
  ans <- ans[idx, , drop=FALSE]
  ## Remove duplicated rows.
  ans <- as.matrix(unique(as.data.frame(ans, stringsAsFactors=FALSE)))
  if (nrow(ans) == 1L && drop)
    ans <- drop(ans)
  else
    rownames(ans) <- NULL
  ans
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

## check whether or not a style is really a supported seqnameStyle
isSupportedSeqnamesStyle <- function(style, species){
  species <- sub(" ", "_", species)
  possible <- supportedSeqnameStyles()
  availStyles <- possible[[species]]
  style %in% availStyles
}


## Tests:
## library(TxDb.Athaliana.BioMart.plantsmart12); txdb = TxDb.Athaliana.BioMart.plantsmart12; isSupportedSeqnamesStyle("NCBI", species)
## isSupportedSeqnamesStyle("UCSC", species)



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

## seqnames = as.character(c(paste0("chr", c(1:22,"M","Y","X")),"chr6_apd_hap1"))
## styles = c("UCSC","NCBI","ensembl")
## species = "Homo sapiens"
## testSeqnames(styles,seqnames, species)

## seqnames2 = (paste0("chr",1:22))



## This helper returns a full listing of all style listings in the entire DB.
## NOTE: this code can probably be made a lot faster if I am willing for it to
## be less pretty.
listAllSupportedStylesBySpecies <- function(species){
  styles <- supportedSeqnameStyles()[[species]]
  res <- lapply(styles,extractSeqnameSet,species)
  names(res) <- paste0(rep(species, length(styles)),"__" ,styles)
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











