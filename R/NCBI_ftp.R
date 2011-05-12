## This file is to download a DB based on NCBI files from their FTP site.

## Need a helper function to make a temp table and put a file into it.  
## This should take an argument to allow it to set things as not-temp 
## (for debugging etc.)

## Need a function to coordinate the DL of the different files and put them 
## into temp tables.

## Need a function to coordinate the processing of the temp tables into 
## permanent org DB tables.

## EGSOURCEURL="ftp://ftp.ncbi.nlm.nih.gov/gene/DATA"
# curl --disable-epsv -O $BASE_URL/gene2go.gz
# curl --disable-epsv -O $BASE_URL/gene2pubmed.gz
# curl --disable-epsv -O $BASE_URL/gene2refseq.gz
# curl --disable-epsv -O $BASE_URL/gene2accession.gz
# curl --disable-epsv -O $BASE_URL/gene2unigene
# curl --disable-epsv -O $BASE_URL/mim2gene
# curl --disable-epsv -O $BASE_URL/gene_info.gz
# curl --disable-epsv -O $BASE_URL/gene_refseq_uniprotkb_collab.gz


# ## RCurl examples (which inexplicably do not work)
# ## So that at least there is a package to support curl:
# file = getURL("ftp://ftp.ncbi.nlm.nih.gov/gene/DATA/README")
# ## This should work:
# url = "ftp://ftp.ncbi.nlm.nih.gov/gene/DATA/gene2go.gz"
# bin = getBinaryURL(url)
# tmp = tempfile()
# write(bin, file = tmp)
# file = gzfile(tmp)
# ## Well this is exasperating! (neither of these works)
# txt = read.delim(file, header=FALSE, sep="\t", skip=1)
# txt2 = read.delim(tmp, header=FALSE, sep="\t", skip=1)



## list requirements
require(RSQLite)

## I need a list of files, along with their column names 
## (needed for schema definitions later)
files = list("gene2go.gz" = c("tax_id","gene_id","go_id","evidence",
               "go_qualifier", "go_description","pubmed_id","category"),
            "gene2pubmed.gz" = c("tax_id","gene_id", "pubmed_id"),
            "gene2accession.gz" = c("tax_id","gene_id","status","rna_accession",
              "rna_gi","protein_accession","protein_gi","genomic_dna_accession",
              "genomic_dna_gi","genomic_start","genomic_end","orientation",
              "assembly"),
            "gene2refseq.gz" = c("tax_id","gene_id","status","rna_accession",
              "rna_gi","protein_accession","protein_gi","genomic_dna_accession",
              "genomic_dna_gi","genomic_start","genomic_end","orientation",
              "assembly"),
            "gene2unigene.gz" = c("gene_id","unigene_id"),
            "gene_info.gz" = c("tax_id","gene_id","symbol","locus_tag",
              "synonyms","dbXrefs","chromosome","map_location","description",
              "gene_type","nomenclature_symbol","nomenclature_name",
              "nomenclature_status","other_designations"),
            "mim2gene.gz" = c("mim_id","gene_id","relation_type"),
            "gene_refseq_uniprotkb_collab.gz" = c("refseq_id","uniprot_id")
)


## For now, I am just going to use what I know works 
## (even though depending on curl hides a dependency which is BAD umkay):
## But the following chunck should at least download a file and return a 
## (cleaned) data.frame from it
.downloadData <- function (file) { ## file is something like: "gene2go.gz"
  url <- paste("ftp://ftp.ncbi.nlm.nih.gov/gene/DATA/",file, sep="")
  tmp <- tempfile()
  download.file(url, tmp, method="curl", quiet=TRUE)
  vals <- read.delim(tmp, header=FALSE, sep="\t", skip=1)
  vals
}


## need to be able to generate the columns data from the files and the types.
.generateCols <- function (file) {
  types <- c("tax_id" = "INTEGER NOT NULL",
             "gene_id" = "INTEGER NOT NULL",
             "go_id" = "TEXT NOT NULL",
             "evidence" = "TEXT",
             "go_qualifier" = "TEXT",
             "go_description" = "TEXT",
             "pubmed_id" = "INTEGER",
             "category" = "TEXT",
             "status" = "TEXT",
             "rna_accession" = "TEXT",
             "rna_gi" = "INTEGER",
             "protein_accession" = "TEXT",
             "protein_gi" = "INTEGER",
             "genomic_dna_accession" = "TEXT",
             "genomic_dna_gi" = "INTEGER",
             "genomic_start" = "INTEGER",
             "genomic_end" = "INTEGER",
             "orientation" = "TEXT",
             "assembly" = "TEXT",
             "unigene_id" = "TEXT",
             "symbol" = "TEXT",
             "locus_tag" = "TEXT",
             "synonyms" = "TEXT",
             "dbXrefs" = "TEXT",
             "chromosome" = "TEXT",
             "map_location" = "TEXT",
             "description" = "TEXT",
             "gene_type" = "TEXT",
             "nomenclature_symbol" = "TEXT",
             "nomenclature_name" = "TEXT",
             "nomenclature_status" = "TEXT",
             "other_designations" = "TEXT",
             "mim_id" = "INTEGER NOT NULL",
             "relation_type" = "TEXT",
             "refseq_id" = "TEXT NOT NULL",
             "uniprot_id" = "TEXT NOT NULL"
             )
  cols <- character()
  for(i in seq_len(length(file[[1]]))){
    cols[i] <-  paste(file[[1]][i],types[file[[1]]][i])
  }
  paste(cols, collapse =",")
}


## also need somethign to generate the insert statement.
.generateINSERTStatement <- function (file) {
  Qs <- paste(rep("?",length(file[[1]])),  collapse=",")
  paste("INSERT INTO ",names(file),"(data) VALUES(",Qs,");",sep="")
}

## need code to generate a table for each of the
## file below is like: files[1] or files[2] 
.createTEMPTable <- function (con, file) {
  data <- .downloadData(names(file))
  table <- names(file)
  cols <- .generateCols(file)
  message(paste("Populating ",table," table:", sep=""))
  sql<- paste("    CREATE TABLE IF NOT EXISTS ",table," (
  ",cols,"
  #     _id INTEGER PRIMARY KEY,
  #     gene_id VARCHAR(10) NOT NULL UNIQUE           -- Entrez Gene ID
    );")
  sqliteQuickSQL(con, sql)
  
  data <- data.frame(data) ## TODO: data.frame() necessary???
  sql <- .generateINSERTStatement(file)
  # sql<- paste("INSERT INTO ",table,"(data) VALUES(?);",sep="")
  dbBeginTransaction(con)
  dbGetPreparedQuery(con, sql, data)
  dbCommit(con)
  message(paste("table ",table," filled",sep=""))
}


## loop through and make the tables
# con <- dbConnect(SQLite(), "TEST.sqlite")
# for(i in seq_len(length(files))){
#   .createTEMPTable(con, files[[i]])
# }






