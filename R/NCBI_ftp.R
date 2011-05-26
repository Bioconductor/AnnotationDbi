#########################################################################
#########################################################################
####         Helper functions for generating a generic DB            ####
#########################################################################
#########################################################################

## This makes the central table of an EG DB.
.makeCentralTable <- function(entrez, con){
  message("Populating genes table:")
  sql<- paste("    CREATE TABLE IF NOT EXISTS genes (
      _id INTEGER PRIMARY KEY,
      gene_id VARCHAR(10) NOT NULL UNIQUE           -- Entrez Gene ID
    );")
  sqliteQuickSQL(con, sql)

  gene_id <- data.frame(entrez) ## TODO: data.frame() necessary???
  sql<- paste("INSERT INTO genes(gene_id) VALUES(?);")
  dbBeginTransaction(con)
  dbGetPreparedQuery(con, sql, gene_id)
  dbCommit(con)
  message("genes table filled")
}

## The following takes a data.frame and produces a simple table from that.  It
## expects that the 1st column of that data.frame will always be entrez gene
## IDs.  All fields are assumed to be varchars of size equal to the values in
## fieldNameLens.  TODO: The cols in data have to be named and of equal
## length.  indFields is a character vector with the names of fields that we
## want indexed.  By default only _id will be indexed.
.makeSimpleTable <- function(data, table, con, fieldNameLens=25,
                             indFields="_id"){
  message(paste("Populating",table,"table:"))
  ## For temp table, lets do it like this:
  if(dim(data)[1] == 0){
    ## if we don't have anything to put into the table, then we don't even
    ## want to make a table.
    warning(paste("no values found for table ",table,
                  " in this data chunk.", sep=""))
    return()
  }else{
    dbWriteTable(con, "temp", data, row.names=FALSE)
    ## Then we have to create our real table.
    tableFieldLines <- paste(paste(names(data)[-1]," VARCHAR(",
                                 fieldNameLens,") NOT NULL,    -- data"),
                           collapse="\n       ")
    sql<- paste("    CREATE TABLE IF NOT EXISTS",table," (
      _id INTEGER NOT NULL,                         -- REFERENCES genes
      ",tableFieldLines,"
      FOREIGN KEY (_id) REFERENCES genes (_id)
    );") 
    sqliteQuickSQL(con, sql)
    selFieldLines <- paste(paste("t.",names(data)[-1],sep=""),collapse=",")
    sql<- paste("
    INSERT INTO ",table,"
     SELECT g._id as _id, ",selFieldLines,"
     FROM genes AS g, temp AS t
     WHERE g.gene_id=t.gene_id
     ORDER BY g._id;
     ", sep="") 
    sqliteQuickSQL(con, sql)

    ## Add index to all fields in indFields (default is all)
    for(i in seq_len(length(indFields))){
    sqliteQuickSQL(con,
        paste("CREATE INDEX IF NOT EXISTS ",
              table,"_",indFields[i],"_ind ON ",table,
              " (",indFields[i],");", sep=""))      
    }
    
    ## drop the temp table
    sqliteQuickSQL(con, "DROP TABLE temp;")
  }
  message(paste(table,"table filled"))
}

## used to gather ancestor nodes for GO terms
.expandGOFrame <- function(frame, AncestMap){
  ## I want to apply through the original frame and call for the ancestor
  ancList <- mget(as.character(frame$go_id), AncestMap, ifnotfound=NA)
  names(ancList) <- frame$gene_id
  eviCodes <- mget(as.character(frame$go_id), AncestMap, ifnotfound=NA)
  names(eviCodes) <- frame$evidence
  expAncList <- unlist2(ancList)
  expEviCodes <- unlist2(eviCodes)
  extraRows <- data.frame(gene_id=names(expAncList), go_id=expAncList,
                          evidence=names(expEviCodes))
  ##remove rows where go_id="all"
  extraRows <- extraRows[extraRows$go_id != "all",]
  unique(rbind(frame,extraRows))
}

## used to make sure that GO IDs that are NOT in the current GO package do not
## end up in our DB
.filterGOFrame <- function(frame){
  frame[frame[["go_id"]] %in% Lkeys(GOTERM),]
  message("Dropping GO IDs that are too new for the current GO.db")
}

## TODO: modify this so that it no longer unwinds lists...
## used to make the 6 custom GO tables
.makeGOTablesFromNCBI <- function(con){
  bp <- .filterGOFrame(sqliteQuickSQL(con,
     "SELECT distinct gene_id, go_id, evidence FROM gene2go
      WHERE category = 'Process'"))

  mf <- .filterGOFrame(sqliteQuickSQL(con,
     "SELECT distinct gene_id, go_id, evidence FROM gene2go
      WHERE category = 'Function'"))
    
  cc <- .filterGOFrame(sqliteQuickSQL(con,
     "SELECT distinct gene_id, go_id, evidence FROM gene2go
      WHERE category = 'Component'")) 

    headerNames = c("gene_id","go_id","evidence")
    names(bp) <- headerNames
    names(mf) <- headerNames
    names(cc) <- headerNames
    
    .makeSimpleTable(bp, table = "go_bp", con, fieldNameLens=c(10,3),
                     indFields = c("_id", "go_id"))
    
    .makeSimpleTable(mf, table = "go_mf", con, fieldNameLens=c(10,3),
                     indFields = c("_id", "go_id"))
    
    .makeSimpleTable(cc, table = "go_cc", con, fieldNameLens=c(10,3),
                     indFields = c("_id", "go_id"))
    
    ## Now expand the three data.frames to incl all ancestor terms 
    bp_all <- .expandGOFrame(bp, GOBPANCESTOR)
    mf_all <- .expandGOFrame(mf, GOMFANCESTOR)
    cc_all <- .expandGOFrame(cc, GOCCANCESTOR)
    
    .makeSimpleTable(bp_all, table = "go_bp_all", con, fieldNameLens=c(10,3),
                     indFields = c("_id", "go_id"))
    
    .makeSimpleTable(mf_all, table = "go_mf_all", con, fieldNameLens=c(10,3),
                     indFields = c("_id", "go_id"))
    
    .makeSimpleTable(cc_all, table = "go_cc_all", con, fieldNameLens=c(10,3),
                     indFields = c("_id", "go_id"))
}


#########################################################################
#########################################################################

## For now, I am just going to use what I know works 
## (even though depending on curl hides a dependency which is BAD umkay):
## But the following chunck should at least download a file and return a 
## (cleaned) data.frame from it
.downloadData <- function (file, tax_id) {
  colClasses1 <- c("character",rep("NULL", times= length(unlist(file))-1))
  colClasses2 <- c(rep("character", times= length(unlist(file))))
  ## names(file) is something like: "gene2go.gz"
  message(paste("Getting data for ",names(file),sep=""))
## The following works, but commented so that I can avoid the wrath of NCBI
#   url <- paste("ftp://ftp.ncbi.nlm.nih.gov/gene/DATA/",names(file), sep="")
#   tmp <- tempfile()
#   download.file(url, tmp, method="curl", quiet=TRUE)
  
##older way  
###   vals <- read.delim(tmp, header=FALSE, sep="\t", skip=1, stringsAsFactors=FALSE, colClasses = colClasses)
  
## TEMPORARILY, lets work from some local files.
  ## Lets also speed this part up by declaring the column types, and
  ## subsetting out the things that don't match our tax ID.
  tmp <- paste("/home/mcarlson/proj/mcarlson/",
               "2011-May24/",names(file), sep="")
## end of TEMP stuff
  
  if("tax_id" %in% unlist(file)){ ## when there is a tax_id we want to subset
    tax_ids <- unlist(read.delim(tmp,header=FALSE,sep="\t",skip=1,
                                 stringsAsFactors=FALSE,
                                 colClasses = colClasses1))
    
    ind <- grep(paste("^",tax_id,"$",sep=""), tax_ids, perl=TRUE)
    
    if(length(ind) == 0){## ie there are no matching tax_ids...
      vals <- data.frame(t(1:length(unlist(file))),stringsAsFactors=FALSE)
      colnames(vals) <- unlist(file)
      vals <- vals[FALSE,]
    }else{
      vals <- read.delim(tmp, header=FALSE, sep="\t", skip=1+min(ind),
                         nrows= max(ind) - min(ind) +1,
                         stringsAsFactors=FALSE,
                         colClasses = colClasses2)
    } 
  }else{
    vals <- read.delim(tmp, header=FALSE, sep="\t", skip=1,
                       stringsAsFactors=FALSE,
                       colClasses = colClasses2)
  }
##   
  ## The following will just keep unwanted data from our temp DB tables.
  ## if there is a tax_id,
  ## then only return the piece that matches the organism in question
  colnames(vals) <- unlist(file)
  if(!is.null(vals[["tax_id"]])){
    vals <- vals[vals[["tax_id"]]==tax_id,]
  }
  vals
}




## helper to populate base tables
.populateBaseTable <- function(con, sql, data, table) {
    dbBeginTransaction(con)
    dbGetPreparedQuery(con, sql, data)
    dbCommit(con)
    message(paste("table ",table," filled",sep=""))
}

## Convenience function for quickly making indices
.makeIndex <- function(con,table, field){
  indName <-paste("ind",table,field,sep="_")
  sqliteQuickSQL(con,
   paste("CREATE INDEX IF NOT EXISTS",indName,"ON",table,"(",field,")"))
}

## Use when there is no GO data
.getBlast2GOData <- function(tax_id, con) {
  url = paste("http://bioinfo.cipf.es/b2gfar/_media/species:data:",
        tax_id,".annot.zip",sep="")
  tmp <- tempfile()
  download.file(url, tmp, method="curl", quiet=TRUE)
  vals <- read.delim(unzip(tmp), header=FALSE, sep="\t",
                     stringsAsFactors=FALSE)
  ## I will need to so extra stuff here to match up categories etc.
  ## (vals has to look like gene2go would, and I have to join to refseq and to
  ## accession just to get my EGs)
  RSVals <- vals[grep("refseq",vals[,4]),c(2,3)]
  GBVals <- vals[grep("genbank",vals[,4]),c(2,6)]
  colnames(GBVals) <- colnames(RSVals) <- c("go_id","accession")
  tables <- sqliteQuickSQL(con,"SELECT * FROM sqlite_master")$name
  
  if("gene2refseq" %in% tables){
    g2rs <- sqliteQuickSQL(con,
      "SELECT gene_id, protein_accession FROM gene2refseq")
    g2rs[,2] <- sub("\\.\\d+$","",g2rs[,2])
    vals <- merge(RSVals,g2rs, by.x="accession" , by.y="protein_accession")
  }else if("gene2accession" %in% tables){
    g2ac <- sqliteQuickSQL(con,
      "SELECT gene_id, protein_accession FROM gene2accession")
    gb <- merge(GBVals,g2ac, by.x="accession" , by.y="protein_accession")
    if("gene2refseq" %in% tables){
      vals <- rbind(vals, gb)
    }else{
      vals <- gb
    }
  }else{
    stop("It is impossible to match up blasted GO terms with NCBI accessions.")
  }
    
  ## then assemble the data back together to make it look like gene2go so
  ## it can be inserted there.
  rlen <- dim(vals)[1]
  data <-data.frame(tax_id = rep(tax_id, rlen),
                   gene_id = as.character(vals[["gene_id"]]),
                   go_id = vals[["go_id"]],
                   evidence = rep("IEA", rlen),
                   go_qualifier = rep("-", rlen),
                   go_description = rep("-", rlen),
                   pubmed_id = rep("-", rlen),
                   category = Ontology(vals[["go_id"]]),
                   stringsAsFactors=FALSE)
  data[,8] <- sub("BP","Process",data[,8])
  data[,8] <- sub("CC","Component",data[,8])
  data[,8] <- sub("MF","Function",data[,8])
  data
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
.generateTempTableINSERTStatement <- function (file) {
  Qs <- paste(rep("?",length(file[[1]])),  collapse=",")
  paste("INSERT INTO ",sub(".gz$","",names(file)),
  " VALUES(",Qs,");",sep="")
}


## need code to generate a table for each of the
## file below is like: files[1] or files[2]
.createTEMPNCBIBaseTable <- function (con, file, tax_id) {
  data <- .downloadData(file, tax_id)
  table <- sub(".gz$","",names(file))
  if(is.null(table)) stop("Unable to infer a table name.")
  cols <- .generateCols(file)
  sql<- paste("CREATE TABLE IF NOT EXISTS ",table," (",cols,");")
  sqliteQuickSQL(con, sql)
  
  message(paste("Populating ",table," table:", sep=""))
  sql <- .generateTempTableINSERTStatement(file)
  if(dim(data)[1] != 0){ ## ie. there has to be SOME data...
    .populateBaseTable(con, sql, data, table)
  }else if(dim(data)[1] == 0 && names(file)=="gene2go.gz"){ ## we need blast2GO
    message(paste("Getting blast2GO data as a substitute for ",table,sep=""))
    data <- .getBlast2GOData(tax_id, con)
    sql <- paste("INSERT INTO gene2go(tax_id, gene_id, go_id, evidence, ",
                 "go_qualifier, go_description,",
                 "pubmed_id, category) VALUES(?,?,?,?,?,?,?,?);")
    .populateBaseTable(con, sql, data, table)
  }else{
    message(paste("No data available for table ",table,sep=""))
  }
}


## loop through and make the tables
.makeBaseDBFromDLs <- function(files, tax_id, con){
  for(i in seq_len(length(files))){
    .createTEMPNCBIBaseTable(con, files[i], tax_id)
  }
  con
}


.dropOldTables <- function(fileNames){
  fileNames <- sub(".gz", "", fileNames)
  message(paste("dropping table",fileNames))
  for(i in seq_len(length(fileNames))){
    sqliteQuickSQL(con, paste("DROP TABLE IF EXISTS",fileNames[i],sep=""))
  }
}




#########################################################################
## Generate the database using the helper functions:
#########################################################################

generateOrgDbFromNCBI <- function(tax_id){
  require(RSQLite)
  require(GO.db)
  ## TODO: "9606_TEST.sqlite" is obviously not ok.
  con <- dbConnect(SQLite(), paste(tax_id,"_TEST.sqlite",sep=""))
  
  ## I need a list of files, along with their column names 
  ## (needed for schema definitions later)
  ## IF ANY OF THESE gets moved in the source files
  ## then things will be in the wrong place!
  files = list(
    "gene2pubmed.gz" = c("tax_id","gene_id", "pubmed_id"),
    "gene2accession.gz" = c("tax_id","gene_id","status","rna_accession",
        "rna_gi","protein_accession","protein_gi","genomic_dna_accession",
        "genomic_dna_gi","genomic_start","genomic_end","orientation",
        "assembly"),
    ## This one might be needed later
    "gene2refseq.gz" = c("tax_id","gene_id","status","rna_accession",
        "rna_gi","protein_accession","protein_gi","genomic_dna_accession",
        "genomic_dna_gi","genomic_start","genomic_end","orientation",
        "assembly"),
    "gene2unigene.gz" = c("gene_id","unigene_id"),
    "gene_info.gz" = c("tax_id","gene_id","symbol","locus_tag",
        "synonyms","dbXrefs","chromosome","map_location","description",
        "gene_type","nomenclature_symbol","nomenclature_name",
        "nomenclature_status","other_designations"),
    ##        "mim2gene.gz" = c("mim_id","gene_id","relation_type"),
    ##        "gene_refseq_uniprotkb_collab.gz" = c("refseq_id","uniprot_id"),
    "gene2go.gz" = c("tax_id","gene_id","go_id","evidence",
        "go_qualifier", "go_description","pubmed_id","category")
  )

  
  ## TEMP commented for speedup
  .makeBaseDBFromDLs(files, tax_id, con)
  
  ## TEMP For post DB testing only:
  ##tax_id = "9606"  

  
  ## Make the central table
  egs <- sqliteQuickSQL(con, "SELECT distinct gene_id FROM gene_info")[,1]
  .makeCentralTable(egs, con)
  
  ## Make the other tables:
  ## gene_info
  symbs <- sqliteQuickSQL(con,
    "SELECT distinct gene_id, description, symbol FROM gene_info")
  colnames(symbs) <- c("gene_id", "gene_name", "symbol")
  ## constraint requires that we always have BOTH a symbol and a name.
  symbs <- symbs[!is.na(symbs[,2]) & !is.na(symbs[,3]),] # strict constraint!
  ## Only 10 things fail this strict constraint for human.
  ## ALL because of no gene symbol (but that have a name)
  ## TODO: fix this so that I can make this table without
  ## such a strict constraint
  .makeSimpleTable(symbs, table="gene_info_temp", con, fieldNameLens=c(255,80))
  
  ## alias ## requires sub-parsing.
  alias <- sqliteQuickSQL(con,
    "SELECT distinct gene_id, synonyms FROM gene_info")
  aliases <- sapply(alias[,2], strsplit, "\\|")
  numAlias <- sapply(aliases, length)
  alGenes <- rep(alias[,1], numAlias)
  alias <- data.frame(gene_id=alGenes,alias_symbol=unlist(aliases))
  .makeSimpleTable(alias, table="alias", con)
  
  ## chr
  chrs <- sqliteQuickSQL(con,
    "SELECT distinct gene_id, chromosome FROM gene_info")
    .makeSimpleTable(chrs, table="chromosomes", con)
  
  ## kegg
  ##(TODO: can use KEGG FTP, NCBI web service, KEGGSOAP or maybe even biomaRt)
  
  ## pubmed
  pm <- sqliteQuickSQL(con,
    "SELECT distinct gene_id, pubmed_id FROM gene2pubmed")
    .makeSimpleTable(pm, table="pubmed", con)
  
  ## refseq ## requires sub-parsing.
  rs <- sqliteQuickSQL(con,
    "SELECT distinct gene_id,rna_accession,protein_accession FROM gene2refseq")
  rsr <- rs[rs[,2]!="-",1:2]
  colnames(rsr)[2] <- "accession"
  rsp <- rs[rs[,3]!="-",c(1,3)]
  colnames(rsp)[2] <- "accession"
  rs <- rbind(rsr,rsp)
  rs[,2] <- sub("\\.\\d+$","",rs[,2])
  .makeSimpleTable(rs, table="refseq", con)
  
  ## unigene 
  ug <- sqliteQuickSQL(con,
    "SELECT distinct g.gene_id, u.unigene_id FROM gene2unigene as u,
     gene_info as g WHERE u.gene_id=g.gene_id")
  .makeSimpleTable(ug, table="unigene", con)
  
  ## Make the GO tables:
  .makeGOTablesFromNCBI(con)
  
  ## Drop all the older tables (which will include the original "gene_info").
  .dropOldTables(names(files))  
  ## Rename "gene_info_temp" to be just "gene_info":
  sqliteQuickSQL(con,"ALTER TABLE gene_info_temp RENAME TO gene_info")
}


## TODO (here):
 ## 1) move over the helper functions from getters that can work with DFs
 ## 2) put them in their own section of the code (label)

 ## 3) make all necessary glue/mods here to enable their usage
 ## 4) keep things generic so that I can use them again for biomaRt stuff
## 5) Add code to "name" the database correctly. (get rid of "TEST.sqlite")

## 6) Test the whole thing in one run.

## 7) Add Metadata to the DB. (including MAPCOUNTS where relevant)

## 8) Make it so that I can automatically make the package around the DB.
## 9) Don't forget to redirect the getData functs to point to NCBI
## 10) Right now there is a problem when there is nothing in a table (like no GO terms) - done?


## generateOrgDbFromNCBI("9606")
## generateOrgDbFromNCBI("1428") ## Bacillus thuringiensis?
## Still don't kwow which kind ... Bacillus thuringiensis?

## try zebrafinch?

## generateOrgDbFromNCBI("59729")

## 11) Some organisms don't have GO terms.  Enable getting these from blast2GO
## when they are MIA. - done?
## To do it, all I need to do is take advantage of the fact that blast2GO
## follows this convention for their files:

## http://bioinfo.cipf.es/b2gfar/_media/species:data:9913.annot.zip?id=species%3A9913&cache=cache
## Therefore:
## url = paste("http://bioinfo.cipf.es/b2gfar/_media/species:data:",
##      tax_id,".annot.zip?id=species%3A",tax_id,"&cache=cache",sep="")
