## This file is for code that calls NCBI and then retrieves data to populate a
## local DB (eventually on the fly).

## I need to have functions that will make tables in a DB, but I also need
## (separate) function(s)? that get the "stuff" from NCBI.  Let's start with
## that.

## Lets start by just making this thing get me some PMIDs for a string of EGs

## getNucleotideStuff <- function(x, ret="acc"){
##   baseUrl <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?"
##   xsep <- paste(x, collapse=",")
##   url <- paste(baseUrl,"db=nucleotide&id=",xsep,"&rettype=",ret, sep="")
##   readLines(url)
## }

## egs = c("10","4557270")
## getNucleotideStuff(egs)
## getNucleotideStuff(egs, "gb")
## getNucleotideStuff(egs, "gp")
## getNucleotideStuff(egs, "seqid")
## getNucleotideStuff(egs, "ft")






#####################################################################
## helper functions for retrieving data from nodes 

## The following is a generic solution for getting stuff from sub-nodes
## Generic way to get src Docs

.getOtherSrcDocs <- function(doc, otherSourcePath){
  ## for each doc, get other Source nodes and make mini-docs
  srcDocs <- lapply(getNodeSet(doc, otherSourcePath), xmlDoc)
  srcDocs
}

## retrieve Db tag from such an other source doc.
.getDBTag <- function(srcDoc, dbTagPath){
  dbTag <- unlist(xpathApply(srcDoc, dbTagPath, xmlValue))
  if(is.null(dbTag)) dbTag <- "Bummer, the dbTag is NULL."
  dbTag
}


## check the node and return the value requested
.checkRetrieveNode <- function(doc, dbTagPath, resIDPath, type){
  results = vector("list",length(dbTagPath))
  for(i in seq_len(length(dbTagPath))){
    if(.getDBTag(doc, dbTagPath[i])==type
       || (is.null(type) & dbTagPath==resIDPath)){
      results[[i]] <- (unlist(xpathApply(doc, resIDPath[i], xmlValue)))
    }
  }
  if(!is.null(results))return(results)
}



## default values just get you KEGG gene IDs (for now)
.getSubNodeInfo <- function(doc,
                           type= "KEGG",
                           otherSourcePath = "/Entrezgene/Entrezgene_comments/Gene-commentary/Gene-commentary_comment/Gene-commentary/Gene-commentary_source/Other-source",
                           dbTagPath = "/Other-source/Other-source_src/Dbtag/Dbtag_db",
                           resIDPath = "/Other-source/Other-source_src/Dbtag/Dbtag_tag/Object-id/Object-id_str"){
  ## Get sub-nodes for looped parsing
  srcDocs <- .getOtherSrcDocs(doc, otherSourcePath)

  ## check to make sure we don't have unequal arguments
  if(length(dbTagPath) != length(resIDPath))
    Stop("result and dbTag (checking) paths must have same number of elements")

  if(length(dbTagPath) == 1){
    results <- unlist(lapply(srcDocs, .checkRetrieveNode,
                             dbTagPath, resIDPath, type))
  }else{  ##compound result is expected so don't simplify it.
    results <- lapply(srcDocs, .checkRetrieveNode,
                             dbTagPath, resIDPath, type)
  }
}




## GO attempt #2 (still fails since there are apparently some GO IDs where
## there is not a GO evidence code available. So I need to fully vectorize the generic code.
## A convenience function to pad GO Ids
.padGOIds <- function(GOIds){
  pads <- 7-nchar(GOIds)
  res<-character(length(GOIds))
  for(i in seq_len(length(GOIds))){
    res[i] <- paste(paste(rep("0",pads[i]),collapse=""),GOIds[i],sep="")
  }
  paste("GO:",res,sep="")
}

.modifyGOs <- function(elem){
  elem[[1]] <- .padGOIds(elem[[1]])
  elem[[2]] <- gsub("evidence: ", "", elem[[2]])
  elem
}

## for GO retrieval I have partially vectorized the helper functions.
getGOInfo <- function(doc){
  
  GOIds <- .getSubNodeInfo(doc, type = "GO",
                    otherSourcePath = "/Entrezgene/Entrezgene_properties/Gene-commentary/Gene-commentary_comment/Gene-commentary/Gene-commentary_comment/Gene-commentary/Gene-commentary_source/Other-source",
                           dbTagPath = rep("/Other-source/Other-source_src/Dbtag/Dbtag_db", 2),
                           resIDPath = c("/Other-source/Other-source_src/Dbtag/Dbtag_tag/Object-id/Object-id_id", "/Other-source/Other-source_post-text"))

  GOIds <- lapply(GOIds, .modifyGOs)  
  GOIds
}



getGeneStuff <- function(entrezGenes){
  require(XML)
  baseUrl <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?"
  xsep <- paste(entrezGenes, collapse=",")
  url <- paste(baseUrl,"db=gene&id=",xsep,"&retmode=xml", sep="")
    
  ## NOW we have to parse the available XML
  EGSet <- xmlParse(url)
  ## Some tags can only occur once per gene
  entrezGeneID <- unlist(xpathApply(EGSet, "//Gene-track_geneid", xmlValue))
  speciesName <- unlist(xpathApply(EGSet, "//Org-ref_taxname", xmlValue))

  ## But most information is either more complex than that or is stored in a
  ## more complex way by the XML
  ## miniDocs are the individual entrez Gene records
  miniDocs <- lapply(getNodeSet(EGSet, "//Entrezgene"), xmlDoc)

  ## Sometimes we are lucky and the information we want has an obviously
  ## unique tag.
  ## Like pubmed Ids
  pmIdPath <- "/Entrezgene/Entrezgene_comments/Gene-commentary/Gene-commentary_refs/Pub/Pub_pmid/PubMedId"
  pmIds <- lapply(miniDocs, function(x)
                  unlist(xpathApply(x, pmIdPath, xmlValue)))

  ## But sometimes we have to do some more checking to make sure that what we
  ## are retrieving is in the context of certain kinds of tags, or of tags
  ## that contain certain information.

  ## custom helper to get and process the GO terms
  GOIds <- lapply(miniDocs, getGOInfo)

  ## KEGG Gene IDs are NOT the path IDs.
  otherSourcePath <- "/Entrezgene/Entrezgene_comments/Gene-commentary/Gene-commentary_comment/Gene-commentary/Gene-commentary_source/Other-source"
  dbTagPath <- "/Other-source/Other-source_src/Dbtag/Dbtag_db"
  resIDPath_str <- "/Other-source/Other-source_src/Dbtag/Dbtag_tag/Object-id/Object-id_str"
  KEGGGeneIds <- lapply(miniDocs, .getSubNodeInfo, type = "KEGG",
                    otherSourcePath = otherSourcePath,
                           dbTagPath = dbTagPath,
                           resIDPath = resIDPath_str)

  KEGGPathIds <- lapply(miniDocs, .getSubNodeInfo, type = "KEGG pathway",
                    otherSourcePath = otherSourcePath,
                           dbTagPath = dbTagPath,
                           resIDPath = resIDPath_str)

  unigeneIds <- lapply(miniDocs, .getSubNodeInfo, type = "UniGene",
                    otherSourcePath = otherSourcePath,
                           dbTagPath = dbTagPath,
                           resIDPath = resIDPath_str)
  resIDPath_id <- "/Other-source/Other-source_src/Dbtag/Dbtag_tag/Object-id/Object-id_id"
  ## May only want to look for MIM if we are tax ID 9606 (or can be empty here)
  MIMIds <- lapply(miniDocs, .getSubNodeInfo, type = "MIM",
                    otherSourcePath = otherSourcePath,
                           dbTagPath = dbTagPath,
                           resIDPath = resIDPath_id)

  ## Refseqs are in another couple of places (we can merge these later, or I
  ## can write a helper like for GO and merge them as we go.)
  RSOtherSourcePath <- "/Entrezgene/Entrezgene_comments/Gene-commentary/Gene-commentary_comment/Gene-commentary"
  RSdbTagPath <- "/Gene-commentary/Gene-commentary_heading" 
  RSResIdTagPathRNA <- "/Gene-commentary/Gene-commentary_products/Gene-commentary/Gene-commentary_accession" 
  RSRNAIds <- lapply(miniDocs, .getSubNodeInfo,
    type = "RefSeqs maintained independently of Annotated Genomes",
                    otherSourcePath = RSOtherSourcePath,
                           dbTagPath = RSdbTagPath,
                           resIDPath = RSResIdTagPathRNA)
  
  RSResIdTagPathProt <- "/Gene-commentary/Gene-commentary_products/Gene-commentary/Gene-commentary_products/Gene-commentary/Gene-commentary_accession"   
  RSProtIds <- lapply(miniDocs, .getSubNodeInfo,
    type = "RefSeqs maintained independently of Annotated Genomes",
                    otherSourcePath = RSOtherSourcePath,
                           dbTagPath = RSdbTagPath,
                           resIDPath = RSResIdTagPathProt)

  ## Get official Symbol
  symbolSourcePath <- "/Entrezgene/Entrezgene_properties/Gene-commentary/Gene-commentary_properties/Gene-commentary"
  symbolDbTag <- "/Gene-commentary/Gene-commentary_label"
  symbolresIDPath <- "/Gene-commentary/Gene-commentary_text"
  symbolIds <- lapply(miniDocs, .getSubNodeInfo,
                      type = "Official Symbol",
                    otherSourcePath = symbolSourcePath,
                           dbTagPath = symbolDbTag,
                           resIDPath = symbolresIDPath)
  ## Get official Name
  fullNames <- lapply(miniDocs, .getSubNodeInfo,
                      type = "Official Full Name",
                    otherSourcePath = symbolSourcePath, ##same path as symbols
                           dbTagPath = symbolDbTag,
                           resIDPath = symbolresIDPath)
  ## Get alternate symbols (merge with official ones when making table)
  ## TODO: would be SAFER to do some of the 1:1 stuff like this too
  ## Therefore redo how I get EG, PMID, and species name.
  aliasSourcePath <- "/Entrezgene/Entrezgene_gene/Gene-ref/Gene-ref_syn"
  aliasDbTag <- "/Gene-ref_syn/Gene-ref_syn_E"
  aliasresIDPath <- "/Gene-ref_syn/Gene-ref_syn_E"
  aliasIds <- lapply(miniDocs, .getSubNodeInfo,
                      type = NULL,
                    otherSourcePath = aliasSourcePath,
                           dbTagPath = aliasDbTag,
                           resIDPath = aliasresIDPath)


  
  ## Data sanity checks:
  ## All genes should be from the same critter:
  ## TODO: move the checks on EG uniqueness to outside of this function
  if(length(unique(entrezGeneID)) != length(entrezGeneID))
     stop("Some of the entrez gene IDs have been repeated.")
  if(length(unique(speciesName))>1)
    stop("The IDs being processed need to all be from the same species.")
     
  ## The following checks can stay at this level though
  if(unique(entrezGeneID %in% entrezGenes) %in% FALSE) ##if any don't match
    stop("The entrez Genes discovered don't match the IDs being looked up!")
  if(length(entrezGenes) > length(entrezGeneID))
    warning("Some of the entrez Genes beings sought were not found.")
  if(length(entrezGeneID) > length(entrezGenes))
    stop("There are more EGs being found than we expected.")
  
  ## return a list of things
  list(entrez = entrezGeneID,
       species = speciesName,
       pmIds = pmIds, 
       GOIds = GOIds,  
       KEGGGeneIds = KEGGGeneIds,
       KEGGPathIds = KEGGPathIds,
       aliasIds = aliasIds, ## requires preprocessing to match up?
       symbolIds = symbolIds,
       fullNames = fullNames,
       RSProtIds = RSProtIds,
       RSRNAIds = RSRNAIds,
       MIMIds = MIMIds,
       unigeneIds = unigeneIds      
       )
  
}




## NCBI got back to me
##An example approach is given below:

## 1) esearch with taxid in the following format:
## http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=gene&term=txid9606%5Borgn%5D+AND+alive%5Bprop%5D&usehistory=y

## 2) parse out the WebEnv and QueryKey value:
## http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=gene&WebEnv=NCID_1_39931064_130.14.18.47_9001_1290210718_1522935737&query_key=1&rettype=uilist&retmode=text

## Regards,

## Tao Tao, PhD
## NCBI User Services


## get EGs from an NCBI tax ID
getEntrezGenesFromTaxId <- function(taxId){
  ## 1st retrieve the WebEnv and QueryKey values
  url1 <- paste("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=gene&term=txid",taxId,"%5Borgn%5D+AND+alive%5Bprop%5D&usehistory=y", sep="")
   
  ## NOW we have to parse the available XML
  XML <- xmlParse(url1)
  ## Some tags can only occur once per gene
  ## TODO: wire up the xpath for this
  webEnv <- unlist(xpathApply(XML, "//WebEnv", xmlValue))
  queryKey <- unlist(xpathApply(XML, "//QueryKey", xmlValue))

  ## Then assemble the final URL
  url2 <- paste(
     "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=gene&WebEnv=",
     webEnv, "&query_key=", queryKey, "&rettype=uilist&retmode=text", sep="")
  readLines(url2)
}




## Then we need to combine the sub-lists found withing our "super"-list
.mergeLists <- function(listOfLists){
  ## mergeLists just takes two lists at a time, and merges them by
  ## concatenating their contents.
  list1 <- listOfLists[[1]]
  result <- vector("list", length(list1))
  for(i in seq_len(length(list1))){
    for(j in seq_len(length(listOfLists))){
      result[[i]] <- c(result[[i]], listOfLists[[j]][[i]])
    }
  }
  ## keep the names
  names(result) = names(list1)
  result
}

## General helper for merging two simple lists
.combineTwoLists <- function(list1,list2){
  if(length(list1) != length(list2)){
    stop("lists must be equal length to be merged")}
  result <- vector("list",length(list1))
  for(i in seq_len(length(list1))){
    result[[i]] <- c(list1[[i]], list2[[i]])
  }
  result
}


##  I will need the following helpers
.makeCentralTable <- function(entrez, con){
  message(cat("Creating genes table"))
  sql<- paste("    CREATE TABLE genes (
      _id INTEGER PRIMARY KEY,
      gene_id VARCHAR(10) NOT NULL UNIQUE           -- Entrez Gene ID
    );")
  sqliteQuickSQL(con, sql)

  gene_id <- data.frame(entrez) ## TODO: data.frame() necessary???
  sql<- paste("INSERT INTO genes(gene_id) VALUES(?);")
  dbBeginTransaction(con)
  dbGetPreparedQuery(con, sql, gene_id)
  dbCommit(con)
}

## this just makes a nice named data.frame from your data list
.makeSimpleDF <- function(entrez, fieldVals, name){
  names(fieldVals) <- entrez
  expandedList <- unlist2(fieldVals)
  result <- data.frame(cbind(names(expandedList),expandedList)) 
  colnames(result) <- c("gene_id", name)
  result
}

## now lets make this work for the case where we pass in a data.frame 
.makeSimpleTable <- function(fieldVals, table,
                             con, fieldNameLens=25){
  message(paste("Creating",table,"table")) 
  ## For temp table, lets do it like this:
  dbWriteTable(con, "temp", fieldVals, row.names=FALSE)
  ## Then we have to create our real table.
  tableFieldLines <- paste(paste(names(fieldVals)[-1]," VARCHAR(",
                           fieldNameLens,") NOT NULL,    -- data"),
                           collapse="\n       ")
  sql<- paste("    CREATE TABLE ",table," (
      _id INTEGER NOT NULL,                         -- REFERENCES genes
      ",tableFieldLines,"
      FOREIGN KEY (_id) REFERENCES genes (_id)
    );") 
  sqliteQuickSQL(con, sql)
  selFieldLines <- paste(paste("t.",names(fieldVals)[-1],sep=""),collapse=",")
  sql<- paste("
    INSERT INTO ",table,"
     SELECT g._id as _id, ",selFieldLines,"
     FROM genes AS g, temp AS t
     WHERE g.gene_id=t.gene_id
     ORDER BY g._id;
     ", sep="") 
  sqliteQuickSQL(con, sql)

  ## drop the temp table
  sqliteQuickSQL(con, "DROP TABLE temp;")
}


#.makeMetaTables <- function(){}

#.makeTablesIndices <- function(){}




## making the GO tables is complicated, because we have to 1) split things up
## based on which ontology they harken from (which means these things must be
## looked up) and 2) produce both a table of the GO terms we have collected
## here already and also produce a table of the go_xx_all terms with their
## parent nodes included.  For a total of 6 tables in all.

## For any gene, the GO list might be empty, but for each list we have two
## pieces of data to extract, and for each of those, we want to also
## retrieve a 3rd piece of data, (the ontology) Fortunately, this last bit
## is easy to do.

## if our GOIds object is called foo:
## tolower(Ontology(unlist(foo$GOIds)[1])) would give me the 1st Ontology

## to get our ancestor GO IDs, we will just use the ancestor mappings from
## the latest GO.db package.  for example, if we had GO term "GO:0044183",
## we would do mget("GO:0044183", GOMFANCESTOR, ifnotfound=NA), (and filter
## out "all"), then give each of those "answers" the same evidence code that
## was used for our seed term of "GO:0044183".


.unwindGOs <- function(GOIds, entrez, type){
  res <- vector("list",length(GOIds))
  for(i in seq_len(length(GOIds))){
    for(j in seq_len(length(GOIds[[i]]))){
      res[[i]] <- c(res[[i]], unlist2(GOIds[[i]][[j]][[type]]))
    }
  }
  names(res) <- entrez
  res
}

.makeGOTables <- function(entrez, GOIds, con){
  ## So I think this is my strategy:
  ## Step 1: collapse the list of genes and GO terms down to a data.frame. - done
  ## Step 2: gather the ontology information for each term. -done
  ## Step 3: populate the three go_xx tables with a helper function that uses
  ## a modified .makeSimpleTable() (separate out the code to collapse the eg
  ## and other data into a data.frame 1st which will now be passed in) and
  ## pass in a data.frame to each call, THEN do the same here with a
  ## data.frame that is split based on the ontology column  - done.
  ## Step 4: make a helper function that expands a data.frame from Step 3 into
  ## a a frame that also holds the parent terms for each GO ID, by adding rows
  ## for all parent terms (with same evidence codes) and then using unique to
  ## drop redundant rows. (which will happen if there are parent nodes among
  ## the leaf terms already).
  ## Step 5: call .makeSimpleTable again with the new data.frame.

  ## GOIds is a list of equal length to the entrez IDs
  if(length(entrez) != length(GOIds)){
    stop("There must be a list of GOIds")}
  go_id <- unlist2(.unwindGOs(GOIds, entrez, type=1))
  evidence <- unlist2(.unwindGOs(GOIds, entrez, type=2))
  ontology <- Ontology(go_id)
  baseFrame <- cbind(gene_id = names(go_id), go_id=go_id,
                     evidence=evidence, ontology=ontology)
  bp <- data.frame(matrix(split(baseFrame, ontology)$BP,
               byrow=FALSE, nrow=length(grep("BP",ontology))))[,1:3]
  mf <- data.frame(matrix(split(baseFrame, ontology)$MF,
               byrow=FALSE, nrow=length(grep("MF",ontology))))[,1:3]
  cc <- data.frame(matrix(split(baseFrame, ontology)$CC,
               byrow=FALSE, nrow=length(grep("CC",ontology))))[,1:3]
  headerNames = c("gene_id","go_id","evidence")
  names(bp) <- headerNames
  names(mf) <- headerNames
  names(cc) <- headerNames

  .makeSimpleTable(bp, table = "go_bp", con, fieldNameLens=c(10,3))
  
  .makeSimpleTable(mf, table = "go_mf", con, fieldNameLens=c(10,3))
  
  .makeSimpleTable(cc, table = "go_cc", con, fieldNameLens=c(10,3))

  ## Now we have to expand the three data.frames
  
}


## Wrap the functionality like so:
buildEntrezGeneDb <- function(entrezGenes, file="test.sqlite"){
  EGs <- entrezGenes
  ## Then break it into chunks
  if(length(EGs)<800){
    sList <- getGeneStuff(EGs)
  }else{
    chunkSize = 800
    numChunks = floor(length(EGs)/chunkSize)
    splitFactor <- rep(1:numChunks, each=chunkSize)
    EGChunks <- split(EGs, as.factor(splitFactor))
    EGChunksFinal <- EGChunks[[1]][(chunkSize+1):length(EGChunks[[1]])]
    EGChunks[[1]] <-  EGChunks[[1]][1:chunkSize]
    EGChunks <- c(EGChunks,list(EGChunksFinal))
    ## Then we just need to apply through and make a super-list
    superListOfLists <- lapply(EGChunks, getGeneStuff)
    sList <- .mergeLists(superListOfLists)
  }
  #file.remove(file) ##remove the old file when they re-run the code?
  ## TODO: check 1st.
  con <- dbConnect(SQLite(), file)
  
  .makeCentralTable(sList$entrez, con)
  .makeSimpleTable(data.frame(gene_id = sList$entrez,
                              gene_name = unlist(sList$fullNames),
                              symbol = unlist(sList$symbolIds)),
                   table = "gene_info", con, fieldNameLens=c(255,80))
  .makeSimpleTable(.makeSimpleDF(entrez = sList$entrez,
                                 fieldVals = sList$pmIds, "pubmed_id"),
                   table = "pubmed", con)
  .makeSimpleTable(.makeSimpleDF(entrez = sList$entrez,
                                 fieldVals = .combineTwoLists(sList$alias,
                                   sList$symbol), "alias_symbol"),
                   table = "alias", con)
  .makeSimpleTable(.makeSimpleDF(entrez = sList$entrez,
                                 fieldVals = sList$KEGGPathIds, "path_id"),
                   table = "kegg", con)
  .makeSimpleTable(.makeSimpleDF(entrez = sList$entrez,
                                 fieldVals = .combineTwoLists(sList$RSProtIds,
                                   sList$RSRNAIds), "accession"),
                   table = "refseq", con)                   
  .makeSimpleTable(.makeSimpleDF(entrez = sList$entrez,
                                 fieldVals = sList$MIMIds, "omim_id"),
                   table = "omim", con)
  .makeSimpleTable(.makeSimpleDF(entrez = sList$entrez,
                                 fieldVals = sList$unigeneIds, "unigene_id"),
                   table = "unigene", con)
  
  .makeGOTables(entrez = sList$entrez, GOIds = sList$GOIds, con)
  
}





##############################################################################
## More TODO:
## 1) Make something to retrieve all the EGs from a tax_id. - no obvious way yet
##
## 2) Wrap this so that we can 1) get all the EGs, then retrieve their results
## serially into one big super-list for import into a DB.
##
## 3) GO IDs will require more processing to make GO2ALL table.  This can be
## handled in much the same way as it is now, just by using it along with the## GO.db package which this workflow will have to depend upon.
##
## 4) Some checking will have to be done as we add contents that are matched
## (thinking of the GO terms here) to the DB.  Specifically, each GO ID needs
## to have an evidence code and I know from making this work, that some of
## them will NOT have that.  Therefore, I have to check as they are formatted
## and put into the DB.








