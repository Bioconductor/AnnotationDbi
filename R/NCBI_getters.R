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



getGeneStuff <- function(x){
  #require(XML)
  baseUrl <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?"
  xsep <- paste(x, collapse=",")
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
  if(unique(entrezGeneID %in% x) %in% FALSE) ##if any don't match
    stop("The entrez Genes discovered don't match the IDs being looked up!")
  if(length(x) > length(entrezGeneID))
    warning("Some of the entrez Genes beings sought were not found.")
  if(length(entrezGeneID) > length(x))
    stop("There are more EGs being found than we expected.")
  
  ## return a list of things
  list(entrez = entrezGeneID,
       species = speciesName,
       pmIds = pmIds, 
       GOIds = GOIds,  
       KEGGGeneIds = KEGGGeneIds,
       KEGGPathIds = KEGGPathIds,
       aliasIds = aliasIds,
       symbolIds = symbolIds,
       RSProtIds = RSProtIds,
       RSRNAIds = RSRNAIds,
       MIMIds = MIMIds,
       unigeneIds = unigeneIds      
       )
  
}



## egs = c("10","100")

## ## Some quick performance tests:
## library(org.Hs.eg.db)
## ##egs = Lkeys(org.Hs.egCHR)[1:100]
## ## 100 is pretty fast


## ## TODO: Above about 800 records, the URL seems to be longer than the web
## ## server will respond too.  So I will have to process the list of EGs in
## ## "chunks", but otherwise the basic operations appear to be vectorizable.  So
## ## to do "chunks", I only need a convenient way to merge the list results.
## ## egs = Lkeys(org.Hs.egCHR)[1:800]


## system.time(getGeneStuff(egs))













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




## Wrap the functionality like so:
buildEntrezGeneDb <- function(taxId){
  ## 1st get a list of EGs
  ## EGs <- getEntrezGenesFromTaxId(taxId) ##There is something wrong here?
  ## Temp hack till I can learn what is wrong with the web service.
  library(org.Hs.eg.db)
  EGs <- Lkeys(org.Hs.egCHR)
  
  ## Then break it into chunks
  chunkSize = 800
  numChunks = floor(length(EGs)/chunkSize)
  splitFactor <- rep(1:numChunks, each=chunkSize)
  EGChunks <- split(EGs, as.factor(splitFactor))
  EGChunksFinal <- EGChunks[[1]][(chunkSize+1):length(EGChunks[[1]])]
  EGChunks[[1]] <-  EGChunks[[1]][1:chunkSize]
  EGChunks <- c(EGChunks,list(EGChunksFinal))
  
  ## TODO: There may be some problems with some of the EGs that we get
  ## from NCBI in this way (having trouble finding an example though)
  
  ## temp for testing:  
  EGChunks = EGChunks[c(1:2, 57)]
  ## Then we just need to apply through and make a super-list
  superList <- lapply(EGChunks, getGeneStuff)

  ## Then we need to combine the elements of that list
  # sList = lapply(superList, mergeLists)

  ## Then we have to make a DB and start populating it with tables for
  ## each kind of element.  We will check the length of each list
  ## element for contents to make sure that we have stuff to populate
  ## before we start to make a table (and thus avoid having an omim
  ## table inside of mouse for example)

  ## For this, I will write a generic function to make a table, and
  ## another generic one to populate it. - actually I think I have
  ## something like this already in sqlForge_tableBuilder.R
  
  
  
}
