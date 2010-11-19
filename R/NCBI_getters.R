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




## ## Helper function to get subsets of the XML.
## .getMiniDocs <- function(set, xmlPath){
##   lapply(getNodeSet(set, xmlPath), xmlDoc)
## }


## getGeneStuff <- function(x){
##   #require(XML)
##   baseUrl <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?"
##   xsep <- paste(x, collapse=",")
##   url <- paste(baseUrl,"db=gene&id=",xsep,"&retmode=xml", sep="")
    
##   ## NOW we have to parse the available XML
##   EGSet <- xmlParse(url)
##   ## And then just grab back out everything we possibly can...
##   entrezGeneID <- unlist(xpathApply(EGSet, "//Gene-track_geneid", xmlValue))
##   ## TODO: vectorize the following check (or decide on a different strategy)
## ##   if(entrezGeneID != as.character(x))
## ##      stop("This is not the ID you are looking for")
##   speciesName <- unlist(xpathApply(EGSet, "//Org-ref_taxname", xmlValue))

##   ## miniDocs are the individual entrez Gene records
##   ##miniDocs <- lapply(getNodeSet(EGSet, "//Entrezgene"), xmlDoc)
##   miniDocs <- .getMiniDocs(EGSet, "//Entrezgene")
    
##   ## pubmed IDs
##   pmidPath <- "/Entrezgene/Entrezgene_comments/Gene-commentary/Gene-commentary_refs/Pub/Pub_pmid/PubMedId"
##   pmids <- lapply(miniDocs, function(x)
##                   unlist(xpathApply(x, pmidPath, xmlValue)))

##   ## TODO: get chrome installed in case it can help
##   ## TODO: put will have to subset each miniDoc into subnodes and then
##   ## retrieve just the onest that meet the criteria for GO etc.

## ##   ## This will retrieve just the GO IDs
## ##   GOIDPath <- "/Entrezgene/Entrezgene_properties/Gene-commentary/Gene-commentary_comment/Gene-commentary/Gene-commentary_comment/Gene-commentary/Gene-commentary_source/Other-source/Other-source_src/Dbtag/Dbtag_tag/Object-id/Object-id_id"
## ##   GOids <- lapply(miniDocs, function(x)
## ##                   unlist(xpathApply(x, GOIDPath, xmlValue)))

##   ## But really, I want to do better, I want to select out the sub-nodes that
##   ## have "GO" in them, and then pull out the IDs, and not rely on the path to
##   ## be unique for the GO terms (when it might also be similar for say KEGG
##   ## terms)

  
##   ## TODO: I need to check that each GOnode has a <Dbtag_db>"GO"</Dbtag_db>
  
##   GONodePath <- "/Entrezgene/Entrezgene_properties/Gene-commentary/Gene-commentary_comment/Gene-commentary/Gene-commentary_comment/Gene-commentary/Gene-commentary_source//Other-source"
##   ##goNodes <- getNodeSet(EGSet, GONodePath)
##   ##miniGOs <- lapply(getNodeSet(EGSet, GONodePath), xmlDoc)
##   miniGOs <- lapply(miniDocs, .getMiniDocs, GONodePath)  
  
##   GOIDPath <- "/Other-source/Other-source_src/Dbtag/Dbtag_tag/Object-id/Object-id_id"
##   GOEvidencePath <- "/Other-source/Other-source_post-text"
##   GOids <- lapply(miniGOs[[1]], function(x)
##                   unlist(xpathApply(x, GOIDPath, xmlValue)))
##   ##I really need to do a double loop to extract this because of the double
##   ##list (wrap below operation into a function we can call).  And even then, I
##   ##still have to check that it's a GO ID each time Call all three functions
##   ##together and check output of the GO getting one 1st). I am not sure that
##   ##it isn't just simpler to just extract out all the terms as was done above
##   ##(much more simply) and then to just cbind them all into a data.frame and
##   ##then remove rows that didn't cooperate.  Especially since I will want to
##   ##have them in a data.frame eventually anyhow.
##   GOEvidCodes <- lapply(miniGOs[[1]], function(x)
##                   unlist(xpathApply(x, GOEvidencePath, xmlValue)))
  
  
##   ## Data sanity checks:
##   ## All genes should be from the same critter:
##   ## TODO: move the checks on EG uniqueness to outside of this function
##   if(length(unique(entrezGeneID)) != length(entrezGeneID))
##      stop("Some of the entrez gene IDs have been repeated.")
##   if(length(unique(speciesName))>1)
##     stop("The IDs being processed need to all be from the same species.")
     
##   ## The following checks can stay at this level though
##   if(unique(entrezGeneID %in% x) %in% FALSE) ##if any don't match
##     stop("The entrez Genes discovered don't match the IDs being looked up!")
##   if(length(x) > length(entrezGeneID))
##     warning("Some of the entrez Genes beings sought were not found.")
##   if(length(entrezGeneID) > length(x))
##     stop("There are more EGs being found than we expected.")
  
##   ## return a list of things
##   list(entrez = entrezGeneID,
##        species = speciesName,
##        pmids = pmids,
##        GOids = GOids)
  
## }



























## helper for padding GOIds
padGOIds <- function(GOIds){
  pads <- 7-nchar(GOIds)
  res<-character(length(GOIds))
  for(i in seq_len(length(GOIds))){
    res[i] <- paste(paste(rep("0",pads[i]),collapse=""),GOIds[i],sep="")
  }
  paste("GO:",res,sep="")
}

## helper function to retrieve a data.frame with GO info.
## At some point, the XML may change and require a strategy more like is
## adopted for retrieving the KEGG IDs
getGOInfo <- function(doc){

  ## This will retrieve just the GO IDs
  GOIDPath <- "/Entrezgene/Entrezgene_properties/Gene-commentary/Gene-commentary_comment/Gene-commentary/Gene-commentary_comment/Gene-commentary/Gene-commentary_source/Other-source/Other-source_src/Dbtag/Dbtag_tag/Object-id/Object-id_id"
  GOIds <- unlist(xpathApply(doc, GOIDPath, xmlValue))
  GOIds <- padGOIds(GOIds)

  ## This will retrieve just the GO Evidence codes
  GOEviPath <- "/Entrezgene/Entrezgene_properties/Gene-commentary/Gene-commentary_comment/Gene-commentary/Gene-commentary_comment/Gene-commentary/Gene-commentary_source/Other-source/Other-source_post-text"
  GOEvidence <- unlist(xpathApply(doc, GOEviPath, xmlValue))
  GOEvidence <- gsub("evidence: ", "", GOEvidence)

  ## This will retrieve what kind of annotation it is (sanity check
  GOPath <- "/Entrezgene/Entrezgene_properties/Gene-commentary/Gene-commentary_comment/Gene-commentary/Gene-commentary_comment/Gene-commentary/Gene-commentary_source/Other-source/Other-source_src/Dbtag/Dbtag_db"
  GOStatus <- unlist(xpathApply(doc, GOPath, xmlValue))

  ##Assemble these into a data.frame and filter
  result <- data.frame(GOId=GOIds, evidenceCode=GOEvidence, dataType=GOStatus,
                       stringsAsFactors=FALSE)
  result <- result[grep("GO", result[,3]),]
  result[,1:2]
}



#####################################################################
## helper functions for retrieving KEGG data. (NOT simpler than GO!)

## Generic way to get src Docs

.getOtherSrcDocs <- function(doc){
  ## for each doc, get other Source nodes and make mini-docs
  otherSourcePath <- "/Entrezgene/Entrezgene_comments/Gene-commentary/Gene-commentary_comment/Gene-commentary/Gene-commentary_source/Other-source"
  srcDocs <- lapply(getNodeSet(doc, otherSourcePath), xmlDoc)
  srcDocs
}

## retrieve Db tag from such an other source doc.
.getDBTag <- function(srcDoc){
  dbTagPath <- "/Other-source/Other-source_src/Dbtag/Dbtag_db"
  dbTag <- unlist(xpathApply(srcDoc, dbTagPath, xmlValue))
  if(is.null(dbTag)) dbTag <- "Bummer, the dbTag is NULL."
  dbTag
}

getKEGGInfo <- function(doc){

##   ## This will retrieve just the KEGG IDs
##   KEGGIDPath <- "/Entrezgene/Entrezgene_comments/Gene-commentary/Gene-commentary_comment/Gene-commentary/Gene-commentary_source/Other-source/Other-source_src/Dbtag/Dbtag_tag/Object-id/Object-id_str"
##   KEGGIds <- unlist(xpathApply(doc, KEGGIDPath, xmlValue))

##   ## This will retrieve the kind of annotation (sanity check again)
##   KEGGPath <-"/Entrezgene/Entrezgene_comments/Gene-commentary/Gene-commentary_comment/Gene-commentary/Gene-commentary_source/Other-source/Other-source_src/Dbtag/Dbtag_db"
##   KEGGStatus <- unlist(xpathApply(doc, KEGGPath, xmlValue))
  
##   ##Assemble these into a data.frame and filter
##   result <- data.frame(KEGGId=KEGGIds, dataType=KEGGStatus,
##                        stringsAsFactors=FALSE)
##   result <- result[grep("KEGG", result[,2]),]
##   result[,1] ## I probably want to leave it as a df.. (for later when I go to add it to a DB)


  ## New strategy.  We have to check as we go and only retrieve things that
  ## match
  srcDocs <- .getOtherSrcDocs(doc)

  ## This is the path for a source node to get the KEGG info (if its a KEGG
  ## doc)
  KEGGIDPath <- "/Other-source/Other-source_src/Dbtag/Dbtag_tag/Object-id/Object-id_str"

##   KEGGIDs = character()
##   for(i in seq_len(length(srcDocs))){
##     if(.getDBTag(srcDocs[[i]])=="KEGG"){
##       KEGGIDs = c(KEGGIDs,
##         unlist(xpathApply(srcDocs[[i]], KEGGIDPath, xmlValue)))
##     }
##   }
  
  checkNode <- function(doc){
    if(.getDBTag(doc)=="KEGG"){
      return(unlist(xpathApply(doc, KEGGIDPath, xmlValue)))
    }
  }
  KEGGIDs <- unlist(lapply(srcDocs, checkNode))
  
  KEGGIDs
}

## TODO: switch to gathering the KEGG pathway data (instead of just the KEGG
## ID)- also useful just not what we want.



getGeneStuff <- function(x){
  #require(XML)
  baseUrl <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?"
  xsep <- paste(x, collapse=",")
  url <- paste(baseUrl,"db=gene&id=",xsep,"&retmode=xml", sep="")
    
  ## NOW we have to parse the available XML
  EGSet <- xmlParse(url)
  ## And then just grab back out everything we possibly can...
  entrezGeneID <- unlist(xpathApply(EGSet, "//Gene-track_geneid", xmlValue))
  ## TODO: vectorize the following check (or decide on a different strategy)
##   if(entrezGeneID != as.character(x))
##      stop("This is not the ID you are looking for")
  speciesName <- unlist(xpathApply(EGSet, "//Org-ref_taxname", xmlValue))

  ## miniDocs are the individual entrez Gene records
  miniDocs <- lapply(getNodeSet(EGSet, "//Entrezgene"), xmlDoc)
    
  ## pubmed Ids
  pmIdPath <- "/Entrezgene/Entrezgene_comments/Gene-commentary/Gene-commentary_refs/Pub/Pub_pmid/PubMedId"
  pmIds <- lapply(miniDocs, function(x)
                  unlist(xpathApply(x, pmIdPath, xmlValue)))
  ## GO Ids
  GOIds <- lapply(miniDocs, getGOInfo)
  ## KEGG Ids (needs a more nested approach than GO)
  KEGGIds <- lapply(miniDocs, getKEGGInfo)
  
  
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
       GOIds = GOIds)
  
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
## ## egs = Lkeys(org.Hs.egCHR)[1:1000]


## system.time(getGeneStuff(egs))














##############################################################################
## More TODO:
## 1) make something to retrieve all the EGs from a tax_id.
## 2) GO IDs will require more processing to make GO2ALL table.  This can be
## handled in much the same way as it is now, just by using it along with the
## GO.db package which this workflow will have to depend upon.
