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



## Another way to approach this is something like this:
## str(sapply(EG, function(elt) unlist(xpathApply(xmlDoc(elt), "//PubMedId", xmlValue))))

## where EGSet is the whole doc ie.
## EGSet = xmlParse(url)
## for (i in 1:n) print(xpathApply(EGSet, sprintf("count(//Entrezgene[%d]//PubMedId)", i)))
## for (i in 1:n) print(xpathApply(EGSet, sprintf("//Entrezgene[%d]//PubMedId/text()", i), xmlValue))

## or
## make queries
## xpq = sprintf("//Entrezgene[%d]//PubMedId", 1:n)
## lapply(xpq, xpathApply, doc=EGSet, fun=xmlValue)

## Other notes from discussion of Xpath with Martin on how to use XPath to
## retrieve more from a node without allocating a node...
## library(XML)
## base <-
## url <-
## sprintf("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=gene&id=%s&retmode=xml",
##                "1,10,100")
## eg <- xmlParse(url)
## sets <- getNodeSet(eg, "//Entrezgene")
## res <-
##     xpathApply(sets[[1]],
## "//Other-source[//Dbtag_db/text()='GO']//Object-id_id/text()", xmlValue)

## xpathApply(eg, "count(//Dbtag_db[text()='GO'])")
## xpathApply(eg, "//Dbtag_db[text()='GO']/text()")



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



getGeneStuff <- function(entrezGenes, dir = "files"){
  require(XML)
  baseUrl <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?"
  xsep <- paste(entrezGenes, collapse=",")
  url <- paste(baseUrl,"db=gene&id=",xsep,"&retmode=xml", sep="")

  ## NOW we have to parse the available XML
  EGSet <- xmlParse(url)

## Here we will save the files out to a dir
  miniDocs <- lapply(getNodeSet(EGSet, "//Entrezgene"), xmlDoc)
  wd <- getwd()
  path <- paste(wd, dir, sep="/")
  if(!file.exists(path)){dir.create(path)}
  setwd(path)
  paths <- paste(paste(path, entrezGenes, sep="/"),".xml",sep="")
  for(i in seq_len(length(miniDocs))){
    saveXML( miniDocs[[i]], file = paths[[i]])
  }
  setwd(wd)
  
  ## attempt to remove miniDocs each time?
  ## (something tells me this is a token gesture only)
  rm(miniDocs)
  ## even just doing this (saving the files) takes nearly 30 gigs of RAM before
  ## too long (but at least it completed).


  
##   ## Some tags can only occur once per gene
##   entrezGeneID <- unlist(xpathApply(EGSet, "//Gene-track_geneid", xmlValue))
##   speciesName <- unlist(xpathApply(EGSet, "//Org-ref_taxname", xmlValue))

##   ## But most information is either more complex than that or is stored in a
##   ## more complex way by the XML
##   ## miniDocs are the individual entrez Gene records
## ##   miniDocs <- lapply(getNodeSet(EGSet, "//Entrezgene"), xmlDoc)

## ## ##   ## Sometimes we are lucky and the information we want has an obviously
## ## ##   ## unique tag.
## ## ##   ## Like pubmed Ids
## ##   pmIdPath <- "/Entrezgene/Entrezgene_comments/Gene-commentary/Gene-commentary_refs/Pub/Pub_pmid/PubMedId"
## ##   pmIds <- lapply(miniDocs, function(x)
## ##                   unlist(xpathApply(x, pmIdPath, xmlValue)))


## ###########################################################################
## ## Lets try to get rid of the minidocs

## ## miniDocs <- lapply(getNodeSet(EGSet, "//Entrezgene"), xmlDoc)
  
##    ##pmIdPath <- "/Entrezgene/Entrezgene_comments/Gene-commentary/Gene-commentary_refs/Pub/Pub_pmid/PubMedId"
## ##   pmIds <- lapply(miniDocs, function(x)
## ##                   unlist(xpathApply(x, pmIdPath, xmlValue)))

##   ## Can try this way: - no improvement?
## ##   egs <- getNodeSet(EGSet, "//Entrezgene")
## ##   ###pmIds <- xpathApply(xmlDoc(egs[[1]]), "//PubMedId", xmlValue)
## ##   pmIds <- sapply(egs, function(x) unlist(xpathApply(xmlDoc(x), "//PubMedId", xmlValue)))

##   ## Can also try this way: - but for 800 genes in a chunk,
##   ## - at least it tops out at 2 GB (for 800 genes/chunk = better on usage AND
##   ## - speed with smaller chunks, but it means we bug some server more
##   ## - often...
##   ## IT also takes MUCH longer though! (probably because of larger parse space))
##   n <- length(entrezGenes)
##   paths <- sprintf("//Entrezgene[%d]//PubMedId", 1:n)
##   pmids <- lapply(paths, xpathApply, doc=EGSet, fun=xmlValue)
##   pmIds <- lapply(pmids, unlist)

## ## This is frustrating.  Total process looks to take about 6-12 Gigs of ram (and be pretty slow for parsing only PMIDs ~ 1 hour).  I mean the ram usage is WAY down when I stop calling getNodeSet() (or maybe NOT - since it seems to climb more as things progress), but I am seriously tempted to just save out individual XML files to a tempDir, in an initial step, and then parse one at a time.  Worst part is that it seems to eat MORE ram the further along it goes...

##   ## I should really do this.  At least if I am working from files in a dir, I
##   ## can have better control over how the parsing is done and where the time
##   ## is going.

  
  
## ##   ## For GO I should be able to do something like this:
## ##   res <- xpathApply(egs[[1]],
## ##        "//Other-source[//Dbtag_db/text()='GO']//Object-id_id/text()", xmlValue)
## ##   ## checks
## ##   res1 <- xpathApply(egs[[1]], "count(//Dbtag_db[text()='GO'])")
## ##   res2 <- xpathApply(egs[[1]], "//Dbtag_db[text()='GO']/text()")
    



  
## ##   ## But sometimes we have to do some more checking to make sure that what we
## ##   ## are retrieving is in the context of certain kinds of tags, or of tags
## ##   ## that contain certain information.

## ##   ## custom helper to get and process the GO terms
## ##   GOIds <- lapply(miniDocs, getGOInfo)

## ##   ## KEGG Gene IDs are NOT the path IDs.
## ##   otherSourcePath <- "/Entrezgene/Entrezgene_comments/Gene-commentary/Gene-commentary_comment/Gene-commentary/Gene-commentary_source/Other-source"
## ##   dbTagPath <- "/Other-source/Other-source_src/Dbtag/Dbtag_db"
## ##   resIDPath_str <- "/Other-source/Other-source_src/Dbtag/Dbtag_tag/Object-id/Object-id_str"
## ##   KEGGGeneIds <- lapply(miniDocs, .getSubNodeInfo, type = "KEGG",
## ##                     otherSourcePath = otherSourcePath,
## ##                            dbTagPath = dbTagPath,
## ##                            resIDPath = resIDPath_str)

## ##   KEGGPathIds <- lapply(miniDocs, .getSubNodeInfo, type = "KEGG pathway",
## ##                     otherSourcePath = otherSourcePath,
## ##                            dbTagPath = dbTagPath,
## ##                            resIDPath = resIDPath_str)

## ##   unigeneIds <- lapply(miniDocs, .getSubNodeInfo, type = "UniGene",
## ##                     otherSourcePath = otherSourcePath,
## ##                            dbTagPath = dbTagPath,
## ##                            resIDPath = resIDPath_str)
## ##   resIDPath_id <- "/Other-source/Other-source_src/Dbtag/Dbtag_tag/Object-id/Object-id_id"
## ##   ## May only want to look for MIM if we are tax ID 9606 (or can be empty here)
## ##   MIMIds <- lapply(miniDocs, .getSubNodeInfo, type = "MIM",
## ##                     otherSourcePath = otherSourcePath,
## ##                            dbTagPath = dbTagPath,
## ##                            resIDPath = resIDPath_id)

## ##   ## Refseqs are in another couple of places (we can merge these later, or I
## ##   ## can write a helper like for GO and merge them as we go.)
## ##   RSOtherSourcePath <- "/Entrezgene/Entrezgene_comments/Gene-commentary/Gene-commentary_comment/Gene-commentary"
## ##   RSdbTagPath <- "/Gene-commentary/Gene-commentary_heading" 
## ##   RSResIdTagPathRNA <- "/Gene-commentary/Gene-commentary_products/Gene-commentary/Gene-commentary_accession" 
## ##   RSRNAIds <- lapply(miniDocs, .getSubNodeInfo,
## ##     type = "RefSeqs maintained independently of Annotated Genomes",
## ##                     otherSourcePath = RSOtherSourcePath,
## ##                            dbTagPath = RSdbTagPath,
## ##                            resIDPath = RSResIdTagPathRNA)
  
## ##   RSResIdTagPathProt <- "/Gene-commentary/Gene-commentary_products/Gene-commentary/Gene-commentary_products/Gene-commentary/Gene-commentary_accession"   
## ##   RSProtIds <- lapply(miniDocs, .getSubNodeInfo,
## ##     type = "RefSeqs maintained independently of Annotated Genomes",
## ##                     otherSourcePath = RSOtherSourcePath,
## ##                            dbTagPath = RSdbTagPath,
## ##                            resIDPath = RSResIdTagPathProt)

## ##   ## Get official Symbol
## ##   symbolSourcePath <- "/Entrezgene/Entrezgene_properties/Gene-commentary/Gene-commentary_properties/Gene-commentary"
## ##   symbolDbTag <- "/Gene-commentary/Gene-commentary_label"
## ##   symbolresIDPath <- "/Gene-commentary/Gene-commentary_text"
## ##   symbolIds <- lapply(miniDocs, .getSubNodeInfo,
## ##                       type = "Official Symbol",
## ##                     otherSourcePath = symbolSourcePath,
## ##                            dbTagPath = symbolDbTag,
## ##                            resIDPath = symbolresIDPath)
## ##   ## Get official Name
## ##   fullNames <- lapply(miniDocs, .getSubNodeInfo,
## ##                       type = "Official Full Name",
## ##                     otherSourcePath = symbolSourcePath, ##same path as symbols
## ##                            dbTagPath = symbolDbTag,
## ##                            resIDPath = symbolresIDPath)
## ##   ## Get alternate symbols (merge with official ones when making table)
## ##   ## TODO: would be SAFER to do some of the 1:1 stuff like this too
## ##   ## Therefore redo how I get EG, PMID, and species name.
## ##   aliasSourcePath <- "/Entrezgene/Entrezgene_gene/Gene-ref/Gene-ref_syn"
## ##   aliasDbTag <- "/Gene-ref_syn/Gene-ref_syn_E"
## ##   aliasresIDPath <- "/Gene-ref_syn/Gene-ref_syn_E"
## ##   aliasIds <- lapply(miniDocs, .getSubNodeInfo,
## ##                       type = NULL,
## ##                     otherSourcePath = aliasSourcePath,
## ##                            dbTagPath = aliasDbTag,
## ##                            resIDPath = aliasresIDPath)


  
##   ## Data sanity checks:
##   ## All genes should be from the same critter:
##   ## TODO: move the checks on EG uniqueness to outside of this function
##   if(length(unique(entrezGeneID)) != length(entrezGeneID))
##      stop("Some of the entrez gene IDs have been repeated.")
##   if(length(unique(speciesName))>1)
##     stop("The IDs being processed need to all be from the same species.")
     
##   ## The following checks can stay at this level though
##   if(unique(entrezGeneID %in% entrezGenes) %in% FALSE) ##if any don't match
##     stop("The entrez Genes discovered don't match the IDs being looked up!")
##   if(length(entrezGenes) > length(entrezGeneID))
##     warning("Some of the entrez Genes beings sought were not found.")
##   if(length(entrezGeneID) > length(entrezGenes))
##     stop("There are more EGs being found than we expected.")
  
##   ## return a list of things
##   list(entrez = entrezGeneID,
##        species = speciesName,
##        pmIds = pmIds##, 
## ##       GOIds = GOIds##,  
## ##        KEGGGeneIds = KEGGGeneIds,
## ##        KEGGPathIds = KEGGPathIds,
## ##        aliasIds = aliasIds, 
## ##        symbolIds = symbolIds,
## ##        fullNames = fullNames,
## ##        RSProtIds = RSProtIds,
## ##        RSRNAIds = RSRNAIds,
## ##        MIMIds = MIMIds,
## ##        unigeneIds = unigeneIds
       
##        )
  
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
    if(is.null(list1[[i]]) && is.null(list2[[i]])){
      result[[i]] <- NA
    }else{
      result[[i]] <- c(list1[[i]], list2[[i]])
    }
  }
  result
}


## conversion Utility:
.convertNullToNA <- function(list){
  ind <- unlist(lapply(list, is.null))
  list[ind] <- NA
  list
}

## this just makes a nice named data.frame from your data lists
.makeSimpleDF <- function(entrez, fieldVals, name){
  if(length(entrez) != length(fieldVals))
    stop("To make data.frame from list + vector, both must be equal len.")
  names(fieldVals) <- entrez
  fieldVals <- .convertNullToNA(fieldVals)
  expandedList <- unlist2(fieldVals)
  result <- data.frame(names(expandedList), expandedList,
                       stringsAsFactors=FALSE) 
  colnames(result) <- c("gene_id", name)
  ## now I have to remove any rows with NA values across the whole row.
  result <- result[!is.na(result[,2]),]
  row.names(result) <- NULL ##1:dim(result)[1]
  result
}


## used to collapse GO lists to a data frame
.unwindGOs <- function(GOIds, entrez, type){
  res <- vector("list",length(GOIds))
  for(i in seq_len(length(GOIds))){
    for(j in seq_len(length(GOIds[[i]]))){
      if( is.null(unlist2(GOIds[[i]][[j]][[type]])) ){
        res[[i]] <- NA
      }else{
        res[[i]] <- c(res[[i]], unlist2(GOIds[[i]][[j]][[type]]))
      }
    }
  }
  names(res) <- entrez
  res <- .convertNullToNA(res)
  res
}


## used to make the 6 custom GO tables
.makeUnWoundGOTables <- function(entrez, GOIds, con){
  ## GOIds is a list of equal length to the entrez IDs
  if(length(entrez) != length(GOIds)){
    stop("There must be a list of GOIds")}  
  uw_gos <- .unwindGOs(GOIds, entrez, type=1)
  if(length(uw_gos[is.na(uw_gos)]) != length(uw_gos)){
    go_id <- unlist2(uw_gos)
    evidence <- unlist2(.unwindGOs(GOIds, entrez, type=2))
    ontology <- Ontology(go_id) ## This step REQUIRES that there be GO IDs
    baseFrame <- cbind(gene_id = names(go_id), go_id=go_id,
                       evidence=evidence, ontology=ontology)
    bp <- data.frame(matrix(split(baseFrame, ontology)$BP,
                            byrow=FALSE,
                            nrow=length(grep("BP",ontology))))[,1:3]
    mf <- data.frame(matrix(split(baseFrame, ontology)$MF,
                            byrow=FALSE,
                            nrow=length(grep("MF",ontology))))[,1:3]
    cc <- data.frame(matrix(split(baseFrame, ontology)$CC,
                            byrow=FALSE,
                            nrow=length(grep("CC",ontology))))[,1:3]
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
  }else{
    ## as with other tables, if we have nothing to populate, then we don't
    ## want to even make a table!
    warning(paste("no values found for any GO tables",
                  " in this data chunk.", sep=""))
    return() 
  }
}


## TODO: flesh out the following:

#.makeMetaTables <- function(){}


.makeOrgDB <- function(sList, con){
  .makeCentralTable(sList$entrez, con)
  ## gene_info table is special
  ## I need to make the data.frame and do some minor filtering.
##   gene_infoData <- data.frame(
##     gene_id = sList$entrez,
##     gene_name = unlist(.convertNullToNA(as.list(sList$fullNames))),
##     symbol = unlist(.convertNullToNA(as.list(sList$symbolIds))))
##   gene_infoData <- ## still have to remove lines with no data! 
##     gene_infoData[!is.na(gene_infoData[,2]) & !is.na(gene_infoData[,2]),]
##   .makeSimpleTable(gene_infoData,
##                    table = "gene_info", con, fieldNameLens=c(255,80),
##                    indFields = character())
  
  .makeSimpleTable(.makeSimpleDF(entrez = sList$entrez,
                                 fieldVals = sList$pmIds, "pubmed_id"),
                   table = "pubmed", con)
##   .makeSimpleTable(.makeSimpleDF(entrez = sList$entrez,
##                                  fieldVals = .combineTwoLists(sList$alias,
##                                    sList$symbol), "alias_symbol"),
##                    table = "alias", con)
##   .makeSimpleTable(.makeSimpleDF(entrez = sList$entrez,
##                                  fieldVals = sList$KEGGPathIds, "path_id"),
##                    table = "kegg", con)
##   .makeSimpleTable(.makeSimpleDF(entrez = sList$entrez,
##                                  fieldVals = .combineTwoLists(sList$RSProtIds,
##                                    sList$RSRNAIds), "accession"),
##                    table = "refseq", con)
##   .makeSimpleTable(.makeSimpleDF(entrez = sList$entrez,
##                                  fieldVals = sList$MIMIds, "omim_id"),
##                    table = "omim", con)
##   .makeSimpleTable(.makeSimpleDF(entrez = sList$entrez,
##                                  fieldVals = sList$unigeneIds, "unigene_id"),
##                    table = "unigene", con)
##   ## GO tables are special
##   .makeUnWoundGOTables(entrez = sList$entrez, GOIds = sList$GOIds, con)
}

getDataAndAddToDb <- function(EGChunk, con){      
    list <- getGeneStuff(EGChunk)
#    .makeOrgDB(list, con)
    print(gc())
}

## Wrap the functionality like so:
buildEntrezGeneDbFromWebServices <- function(entrezGenes, file="test.sqlite"){
  EGs <- entrezGenes
  chunkSize <- 400  ## 800 is the max here but is probably NOT optimal
  ## TODO: check if there is a file and if so just remove it.
  ## file.remove(file) ##remove the old file when they re-run the code?
  con <- dbConnect(SQLite(), file)

  ## Then break it into chunks
  if(length(EGs)<chunkSize){
    sList <- getGeneStuff(EGs)
    .makeOrgDB(sList, con)
  }else{
    numChunks <- length(EGs) %/% chunkSize
    remChunks <- length(EGs) %% chunkSize
    splitFactor <- rep(seq_len(numChunks), each=chunkSize)
    splitFactor <- c(splitFactor, rep(numChunks+1, each=remChunks))
    EGChunks <- split(EGs, splitFactor)    
    ## Then we just need to apply through and make a super-list
##     superListOfLists <- lapply(EGChunks, getGeneStuff)
##     sList <- .mergeLists(superListOfLists)
##     .makeOrgDB(sList, con)
    ##lapply(EGChunks, getDataAndAddToDb, con)
    for(chunk in EGChunks){
      getDataAndAddToDb(chunk, con)
    }
  }
}





##############################################################################
## More TODO:
##
## 1) Wrap this so that we can 1) get all the EGs, then retrieve their results
## serially into one big super-list for import into a DB.
##
## 2) Some checking will have to be done as we add contents that are matched
## (thinking of the GO terms here) to the DB.  Specifically, each GO ID needs
## to have an evidence code and I know from making this work, that some of
## them will NOT have that.  Therefore, I have to check as they are formatted
## and put into the DB.

