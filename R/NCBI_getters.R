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






## What I want is almost certainly something more like this.  Do I want x to
## be 1 or 500 things?  I think i want it to be 500.  So then how can I
## separate these elements from each other???
## For now, it seems I *may* need to make it so that x is just one thing,
## since I don't have a good way within the XML that comes back, of finding
## which things from a 1:many relationship (like pmid) belong to which gene...


## getGeneStuff <- function(x){
##   require(XML)
##   baseUrl <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?"
##   xsep <- paste(x, collapse=",")
##   url <- paste(baseUrl,"db=gene&id=",xsep,"&retmode=xml", sep="")
  
  
##   ## NOW we have to parse some XML
##   res <- xmlParse(url)
##   ## And then just grab back out everything we possibly can...
##   entrezGeneID <- unlist(xpathApply(res, "//Gene-track_geneid", xmlValue))
##   ## TODO: vectorize the following check (or decide on a different strategy)
## ##   if(entrezGeneID != as.character(x))
## ##      stop("This is not the ID you are looking for")
##   speciesName <- unlist(xpathApply(res, "//Org-ref_taxname", xmlValue))

##   pmNodes <- getNodeSet(res, "//Entrezgene")
##   ##xmlGetAttr
##   ##xmlValue
##   ##pmids <- sapply(pmNodes, xpathApply, "//PubMedId", xmlValue)

##   ## length(unlist(xpathApply(pmNodes[[1]], "/Entrezgene/Entrezgene_comments/Gene-commentary/Gene-commentary_refs/Pub/Pub_pmid/PubMedId", xmlValue)))

##   ## test <- sapply(pmNodes, xmlValue, "/Entrezgene/Entrezgene_comments/Gene-commentary/Gene-commentary_refs/Pub/Pub_pmid/PubMedId")

##   ## test <- sapply(pmNodes, xmlValue, "//PubMedId")

##   ## closer (except that xpathApply is using ONLY the entire tree to search
##   ## which puts us back to square 1)
##   ## tst <- sapply(pmNodes, function(x) xpathApply(x, "//PubMedId", xmlValue))
  
    
## ## So this is getting closer as we now know that we must xmlDoc() our nodes.
## ##   tst <- lapply(pmNodes, function(x) unlist(xpathApply(xmlDoc(x),
## ##                                                        "//PubMedId", xmlValue)))

##   ## NOW lets just get this thing to be more specific:
##   pmidPath <- "/Entrezgene/Entrezgene_comments/Gene-commentary/Gene-commentary_refs/Pub/Pub_pmid/PubMedId"
##   tst <- lapply(pmNodes, function(x) unlist(xpathApply(xmlDoc(x),
##                                                        pmidPath, xmlValue)))

  
                  
##   ## return a list of things
##   list(entrez = entrezGeneID,
##        species = speciesName,
##        pmids = pmids)
  
## }




## And there is also the SLOW solution...  but we don't want that...
## OR the slow solution...  (takes 12 minutes for 1000 records which is
## probably too slow for 40,000 records)

## system.time(lapply(egs, getGeneStuff))


## More TODO: make something to retrieve the EGs from a taxid




















getGeneStuff <- function(x){
  #require(XML)
  baseUrl <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?"
  xsep <- paste(x, collapse=",")
  url <- paste(baseUrl,"db=gene&id=",xsep,"&retmode=xml", sep="")
    
  ## NOW we have to parse the available XML
  res <- xmlParse(url)
  ## And then just grab back out everything we possibly can...
  entrezGeneID <- unlist(xpathApply(res, "//Gene-track_geneid", xmlValue))
  ## TODO: vectorize the following check (or decide on a different strategy)
##   if(entrezGeneID != as.character(x))
##      stop("This is not the ID you are looking for")
  speciesName <- unlist(xpathApply(res, "//Org-ref_taxname", xmlValue))

  ## pubmed IDs
  miniDocs <- lapply(getNodeSet(res, "//Entrezgene"), xmlDoc)
  pmidPath <- "/Entrezgene/Entrezgene_comments/Gene-commentary/Gene-commentary_refs/Pub/Pub_pmid/PubMedId"
  pmids <- lapply(miniDocs, function(x)
                  unlist(xpathApply(x, pmidPath, xmlValue)))

  
  
                  
  ## return a list of things
  list(entrez = entrezGeneID,
       species = speciesName,
       pmids = pmids)
  
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



