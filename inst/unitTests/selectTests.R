## One strategy is to put some of my examples (with expected output into here)
## - but this is fragile unless I also provide a localized example DB (which I
## probably should do anyways for other reasons, but is not necessarily useful
## here).
## Another strategy is that I should probably have tests for all my helper
## functions to make sure that they are returning what is expected.
## Generally, I want to make tests for each thing that can go wrong due to
## changes elsewhere.  The strategy of writing tests for the helpers will
## catch some of this, but also I need to anticipate things that will change
## in the annotations etc.

require(org.Hs.eg.db)
require(GO.db)
require(hgu95av2.db)
x <- org.Hs.eg.db
cols <- c("CHR","PFAM","GO")
keys <- c(1,10)
jointype <- "gene_id"


test_getObjList <- function(x){
  res <- .getObjList(x)
  checkTrue(is.list(res))
  checkTrue(is.character(res[[1]][[1]]))
}

test_makeBimapsFromStrings <- function(x){
  res <- .makeBimapsFromStrings(x, cols=cols)
  checkTrue(is.list(res))
  checkTrue(is(res[[1]], "AnnDbBimap"))
  checkTrue(is(res[[2]], "IpiAnnDbMap"))
  checkTrue(is(res[[3]], "Go3AnnDbBimap"))
}

test_toTableAndCleanCols <- function(x){
  res <- .makeBimapsFromStrings(x, cols=cols)
  tab <- .toTableAndCleanCols(res[[1]])
  checkTrue(is.data.frame(tab))
  checkTrue(length(colnames(tab)) == length(unique(colnames(tab))))
}

test_mergeBimaps <- function(x){
  objs <- .makeBimapsFromStrings(x, cols=cols)
  keys <- keys
  tab <-.mergeBimaps(objs, keys, jointype)
  checkTrue(length(colnames(tab)) == length(unique(colnames(tab))))
  checkTrue(dim(tab)[2] == 7)
  checkTrue(dim(tab)[2] == length(keys))
  checkTrue(length(unique(tab[[jointype]])) == length(keys))
}

test_mergeBimapsPostFilt <- function(x){
  objs <- .makeBimapsFromStrings(x, cols="CHR")
  keys <- keys
  tab <-.mergeBimapsPostFilt(objs, keys, jointype="gene_id")
  checkTrue(length(colnames(tab)) == length(unique(colnames(tab))))
  checkTrue(dim(tab)[2] == 2)
  checkTrue(length(unique(tab[[jointype]])) == length(keys))
}

## "ENTREZID", "PROBEID" and "GOID" do not need getRKeyNames
test_getRKeyNames <- function(x){
  res <- .getRKeyNames(x, keytypes=c("SYMBOL","GO","PFAM"))
  exp <- c("symbol","go_id","ipi_id")
  checkIdentical(res,exp)
}


test_makeColAbbrs <- function(x){
  res <- .makeColAbbrs(x)
  checkTrue(length(res) == length(.getObjList(x)))
  checkIdentical(.getRKeyName(x, res[[1]]), names(res)[1])
}

test_swapSymbolExceptions <- function(x){
  res <- .swapSymbolExceptions(x, strings=c("GO","ALIAS","PFAM","ALIAS2EG"))
  exp <- c("GO","ALIAS2EG","PFAM","ALIAS")
  checkIdentical(res, exp)
}

test_getAllColAbbrs <- function(x){
  res <- .getAllColAbbrs(x)
  checkTrue(length(res) >= length(.getObjList(x)))
  checkIdentical(.getRKeyName(x, res[[1]]), names(res)[1])
}



test_nameExceptions <- function(names){  
  names <- c("ALIAS","SYMBOL","GO","FOOBAZZLE",NA)
  names(names) <-  c("alias_symbol","symbol","go_id","accession",NA)
  cols <- c("ALIAS2EG","SYMBOL","GO","REFSEQ",NA)
  res <- .nameExceptions(names, cols=cols)
  checkTrue(length(res) == length(names))
  swappedElem <- res[4]
  names(swappedElem) <- NULL
  checkIdentical(swappedElem, cols[4])
}


test_addNAsInPlace <- function(x){
  names <- c("ALIAS","SYMBOL","GO","FOOBAZZLE",NA)
  names(names) <-  c("alias_symbol","symbol","go_id","accession",NA)
  cols <- c("ALIAS2EG","CHRLOC","PFAM","SYMBOL","GO","REFSEQ")
  res <- .addNAsInPlace(x,cols=cols)
  exp <- c("ALIAS2EG","CHRLOC",NA,"PFAM",NA,"SYMBOL","GO",NA,NA,"REFSEQ")
  checkIdentical(res,exp)

  x = GO.db
  names <- c("BPPARENTS",NA,NA,NA,NA,NA)
  names(names) <-  c("go_id",NA,NA,NA,NA,NA)
  cols <- c("GOID","TERM")
  res <- .addNAsInPlace(x,cols=cols)
  exp <- c("GOID","TERM",NA,NA,NA,NA)
  checkIdentical(res,exp)
}


test_renameColumnsWithRepectForExtras <- function(x, keys, jointype){
  ## cols can be expected to always include keytypes for testing this helper.
  cols <- c("CHR","PROSITE","GO","REFSEQ")
  objs <- .makeBimapsFromStrings(x, cols=cols)
  res <- .mergeBimaps(objs, keys, jointype)
  oriCols <- c("ENTREZID","CHR","PROSITE","GO","REFSEQ")
  res2 <- .renameColumnsWithRepectForExtras(x, res, oriCols)
  exp <- c("ENTREZID","CHR","PROSITE","PrositeId","GO","Evidence","Ontology",
           "REFSEQ")
  checkIdentical(exp,res2)
}

## mostly this is making sure I don't throw certain things out.
## Honestly, I am not 100% sure if .cleanOutUnwantedCols actually ever does
## throw anything out, but I want to be sure that it does not throw out things
## like Ontology etc.
test_cleanOutUnwantedCols <- function(x, keys, jointype){
  cols <- c("CHR","PROSITE","GO","REFSEQ") 
  objs <- .makeBimapsFromStrings(x, cols=cols)
  res <- .mergeBimaps(objs, keys, jointype)
  oriCols <- c("ENTREZID","CHR","PROSITE","GO","REFSEQ")
  keytype <- "ENTREZID"
  res2 <-.cleanOutUnwantedCols(x, res, keytype, oriCols)
  checkIdentical(colnames(res),colnames(res2))
}


test_cleanupBaseTypesFromCols <- function(x, cols){
  res <- .cleanupBaseTypesFromCols(x, cols)
  res2 <- .cleanupBaseTypesFromCols(x, c(cols,"ENTREZID"))
  checkIdentical(res,res2)
}


## This one is really important as it is generic enough to be reused elsewhere.
test_resort <- function(x){
  cols <- c("CHR","SYMBOL", "PFAM")
  objs <- .makeBimapsFromStrings(x, cols=cols)
  res <- .mergeBimaps(objs, keys, jointype)
  ## jumble res to simulate trouble
  resRO = res[order(sort(res$gene_id,decreasing=TRUE)),]
  oriCols <- c("ENTREZID","CHR","SYMBOL","PFAM")
  Rres <- .resort(resRO, keys, jointype)
  checkIdentical(Rres$gene_id,Rres$gene_id)
}

test_keys <- function(){
  checkException(keys(org.Hs.eg.db, keytype="PROBEID"))
}

## This one to test out some real use cases...
test_select <- function(x){
  keys <- head(keys(hgu95av2.db, "ALIAS"),n=2)
  cols <- c("SYMBOL","ENTREZID","PROBEID")
  res <- select(hgu95av2.db, keys, cols, keytype="ALIAS")
  checkTrue(dim(res)[1]>0)
  checkTrue(dim(res)[2]==4)
  checkIdentical(c("ALIAS","PROBEID","SYMBOL","ENTREZID"), colnames(res))

  keys <- head(keys(org.Hs.eg.db),n=2)
  cols <- c("PFAM","ENTREZID", "GO")
  res <- select(org.Hs.eg.db, keys, cols, keytype="ENTREZID")
  checkTrue(dim(res)[1]>0)
  checkTrue(dim(res)[2]==6)
  checkIdentical(c("ENTREZID","IPI","PfamId","GO","Evidence","Ontology"),
                 colnames(res))

  keys <- head(keys(org.Hs.eg.db,keytype="OMIM"),n=4)
  cols <- c("SYMBOL", "UNIPROT", "PATH")
  res <- select(hgu95av2.db, keys, cols, keytype="OMIM")
  checkTrue(dim(res)[1]>0)
  checkTrue(dim(res)[2]==4)
  checkIdentical(c("OMIM","SYMBOL","UNIPROT","PATH"), colnames(res))

  keys <- head(keys(org.Hs.eg.db),n=2)
  cols <- c("ACCNUM","REFSEQ")
  res <- select(org.Hs.eg.db, keys, cols)
  checkTrue(dim(res)[1]>0)
  checkTrue(dim(res)[2]==3)
  checkIdentical(c("ENTREZID","ACCNUM","REFSEQ"), colnames(res))

  ## TODO: investigate this warning!:
  ## ALSO this causes a bug (the header should say "GO" and NOT "BPPARENTS")
  keys <- head(keys(GO.db), n=4)
  cols <- c("TERM")
  res <- select(GO.db, keys, cols)
  checkTrue(dim(res)[1]>0)
  checkTrue(dim(res)[2]==6)
  checkIdentical(c("GO","Term","Ontology","Definition","Synonym",
                   "Secondary"), colnames(res))
  

  ## bad keys should result in failure
  keys = head(keys(hgu95av2.db))
  cols = c("SYMBOL","ENTREZID", "GO")
  checkException(select(hgu95av2.db, keys, cols, keytype="ENTREZID"))
}
