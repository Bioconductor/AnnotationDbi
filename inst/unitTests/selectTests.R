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

## What about "ENTREZID"?  Do I need to be ablt to getRKeyNames on that???
## I don't think so (but should check)
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






