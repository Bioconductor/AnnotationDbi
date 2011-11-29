## One strategy is to put some of my examples (with expected output into here)
## - but this is fragile unless I also provide a localized example DB (which I
## probably should do anyways for other reasons, but is not necessarily useful
## here).

## another strategy is that I should probably have tests for all my helper
## functions to make sure that they are returning what is expected.

## Generally, I want to make tests for each thing that can go wrong due to
## changes elsewhere.  The strategy of writing tests for the helpers will
## catch some of this, but also I need to anticipate things that will change
## in the annotations etc.

x <- org.Hs.eg.db

test_getObjList <- function(x){
  res <- .getObjList(x)
  checkTrue(is.list(res))
  checkTrue(is.character(res[[1]][[1]]))
}

test_makeBimapsFromStrings <- function(x){
  res <- .makeBimapsFromStrings(x, cols=c("CHR","PFAM","GO"))
  checkTrue(is.list(res))
  checkTrue(is(res[[1]], "AnnDbBimap"))
  checkTrue(is(res[[2]], "IpiAnnDbMap"))
  checkTrue(is(res[[3]], "Go3AnnDbBimap")) 
}

test_toTableAndCleanCols <- function(x){
  res <- .makeBimapsFromStrings(x, cols=c("CHR","PFAM","GO"))
  tab <- .toTableAndCleanCols(res[[1]])
  checkTrue(is.data.frame(tab))
  checkTrue(length(colnames(tab)) == length(unique(colnames(tab))))
}





## check that .swapSymbolExceptions will swap ALIAS2EG and ALIAS2PROBE




