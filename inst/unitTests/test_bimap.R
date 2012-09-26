require(org.Hs.eg.db)
require(RUnit)
## map is just to represent a classic Bimap
map  <- org.Hs.egSYMBOL
## map2 represents an AnnotationDbMap mapping made by some other process for BC
map2 <- new("AnnotationDbMap", AnnotDb=org.Hs.eg.db, cols="ONTOLOGY")


## test ls
test_ls <- function(){
  res <- ls(map)
  checkTrue(is.character(res))
  checkTrue(length(res) > 0)
  checkEquals(c("1","2","3"), head(res,n=3))
  res2 <- ls(map2)
  checkTrue(is.character(res2))
  checkTrue(length(res2) > 0)
  checkEquals(c("1","2","3"), head(res,n=3))
}

## test revmap (add a test now that it seems to work...


## test mget
test_mget <- function(){
  k <- c("1","2")
  res <- mget(k, map)

  res2 <- mget(k, map2)

  ## reverse test ## this part still needs some work...
  ## res3 <- mget(k, revmap(map2))
}



## test as.list
test_as.list <- function(){
  res <- as.list(map)

  res2 <- as.list(map2)

  ## reverse test 
  res3 <- as.list(revmap(map2))
  ## the length should be 3...
}


## test as.character
test_as.character <- function(){
  res <- as.character(map)

  res2 <- as.character(map2)
  
  ## reverse test
  res3 <- as.character(revmap(map2))
}

