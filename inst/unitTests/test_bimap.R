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
test_revmap <- function(){
  rmap <- revmap(map)

  rmap2 <- revmap(map2)
}

## test mget
test_mget <- function(){
  k <- c("1","2")
  res <- mget(k, map)

  res2 <- mget(k, map2)

  ## reverse test 
  kr <- c("CC","MF")
  res3 <- mget(kr, revmap(map2))
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


## test eapply
test_eapply <- function(){
res <- eapply(map, length)

res2 <- eapply(map2, length)
}


## test get
test_get <- function(){
  k <- "1"
  res <- get(k, map)

  res2 <- get(k, map2)

  ## reverse test 
  kr <- "CC"
  res3 <- get(kr, revmap(map2))
}

## test exists
test_exists <- function(){
  checkTrue(exists("2", map) == TRUE)     
  checkTrue(exists("titi", map) == FALSE)
  
  checkTrue(exists("3", map2) == TRUE)  
  checkTrue(exists("titi", map2) == FALSE)  
}


## test "[["
test_dblBrackets <- function(){
res <- map[["1"]]

res2 <- map2[["1"]]
}

## test "$"
test_Dollar <- function(){
res <- map$"1"

res2 <- map2$"1"
}


## test toTable as.data.frame
test_toTable <- function(){
res <- toTable(map)
resdf <- as.data.frame(map)

## So one potential issue I have is that I get the "wrong" sort of headings?
## this is largely a cosmetic issue though...
res2 <- toTable(map2)
resdf2 <- as.data.frame(map2)

}


test_contents <- function(){
res <- contents(map)

res2 <- contents(map2)
}


test_sample <- function(){
res <- sample(map,size=2)

res2 <- sample(map2,size=2)
}


## test_head <- function(){
## res <- head(map, n=2)

## res2 <- head(map2, n=2)  ## implement Lkeys and Rkeys
## }

## test_tail <- function(){
## res <- tail(map, n=2)

## res2 <- tail(map2, n=2)
## }

