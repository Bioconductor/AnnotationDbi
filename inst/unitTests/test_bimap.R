require(org.Hs.eg.db)
require(RUnit)
## map is just to represent a classic Bimap
map  <- org.Hs.egSYMBOL
## map2 represents an AnnotationDbMap mapping made by some other process for BC
## map2 <- new("AnnotationDbMap", AnnotDb=org.Hs.eg.db, columns="SYMBOL")
map3 <- AnnotationDbi:::makeFlatBimapUsingSelect(org.Hs.eg.db, col="SYMBOL")
## And another map object expected by most other tests.
map2 <- AnnotationDbi:::makeFlatBimapUsingSelect(org.Hs.eg.db, col="ONTOLOGY")

##map3 <- AnnotationDbi:::flatten(map)
# molecular function, cellular component, biological process
bioconcepts <- c("MF", "CC", "BP")


## test ls
test_ls <- function(){
  res <- ls(map)
  checkTrue(is.character(res))
  checkTrue(length(res) > 0)
  checkEquals(c("1","10","100"), head(res,n=3))
  res2 <- ls(map3)
  checkTrue(is.character(res2))
  checkTrue(length(res2) > 0)
  checkEquals(c("1","10","100"), head(res2,n=3))
}

## test Lkeys and Rkeys
test_Lkeys <- function(){
  lres <- Lkeys(map)
  checkTrue(length(lres) >0)
  checkTrue(!any(is.na(lres)))
  checkTrue(length(lres) == length(unique(lres)))

  lres2 <- Lkeys(map2)
  checkTrue(length(lres2) >0)
  checkTrue(!any(is.na(lres2)))
  checkTrue(length(lres2) == length(unique(lres2)))
}

test_Rkeys <- function(){
  rres <- Rkeys(map)
  checkTrue(length(rres) >0)
  checkTrue(!any(is.na(rres)))
  checkTrue(length(rres) == length(unique(rres)))

  rres2 <- Rkeys(map2)
  checkTrue(length(rres2) >0)
  checkTrue(!any(is.na(rres2)))
  checkTrue(length(rres2) == length(unique(rres2)))
}

## test revmap (add a test now that it seems to work...
test_revmap <- function(){
  rmap <- revmap(map)
  checkTrue(rmap@direction == -1)
  rmap2 <- revmap(map2)
  checkTrue(rmap2@direction == -1)
}

## test mget
test_mget <- function(){
  k <- c("1", "2")
  res <- mget(k, map)
  checkIdentical(names(res), k)
  checkIdentical(res[[1]], "A1BG")
  checkIdentical(res[[2]], "A2M")
  checkTrue(length(res) == length(k))

  res2 <- mget(k, map2)
  checkIdentical(names(res2), k)
  checkTrue(setequal(res2[[2]], bioconcepts))
  checkTrue(length(res2) == length(k))

  ## reverse test
  kr <- c("CC", "MF")
  res3 <- mget(kr, revmap(map2))
  checkIdentical(names(res3), kr)
  checkIdentical(res3[[1]][2], "2")
  checkTrue(length(res3) == length(kr))
}

## test as.list
test_as.list <- function(){
  res <- as.list(map)
  checkIdentical(names(res)[2], "2")
  checkIdentical(res[[2]], "A2M")
  checkTrue(length(res) > 1000)

  res2 <- as.list(map2)
  checkIdentical(names(res2)[2], "2")
  checkTrue(setequal(res2[[2]], bioconcepts))
  checkTrue(length(res2) > 1000)

  ## reverse test
  res3 <- as.list(revmap(map2))
  checkTrue(setequal(names(res3), bioconcepts))
  checkIdentical(res3[[1]][2], "2")
  checkIdentical(length(res3), 3L)
}

## test as.character
test_as.character <- function(){
  res <- as.character(map)
  checkIdentical(names(res)[2], "2")
  checkIdentical(res[["2"]], "A2M")

  res2 <- as.character(map2)
  checkIdentical(names(res2)[3:5], rep("2", 3))
  checkTrue(setequal(res2[names(res2) == "2"], bioconcepts))

  ## reverse test
  res3 <- as.character(revmap(map2))
  checkTrue(setequal(names(res3[res3 == "2"]), bioconcepts))
  checkIdentical(unname(res3[3:5]), rep("2", 3))
}

## test eapply
test_eapply <- function(){
  res <- eapply(map, length)
  checkIdentical(names(res)[2], "2")
  checkTrue(res[["2"]] == 1)

  res2 <- eapply(map2, length)
  checkIdentical(names(res2)[2], "2")
  checkTrue(res2[["2"]] == 3)
}

## test get
test_get <- function(){
  k <- "2"
  res <- get(k, map)
  checkTrue(res == "A2M")

  res2 <- get(k, map2)
  checkTrue(setequal(res2, bioconcepts))

  ## reverse test
  kr <- "CC"
  res3 <- get(kr, revmap(map2))
  checkTrue(res3[2] == "2")
}

## test exists
test_exists <- function(){
  checkTrue(exists("2", map) == TRUE)
  checkTrue(exists("titi", map) == FALSE)

  checkTrue(exists("9", map2) == TRUE)
  checkTrue(exists("titi", map2) == FALSE)
}

## test "[["
test_dblBrackets <- function(){
  res <- map[["2"]]
  checkTrue(res == "A2M")
  res2 <- map2[["2"]]
  checkTrue(setequal(res2, bioconcepts))
}

test_head <- function(){
  res <- head(map, n=3)
  checkTrue( class(res) == "AnnDbBimap" )

  res2 <- head(map2, n=3)  ## implement Lkeys and Rkeys
  checkTrue( class(res2) == "data.frame" )
  checkTrue( dim(res2)[1] == 3 )
  checkTrue( dim(res2)[2] == 2 )
}

test_tail <- function(){
  res <- tail(map, n=3)
  checkTrue( class(res) == "AnnDbBimap" )

  res2 <- tail(map2, n=3)
  checkTrue( class(res2) == "data.frame" )
  checkTrue( dim(res2)[1] == 3 )
  checkTrue( dim(res2)[2] == 2 )
}

## test "$"
test_Dollar <- function(){
  res <- map$"2"
  checkTrue(res == "A2M")
  res2 <- map2$"2"
  checkTrue(setequal(res2, bioconcepts))
}

## test toTable as.data.frame
test_toTable <- function(){
  res <- toTable(map)
  resdf <- as.data.frame(map)
  checkEquals(res, resdf)
  checkEquals(colnames(res), c("gene_id","symbol"))
  checkTrue(res[2, 1] == "2")
  checkTrue(res[2, 2] == "A2M")

  ## So one potential issue I have is that I get the "wrong" sort of headings?
  ## this is largely a cosmetic issue though...
  res2 <- toTable(map2)
  resdf2 <- as.data.frame(map2)
  checkEquals(res2, resdf2)
  checkEquals(colnames(res2), c("ENTREZID","ONTOLOGY"))
  checkTrue(all(res2[3:5, 1] == "2"))
  checkTrue(
      setequal(res2[res2[["ENTREZID"]] == "2", "ONTOLOGY"], bioconcepts)
  )
}

test_sample <- function(){
  res <- sample(map,size=2)
  checkTrue(length(res)==2)
  checkTrue(class(res)=="list")

  res2 <- sample(map2,size=2)
  checkTrue(length(res2)==2)
  checkTrue(class(res2)=="list")
}

