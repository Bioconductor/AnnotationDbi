## this will install a testDb stashed in the

## this is the package name
pkgName <- "org.TguttataTestingSubset.eg.db"

## Get the package path
pkgPath <- system.file("extdata", pkgName, package="AnnotationDbi")

## Then install it
install.packages(pkgPath, repos=NULL)
## And load it
library("org.TguttataTestingSubset.eg.db")
x <- org.TguttataTestingSubset.eg.db
finchCsomes <- c(as.character(1:15),as.character(17:28),
                 "MT","Un","W","Z","4A","1A","1B")
finchCols <- c("CHROMOSOME","SYMBOL","GENENAME","GID","GO","EVIDENCE",
               "ONTOLOGY","GOALL","EVIDENCEALL","ONTOLOGYALL")

## lower level tests (more useful)
test_keysLow <- function(){
    keytype <- "CHROMOSOME"
    res <- unique(AnnotationDbi:::.noSchemaKeys(x, keytype))
    checkTrue(all(sort(res) == sort(finchCsomes)))
}

test_selectLow <- function(){
    keys <- "100008579"
    cols <- "SYMBOL"
    keytype <- "GID"
    res <- AnnotationDbi:::.noSchemaSelect(x, keys, cols, keytype)
    checkTrue(all(res==c("100008579","EGR1")))
    checkTrue(all(colnames(res)==c("GID","SYMBOL")))
}

## high level tests (does this dispatch right etc.?)
test_columns <- function(){
    res <- columns(x)
    checkTrue(all(sort(res) == sort(finchCols)))
}

test_keytypes <- function(){
    res <- keytypes(x)
    checkTrue(all(sort(res) == sort(finchCols)))
}

test_keys<- function(){
    ## most basic case
    res <- keys(x, "CHROMOSOME")
    checkTrue(all(sort(res) == sort(finchCsomes)))
    
    res <- head(keys(x, "GID"), n=2)
    checkTrue(all(res==c("100008579","100008580")))
    
    res <- head(keys(x, "SYMBOL", pattern="BDNF"))
    checkTrue(res=="BDNF")
    
    res <- head(keys(x, "GID", pattern="BDNF", column="SYMBOL"))
    checkTrue(res=="751584")
    
    res <- head(keys(x, "SYMBOL", column="GID"),n=2)
    checkTrue(all(res==c("ACT5C","AHSA2")))
}


test_select <- function(){
    ## most basic case
    res <- select(x, keys="100008579",
                  columns="SYMBOL", keytype="GID")
    checkTrue(all(res==c("100008579","EGR1")))
    checkTrue(all(colnames(res)==c("GID","SYMBOL")))

    ## return more than one column
    res <- select(x, keys="100008579",
                  columns=c("SYMBOL","CHROMOSOME"), keytype="GID")
    checkTrue(all(res==c("100008579","EGR1","13")))
    checkTrue(all(colnames(res)==c("GID","SYMBOL","CHROMOSOME")))

    ## return GO and evidence codes
    suppressWarnings(res <- head(select(x, keys="100008579",
                       columns=c("GO","EVIDENCE"), keytype="GID"),n=1))
    checkTrue(all(res==c("100008579","GO:0000122","IEA")))
    checkTrue(all(colnames(res)==c("GID","GO","EVIDENCE")))

    ## test lookup from alt-key
    res <- select(x, keys="BDNF",
                  columns="GENENAME", keytype="SYMBOL")
    checkTrue(all(res==c("BDNF","brain-derived neurotrophic factor")))
    checkTrue(all(colnames(res)==c("SYMBOL","GENENAME")))
    
}
