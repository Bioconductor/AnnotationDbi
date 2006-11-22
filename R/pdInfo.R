setClass("PDInfo",
         representation=representation(
           get_db="function",
           manufacturer="character",
           genome_build="character"))

setClass("SNPPDInfo", contains="PDInfo")


allPMIds <- function(map) {
    sqliteQuickColumn(map@get_db(), "pmfeature", "fid")
}


allMMIds <- function(map) {
    sqliteQuickColumn(map@get_db(), "mmfeature", "fid")
}


allPMAllele <- function(map) {
    sqliteQuickColumn(map@get_db(), "pmfeature", "allele")
}


allPMStrand <- function(map) {
    sqliteQuickColumn(map@get_db(), "pmfeature", "strand")
}

pmIdsByAllele1 <- function(map, allele=c("A", "B")) {
    if (allele == "A")
      allPMIds(map)[!allPMAllele(map)]
    else
      allPMIds(map)[allPMAllele(map)]
}

pmIdsByAllele2 <- function(map, allele=c("A", "B")) {
    sql <- "select fid from pmfeature where allele = "
    if (allele == "A")
      sql <- paste(sql, "'1'")
    else
      sql <- paste(sql, "'0'")
    ans <- dbGetQuery(map@get_db(), sql)[[1]]
    ans
}

pmFeatures <- function(map, featureSetIds) {
    fsetIds <- paste("'", featureSetIds, "'", sep="", collapse=",")
    sql <- paste("select man_fsetid, pmfeature.fid, pmfeature.strand,
pmfeature.allele, pmfeature.x, pmfeature.y from
pmfeature, featureSet where man_fsetid in (",
                 fsetIds, ") and featureSet.fsetid = pmfeature.fsetid")
    dbGetQuery(map@get_db(), sql)
}

allFeatureSetIds <- function(map) {
    sqliteQuickColumn(map@get_db(), "featureSet", "man_fsetid")
}





