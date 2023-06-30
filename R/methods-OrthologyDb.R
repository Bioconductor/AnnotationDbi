## Code specifically for the Orthology.eg.db package
## the underlying DB only has two tables, one that has
## species -> taxonid mappings, and another that has
## taxon/gene -> othertaxon/gene orthology mappings
## we have to map the column and keytype to taxon ID
## and then make the query between taxa

.keyColMapper <- function(x, colOrKey){
    colOrKey <- toupper(sub("\\.", "_", colOrKey))
    taxonid <- dbGetQuery(dbconn(x), paste0("select taxid from names where name='", colOrKey, "';"))
    taxonid
}

## This part refactored in 11/2022.
## the genes table in the underlying DB was converted to have
## each row duplicated in reverse order, which makes lookups easier
## e.g., its now like this where the second row is the reverse map of the first
##
## taxid gene_id other_taxid other_gene_id
## 9606  100       10090       12345
## 10090 12345     9606        100
##
## What this gains us is quicker direct lookups
## e.g., we can get from human to mouse and vice versa
## by just doing a query on gene_id -> other_gene_id
##
## Also, there are any number of mappings that are like this:

##  taxid   gene_id other_taxid other_gene_id 
## 8030   100136351        9606          5894
## 9606    5894            7955        557109

## Where there isn't a direct mapping from 8030 -> 7955,
## but there is an implicit mapping from 8030 -> 9606 -> 7955
## By duplicating the rows we can make a performant lookup that
## includes any mapping via a single extra species

.straightQuery <- function(x, fromTax, toTax, keys){
    sql <- paste0("SELECT gene_id, other_gene_id FROM genes WHERE ",
                  "taxid = '", fromTax, "' AND other_taxid='", toTax, "' AND ",
                  "gene_id IN ('", paste(keys, collapse = "','"), "');")
    res <- dbGetQuery(dbconn(x), sql)
    res
}

.doubleQuery <- function(x, fromTax, toTax, keys){
    sql <- paste0("SELECT G1.gene_id, G2.other_gene_id FROM genes ",
                  "AS G1 INNER JOIN genes AS G2 ON G1.other_gene_id=G2.gene_id ",
                  "WHERE G1.taxid='", fromTax, "' AND G2.other_taxid='",
                  toTax, "' AND G1.gene_id IN ('", paste(keys, collapse = "','"),
                  "');")
    res <- dbGetQuery(dbconn(x), sql)
    res
}


## select function for Ontology.eg.db package
.selectOnto <- function(x, keys, columns, keytype, ...){
    fromTax <- .keyColMapper(x, keytype)
    toTax <- .keyColMapper(x, columns)
    res <- rbind(.straightQuery(x, fromTax, toTax, keys),
                 .doubleQuery(x, fromTax, toTax, keys))
    out <- data.frame(keys, rep(NA, length(keys)))
    names(out) <- c(keytype, columns)
    out[,2] <- res[match(out[,1], res[,1]),2]
    out
                      
}

.ontoKeys <- function(x, keytype) {
    tax <- .keyColMapper(x, keytype)
    sql <- paste0("SELECT gene_id FROM genes WHERE taxid='",
                  tax, "' UNION SELECT other_gene_id FROM ",
                  "genes WHERE other_taxid='", tax, "';")
    res <- dbGetQuery(dbconn(x), sql)[,1]
    res
}

.justFirstUpper <- function(string){
    stringlst <- strsplit(string, "")
    gsub("_", ".", do.call(c, lapply(stringlst, function(x) paste(c(x[1], tolower(x[-1])), collapse = ""))))
}

    
    
