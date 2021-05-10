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

.straightQuery <- function(x, fromTax, toTax, keys){
    sql <- paste0("SELECT gene_id, other_gene_id FROM genes WHERE taxid='",
                  fromTax, "' AND other_taxid='", toTax, "' AND gene_id IN ('",
                  paste(keys, collapse = "','"), "') UNION SELECT other_gene_id, gene_id ",
                  "FROM genes WHERE other_taxid='", fromTax, "' AND taxid='", toTax,
                  "' AND other_gene_id IN ('", paste(keys, collapse = "','"), "');")
    res <- dbGetQuery(dbconn(x), sql)
    res
}

## this part is fun. For any fish that ISN'T zebrafish, NCBI first maps orthologs
## from that fish to zebrafish. If a zebrafish gene ALSO has a human ortholog, it's
## then mapped to human, and the fish:zebrafish mapping is dropped. So we can have
## fish:human and fish:zebrafish. And for the former we need to map fish:human:zebrafish
.FISH_TAXA <- c(7757,7868,7897,7918,7950,7957,7962,7994,7998,8005,8010,8019,8022,8023,8030,
                8032,8036,8049,8078,8081,8083,8084,8090,8128,8153,8154,8167,8175,8187,8208,
                8255,27687,28743,29144,30732,41447,42514,43700,47969,48698,48699,48701,52670,
                54343,56716,56723,63155,64144,74940,75366,80966,80972,105023,106582,109280,
                113540,144197,150288,158456,173247,181472,205130,210632,215358,244447,259920,
                283305,299321,313518,307959,310915,375764,386614,433405,441366,586833,1234273,
                1608454,1676925,1841481)
## the preceding list was done by hand - probably eutils is the way to go...

.fishQuery <- function(x, fromTax, toTax, keys){
    sqlf2h <- paste0("SELECT gene_id, other_gene_id FROM genes WHERE taxid='",
                  fromTax, "' AND other_taxid='9606' AND gene_id IN ('",
                  paste(keys, collapse = "','"), "') UNION SELECT other_gene_id, gene_id ",
                  "FROM genes WHERE other_taxid='", fromTax, "' AND taxid='9606'",
                  " AND other_gene_id IN ('", paste(keys, collapse = "','"), "');")
    res1 <- dbGetQuery(dbconn(x), sqlf2h)
    sqlh2z <- paste0("SELECT gene_id, other_gene_id FROM genes WHERE taxid='9606'",
                  " AND other_taxid='7955' AND gene_id IN ('",
                  paste(res1[,2], collapse = "','"), "') UNION SELECT other_gene_id, gene_id ",
                  "FROM genes WHERE other_taxid='9606' AND taxid='7955'",
                  " AND other_gene_id IN ('", paste(res1[,2], collapse = "','"), "');")
    res2 <- dbGetQuery(dbconn(x), sqlh2z)
    res <- merge(res1, res2, by.x = 2, by.y = 1)[,-1]
    res
}


## select function for Ontology.eg.db package
.selectOnto <- function(x, keys, columns, keytype, ...){
    fromTax <- .keyColMapper(x, keytype)
    toTax <- .keyColMapper(x, columns)
    if(fromTax %in% .FISH_TAXA && toTax == "7955") {
        res <- .fishQuery(x, fromTax, toTax, keys)
        res <- rbind(res, .straightQuery(x, fromTax, toTax, keys))
    } else {
        res <- .straightQuery(x, fromTax, toTax, keys)
    }
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

    
    
