## Three helpers for deprecating keytypes
## One to just list the bum keytypes
.listDeprecatedKeytypes <- function(){
##    c('CHR','CHRLOC','CHRLOCEND') ## Uncomment after the release
}
## Another for keytypes to remove unwanted keytypes
.filterDeprecatedKeytypes <- function(keytypes){
    keytypes[!(keytypes %in% .listDeprecatedKeytypes())]
}
## And one for keys and select to warn if the user tries to use them
.checkForDeprecatedKeytype <- function(keytype){
    if(any(.listDeprecatedKeytypes() %in% keytype )){
        warning(wmsg(paste0(keytype,
    " is deprecated. ",
    "Please use an appropriate TxDb object or package for this kind of data.")))
    }
}

## Need an accessor for getting the central ID for a DB (when appropriate)
.getCentralID <- function(x){
  as.character(dbQuery(dbConn(x),
                       "SELECT value FROM metadata WHERE name='CENTRALID'"))
}

## Sometimes we need to translate a centralID into a central keytype.
.chooseCentralOrgPkgSymbol <- function(x){
  centralID <- .getCentralID(x)
  keytype <- switch(EXPR = centralID,
                    "EG" = "ENTREZID",
                    "TAIR" = "TAIR",
                    "ORF" = "ORF",
                    "GID" = "GID")
  keytype
}


##############################################################################
## Methods for mapping keytypes to table and fields


## For GO, I should just define a whole distinct set.

## For Chip packages, they should be nearly the same, but we either have to do
## some extra complexity to join across DBs OR I can just do one final merge
## in R.  I may want my own .generateQuery() function for chip packages, or I
## may just want my own .extractData() function(), and to point to a different
## "x" when using chip packages.

## ALSO: I will need a way to deduce the org package that goes with each chip
## package. hgu95av2ORGPKG will (for example) get you this.  DONE:
.getOrgPkg <- function(x){
  pkgname <- sub(".db$","", packageName(x))
  orgPkgName <- eval(parse(text=paste0(pkgname, "ORGPKG")))
  orgPkgName <- paste0(orgPkgName,".db")
  eval(parse(text=orgPkgName))
}

## This gets the exact path to the chip DB.
.getChipDbFile <- function(x){
  pkgname <- sub(".db$","", packageName(x))
  eval(call(paste0(pkgname, "_dbfile")))
}

## Limitation: I can only have ONE table and ONE field for each list name.
## So if we have fields like GO that should pull back multiple things, then we
## have to expand those ahead of time.
.expandCols <- function(cols){
  ## known expansions for cols:
  if("CHRLOC" %in% cols){ 
    after <- match("CHRLOC", cols)
    cols <- append(cols, c("CHRLOCCHR"),after) 
  }
  if("GO" %in% cols){ 
    after <- match("GO", cols)
    cols <- append(cols, c("EVIDENCE","ONTOLOGY"),after) 
  }
  if("GOALL" %in% cols){ 
    after <- match("GOALL", cols)
    cols <- append(cols, c("EVIDENCEALL","ONTOLOGYALL"),after) 
  }
  if("ORF" %in% cols){ 
    after <- match("ORF", cols)
    cols <- append(cols, c("SGD"),after) 
  }  
  if("COMMON" %in% cols){ 
    after <- match("COMMON", cols)
    cols <- append(cols, c("SGD"),after) 
  } ## special case: if PROBEIDS are requested, they MUST be in front!
  if("PROBEID" %in% cols){ 
    cols <- append("PROBEID", setdiff(cols, "PROBEID"))
  }
  unique(cols)
}


## ORG PKGS CHECKLIST: 
## org.Ag.eg.sqlite           ## done
## org.At.tair.sqlite         ## done
## org.Bt.eg.sqlite           ## done
## org.Ce.eg.sqlite           ## done
## org.Cf.eg.sqlite           ## done
## org.Dm.eg.sqlite           ## done
## org.Dr.eg.sqlite           ## done
## org.EcK12.eg.sqlite        ## done
## org.EcSakai.eg.sqlite      ## done
## org.Gg.eg.sqlite           ## done
## org.Hs.eg.sqlite           ## done
## org.Mm.eg.sqlite           ## done
## org.Mmu.eg.sqlite          ## done
## org.Pf.plasmo.sqlite       ## done
## org.Pt.eg.sqlite           ## done
## org.Rn.eg.sqlite           ## done
## org.Sc.sgd.sqlite          ## done
## org.Ss.eg.sqlite           ## done
## org.Xl.eg.sqlite           ## done
## none of the above..        ## done



.defineTables <- function(x){
  class <- class(x)
  species <- species(x)
  .definePossibleTables(class, species)
}

.definePossibleTables <- function(class, species){  
  ## 1st the generic/universal things
  .defTables <- list("ENTREZID" = c("genes","gene_id"),
                     "PFAM" = c("pfam","pfam_id"),
                     "IPI" = c("pfam","ipi_id"),
                     "PROSITE" = c("prosite","prosite_id"),
                     "ACCNUM" = c("accessions","accession"),
                     "ALIAS" = c("alias","alias_symbol"),
                     "ALIAS2EG" = c("alias","alias_symbol"),
                     "ALIAS2PROBE" = c("alias","alias_symbol"),
                     "CHR" = c("chromosomes","chromosome"),
                     "CHRLOCCHR" = c("chromosome_locations","seqname"),
                     "CHRLOC" = c("chromosome_locations","start_location"),
                     "CHRLOCEND" = c("chromosome_locations","end_location"),
                     "ENZYME" = c("ec","ec_number"),
                     "MAP" = c("cytogenetic_locations","cytogenetic_location"),
                     "PATH" = c("kegg","path_id"),
                     "PMID" = c("pubmed","pubmed_id"),
                     "REFSEQ" = c("refseq","accession"),
                     "SYMBOL" = c("gene_info","symbol"),
                     "UNIGENE" = c("unigene","unigene_id"),
                     "ENSEMBL" = c("ensembl","ensembl_id"),
                     "ENSEMBLPROT" = c("ensembl_prot","prot_id"),
                     "ENSEMBLTRANS" = c("ensembl_trans","trans_id"),
                     "GENENAME" = c("gene_info","gene_name"),
                     "UNIPROT" = c("uniprot","uniprot_id"),
                     "GO" = c("go","go_id"),
                     "EVIDENCE" = c("go","evidence"),
                     "ONTOLOGY" = c("go","ontology"),
                     "GOALL" = c("go_all","go_id"),
                     "EVIDENCEALL" = c("go_all","evidence"),
                     "ONTOLOGYALL" = c("go_all","ontology")                     
                     )
  ## exceptions for ALL OrgDbs 
  if(class=="OrgDb"){
    ## I should probably remove ucsckg from select...
    #.defTables <- c(.defTables, list("UCSCKG" = c("ucsc","ucsc_id")) )
    .defTables <- .defTables[!(names(.defTables) %in% c("ALIAS2PROBE"))]
  }
  ## exceptions for ALL ChipDbs
  if(class=="ChipDb"){
    .defTables <- c(.defTables, list("PROBEID" = c("c.probes","probe_id")) )
  }

  ## species specific exceptions
  if(species=="Anopheles gambiae"){
    ## drop unsupported mappings
    .defTables <- .defTables[!(names(.defTables) %in% c("ALIAS",
                                                        "ALIAS2EG",
                                                        "ALIAS2PROBE",
                                                        "MAP",
                                                        "CHRLOC",
                                                        "CHRLOCEND",
                                                        "CHRLOCCHR",
                                                        "PFAM",
                                                        "IPI",
                                                        "PROSITE") )]
  }
  if(species=="Arabidopsis thaliana"){
    ## add these
    .defTables <- c(.defTables, list("TAIR" = c("genes","gene_id"),
                                     "ARACYC" = c("aracyc","pathway_name"),
                                     "ARACYCENZYME" = c("enzyme","ec_name")))
    ## remove these:
    .defTables <- .defTables[!(names(.defTables) %in% c("ACCNUM",
                                                        "ALIAS",
                                                        "ALIAS2EG",
                                                        "ALIAS2PROBE",
                                                        "MAP",
                                                        "UNIGENE",
                                                        "PFAM",
                                                        "IPI",
                                                        "PROSITE",
                                                        "ENSEMBL",
                                                        "ENSEMBLPROT",
                                                        "ENSEMBLTRANS",
                                                        "UNIPROT",
                                                        "ENTREZID",
                                                        "CHR") )]
    ## "re-add" these (redefine, so must have been removed in prior step)
    .defTables <- c(.defTables, list("ENTREZID" = c("entrez_genes","gene_id"),
                                     "CHR"=c("gene_info","chromosome") ))
  }
  if(species=="Bos taurus"){
    .defTables <- .defTables[!(names(.defTables) %in% c("MAP") )]
  }
  if(species=="Caenorhabditis elegans"){
    .defTables <- c(.defTables, list("WORMBASE" = c("wormbase","wormbase_id")))
    .defTables <- .defTables[!(names(.defTables) %in% c("MAP",
                                                        "PFAM",
                                                        "IPI",
                                                        "PROSITE") )]
  }
  if(species=="Canis familiaris"){
    .defTables <- .defTables[!(names(.defTables) %in% c("MAP",
                                                        "PFAM",
                                                        "IPI",
                                                        "PROSITE") )]
  }
  if(species=="Drosophila melanogaster"){
    .defTables <- c(.defTables, list("FLYBASE" = c("flybase","flybase_id"),
                                  "FLYBASECG" = c("flybase_cg","flybase_cg_id"),
                                  "FLYBASEPROT" = c("flybase_prot","prot_id")))
    .defTables <- .defTables[!(names(.defTables) %in% c("PFAM",
                                                        "IPI",
                                                        "PROSITE") )]
  }
  if(species=="Danio rerio"){
    .defTables <- c(.defTables, list("ZFIN" = c("zfin","zfin_id")))
    .defTables <- .defTables[!(names(.defTables) %in% c("MAP"))]
  }
  if(species=="Escherichia coli"){
    .defTables <- .defTables[!(names(.defTables) %in% c("CHR",
                                                        "MAP",
                                                        "UNIGENE",
                                                        "CHRLOC",
                                                        "CHRLOCEND",
                                                        "CHRLOCCHR",
                                                        "PFAM",
                                                        "IPI",
                                                        "PROSITE",
                                                        "ENSEMBL",
                                                        "ENSEMBLPROT",
                                                        "ENSEMBLTRANS",
                                                        "UNIPROT"))]
  }
  if(species=="Gallus gallus"){
    .defTables <- .defTables[!(names(.defTables) %in% c("MAP"))]
  }
  if(species=="Homo sapiens"){
    .defTables <- c(.defTables, list("OMIM" = c("omim","omim_id"),
                                     "UCSCKG" = c("ucsc","ucsc_id")) )
  }
  if(species=="Mus musculus"){
    .defTables <- c(.defTables, list("MGI" = c("mgi","mgi_id")) )#,
                                    # "UCSCKG" = c("ucsc","ucsc_id")) )
    .defTables <- .defTables[!(names(.defTables) %in% c("MAP") )]
  }
  if(species=="Macaca mulatta"){
    .defTables <- .defTables[!(names(.defTables) %in% c("ALIAS",
                                                        "ALIAS2EG",
                                                        "ALIAS2PROBE",
                                                        "MAP",
                                                        "UNIGENE",
                                                        "PFAM",
                                                        "IPI",
                                                        "PROSITE"))]
  }
  if(species=="Plasmodium falciparum"){
    .defTables <- c(.defTables, list(
      "ORF" = c("genes","gene_id") ))
    .defTables <- .defTables[!(names(.defTables) %in% c("ENTREZID",
                                                        "ACCNUM",
                                                        "ALIAS",
                                                        "ALIAS2PROBE",
                                                        "ALIAS2EG",
                                                        "CHR",
                                                        "CHRLOC",
                                                        "CHRLOCEND",
                                                        "CHRLOCCHR",
                                                        "MAP",
                                                        "PMID",
                                                        "REFSEQ",
                                                        "UNIGENE",
                                                        "PFAM",
                                                        "IPI",
                                                        "PROSITE",
                                                        "ENSEMBL",
                                                        "ENSEMBLPROT",
                                                        "ENSEMBLTRANS",
                                                        "UNIPROT") )]
    .defTables <- c(.defTables, list("ALIAS2ORF" = c("alias","alias_symbol") ))
  }
  if(species=="Pan troglodytes"){
    .defTables <- .defTables[!(names(.defTables) %in% c("ALIAS",
                                                        "ALIAS2PROBE",
                                                        "ALIAS2EG",
                                                        "MAP",
                                                        "UNIGENE",
                                                        "PFAM",
                                                        "IPI",
                                                        "PROSITE") )]
  }
  if(species=="Rattus norvegicus"){
    #.defTables <- .defTables ## no changes (for now)
    .defTables <- .defTables[!(names(.defTables) %in% c("MAP") )]
  }  
  if(species=="Saccharomyces cerevisiae"){
    .defTables <- c(.defTables, list(       
      "ORF" = c("gene2systematic","systematic_name"),
      "DESCRIPTION" = c("chromosome_features","feature_description"),
      "COMMON" = c("gene2systematic","gene_name"),
      "INTERPRO" = c("interpro","interpro_id"),
      "SMART" = c("smart","smart_id"),
      "SGD" = c("sgd","sgd_id") ))
    .defTables <- .defTables[!(names(.defTables) %in% c("ACCNUM",
                                                        "MAP",
                                                        "SYMBOL",
                                                        "UNIGENE",
                                                        "PROSITE",
                                                        "ALIAS",
                                                        "ALIAS2EG",
                                                        "ALIAS2PROBE",
                                                        "CHRLOC",
                                                        "CHRLOCEND",
                                                        "CHRLOCCHR",
                                                        "GENENAME",
                                                        "IPI",
                                                        "CHR") )]
    .defTables <- c(.defTables, list("ALIAS" = c("gene2alias","alias"),
                           "CHRLOC" = c("chromosome_features","start"),
                           "CHRLOCEND" = c("chromosome_features","stop"),
                           "CHRLOCCHR" = c("chromosome_features","chromosome"),
                           "GENENAME" = c("sgd","gene_name"),
                           "CHR" = c("chromosome_features","chromosome") ))
  }
  if(species=="Sus scrofa"){
    .defTables <- .defTables[!(names(.defTables) %in% c("MAP",
                                                        "CHRLOC",
                                                        "CHRLOCEND",
                                                        "CHRLOCCHR",
                                                        "PFAM",
                                                        "IPI",
                                                        "PROSITE",
                                                        "ENSEMBL",
                                                        "ENSEMBLPROT",
                                                        "ENSEMBLTRANS") )]
  }
  if(species=="Xenopus laevis"){
    .defTables <- .defTables[!(names(.defTables) %in% c("ALIAS",
                                                        "ALIAS2PROBE",
                                                        "ALIAS2EG",
                                                        "MAP",
                                                        "CHRLOC",
                                                        "CHRLOCEND",
                                                        "CHRLOCCHR",
                                                        "PFAM",
                                                        "IPI",
                                                        "PROSITE",
                                                        "ENSEMBL",
                                                        "ENSEMBLPROT",
                                                        "ENSEMBLTRANS") )]
  }

  stockSpecies <- c("Anopheles gambiae",
                    "Arabidopsis thaliana",
                    "Bos taurus",
                    "Caenorhabditis elegans",
                    "Canis familiaris",
                    "Drosophila melanogaster",
                    "Danio rerio",
                    "Escherichia coli",
                    "Gallus gallus",
                    "Homo sapiens",
                    "Mus musculus",
                    "Macaca mulatta",
                    "Plasmodium falciparum",
                    "Pan troglodytes",
                    "Rattus norvegicus",
                    "Saccharomyces cerevisiae",
                    "Sus scrofa",
                    "Xenopus laevis")
  
  if(!(species %in% stockSpecies)){
    ## What follows is a very optimistic list!
  .defTables <- list("ENTREZID" = c("genes","gene_id"),
                     "ACCNUM" = c("accessions","accession"),
                     "ALIAS" = c("alias","alias_symbol"),
                     "ALIAS2EG" = c("alias","alias_symbol"),
                     "ALIAS2PROBE" = c("alias","alias_symbol"),
                     "CHR" = c("chromosomes","chromosome"),
                     "PMID" = c("pubmed","pubmed_id"),
                     "REFSEQ" = c("refseq","accession"),
                     "SYMBOL" = c("gene_info","symbol"),
                     "UNIGENE" = c("unigene","unigene_id"),
                     "GENENAME" = c("gene_info","gene_name"),
                     "GO" = c("go","go_id"),
                     "EVIDENCE" = c("go","evidence"),
                     "ONTOLOGY" = c("go","ontology")
                     )
  }

  
  ## ultimately I think I need GO.db to have it's OWN select methods.
  ## it's just too many responsibilities for this select to also handle the
  ## somewhat obscure GO database schema.  So many things will not really work
  ## untill I do that change (ancestors etc.)
  if(class=="GODb"){
    .defTables <- list("GOID" = c("go_term","go_id"),
                          "TERM" = c("go_term","term"),
                          "ONTOLOGY" = c("go_term","ontology"),
                          "DEFINITION" = c("go_term","definition")
                    ## "BPPARENTS" = c("go_bp_parents","_parent_id"),
                    ## "CCPARENTS" = c("go_cc_parents","_parent_id"),
                    ## "MFPARENTS" = c("go_mf_parents","_parent_id"),
                    ## "BPANCESTOR" = c("",""),
                    ## "CCANCESTOR" = c("",""),
                    ## "MFANCESTOR" = c("",""),
                    ## "OBSOLETE" = c("",""),
                    ## "SYNONYM" = c("go_synonym","synonym"),
                    ## "BPCHILDREN" = c("",""),
                    ## "CCCHILDREN" = c("",""),
                    ## "MFCHILDREN" = c("",""),
                    ## "BPOFFSPRING" = c("go_bp_offspring","_offspring_id"),
                    ## "CCOFFSPRING" = c("go_cc_offspring","_offspring_id"),
                    ## "MFOFFSPRING" = c("go_mf_offspring","_offspring_id")
                       )
  }

  
  ## then return with this result
  .defTables
}


## helper for getting out the table OR the field from .defineTables
.getDBLoc <- function(x, col, value="table"){
  res <- .defineTables(x)
  if(col %in% names(res)){
    res <- res[[col]]
  } else {
      stop("'col' value '", col, "' is not defined")
  }
  ## Then test and return appropriate records.
  if(value=="table"){
    res <- res[1]
  }else if(value=="field"){
    res <- res[2]
  }else if(value=="full.field"){
    res <- paste(res[1],res[2],sep=".")
  }
  res
}


## vectorized version of .getDBLoc
.getDBLocs <- function(x, cols, value="table"){
  res <- character(length(cols))
  for(i in seq_len(length(cols))){
      res[i] <- .getDBLoc(x, cols[i], value=value)
  }
  names(res) <- cols 
  unique(res)
}


.getFullyQualifiedDBLocs <- function(x, cols){
  tables <- .getDBLocs(x, cols, value="table")
  fields <- .getDBLocs(x, cols, value="field")
  paste(tables, fields, sep=".")
}


## x is the org package object, y is the chip package object. 
.attachDB <- function(x,y){
  chipDb <- .getChipDbFile(y)
  chipSQL <- paste0("ATTACH '",chipDb,"' AS c")
  #message(chipSQL)
  dbQuery(dbConn(x), chipSQL)
}




.generateQuery <- function(x, cols, keytype, keys){
  ## If it is a chip package, get the org package instead
  if(class(x)=="ChipDb"){
    y <- x
    ## then flip to using the org package, and actually attach to that.
    x <- .getOrgPkg(x)
    try(.attachDB(x,y), silent=TRUE) ## not really a disaster if we fail here
    ## because we have a "y" defined, we have to define the dblocs this way:
    dblocs <- .getDBLocs(y, cols)
    ## Fully qualified keytype and species are set up like this
    fullKeytype <- .getFullyQualifiedDBLocs(y, keytype)
    species <- species(x)
    ## if we have c.probes in dblocs, then we MUST join to genes table
    if("c.probes" %in% dblocs && species(x)!="Saccharomyces cerevisiae"){
      dblocs <- unique(append(dblocs, c("genes"), match("c.probes", dblocs)))
    }
    if("c.probes" %in% dblocs && species(x)=="Saccharomyces cerevisiae"){
      dblocs <- unique(append(dblocs, c("sgd"), match("c.probes", dblocs)))
    }
  }else{ ## this means there is only an org pkg...
    dblocs <- .getDBLocs(x, cols)
    fullKeytype <- .getFullyQualifiedDBLocs(x, keytype)
    species <- species(x)
  }
  
  ## Get fields
  if(exists("y", inherits=FALSE)){ ## IOW if it was a chip package at the top...
    fields <- paste(.getDBLocs(y,cols,value="full.field"), collapse=",")
  }else{
    fields <- paste(.getDBLocs(x,cols,value="full.field"), collapse=",")
  }

  
  #message(paste(dblocs,collapse=","))
  ## then make the 1st part of the query.
  for(i in seq_len(length(dblocs))){
    if(i==1){
      res <- paste("SELECT ",fields," FROM",dblocs[i])
    }else{
##       if(species=="Saccharomyces cerevisiae" &&
##          (dblocs[i]=="gene2systematic" || dblocs[i-1]=="gene2systematic")){
##           join <- "systematic_name"
##       }else
      if(dblocs[i]=="c.probes" || dblocs[i-1]=="c.probes"){
         ## IOW if joining to OR from c.probes we want "gene_id"
        if(species=="Saccharomyces cerevisiae"){
          join <- "systematic_name"
        }else{
          join <- "gene_id"
        }
      }else{
        join <- "_id"
      }
      res <- c(res, paste("LEFT JOIN ",dblocs[i],"USING (",join,")"))
    }
  }
  res <- paste(res, collapse=" ")
  ## then use the keytype and keys to append the WHERE clause
  strKeys <- paste0('"',keys,'"',collapse=",")
  where <- paste("WHERE ",fullKeytype,"in (",strKeys,")" )
  paste(res, where)
}

## usage:
## library(org.Hs.eg.db)
## cols = c("ENTREZID","SYMBOL","PFAM")
## keytype = "ALIAS"
## keys = "ITGA7"
## x = org.Hs.eg.db
## AnnotationDbi:::.generateQuery(x, cols, keytype, keys)

## cols = c("ENTREZID","SYMBOL","PFAM")
## keytype = "ENTREZID"; keys = head(keys(org.Hs.eg.db)); x = org.Hs.eg.db
## AnnotationDbi:::.generateQuery(x, cols, keytype, keys)
## AnnotationDbi:::.extractData(x, cols, keytype, keys)
## select(org.Hs.eg.db,keys, cols, "ENTREZID")

## library(hgu95av2.db); x = hgu95av2.db;cols = c("ENTREZID","SYMBOL","PFAM")
## keytype = "PROBEID"; keys = head(keys(hgu95av2.db))
## AnnotationDbi:::.generateQuery(x, cols, keytype, keys) ## OK
## AnnotationDbi:::.extractData(x, cols, keytype, keys)
## select(hgu95av2.db,keys, cols, keytype)

## cols = c("ENTREZID", "SYMBOL", "CHRLOC")
## AnnotationDbi:::.extractData(x, cols, keytype, keys)
## select(hgu95av2.db,keys, cols, keytype)

## cols = c("ENTREZID", "SYMBOL", "GO")
## AnnotationDbi:::.extractData(x, cols, keytype, keys)
## select(hgu95av2.db,keys, cols, keytype)

## GO.db example attempt:
## library(GO.db);x<- GO.db;keys<-head(keys(GO.db)); cols = c("ONTOLOGY", "DEFINITION", "TERM"); keytype="GOID"
## AnnotationDbi:::.extractData(x, cols, keytype, keys)
## select(hgu95av2.db,keys, cols, keytype)


## I also need a method to call my generated sql and get the data.
## NOTE: the order that cols come back in is determined by the DB.
## Some resorting is done downstream.
.extractData <- function(x, cols, keytype, keys){
  ## Take the cols, append the keytype to FRONT
  cols <- unique(c(keytype, cols))
  ## do any necessary col expansion:
  cols <- unique(.expandCols(cols))
  ## generate the query
  sql <- .generateQuery(x, cols, keytype, keys)
  #message(sql)
  ## get field names for relevant cols
  cols <- unique(c(keytype, cols))
  headerTables <- .getDBLocs(x, cols, value="field")
  if(class(x)=="ChipDb"){
    y <- x ## save for test below
    x <- .getOrgPkg(x) ## then flip to using the org package
  }
  res <- dbQuery(dbConn(x), sql)
  ## then cleanup by doing a detach:
  if(exists("y", inherits=FALSE)){ ## I should not have to use inherits=FALSE?
    dbQuery(dbConn(x), "DETACH DATABASE c")
  }
  ## then subset to only relevant cols
  res[,(colnames(res) %in% headerTables)]
}


## usage:
## .extractData(x, cols, keytype, keys)



## helper used for dropping out ugly redundant col names.
## (BIMAP FREE!)
.simplifyCols <- function(x, cols){
  blackList <- c(ALIAS="ALIAS2PROBE",
                 ALIAS="ALIAS2EG",
                 CHR="CHRLOCCHR")
  idx <- !(cols %in% blackList)
  unique(cols[idx])
}



.filterSuffixes <- function(tab){
  ## clean up .x and .y extensions?
  colnames(tab) <- gsub("\\.x","",colnames(tab))
  colnames(tab) <- gsub("\\.y","",colnames(tab))
  ## clean up .1's
  colnames(tab) <- gsub("\\.1","",colnames(tab))  
  tab
}



###############################################################################
## Helpers for tidying up the final table.
## .resort drops unwanted rows, rearanges cols and puts things into order that
## the keys were initially

## drop rows that don't match
.dropUnwantedRows <- function(tab, keys, jointype) {
    ## drop duplicated or 'all NA' (other than jointype) rows
    ntest <- ncol(tab) - sum(colnames(tab) == jointype)
    idx <- duplicated(tab) | (rowSums(is.na(tab)) == ntest)
    tab <- tab[!idx,, drop=FALSE]
    ## add back rows for keys that were completely removed
    noMatchKeys <- unique(keys[!keys %in% tab[[jointype]]])
    if (n <- length(noMatchKeys)) {
        ridx <- nrow(tab) + seq.int(n)
        cidx <- colnames(tab) %in% jointype
        tab[ridx, cidx] <- noMatchKeys
    }
    ## place rows in order of first appearance of key
    idx <- order(match(tab[[jointype]], keys))
    tab <- tab[idx,, drop=FALSE]
    rownames(tab) <- NULL
    tab
}

## resort the Column Names
.resortColumns <- function(tab, jointype, reqCols) {
    tab <- .filterSuffixes(tab) ## Removes duplicate suffixes
    if (!all(colnames(tab) %in% reqCols))
        stop("[internal] some of 'reqCols' are not in 'tab'")
    cnames <- c(jointype, reqCols[!reqCols %in% jointype])
    tab[, cnames, drop=FALSE]
}

## Create extra rows
.generateExtraRows <- function(tab, keys, jointype) {
    ## 4 possibilities
    ## if there are not dups, then we skip this function.
    ## if(any(duplicated(keys)) ## expand the keys
    ## if(any(duplicated(tab[[jointype]]))) ## expand the table...
    ## AND if they are BOTH redundant how do I decide which row to expand?
    ## I think that I have to throw a warning and NOT do this step in that case?
    keyTest <- any(duplicated(keys))
    rowTest <-  any(duplicated(tab[[jointype]]))         
    if (keyTest && !rowTest) { ## Need to account for row dups
        ind = match(keys, tab[[jointype]])
        tab <- tab[ind,,drop=FALSE]
        rownames(tab) <- NULL
    } else if (!keyTest && rowTest) {
        txt <- "'select' resulted in 1:many mapping between keys and
                return rows"
        warning(paste(strwrap(txt), collapse="\n"))
    } else if (keyTest && rowTest) { ## User will get data "as is"
        txt <- "'select' and duplicate query keys resulted in 1:many
                mapping between keys and return rows"
        warning(paste(strwrap(txt), collapse="\n"))
    }
    tab
}

## .resort is the main function for cleaning up a table so that results look
## formatted the way we want them to.
.resort <- function(tab, keys, jointype, reqCols) {
    if (jointype %in% colnames(tab)) {
        tab <- .dropUnwantedRows(tab, keys, jointype)
        ## rearrange to make sure cols are in correct order
        tab <- .resortColumns(tab, jointype, reqCols)
    }
    ## Duplicate any rows as appropriate (based on those keys)
    .generateExtraRows(tab, keys, jointype)
}



## helper so that we can be ready for when there are multiple things getting
## duplicated...
## The current version of these helper functions are not "smart", but it could
## be made so if needed.  The reason why not smart is because it will be a lot
## faster if we can get away with it being "dumb" and not looking up the types
## of values from the DB for each type.
.replaceValues <- function(dups, fieldNames, expectedCols){
  newVals <- expectedCols[fieldNames %in% dups]
  after <- match(dups, fieldNames) - 1 ## -1 b/c we aim to replac: not follow
  cols <- append(fieldNames, newVals, after)
  ## then remove the dups values
  cols[!(cols %in% dups)]
}


## helper for ambiguous/duplicated columns
.adjustForDupColNames <- function(res, expectedCols){
  fieldNames <- colnames(res)
  ## get duplicated vals
  dups <- fieldNames[duplicated(fieldNames)]
  ## for each value of dups, we want to call .replaceValues
  for(i in seq_len(length(dups))){
    if(i==1){
      cols <- .replaceValues(dups[i], fieldNames, expectedCols)
    }else{
      cols <- .replaceValues(dups[i], cols, expectedCols)
    }
  }
  cols
}


## helper looks at cols and trys to warn users about many:1 relationships
.warnAboutManyToOneRelationships <- function(cols){
    file=system.file("extdata","manyToOneBlackList.Rda",package="AnnotationDbi")
    blackList <- get(load(file))
    problemCols <- cols[cols %in% blackList]
    ## give message if there are more than 4 (for now)
    if(length(problemCols) > 4){
        msg = paste("You have selected the following columns that can have a many to one relationship with the primary key: ", paste(problemCols,collapse=", "),". Because you have selected more than a few such columns there is a risk that this selection may balloon up into a very large result as the number of rows returned multiplies accordingly. To experience smaller/more manageable results and faster retrieval times, you might want to consider selecting these columns separately.") 
        warning(paste(strwrap(msg, exdent=2), collapse="\n"),
                immediate.=TRUE, call.=FALSE)
    }
}


## the core of the select method for GO org and chip packages.
.legacySelect <- function(x, keys=NULL, cols=NULL, keytype, jointype) {
  ## if asked for what they have, just return that.
  if(all(cols %in% keytype)  && length(cols)==1){
    res <- data.frame(keys=keys)
    colnames(res) <- cols
    return(res)
  }
  if(is.null(keys)) keys <- keys(x) ## if no keys provided: use them all
  if(is.null(cols)) cols <- columns(x) ## if no cols provided: use them all
  
  ## call .simplifyCols to ensure we use same colnames as columns()
  cols <- .simplifyCols(x, cols)
  ## keytype <- .swapSymbolExceptions(x, keytype)
  keytype <- .simplifyCols(x, keytype)
  ## oriCols is a snapshot of col requests needed for column filter below
  oriCols <- unique(c(keytype, cols))

  ## keys should NOT be NAs, but if they are, warn and then filter them.
  if (any(is.na(keys))) {
      warning("'NA' keys have been removed")
      keys <- keys[!is.na(keys)]
  }

  ## Check if the user is selecting too many cols with many:1 relationships
  .warnAboutManyToOneRelationships(cols)

  ## Generate query and extract the data
  res <- .extractData(x, cols=cols, keytype=keytype, keys=keys)
  
  ## these are the colnames we need to have gotten back from the DB
  expectedCols <- .expandCols(oriCols)
  oriTabCols <- .getDBLocs(x, expectedCols, value="field")
  
  ## I need to know the jointype...
  jointype <- .getDBLocs(x, keytype, value="field")

  ## Remove suffixes in case there were dups
  res <- .filterSuffixes(res)

  
  ## If we can, then we should re-arrange to make sure cols come back in same
  ## order as they asked for initially.  Expanded cols cannot be re-arranged.
  if(all(expectedCols %in%  oriCols) &&
     any(oriCols != expectedCols) ){
    ## We need to make it so that oriTabCols is in the SAME order as oriCols
    oriTabCols <- .getDBLocs(x, oriCols, value="field")
    ## then we need to make expectedCols to match oriCols
    expectedCols <- oriCols
  }

  
  ## Then if any suffixes were actually removed, it means there were duplicated
  ## cols.  Duplicated cols means I have to do some label swapping.
  if( length(oriTabCols) < length(colnames(res))){
    oriTabCols <- .adjustForDupColNames(res, expectedCols) ## BADNESS!
    colnames(res) <- .adjustForDupColNames(res, expectedCols)
  }
  
  
  ## .resort will resort the rows relative to the jointype etc.
  if(dim(res)[1]>0){
    res <- .resort(res, keys, jointype, oriTabCols)
  }

  colnames(res) <- expectedCols[match(colnames(res), oriTabCols)]
  
  rownames(res) <- NULL
  res
}


## This just needs to generate a simple query and then return the
## results with no _id's
## A simple strategy will work EXCEPT for the weird case where I need
## to pull out multiple fields at once. (like GO)

## Do I need to pay attention to the order??? - it seems that I don't...
.appendGenesToTabs <- function(tabs){
    names <- names(tabs)
    tabs <- c("genes",tabs)
    names(tabs) <- c("GID", names)
    tabs
}

.noSchemaSelect <- function(x, keys=NULL, cols=NULL, keytype){

    ## 1st pool all the fields we need to extract
    fields <- unique(c(cols, keytype))
    ## Then get the tables to go with each one.
    tabs <- sapply(fields, .deriveTableNameFromField, x=x)
    ## make fully qualified fields of these tabs (the ones we want to extract)
    f.fields <- paste(tabs, fields, sep=".")

    ## if it's a Chip package, attach and point to org package for x
    if(class(x)=="ChipDb"){
        y <- x
        ## then flip to using the org package, and actually attach to that.
        x <- .getOrgPkg(x)
        try(.attachDB(x,y), silent=TRUE) ## not a disaster if we fail        
        ## Also make sure we include the genes table (only needed for the join)
        if(!("genes" %in% tabs)) tabs <- .appendGenesToTabs(tabs)
    }
    
    ## Make non-redundant list of tables to visit
    nrTabs <- unique(tabs)
    ## Now join to each table
    for(i in seq_along(nrTabs)){
        if(i==1){
            sql <- paste("SELECT ",paste(f.fields, collapse=","),
                         " FROM",tabs[1])
        }else{
            ## IF we see c.probes in nrTabs[i], it means we have to
            ## use gene_id instead.
            if("c.probes" %in% nrTabs[i]){
                sql <- c(sql, paste("LEFT JOIN ",nrTabs[i],"USING (GID)"))
            }else{
                sql <- c(sql, paste("LEFT JOIN ",nrTabs[i],"USING (_id)"))
            }
        }
    }
    sql <- paste(sql, collapse=" ")
    ## add the where clause
    strKeys <- paste0('"',keys,'"',collapse=",")
    fullKeytype <- tabs[names(tabs)==keytype]
    fullKeytype <- paste(fullKeytype, names(fullKeytype), sep=".") 
    where <- paste("WHERE ",fullKeytype,"in (",strKeys,")" )
    sql <- paste(sql, where)
    ## then call that
    res <- dbQuery(dbConn(x), sql)    
    ## cleanup and re-organize
    .resort(res, keys, jointype=keytype, fields)
}

###############################################################################
## HELPERS for argument validation {in select() and keys()}

.isSingleString <- function(x){
  is.atomic(x) && length(x) == 1L && is.character(x)
}

.testForValidKeytype <- function(x, keytype){
  if(!.isSingleString(keytype)){
      stop("'keytype' must be a a single string")
  }
  ## deprecated keytypes are still technically 'valid'
  pkts <- c(keytypes(x), .listDeprecatedKeytypes())
  if(!(keytype %in% pkts)){
      msg <- paste0("Invalid keytype: ",keytype,". Please use the keytypes method to see a listing of valid arguments.")
      stop(msg)
  }
}

.testForValidCols <- function(x, cols){
  if (!is.character(cols)){
      stop("'columns' must be a character vector")
  }
  pcols <- columns(x)
  if(!all(cols %in% pcols) && !is.null(cols)){
      badCols <- cols[!(cols %in% pcols)]
      msg <- paste0("Invalid columns: ",paste(badCols, collapse=","),". Please use the columns method to see a listing of valid arguments.")
      stop(msg)
  }
}

.testForValidKeys <- function(x, keys, keytype){
  if (!is.character(keys)){
      stop("'keys' must be a character vector")
  }
  ktKeys <- keys(x, keytype)
  if(!(any(ktKeys %in% keys))){
      msg <- paste0("None of the keys entered are valid keys for '",keytype,
         "'. Please use the keys method to see a listing of valid arguments.")
      stop(msg)
  }
}

.testSelectArgs <- function(x, keys, cols, keytype){
    .testForValidKeytype(x, keytype)
    .testForValidCols(x, cols)
    .testForValidKeys(x, keys, keytype)
}


## general select function
.select <- function(x, keys=NULL, cols=NULL, keytype, jointype){
    ## Some argument checking
    .testSelectArgs(x, keys=keys, cols=cols, keytype=keytype)
    ## Now get the schema
    schema <- metadata(x)[metadata(x)$name=="DBSCHEMA",]$value
    if(schema=="NOSCHEMA_DB" || schema=="NOCHIPSCHEMA_DB"){
        .noSchemaSelect(x, keys, cols, keytype)
    }else{
        .legacySelect(x, keys, cols, keytype, jointype)
    }
}




## Helper for setting the jointype to an appropriate default
.chooseJoinType  <- function(x){
  if(.getCentralID(x) == "ORF"){
    jointype <- "systematic_name"
  }else{
    jointype <- "gene_id"
  }
  jointype
}


#######################################################################
## So an issue was that keytype was not defined carefully enough and
## was getting substituted when cols arg was used in conjunction with
## no keytype specified...  So when there is not keytype, the user
## must tell us which one it is OR we have to somehow "know"

## TO FIX: I need to guess the keytype and pass it along as some other
## name into ...
## Hack: I will pass it in as kt
## And halfway through this hack, it seems that if there is an
## argument passed in AFTER your other ones that the problem I was
## having magically clears up???  WTH?


setMethod("select", "OrgDb",
          function(x, keys, columns, keytype, ...) {
##               if (missing(keytype)){
##                   keytype <- .chooseCentralOrgPkgSymbol(x)
##               }
              kt <- .chooseCentralOrgPkgSymbol(x)
              jointype <- .chooseJoinType(x)

              .selectWarnJT(x, keys, columns, keytype, jointype=jointype,
                            kt=kt, ...)
              ## put back following line after 2.13 releases
 ##              .select(x, keys, columns, keytype, jointype=jointype)
          }
)

setMethod("select", "ChipDb",
    function(x, keys, columns, keytype, ...){
##         if (missing(keytype)) keytype <- "PROBEID"
        kt <- "PROBEID"
        .selectWarnJT(x, keys, columns, keytype, jointype="probe_id",
                      kt=kt, ...)
        ## put back following line after 2.13 releases
        ##           .select(x, keys, columns, keytype, jointype="probe_id")
    }
)

setMethod("select", "GODb",
    function(x, keys, columns, keytype, ...){
          if (missing(keytype)) keytype <- "GOID"
          .selectWarnJT(x, keys, columns, keytype, jointype="go_id", ...)
          ## put back following line after 2.13 releases
##           .select(x, keys, columns, keytype, jointype="go_id")
        }
)






#############################
## Internally we want to reconstruct these guys so we can merge() on them
## c <- columns(GO.db)[7]
## prefix = "GO"
## foo = paste0(prefix,c)
## bar = eval(parse(text=foo))



##############################################################################
## cols methods return the list of things that users can ask for.  This can be
## just the table names, or it might be a list of mappings


.legacyCols <- function(x, baseType){
  ## cols <- .makeColAbbrs(x)
  cols <- names(.defineTables(x))
  if(!missing(baseType)){
    cols <- c(baseType, cols)
  }
  ## translate relevant short bimap names to "cute" names
  ## cols <- .swapSymbolExceptions(x, cols) 
  cols <- .simplifyCols(x, cols)
  
  ## .cols does not care about your names
  names(cols) <- NULL
  unique(cols)
}

## These helpers will go into the DB and extract col values for newer
## NOSCHEMA_DB's

.getDataTables <- function(con){
  tables <- dbListTables(con)
  tables[!tables %in% c("metadata","map_metadata","map_counts")]
}

.noSchemaCols <- function(x){
  if(class(x)=="ChipDb"){
      y <- x ## The old switcheroo
      x <- .getOrgPkg(x)  
  }
  con <- dbConn(x)
  tables <- .getDataTables(con)
  cols <- unique(unlist(sapply(tables, FUN=dbListFields, con=con)))
  cols <- cols[!cols %in% "_id"]
  if(exists('y')){cols <- c("PROBEID", cols)}
  cols
}

## general .cols function
.cols <- function(x, baseType){
    schema <- metadata(x)[metadata(x)$name=="DBSCHEMA",]$value
    if(schema=="NOSCHEMA_DB" || schema=="NOCHIPSCHEMA_DB"){
        .noSchemaCols(x)
    }else{
        .legacyCols(x, baseType)
    }
}


setMethod("columns", "OrgDb",
    function(x){
      baseType <- .chooseCentralOrgPkgSymbol(x)
      .cols(x, baseType)
    }
)

setMethod("columns", "ChipDb",
    function(x) .cols(x, baseType="PROBEID")
)

setMethod("columns", "GODb",
    function(x) .cols(x) ## does not have a missing baseType
)











## keys methods return the possible primary keys.  So for EG based packages,
## this will be the viable entrez gene IDs.
## Must use SELECT DISTINCT for now because some packages like ag.db
## (Arabidopsis) have repeated probe ids in the probes table (those are the
## probe ids that hit multiple genes).
## TODO: When 'x' has the new slot containing the package name, use
## dbUniqueVals() (defined in SQL.R) and pass pkgname:::datacache to it.
## dbUniqueVals() is what's used behind the scene by the Lkeys/Rkeys/keys
## methods for AnnDbBimap objects so the "keys" methods below will give a
## consistent answer (and will take advantage of the cache).
## helper to get keys
.queryForKeys <- function(x, keytype){
  if(class(x)=="ChipDb"){
    x <- .getOrgPkg(x)
  }
  table <- .getDBLocs(x, keytype)
  field <- .getDBLocs(x, keytype, value="field")
  sql <- paste("SELECT DISTINCT",field,"FROM",table)
  res <- dbQuery(dbConn(x), sql)
  t(res)
}


.legacyKeys <- function(x, keytype){
  ## have to swap keytype
  ## keytype <- .swapSymbolExceptions(x, keytype)
  keytype <- .simplifyCols(x, keytype)
  
  ## Some org packages may have entrez genes in weird places...
  centralID <- .getCentralID(x)
  EGgeneTable <- character()
  if(centralID == "EG" || centralID == "ORF"){
    EGgeneTable <- "genes"
  }else if(centralID == "TAIR"){
    EGgeneTable <- "entrez_genes"
  }
  ## now decide
  if(class(x) == "OrgDb"){
    res <- switch(EXPR = keytype,
                  "ENTREZID" = dbQuery(dbConn(x),
                    paste("SELECT gene_id FROM", EGgeneTable), 1L),
                  "TAIR" = dbQuery(dbConn(x),
                    "SELECT gene_id FROM genes", 1L),
                  "ORF" = dbQuery(dbConn(x),
                    "SELECT systematic_name FROM sgd", 1L),
                  "PROBEID" =
                     stop("PROBEID is not supported for Organism packages"),
                  .queryForKeys(x, keytype))
  }
  if(class(x) == "ChipDb"){
    res <- switch(EXPR = keytype,
                  "ENTREZID" = dbQuery(dbConn(x),
                    "SELECT gene_id FROM probes", 1L),
                  "PROBEID" =  dbQuery(dbConn(x),
                    "SELECT DISTINCT probe_id FROM probes", 1L),
                  .queryForKeys(x, keytype))
  }
  if(class(x) == "GODb"){
    res <- switch(EXPR = keytype,
                  "GOID" =  dbQuery(dbConn(x),
                    "SELECT DISTINCT go_id FROM go_term", 1L),
                  .queryForKeys(x, keytype))
  }
  as.character(res[!is.na(res)])
}


## special functions for newer NOSCHEMA_DB's
.deriveTableNameFromField <- function(field, x){
    if(class(x)=="ChipDb"){
        y <- x ## Switcheroo
        x <- .getOrgPkg(x)
        try(.attachDB(x,y), silent=TRUE) ## not a disaster if we fail
    }
    con <- dbConn(x)
    tables <- .getDataTables(con)
    if(exists("y")){
        tables <- c("c.probes", tables)
    }
    colTabs <- sapply(tables, FUN=dbListFields, con=con)
    m <- unlist2(sapply(colTabs, match, field))  ## cannot ever be repeated
    tab <- names(m)[!is.na(m)]
    if(length(tab)!=1){stop("Two fields in the source DB have the same name.")}
    tab
}

.noSchemaKeys <- function(x, keytype){
    tab <- .deriveTableNameFromField(field=keytype, x)
    ## So now we know table name (tab) and field (keytype)
    if(class(x)=="ChipDb"){
        y <- x ## Switcheroo
        x <- .getOrgPkg(x)
        try(.attachDB(x,y), silent=TRUE) ## not a disaster if we fail
    }
    sql <- paste("SELECT",keytype,"FROM",tab)
    res <- dbQuery(dbConn(x), sql, 1L)
    as.character(res[!is.na(res)])
}


## general keys function
.keys <- function(x, keytype){
    .checkForDeprecatedKeytype(keytype)
    .testForValidKeytype(x, keytype)
    schema <- metadata(x)[metadata(x)$name=="DBSCHEMA",]$value
    if(schema=="NOSCHEMA_DB" || schema=="NOCHIPSCHEMA_DB"){
        .noSchemaKeys(x, keytype)
    }else{
        .legacyKeys(x, keytype)
    }
}


####################################################################
## So the new idea is that each place where I want to "enhance" keys,
## I should just be able to use a helper to wrap up the actual keys
## method...


## And we need a master helper to tie it all together
smartKeys <-
    function(x, keytype, ..., pattern, column, fuzzy=FALSE, FUN)
{
    ## check args, then...

    ## FUN is the base keys method
    .keys <- FUN
    
    ## So 1st we need helpers for other "keys" situations
    ## keys0 is for when we have a pattern we want to match in the keys
    .keys0 <- function(x, keytype, ..., pattern, fuzzy=FALSE)
        {   ## assumes 'pattern' present
            FUN <- if (fuzzy) agrep else grep
            FUN(pattern, .keys(x, keytype), value=TRUE, ...)
        }
    ## keys1 is for when we have a column but no pattern
    ## so we want to filter by column
    .keys1 <- function(x, keytype, ..., column)
        {   ## column acts as filter
            k <- suppressWarnings(select(x, as.character(.keys(x, keytype)),
                                         column, keytype))
            k[[keytype]][ !is.na(k[[column]]) ]
        }
    ## keys2 is for when we have a column, and a pattern to match on that
    ## column, and we want all the keys of a particular keytype that match
    ## that column.
    .keys2 <- function(x, keytype, ..., pattern, column, fuzzy=FALSE)
        {   ## assumes 'pattern', 'column' present
            FUN <- if (fuzzy) agrep else grep
            k <- suppressWarnings(select(x, as.character(.keys(x, keytype)),
                                         column, keytype))
            k[[keytype]][ FUN(pattern, k[[column]], ...) ]
        }

    ## Now decide which function to call...
    if (missing(pattern) && missing(column))
        k <- .keys(x, keytype)
    else if (missing(column))
        k <- .keys0(x, keytype, ..., pattern=pattern, fuzzy=fuzzy)
    else if (missing(pattern))
        k <- .keys1(x, keytype, ..., column=column)
    else
        k <- .keys2(x, keytype, ..., pattern=pattern, column=column,
                    fuzzy=fuzzy)
    
    unique(k)
}



## TODO: don't fail to document all the new arguments (pattern, column and fuzzy)
setMethod("keys", "OrgDb",
    function(x, keytype, ...){
      if(missing(keytype)){
        keytype <- .chooseCentralOrgPkgSymbol(x)
      }
      smartKeys(x=x, keytype=keytype, ..., FUN=.keys)
  }
)

setMethod("keys", "ChipDb",
    function(x, keytype, ...){
      if(missing(keytype)) keytype <- "PROBEID"
      smartKeys(x=x, keytype=keytype, ..., FUN=.keys)
  }
)

setMethod("keys", "GODb",
    function(x, keytype, ...){
      if(missing(keytype)) keytype <- "GOID"
      smartKeys(x=x, keytype=keytype, ..., FUN=.keys)
  }
)


## new uses for keys:
## now TERM is a real key? (TODO: someone tell the keytypes)
## head(keys(GO.db, keytype="TERM"))



## get TERM keys that match a particular pattern
## head(keys(GO.db, keytype="TERM", pattern="mitochondrion"))

## get GOIDs where a TERM exists.
## head(keys(GO.db, keytype="GOID", column="TERM"))



## get keys of type GOID that go with a pattern match in TERM
## head(keys(GO.db, keytype="GOID", pattern="mitochondrion", column="TERM"))
## select(GO.db, keys =head(keys(GO.db, keytype="GOID", pattern="mitochondrion", column="TERM")), cols=c("GOID","TERM"))


## do the above but use fuzzy matching
## head(keys(GO.db, keytype="GOID", pattern="mitochondrion", column="TERM", fuzzy=TRUE))
## select(GO.db, keys = head(keys(GO.db, keytype="GOID", pattern="mitochondrion", column="TERM", fuzzy=TRUE)), cols=c("GOID","TERM"))



## Can just get keys (straight up)
## head(keys(org.Hs.eg.db, keytype="SYMBOL"))

## keys1 situation works fine (and smartKeys is called twice.)
## Can filter by column (only return keys where there is a value for "PATH"
## length(keys(org.Hs.eg.db, keytype="ENTREZID", column="PATH"))
## is shorter than:
## length(keys(org.Hs.eg.db, keytype="ENTREZID"))


## debug(AnnotationDbi:::smartKeys)


## Can just get keys that match a pattern
## keys(org.Hs.eg.db, keytype="SYMBOL", pattern="BRCA")


## Can get a key that matches a pattern on some other column
## head(keys(org.Hs.eg.db,keytype="ENTREZID",pattern="MSX",column="SYMBOL"))







## keytypes method is to allow the user to specify what kind of keytype is
## passed in to either keys or the select methods.
## temporarily:this method will be VERY unsophisticated.

## TODO: would like to find a way to restore these blacklisted types to being
## able to be used, but I need a way around the lack of an Rkeys() method etc.

## keytypesBlackList <- c("CHRLOCEND","CHRLOC","PFAM","PROSITE",
##                        "DESCRIPTION", "GENENAME")
## .filterKeytypes <- function(x, baseType, keytypesBlackList){
##   res <- .cols(x, baseType=baseType)
##   res <- res[!res %in% keytypesBlackList]
##   ## append the centralID (if not already present)
##   centralID <- .getCentralID(x)
##   if(centralID == "EG"){ centralID <- "ENTREZID" }
##   res <- c(res, centralID)
##   unique(res)
## }

setMethod("keytypes", "OrgDb",
    ## function(x) .filterKeytypes(x, baseType="ENTREZID", keytypesBlackList)
    function(x){ 
        kts <- .cols(x, baseType="ENTREZID")
        .filterDeprecatedKeytypes(kts)
    }
)

setMethod("keytypes", "ChipDb",
    ## function(x) .filterKeytypes(x, baseType="PROBEID", keytypesBlackList) 
    function(x){ 
        kts <- .cols(x, baseType="ENTREZID")
        .filterDeprecatedKeytypes(kts)
    }          
)

setMethod("keytypes", "GODb",
    function(x) return(c("GOID","TERM","ONTOLOGY","DEFINITION")) ## only one type makes sense
)





## TODO:
##X .5) make keytype so that it uses the mapping names instead of internal stuff
##X 1) make keytypes so that it returns all possible keytypes
##X 2) make keys() so that it gets correct keys for correct keytypes
##X 3) make select() so that it is more efficient (pre-filter)
##X 4) make select() so that it uses keytypes to initially map in to the correct thing and then call internal funcs.
## 4.5) Make sure this thing is sorting correctly!
## 5) document all this stuff.





#############################
## TEST CODE:
## library(org.Hs.eg.db)
## ls(2)

## con = AnnotationDbi:::dbConn(org.Hs.eg.db)
## keys = head(keys(org.Hs.egCHR))


## debug(AnnotationDbi:::.queryForKeys)
## debug(AnnotationDbi:::.keys)

## example of keys that uses keytype
## keys = keys(org.Hs.eg.db, keytype="ALIAS2EG")[1:4]

## example of keys that does not use keytype
## keys = keys(org.Hs.eg.db)[1:5]



## default keytype example
## keys = keys(org.Hs.eg.db)[1:5]
## cols = c("SYMBOL", "UNIPROT")
## select(org.Hs.eg.db, keys, cols)

## idType = "gene_id"



## debug(AnnotationDbi:::.resort)

## debug(AnnotationDbi:::.mergeBimaps)

## debug(AnnotationDbi:::.select)


#############################
## keytype example

## library(hgu95av2.db); columns(hgu95av2.db); columns(org.Hs.eg.db); head(keys(org.Hs.eg.db, "ALIAS")); keys(org.Hs.eg.db, keytype="PROBEID")## should be an error

## library(org.Hs.eg.db); keys2 = head(keys(org.Hs.eg.db, "ALIAS"));cols = c("SYMBOL", "GO");res <- select(org.Hs.eg.db, keys2, cols, keytype="ALIAS"); head(res); dim(res)

## debug(AnnotationDbi:::.select)

## library(hgu95av2.db); keys2 = head(keys(hgu95av2.db, "ALIAS"));cols = c("SYMBOL", "GO");res <- select(hgu95av2.db, keys2, cols, keytype="ALIAS"); head(res); dim(res)


## works now
## library(org.Hs.eg.db);keys2 = head(Rkeys(org.Hs.egALIAS2EG));cols = c("SYMBOL","ENTREZID", "GO");res <- select(org.Hs.eg.db, keys2, cols, keytype="ALIAS")

## works now
## keys = head(keys(org.Hs.eg.db)); cols = c("SYMBOL","ENTREZID", "GO");res <- select(org.Hs.eg.db, keys, cols, keytype="ENTREZID")

## also works now
## library(hgu95av2.db); keys = head(keys(hgu95av2.db)); cols = c("SYMBOL","ENTREZID", "GO", "PROBEID"); res <- select(hgu95av2.db, keys, cols, keytype="PROBEID"); head(res)


## This shouldn't work - wrong keytype):
## keys = head(keys(hgu95av2.db)); cols = c("SYMBOL","ENTREZID", "GO"); res <- select(hgu95av2.db, keys, cols, keytype="ENTREZID")

## This does work (and should):
## library(hgu95av2.db); keys = head(keys(hgu95av2.db)); cols = c("SYMBOL","ENTREZID", "GO"); res <- select(hgu95av2.db, keys, cols, keytype="PROBEID"); head(res)



## library(GO.db); select(GO.db, keys(GO.db)[1:4], c("TERM","SYNONYM"))

## library(hgu95av2.db); okeys = keys(hgu95av2.db,keytype="OMIM")[1:4]; cols = c("SYMBOL", "UNIPROT", "PATH"); select(hgu95av2.db, okeys, cols, keytype="OMIM")




## TODO Bugs/refinements:

## TODO: this one should produce an output... - FIXED
## keys = head(keys(hgu95av2.db, "ENTREZID")); cols = c("PROBEID","SYMBOL","ENTREZID", "GO"); res <- select(hgu95av2.db, keys, cols, keytype="ENTREZID"); head(res)

## 3) Add a NEWS page with info. about these changes.




## strange bug: - killed
## 4) Putting "ENTREZID" in for keytype and then giving probe IDs as keys should NOT work for hgu95av2.db: but it does... (it only seems to allow this with the one kind of key) 








## also need to roll back the removal of the keytype from the columns.
## library(org.Hs.eg.db);keys2 = head(Rkeys(org.Hs.egALIAS2EG));cols = c("SYMBOL","ENTREZID", "GO");res <- select(org.Hs.eg.db, keys2, cols, keytype="ALIAS");







## Strange bug:
## the following all work:
## foo = select(org.Hs.eg.db, keys=head(keys(org.Hs.eg.db),n=2),cols="REFSEQ"); head(foo)
## foo = select(org.Hs.eg.db, keys=head(keys(org.Hs.eg.db),n=2),cols=c("REFSEQ","ACCNUM")); head(foo)
## foo = select(org.Hs.eg.db, keys=head(keys(org.Hs.eg.db),n=2),cols=c("ACCNUM")); head(foo)

## But this does NOT work (fixed):
## foo = select(org.Hs.eg.db, keys=head(keys(org.Hs.eg.db),n=2),cols=head(columns(org.Hs.eg.db))); head(foo); head(columns(org.Hs.eg.db))

## debug(AnnotationDbi:::.nameExceptions)
## debug(AnnotationDbi:::.addNAsInPlace)




## Requirements for having a select method that works and plays well with
## others:
## 1) Use the same arguments for the method (obvious)
## 2) remove dulicated columns.





## Martins slow select example.  It takes advantage of the fact that for
## simple cases, like the one below, our select method has to gather each
## piece and then merge them together which costs a lot of time (both to merge
## and also because we don't pre-subset).
## Also our code is doing more post-processing (returning prettier results in
## particular order etc.)
## Also because our code is blind to what the user wants out, we move ALL of
## each bimap through memory and don't pare them down till we merge them
## together and this is ultimately inefficient.

## If the code knew (as Martin did in this case) the relationships between
## these different elements (perhaps it could learn that graph from the DB),
## then it could make smarter decisions about how to query.

## library(org.Hs.eg.db)
## sym <- "ITGA7"
## system.time(res0 <- toTable(org.Hs.egPFAM[ org.Hs.egALIAS2EG[[sym]] ]))
## system.time(res1 <- select(org.Hs.eg.db, sym, "PFAM", "ALIAS"))
## system.time(res3 <- toTable(org.Hs.egGO[ org.Hs.egALIAS2EG[[sym]] ]))
## system.time(res4 <- select(org.Hs.eg.db, sym, "GO", "ALIAS"))


## ALSO: there is something to be said for the notion that we need a general
## solution to this problem that does NOT involve a Bimap.  Bimaps are nice,
## but we don't normally have them for a new resource and we might want a
## faster way to handle these sorts of manipulations when we don't have them.


## Basically, I think that I want to use a graph here, but not require one
## from the user, I need to 1) be able to infer the graph from SQL, 2) be able
## to path-find through the graph such that all the keys requested are
## hit. and 3) be able to construct a sensible query from that graph.  Tall
## order, but a fun problem.


## Reasons for generalizing this: 1) I need to be able to do this in ALL
## databases (not just bimap ones). and 2) We are moving away from bimaps and
## 3) I want to be able to add mappings to existing bimap based data resources
## that are actually not available as a bimap (reactome) and 4) I would really
## like to be able to transparently pull data from another resource and just
## have it appear to be in one place.  Sort of like we currently do for
## microRNAs with TxDbs


## Really radical thoughts:
## What if discovery functions just reported based on which databases were
## installed (instead of just what was in a package?).

## What if select searched across all of these databases to make joins on the
## fly as appropriate by already knowing how to connect the dots?

## What if we could have select work out how things connect based on the type
## of package, and some internal information about how those would be joined?





## THE R CMD build bug:
## For AnnotationDbi:
## R --vanilla
## utils::Sweave("IntroToAnnotationPackages.Rnw") ## runs no problem
## BUT:
## R --vanilla
## utils::Sweave("AnnotationDbi.Rnw")
## utils::Sweave("IntroToAnnotationPackages.Rnw") ## FAILBOAT
## This failure is happening because the values that are left littered in the
## global namespace are allowed to leak down into the scope of the functions
## being called...

## Bug was able to happen this way:
## y = "foo"
## source("AnnotationDbi/inst/doc/IntroToAnnotationPackages.R")

## was caused by overly grabby exists() calls combined with the sloppy way
## that R CMD check leaves variables all over the place when it runs R CMD
## check.  exists() calls are no longer grabby.
## ALSO, lexical scoping meant that the exists() call being falsely tripped
## led to a call of species(y) actually being executed when I should never
## have been






## fieldNames <- c("gene_id","accession","accession")
## expectedCols <- c("ENTREZID","ACCNUM","REFSEQ")
## type <- 



## the na bug
## sym <- "ITGA7"
## select(org.Hs.eg.db,sym,"PFAM",keytype="ALIAS");

## Problem: our code that filters rows needs to drop the keytype column before filtering NAs





#######################################################################
## Some testing of my deprecation:
## library(org.Hs.eg.db);  k = head(keys(org.Hs.eg.db, 'ENTREZID')); k; 
## head(keys(org.Hs.eg.db, 'CHR'));
## select(org.Hs.eg.db, k, 'SYMBOL', 'ENTREZID');
## head(select(org.Hs.eg.db, k[1], 'SYMBOL', 'CHR'));











## new method for mapping Ids (idea to return a vector)
## This method will just get all data for one column, one keytype 
## and one set of keys.  Then it will return either the 1st match for each, 
## filter out based on a rule OR return a CharacterList

## If the user instead gives a test function for 'returnVal', then it will 
## be applied to all the resulting matches (should take a vector and return 
## a result)

## Future ideas:
## Add 'filter' as a later option for returnVal which just means that the user needs to provide an actual function for the returnVal
## Add some of the arguments that are now supported by 'keys' to this function.

setMethod("mapIds", "AnnotationDb", function(x, keys, column, keytype, 
            returnVal=c("filterMultiples","NAMultiples","first", 
                        "CharacterList"), ...){
    if(missing(returnVal)) returnVal <- 'first'
    ## make sure we have reasonable value for returnVal.
    if(!is.function(returnVal)){
       match.arg(returnVal)
    }        
    ## 1st we have to insist that they NOT use more than one column 
    ## or keytype
    if(length(keys)<1){stop(wmsg(
        "mapIds must have at least one key to match against."))}
    if(length(column)>1){stop(wmsg("mapIds can only use one column."))}
    if(length(keytype)>1){stop(wmsg("mapIds can only use one keytype."))}
    
    ## next call select()
    ## TODO: remove the suppressWarnings() call once you get rid of that warning
suppressWarnings( res <- select(x, keys=keys, columns=column, keytype=keytype) )
    ## then split accordingly
    res <- split(res[[column]], f=res[[keytype]])

    ## internal helper to toss out multiply matching things
    .filtMults <- function(data){
        idx <- sapply(data, FUN=function(x){
            if(length(x)==1){x=FALSE}else{x=TRUE}; x })
        data[idx]
    }
    
    ## If it's a function then call that
    if(is.function(returnVal)){
        res <- sapply(res, FUN=returnVal)
    }else{
        res <- switch(returnVal,
            "filterMultiples"=.filtMults(res),
            "NAMultiples"=sapply(res, FUN=function(x){
                if(length(x)>1){return(NA)}else{return(x)} }),
            "CharacterList" = as(res, 'CharacterList'),
            "first" = sapply(res, FUN=function(x){x[[1]]})
            )
    }
    ## names will already be present
    res
})

## TODO: add option to replace multi-matches with NAs or to just remove them.
## To cleanly handle having 'returnVal' being EITHER a FUN or something else:
## DO like: if(is.function(returnVal)){}else{match.arg(returnVal)}







