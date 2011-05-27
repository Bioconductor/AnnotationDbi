### =========================================================================
### An SQLite-based ann data package (AnnDbPkg) provides a set of pre-defined
### AnnObj objects that are created at load-time. This is a more dynamic set
### of mappings that will vary with the schema that is available.
###
### This file describes the set of AnnObj objects provided by any
### ORGANISM_DB-based package i.e. any SQLite-based ann data package based
### on the ORGANISM_DB schema.
### The createAnnObjs.ORGANISM_DB() function is the main entry point for
### this file: it is called by any ORGANISM_DB-based package at load-time.
### -------------------------------------------------------------------------


ORGANISM_DB_L2Rlink1 <- list(tablename="genes",
                             Lcolname="gene_id",
                             Rcolname="_id")



makeAnnDbBiMapSeed <- function(objName,tablename,Rcolname){
    list(
        objName=objName,
        Class="AnnDbBimap",
        L2Rchain=list(
            ORGANISM_DB_L2Rlink1,
            list(
                tablename=tablename,
                Lcolname="_id",
                Rcolname=Rcolname
            )
        )
    )
}

makeGo3AnnDbBiMapSeed <- function(){
  list(
    list(
        objName="GO",
        Class="Go3AnnDbBimap",
        L2Rchain=list(
            ORGANISM_DB_L2Rlink1,
            list(
                #tablename="go_term", # no rightmost table for a Go3AnnDbBimap
                Lcolname="_id",
                tagname=c(Evidence="{evidence}"),
                Rcolname="go_id",
                Rattribnames=c(Ontology="NULL")
            )
        ),
        rightTables=Go3tablenames()
    )  
  )
}


## ## for now, there are only two like this, and they are both from an odd table
## ## this may go away in time, so fine for this to be an exception
## makeAnnDbMapSeeds <- function(){
##   list(
##      list(
##         objName="CHRLOC",
##         Class="AnnDbMap",
##         L2Rchain=list(
##             ORGANISM_DB_L2Rlink1,
##             list(
##                 tablename="chromosome_locations",
##                 Lcolname="_id",
##                 tagname=c(Chromosome="{seqname}"),
##                 Rcolname="start_location"
##             )
##         ),
##         rightColType="integer"
##     ),
##     list(
##         objName="CHRLOCEND",
##         Class="AnnDbMap",
##         L2Rchain=list(
##             ORGANISM_DB_L2Rlink1,
##             list(
##                 tablename="chromosome_locations",
##                 Lcolname="_id",
##                 tagname=c(Chromosome="{seqname}"),
##                 Rcolname="end_location"
##             )
##         ),
##         rightColType="integer"
##     )
##   )
## }


## for future expansion (assuming we ever get access to these)
## makeIpiAnnDbBiMapSeed <- function(objName,tablename,Rcolname){
## }



getDBMaps <- function(dbconn){
  ## Get into the DB and list the tables
  rawtabs <- dbListTables(dbconn)
  ## drop unsupported (sqlite_stat1)  and specialized (go, and chrloc) tables
  idx <- grep(paste("sqlite_stat1|map_counts|metadata|map_metadata|genes",
                    "|^go_|prosite|pfam|chrlengths",
                    "|chromosome_locations",sep=""),
              rawtabs, invert=TRUE)
  tabs <- rawtabs[idx]
  ## I require a reference frame
  objNames = c("ACCNUM",    "ALIAS2EG",     "CHR",        "ENZYME",   "GENENAME",   "MAP",                   "OMIM",   "PATH",    "PMID",      "REFSEQ",    "SYMBOL",    "UNIGENE",    "ENSEMBL",    "ENSEMBLPROT",  "ENSEMBLTRANS",  "UNIPROT")
  tables   = c("accessions","alias",        "chromosomes","ec",       "gene_info",  "cytogenetic_locations", "omim",   "kegg",    "pubmed",    "refseq",    "gene_info", "unigene",    "ensembl",    "ensembl_prot", "ensembl_trans", "uniprot")
  fields   = c("accession", "alias_symbol", "chromosome", "ec_number", "gene_name", "cytogenetic_location",  "omim_id","path_id", "pubmed_id", "accession", "symbol",     "unigene_id", "ensembl_id", "prot_id",      "trans_id",      "uniprot_id")
  possibleMaps <- data.frame(objName=objNames, tables=tables,
                             fields=fields, stringsAsFactors=FALSE)
  ## subset to just ones that we see tables for
  possibleMaps[as.vector(t(possibleMaps["tables"]))%in% tabs,]
}


## Used to create the seeds:
createORGANISMSeeds <- function(maps, dbconn){
  ## 1st lets loop and make the regular ones
  seeds <- list()
  for(i in seq_len(dim(maps)[1])){
    seeds[[i]] <- makeAnnDbBiMapSeed(objName = maps[i,"objName"],
                                     tablename = maps[i,"tables"],
                                     Rcolname = maps[i,"fields"])
  }
  
  ## Then we have to append a couple of exceptions.
  ## And, we have to test for these separately in here.
  if(length(grep("go_bp", dbListTables(dbconn)))>0 &&
     length(grep("go_cc", dbListTables(dbconn)))>0 &&
     length(grep("go_mf", dbListTables(dbconn)))>0 &&
     length(grep("go_bp_all", dbListTables(dbconn)))>0 &&
     length(grep("go_cc_all", dbListTables(dbconn)))>0 &&
     length(grep("go_mf_all", dbListTables(dbconn)))>0 ){
    seeds <- c(seeds, makeGo3AnnDbBiMapSeed())
  }

  if(length(grep("chromosome_locations", dbListTables(dbconn)))>0){
    seeds <- c(seeds, makeAnnDbMapSeeds())
  }
  
  seeds
}



createAnnObjs.ORGANISM_DB <- function(prefix, objTarget, dbconn, datacache)
{
    checkDBSCHEMA(dbconn, "ORGANISM_DB")

    ## AnnDbBimap objects
    seed0 <- list(
        objTarget=objTarget,
        datacache=datacache
    )
    ## troll the DB and set up the Mapping "seeds"
    maps <- getDBMaps(dbconn)
    ORGANISM_DB_AnnDbBimap_seeds <- createORGANISMSeeds(maps, dbconn)
    ann_objs <- createAnnDbBimaps(ORGANISM_DB_AnnDbBimap_seeds, seed0)

    ## GO2ALL mapping requires GO mappings.
    if(length(grep("go_bp", dbListTables(dbconn)))>0 &&
       length(grep("go_cc", dbListTables(dbconn)))>0 &&
       length(grep("go_mf", dbListTables(dbconn)))>0 &&
       length(grep("go_bp_all", dbListTables(dbconn)))>0 &&
       length(grep("go_cc_all", dbListTables(dbconn)))>0 &&
       length(grep("go_mf_all", dbListTables(dbconn)))>0 ){
      ann_objs$GO2EG <- revmap(ann_objs$GO, objName="GO2EG")
      map <- ann_objs$GO2EG
      map@rightTables <- Go3tablenames(all=TRUE)
      map@objName <- "GO2ALLEGS"
      ann_objs$GO2ALLEGS <- map
      ## Some pre-caching
      Lkeys(ann_objs$GO)
    }
    
    ## 2 special maps that are not AnnDbBimap objects (named int vectors)
    if(length(grep("chrlengths", dbListTables(dbconn)))>0){
      ann_objs$CHRLENGTHS <- createCHRLENGTHS(dbconn)
    }
    if(length(grep("map_counts", dbListTables(dbconn)))>0){
      ann_objs$MAPCOUNTS <- createMAPCOUNTS(dbconn, prefix)
    }
    
    prefixAnnObjNames(ann_objs, prefix)
}

