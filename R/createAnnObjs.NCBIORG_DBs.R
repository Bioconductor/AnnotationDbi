## Generic code to createAnnObjs for bimaps of NCBIORG packages

## This file will define what it means to be MOUSE_DB or HUMAN_DB etc. as a
## series of functions named like createAnnObjs.MOUSE_DB() etc.


## Start with a huge named list of all the possible mappings.

NCBIORG_DB_SeedGenerator <- function(orgPkg){
  
NCBIORG_DB_L2Rlink1 <- list(tablename="genes", Lcolname="gene_id", Rcolname="_id")

NCBIORG_DB_AnnDbBimap_seeds <- list(
    list(
        objName="ACCNUM",
        Class="AnnDbBimap",
        L2Rchain=list(
            NCBIORG_DB_L2Rlink1,
            list(
                tablename="accessions",
                Lcolname="_id",
                Rcolname="accession"
            )
        )
    ),
    list(
        objName="ALIAS2EG",
        Class="AnnDbBimap",
        L2Rchain=list(
            NCBIORG_DB_L2Rlink1,
            list(
                tablename="alias",
                Lcolname="_id",
                Rcolname="alias_symbol"
            )
        ),
        direction=-1L
    ),
    list(
        objName="CHR",
        Class="AnnDbBimap",
        L2Rchain=list(
            NCBIORG_DB_L2Rlink1,
            list(
                tablename="chromosomes",
                Lcolname="_id",
                Rcolname="chromosome"
            )
        )
    ),
    list(
        objName="ENZYME",
        Class="AnnDbBimap",
        L2Rchain=list(
            NCBIORG_DB_L2Rlink1,
            list(
                tablename="ec",
                Lcolname="_id",
                Rcolname="ec_number"
            )
        )
    ),
    list(
        objName="GENENAME",
        Class="AnnDbBimap",
        L2Rchain=list(
            NCBIORG_DB_L2Rlink1,
            list(
                tablename="gene_info",
                Lcolname="_id",
                Rcolname="gene_name"
            )
        )
    ),
    list(
        objName="MAP",
        Class="AnnDbBimap",
        L2Rchain=list(
            NCBIORG_DB_L2Rlink1,
            list(
                tablename="cytogenetic_locations",
                Lcolname="_id",
                Rcolname="cytogenetic_location"
            )
        )
    ),
    list(
        objName="OMIM",
        Class="AnnDbBimap",
        L2Rchain=list(
            NCBIORG_DB_L2Rlink1,
            list(
                tablename="omim",
                Lcolname="_id",
                Rcolname="omim_id"
            )
        )
    ),
    list(
        objName="PATH",
        Class="AnnDbBimap",
        L2Rchain=list(
            NCBIORG_DB_L2Rlink1,
            list(
                tablename="kegg",
                Lcolname="_id",
                Rcolname="path_id"
            )
        )
    ),
    list(
        objName="PMID",
        Class="AnnDbBimap",
        L2Rchain=list(
            NCBIORG_DB_L2Rlink1,
            list(
                tablename="pubmed",
                Lcolname="_id",
                Rcolname="pubmed_id"
            )
        )
    ),
    list(
        objName="REFSEQ",
        Class="AnnDbBimap",
        L2Rchain=list(
            NCBIORG_DB_L2Rlink1,
            list(
                tablename="refseq",
                Lcolname="_id",
                Rcolname="accession"
            )
        )
    ),
    list(
        objName="SYMBOL",
        Class="AnnDbBimap",
        L2Rchain=list(
            NCBIORG_DB_L2Rlink1,
            list(
                tablename="gene_info",
                Lcolname="_id",
                Rcolname="symbol"
            )
        )
    ),
    list(
        objName="UNIGENE",
        Class="AnnDbBimap",
        L2Rchain=list(
            NCBIORG_DB_L2Rlink1,
            list(
                tablename="unigene",
                Lcolname="_id",
                Rcolname="unigene_id"
            )
        )
    ),
    list(
        objName="CHRLOC",
        Class="AnnDbMap",
        L2Rchain=list(
            NCBIORG_DB_L2Rlink1,
            list(
                tablename="chromosome_locations",
                Lcolname="_id",
                tagname=c(Chromosome="{seqname}"),
                Rcolname="start_location"
            )
        ),
        rightColType="integer"
    ),
    list(
        objName="CHRLOCEND",
        Class="AnnDbMap",
        L2Rchain=list(
            NCBIORG_DB_L2Rlink1,
            list(
                tablename="chromosome_locations",
                Lcolname="_id",
                tagname=c(Chromosome="{seqname}"),
                Rcolname="end_location"
            )
        ),
        rightColType="integer"
    ),
    list(
        objName="PFAM",
        Class="IpiAnnDbMap",
        L2Rchain=list(
            NCBIORG_DB_L2Rlink1,
            list(
                tablename="pfam",
                Lcolname="_id",
                Rcolname="ipi_id",
                Rattribnames=c(PfamId="{pfam_id}")
            )
        )
    ),
    list(
        objName="PROSITE",
        Class="IpiAnnDbMap",
        L2Rchain=list(
            NCBIORG_DB_L2Rlink1,
            list(
                tablename="prosite",
                Lcolname="_id",
                Rcolname="ipi_id",
                Rattribnames=c(PrositeId="{prosite_id}")
            )
        )
    ),
    list(
        objName="ENSEMBL",
        Class="AnnDbBimap",
        L2Rchain=list(
            NCBIORG_DB_L2Rlink1,
            list(
                tablename="ensembl",
                Lcolname="_id",
                Rcolname="ensembl_id"
            )
        )
    ),
    list(
        objName="ENSEMBLPROT",
        Class="AnnDbBimap",
        L2Rchain=list(
            NCBIORG_DB_L2Rlink1,
            list(
                tablename="ensembl_prot",
                Lcolname="_id",
                Rcolname="prot_id"
            )
        )
    ),
    list(
        objName="ENSEMBLTRANS",
        Class="AnnDbBimap",
        L2Rchain=list(
            NCBIORG_DB_L2Rlink1,
            list(
                tablename="ensembl_trans",
                Lcolname="_id",
                Rcolname="trans_id"
            )
        )
    ),
    list(
        objName="UNIPROT",
        Class="AnnDbBimap",
        L2Rchain=list(
            NCBIORG_DB_L2Rlink1,
            list(
                tablename="uniprot",
                Lcolname="_id",
                Rcolname="uniprot_id"
            )
        )
    ),
    list(
        objName="UCSCKG",
        Class="AnnDbBimap",
        L2Rchain=list(
            NCBIORG_DB_L2Rlink1,
            list(
                tablename="ucsc",
                Lcolname="_id",
                Rcolname="ucsc_id"
            )
        )
    ),
    list(
        objName="FLYBASE",
        Class="AnnDbBimap",
        L2Rchain=list(
            NCBIORG_DB_L2Rlink1,
            list(
                tablename="flybase",
                Lcolname="_id",
                Rcolname="flybase_id"
            )
        )
    ),
    list(
        objName="FLYBASECG",
        Class="AnnDbBimap",
        L2Rchain=list(
            NCBIORG_DB_L2Rlink1,
            list(
                tablename="flybase_cg",
                Lcolname="_id",
                Rcolname="flybase_cg_id"
            )
        )
    ),
    list(
        objName="FLYBASEPROT",
        Class="AnnDbBimap",
        L2Rchain=list(
            NCBIORG_DB_L2Rlink1,
            list(
                tablename="flybase_prot",
                Lcolname="_id",
                Rcolname="prot_id"
            )
        )
    ),                                 
    list(
        objName="MGI",
        Class="AnnDbBimap",
        L2Rchain=list(
            NCBIORG_DB_L2Rlink1,
            list(
                tablename="mgi",
                Lcolname="_id",
                Rcolname="mgi_id"
            )
        )
    ),
    list(
        objName="WORMBASE",
        Class="AnnDbBimap",
        L2Rchain=list(
            NCBIORG_DB_L2Rlink1,
            list(
                tablename="wormbase",
                Lcolname="_id",
                Rcolname="wormbase_id"
            )
        )
    ),
    list(
        objName="ZFIN",
        Class="AnnDbBimap",
        L2Rchain=list(
            NCBIORG_DB_L2Rlink1,
            list(
                tablename="zfin",
                Lcolname="_id",
                Rcolname="zfin_id"
            )
        )
    ),
    list(
        objName="GO",
        Class="Go3AnnDbBimap",
        L2Rchain=list(
            NCBIORG_DB_L2Rlink1,
            list(
                #tablename="go_term", # no rightmost table for a Go3AnnDbBimap
                Lcolname="_id",
                tagname=c(Evidence="{evidence}"),
                Rcolname="go_id",
                Rattribnames=c(Ontology="NULL")
            )
        ),
        rightTables=AnnotationDbi:::Go3tablenames()
    )
)
## return this
NCBIORG_DB_AnnDbBimap_seeds
}



## Then some helper functions to take a list of arguments and convert them into the ann_objs object that is returned by each function

## helper to make MOUSE_DB_AnnDbBimap_seeds from the big list and a list of mappings that are needed



## helper to translate schema to species name
getSpeciesFromSchema <- function(schema){
  species <- switch(EXPR = schema,
                    "ANOPHELES_DB"="Anopheles gambiae",
                    "ARABIDOPSISCHIP_DB"="Arabidopsis thaliana",
                    "ARABIDOPSIS_DB"="Arabidopsis thaliana",
                    "BOVINECHIP_DB"="Bos taurus",
                    "BOVINE_DB"="Bos taurus",
                    "CANINECHIP_DB"="Canis familiaris",
                    "CANINE_DB"="Canis familiaris",
                    "CHICKENCHIP_DB"="Gallus gallus",
                    "CHICKEN_DB"="Gallus gallus",
                    "CHIMP_DB"="Pan troglodytes",
                    "COELICOLOR_DB"="Streptomyces coelicolor",
                    "ECOLICHIP_DB"="Escherichia coli",
                    "ECOLI_DB"="Escherichia coli",
                    "FLYCHIP_DB"="Drosophila melanogaster",
                    "FLY_DB"="Drosophila melanogaster",
                    "GO_DB"=NA,
                    "HUMANCHIP_DB"="Homo sapiens",
                    "HUMAN_DB"="Homo sapiens",
                    "INPARANOID_DB"=NA,
                    "KEGG_DB"=NA,
                    "MALARIA_DB"="Plasmodium falciparum",
                    "MOUSECHIP_DB"="Mus musculus",
                    "MOUSE_DB"="Mus musculus",
                    "ORGANISM_DB"=NA,
                    "PFAM_DB"=NA,
                    "PIGCHIP_DB"="Sus scrofa",
                    "PIG_DB"="Sus scrofa",
                    "RATCHIP_DB"="Rattus norvegicus",
                    "RAT_DB"="Rattus norvegicus",
                    "RHESUSCHIP_DB"="Macaca mulatta",
                    "RHESUS_DB"="Macaca mulatta",
                    "WORMCHIP_DB"="Caenorhabditis elegans",
                    "WORM_DB"="Caenorhabditis elegans",
                    "XENOPUSCHIP_DB"="Xenopus laevis",
                    "XENOPUS_DB"="Xenopus laevis",
                    "YEASTCHIP_DB"="Saccharomyces cerevisiae",
                    "YEAST_DB"="Saccharomyces cerevisiae",
                    "YEASTNCBI_DB"="Saccharomyces cerevisiae",
                    "ZEBRAFISHCHIP_DB"="Danio rerio",
                    "ZEBRAFISH_DB"="Danio rerio")
  species
}

## helper to translate schema to assoicated org package
getOrgPkgForSchema <- function(schema){
  species <- switch(EXPR = schema,
                    "ARABIDOPSISCHIP_DB"="org.At.tair",
                    "BOVINECHIP_DB"="org.Bt.eg",
                    "CANINECHIP_DB"="org.Cf.eg",
                    "CHICKENCHIP_DB"="org.Gg.eg",
                    "ECOLICHIP_DB"="org.EcK12.eg",
                    "FLYCHIP_DB"="org.Dm.eg",
                    "HUMANCHIP_DB"="org.Hs.eg",
                    "MOUSECHIP_DB"="org.Mm.eg",
                    "PIGCHIP_DB"="org.Ss.eg",
                    "RATCHIP_DB"="org.Rn.eg",
                    "RHESUSCHIP_DB"="org.Mmu.eg",
                    "WORMCHIP_DB"="org.Ce.eg",
                    "XENOPUSCHIP_DB"="org.Xl.eg",
                    "YEASTCHIP_DB"="org.Sc.sgd",
                    "ZEBRAFISHCHIP_DB"="org.Dr.eg")
  species
}


## helper to filter seeds above based on the schema
.filterSeeds <- function(allSeeds, schema, class){
  ## logic to decide what to leave and what to keep:
  ## 1st get the schema name
  species <- getSpeciesFromSchema(schema)

  ## then call .definePossibleTables to get detailed schema info.
  schemaDet <- .definePossibleTables(class, species)
  classNames <- names(schemaDet)
  names <-  sapply(allSeeds, function(x){x$objName})

  ## if you are in both lists, then we want you as a seed
  idx <- match(classNames,names)
  idx <- idx[!is.na(idx)]
  seeds <- allSeeds[idx]
  names(seeds) <- NULL  ## clean out names

  ## return filtered seeds
  seeds
}

.revmapper <-function(name, ann_objs, class){
  if(class=="OrgDb"){
    name2<- paste0(name,"2EG")
  }else if (class=="ChipDb"){
    name2<- paste0(name,"2PROBE")
  }  
  ann_objs[[name2]] <- revmap(ann_objs[[name]], objName=name2)
}


## helper to add the revmaps (based on that same list and the maps that are
## sometimes reversed) and also to put things together.
.addRevMapSeeds <- function(ann_objs, class){
  
  ## We have a list of things we would like to add revmaps for, but we can
  ## only do that if they exist.
  if(class=="OrgDb"){
    revMapables <- c("ACCNUM","ENZYME","MAP","OMIM","PATH","PMID","REFSEQ",
                     "SYMBOL","UNIGENE","ENSEMBL","ENSEMBLPROT", "ENSEMBLTRANS",
                     "MGI","FLYBASE","FLYBASECG","FLYBASEPROT","GO")
  }else if (class=="ChipDb"){
    revMapables <- c("ENZYME","PATH","PMID","ENSEMBL","MGI","FLYBASE",
                     "FLYBASECG","GO")
  }
  ## now filter so that rMapNames is only the names that we actually have
  mapNames <- ls(ann_objs)
  idx <- match(revMapables, mapNames)
  idx <- idx[!is.na(idx)]
  rMapNames <- mapNames[idx]

  ## then loop through the rMapNames to call revmap and assign values in.
  lapply(rMapNames, .revmapper, ann_objs, class)
  
  ## After we make our revmaps, we need to do some special work for GO (always)
  ## BUT the following still has to be customized for either 2EG or 2PROBE...
  if(class=="OrgDb"){
    map <- ann_objs$GO2EG
    map@rightTables <- Go3tablenames(all=TRUE)
    map@objName <- "GO2ALLEGS"
    ann_objs$GO2ALLEGS <- map
  }else if (class=="ChipDb"){
    map <- ann_objs$GO2PROBE
    map@rightTables <- Go3tablenames(all=TRUE)
    map@objName <- "GO2ALLPROBES"
    ann_objs$GO2ALLPROBES <- map
  }
  
  ## return the objects
  ann_objs
}


## 1st three arguments for this are all hard coded below for each case
createAnnObjs.NCBI_DB <- function(prefix,
                                  objTarget,
                                  dbconn,
                                  datacache,
                                  schema,
                                  class){
  checkDBSCHEMA(dbconn, schema)
  ## AnnDbBimap objects
  seed0 <- list(
                objTarget=objTarget,
                datacache=datacache
                )
  ## Get allSeeds
  if(class=="OrgDb"){
    allSeeds <- NCBIORG_DB_SeedGenerator()
  }else if (class=="ChipDb"){
    dbname <- getOrgPkgForSchema(schema)
    allSeeds <- NCBICHIP_DB_SeedGenerator(dbname)
  }  
  ## filter the seeds to match the schema
  seeds <- .filterSeeds(allSeeds, schema, class)
  ## now make the bimaps
  ann_objs <- createAnnDbBimaps(seeds, seed0)
  
  ## Then add reversemaps
  ann_objs <- .addRevMapSeeds(ann_objs, class)
                      
  ## 2 special maps that are not AnnDbBimap objects (just named integer vectors)
  if(class=="OrgDb"){
    if(!any(c("ECOLI_DB","XENOPUS_DB","PIG_DB") %in%  schema)){ ## CHRLENGTHS exceptions.
      ann_objs$CHRLENGTHS <- createCHRLENGTHS(dbconn)
    }
  }else if (class=="ChipDb"){
    attachDBs(dbconn, ann_objs)
    if(!any(c("ECOLICHIP_DB","XENOPUSCHIP_DB","PIGCHIP_DB") %in% schema)){ ## more exceptions
      ann_objs$CHRLENGTHS <- createCHRLENGTHS(dbconn, dbname="")
    }
    ann_objs$ORGPKG <- dbname
  }
  ann_objs$MAPCOUNTS <- createMAPCOUNTS(dbconn, prefix)

  ## Some pre-caching
  Lkeys(ann_objs$GO)
 
  prefixAnnObjNames(ann_objs, prefix)
}



## Then define each function briefly like this:
createAnnObjs.ANOPHELES_DB <- function(prefix,
                                       objTarget,
                                       dbconn,
                                       datacache,
                                       schema="ANOPHELES_DB",
                                       class="OrgDb"){
  createAnnObjs.NCBI_DB(prefix,objTarget,dbconn,datacache,schema,class)
}

createAnnObjs.BOVINE_DB <- function(prefix,
                                    objTarget,
                                    dbconn,
                                    datacache,
                                    schema="BOVINE_DB",
                                    class="OrgDb"){
  createAnnObjs.NCBI_DB(prefix,objTarget,dbconn,datacache,schema,class)
}

createAnnObjs.CANINE_DB <- function(prefix,
                                    objTarget,
                                    dbconn,
                                    datacache,
                                    schema="CANINE_DB",
                                    class="OrgDb"){
  createAnnObjs.NCBI_DB(prefix,objTarget,dbconn,datacache,schema,class)
}

createAnnObjs.CHICKEN_DB <- function(prefix,
                                     objTarget,
                                     dbconn,
                                     datacache,
                                     schema="CHICKEN_DB",
                                     class="OrgDb"){
  createAnnObjs.NCBI_DB(prefix,objTarget,dbconn,datacache,schema,class)
}

createAnnObjs.CHIMP_DB <- function(prefix,
                                   objTarget,
                                   dbconn,
                                   datacache,
                                   schema="CHIMP_DB",
                                   class="OrgDb"){
  createAnnObjs.NCBI_DB(prefix,objTarget,dbconn,datacache,schema,class)
}


createAnnObjs.ECOLI_DB <- function(prefix,
                                   objTarget,
                                   dbconn,
                                   datacache,
                                   schema="ECOLI_DB",
                                   class="OrgDb"){
  createAnnObjs.NCBI_DB(prefix,objTarget,dbconn,datacache,schema,class)
}


createAnnObjs.FLY_DB <- function(prefix,
                                 objTarget,
                                 dbconn,
                                 datacache,
                                 schema="FLY_DB",
                                 class="OrgDb"){
  createAnnObjs.NCBI_DB(prefix,objTarget,dbconn,datacache,schema,class)
}


createAnnObjs.HUMAN_DB <- function(prefix,
                                   objTarget,
                                   dbconn,
                                   datacache,
                                   schema="HUMAN_DB",
                                   class="OrgDb"){
  createAnnObjs.NCBI_DB(prefix,objTarget,dbconn,datacache,schema,class)
}


createAnnObjs.MOUSE_DB <- function(prefix,
                                   objTarget,
                                   dbconn,
                                   datacache,
                                   schema="MOUSE_DB",
                                   class="OrgDb"){
  createAnnObjs.NCBI_DB(prefix,objTarget,dbconn,datacache,schema,class)
}


createAnnObjs.PIG_DB <- function(prefix,
                                 objTarget,
                                 dbconn,
                                 datacache,
                                 schema="PIG_DB",
                                 class="OrgDb"){
  createAnnObjs.NCBI_DB(prefix,objTarget,dbconn,datacache,schema,class)
}


createAnnObjs.RAT_DB <- function(prefix,
                                 objTarget,
                                 dbconn,
                                 datacache,
                                 schema="RAT_DB",
                                 class="OrgDb"){
  createAnnObjs.NCBI_DB(prefix,objTarget,dbconn,datacache,schema,class)
}


createAnnObjs.RHESUS_DB <- function(prefix,
                                    objTarget,
                                    dbconn,
                                    datacache,
                                    schema="RHESUS_DB",
                                    class="OrgDb"){
  createAnnObjs.NCBI_DB(prefix,objTarget,dbconn,datacache,schema,class)
}


createAnnObjs.WORM_DB <- function(prefix,
                                  objTarget,
                                  dbconn,
                                  datacache,
                                  schema="WORM_DB",
                                  class="OrgDb"){
  createAnnObjs.NCBI_DB(prefix,objTarget,dbconn,datacache,schema,class)
}


createAnnObjs.XENOPUS_DB <- function(prefix,
                                     objTarget,
                                     dbconn,
                                     datacache,
                                     schema="XENOPUS_DB",
                                     class="OrgDb"){
  createAnnObjs.NCBI_DB(prefix,objTarget,dbconn,datacache,schema,class)
}


createAnnObjs.ZEBRAFISH_DB <- function(prefix,
                                       objTarget,
                                       dbconn,
                                       datacache,
                                       schema="ZEBRAFISH_DB",
                                       class="OrgDb"){
  createAnnObjs.NCBI_DB(prefix,objTarget,dbconn,datacache,schema,class)
}





