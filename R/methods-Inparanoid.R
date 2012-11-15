## Inparanoid select methods:

.getBaseSpecies <- function(x){
  meta <- metadata(x)  
  ## TODO: switch to helper function:
  baseSpecies <- sub(" ","_",meta[meta$name=="ORGANISM","value"])
  baseSpecies
}


## match a "set" of cols
.getInpTables <- function(x, cols){
  res <- .getTableNames(x)
  res <- res[res %in% cols]
  unique(res)
}

## the mapping of 5 letter codes to the short names.
.makeFiveLetterMapping <- function(){
  fields = c(
    "Acyrthosiphon_pisum" = "ACYPI",
    "Aedes_aegypti" = "AEDAE",
    "Anopheles_gambiae" = "ANOGA",
    "Apis_mellifera" = "APIME",
    "Arabidopsis_thaliana" = "ARATH",
    "Aspergillus_fumigatus" = "ASPFU",
    "Batrachochytrium_dendrobatidis" = "BATDE",
    "Bombyx_mori" = "BOMMO",
    "Bos_taurus" = "BOSTA",
    "Branchiostoma_floridae" = "BRAFL",
    "Brugia_malayi" = "BRUMA",
    "Caenorhabditis_brenneri" = "CAEBRE",
    "Caenorhabditis_briggsae" = "CAEBR",
    "Caenorhabditis_elegans" = "CAEEL",
    "Caenorhabditis_japonica" = "CAEJA",
    "Caenorhabditis_remanei" = "CAERE",
    "Candida_albicans" = "CANAL",
    "Candida_glabrata" = "CANGL",
    "Canis_familiaris" = "CANFA",
    "Capitella_spI" = "CAPSP",
    "Cavia_porcellus" = "CAVPO",
    "Chlamydomonas_reinhardtii" = "CHLRE",
    "Ciona_intestinalis" = "CIOIN",
    "Ciona_savignyi" = "CIOSA",
    "Coccidioides_immitis" = "COCIM",
    "Coprinopsis_cinereus" = "COPCI",
    "Cryptococcus_neoformans" = "CRYNE",
    "Cryptosporidium_hominis" = "CRYHO",
    "Cryptosporidium_parvum" = "CRYPA",
    "Culex_pipiens" = "CULPI",
    "Cyanidioschyzon_merolae" = "CYAME",
    "Danio_rerio" = "DANRE",
    "Daphnia_pulex" = "DAPPU",
    "Debaryomyces_hansenii" = "DEBHA",
    "Dictyostelium_discoideum" = "DICDI",
    "Drosophila_ananassae" = "DROAN",
    "Drosophila_grimshawi" = "DROGR",
    "Drosophila_melanogaster" = "DROME",
    "Drosophila_mojavensis" = "DROMO",
    "Drosophila_pseudoobscura" = "DROPS",
    "Drosophila_virilis" = "DROVI",
    "Drosophila_willistoni" = "DROWI",
    "Entamoeba_histolytica" = "ENTHI",
    "Equus_caballus" = "EQUCA",
    "Escherichia_coliK12" = "ESCCO",
    "Fusarium_graminearum" = "FUSGR",
    "Gallus_gallus" = "GALGA",
    "Gasterosteus_aculeatus" = "GASAC",
    "Giardia_lamblia" = "GIALA",
    "Helobdella_robusta" = "HELRO",
    "Homo_sapiens" = "HOMSA",
    "Ixodes_scapularis" = "IXOSC",
    "Kluyveromyces_lactis" = "KLULA",
    "Leishmania_major" = "LEIMA",
    "Lottia_gigantea" = "LOTGI",
    "Macaca_mulatta" = "MACMU",
    "Magnaporthe_grisea" = "MAGGR",
    "Monodelphis_domestica" = "MONDO",
    "Monosiga_brevicollis" = "MONBR",
    "Mus_musculus" = "MUSMU",
    "Nasonia_vitripennis" = "NASVI",
    "Nematostella_vectensis" = "NEMVE",
    "Neurospora_crassa" = "NEUCR",
    "Ornithorhynchus_anatinus" = "ORNAN",
    "Oryza_sativa" = "ORYSA",
    "Oryzias_latipes" = "ORYLA",
    "Ostreococcus_tauri" = "OSTTA",
    "Pan_troglodytes" = "PANTR",
    "Pediculus_humanus" = "PEDPA",
    "Physcomitrella_patens" = "PHYPA",
    "Phytophthora_ramorum" = "PHYRA",
    "Phytophthora_sojae" = "PHYSO",
    "Plasmodium_falciparum" = "PLAFA",
    "Plasmodium_vivax" = "PLAVI",
    "Pongo_pygmaeus" = "PONPY",
    "Populus_trichocarpa" = "POPTR",
    "Pristionchus_pacificus" = "PRIPA",
    "Puccinia_graminis" = "PUCGR",
    "Rattus_norvegicus" = "RATNO",
    "Rhizopus_oryzae" = "RHIOR",
    "Saccharomyces_cerevisiae" = "SACCE",
    "Schistosoma_mansoni" = "SCHMA",
    "Schizosaccharomyces_pombe" = "SCHPO",
    "Sclerotinia_sclerotiorum" = "SCLSC",
    "Sorghum_bicolor" = "SORBI",
    "Stagonospora_nodorum" = "STANO",
    "Strongylocentrotus_purpuratus" = "STRPU",
    "Takifugu_rubripes" = "TAKRU",
    "Tetrahymena_thermophila" = "TETTH",
    "Tetraodon_nigroviridis" = "TETNI",
    "Thalassiosira_pseudonana" = "THAPS",
    "Theileria_annulata" = "THEAN",
    "Theileria_parva" = "THEPA",
    "Tribolium_castaneum" = "TRICA",
    "Trichomonas_vaginalis" = "TRIVA",
    "Trichoplax_adhaerens" = "TRIAD",
    "Trypanosoma_cruzi" = "TRYCR",
    "Ustilago_maydis" = "USTMA",
    "Xenopus_tropicalis" = "XENTR",
    "Yarrowia_lipolytica" = "YARLI"
    )
  fields
}

.getBaseFiveCode <- function(baseSpecies){
  fiveMap <- .makeFiveLetterMapping()
  baseFiveCode <- fiveMap[names(fiveMap) %in% baseSpecies]
}

## helpers
.getLCcolnames <- function(x){
  con <- AnnotationDbi:::dbConn(x)
  list <- dbListTables(con)
  ## drop unwanted tables
  unwanted <- c("map_counts","map_metadata","metadata")
  list <- list[!list %in% unwanted]
  ## Add baseSpecies to the front...
  list <- c(.getBaseSpecies(x), list)
}

.inpCols <- function(x, type="all"){
  list <- .getLCcolnames(x)
  ## Then just to format things in the usual way
  list <- toupper(list)
  if(type=="noBase"){
    baseSpecies <- .getBaseSpecies(x)  
    list <- list[!(list %in% toupper(baseSpecies))] 
  }
  list
}


setMethod("cols", "InparanoidDb", function(x){.inpCols(x)})

## currently, I think the base type CAN be a valid keytype.
setMethod("keytypes", "InparanoidDb", function(x){.inpCols(x)})
## setMethod("keytypes", "InparanoidDb", function(x){.inpCols(x,type="noBase")})
          


## new helper to map Table names to UC names used by cols and keytypes
.getTableNames <- function(x){
  LC <- .getLCcolnames(x)
  UC <- .inpCols(x)
  names(UC) <- LC
  UC
}

.keysInp <- function(x, keytype){
  ## translate keytype back to table name
  tabNames <- .getTableNames(x)
  lckeytype <- names(tabNames[tabNames %in% keytype])
  ## get the baseSpecies
  baseSpecies <- .getBaseSpecies(x)
  baseFiveCode <- .getBaseFiveCode(baseSpecies)
  ## get connection to DB
  con <- dbConn(x)
  if(baseSpecies != lckeytype){
    sql <- paste("SELECT inp_id FROM", lckeytype,
                 paste0("WHERE species!='",baseFiveCode,"'"))
    res <- dbGetQuery(con, sql)
    res <- as.vector(t(res))
  }else{
    res <- character()
    fiveMap <- .makeFiveLetterMapping()
    ## remove the baseSpecies from fiveMap names to get all real tables
    tables <- names(fiveMap)[!(names(fiveMap) %in% baseSpecies)]  
    for(i in seq_len(length(tables))){
      sql <- paste("SELECT inp_id FROM", tables[i],
                   paste0("WHERE species=='",baseFiveCode,"'"))
      rs <- dbGetQuery(con, sql)
      rs <- as.vector(t(rs))
      res <- unique(c(res, rs)) ## should not be too bad
    }
  }
  res
}

setMethod("keys", "InparanoidDb",
    function(x, keytype){
      if(missing(keytype)){stop("Please supply a keytype argument.")}
      .keysInp(x, keytype)
    }
)



##############################################################################
## Select is more complicated, but I should be able to implement it similar to
## how I did it for Reactome.db

.UCToStandard <- function(strVec){
  strVec <- tolower(strVec)
  firstLett <- toupper(substr(strVec,1,1))
  rest <- substr(strVec,2,nchar(strVec))
  paste0(firstLett, rest)
}

## This is for making simple queries
## SELECT base.inp_id,alt.inp_id FROM
## (SELECT * FROM Mus_musculus WHERE species='HOMSA' AND seed_status='100%')
## AS base,
## (SELECT * FROM Mus_musculus WHERE species='MUSMU' AND seed_status='100%')
## AS alt WHERE base.clust_id=alt.clust_id
## AND alt.inp_id IN ('ENSMUSP00000097561','ENSMUSP00000051825','ENSMUSP00000074773','ENSMUSP00000074340','ENSMUSP00000093245','ENSMUSP00000095433');
## The trouble is that this query seems extremely slow...  Addding an index
## helps, but it is still awful.

## .extractWithSimpleInpQuery <- function(x, table, keys, keytype,
##                                        baseFiveCode, fiveMap){
##   fiveCode <- fiveMap[names(fiveMap) %in% table]
##   subQueryBase <- paste0("(SELECT * FROM ", table,
##                         " WHERE species='",baseFiveCode,
##                         "' AND seed_status='100%')")
##   subQueryAlt <- paste0("(SELECT * FROM ", table,
##                         " WHERE species='",fiveCode,
##                        "' AND seed_status='100%')")
##   ## base or not? - based on the keytype (always one of two options)
##   inTableClause <- character()
##   fiveMap <- .makeFiveLetterMapping()
##   if(keytype==names(fiveMap)[fiveMap %in% baseFiveCode]){
##     inTableClause <- 'base.inp_id'
##   }else{
##     inTableClause <- 'alt.inp_id'
##   }
##   ## generate a simple query for each table
##   sql <- paste("SELECT base.inp_id,alt.inp_id FROM ",
##                subQueryBase, "AS base,", subQueryAlt,
##                "AS alt WHERE base.clust_id=alt.clust_id",
##                "AND",inTableClause,"IN",               
##                paste0("('",paste(keys, collapse="','"),"')"))
##   ## then extract
##   res <- dbQuery(dbConn(x), sql)
## }


###########################################################################
## New idea:
## It might actually be faster to just get the subqueries for each in another
## set of helpers, and then to merge them in R on the clust_ids...
## This is indeed about a billion times faster.

.extractWithSimpleInpQuery <- function(x, table, keys, keytype,
                                       baseFiveCode, fiveMap){
  fiveCode <- fiveMap[names(fiveMap) %in% table]

  ## Base query for Alt portion
  subQueryAlt <- paste0("SELECT * FROM ", table,
                        " AS alt WHERE species='",fiveCode,
                        "' AND seed_status='100%'")

  ## Base query for base portion
  subQueryBase <- paste0("SELECT * FROM ", table,
                         " AS base WHERE species='",baseFiveCode,
                         "' AND seed_status='100%'")
  
  
  ## Clause to append to whichever of these matches the keytype
  inClause <- paste( "AND inp_id IN",               
               paste0("('",paste(keys, collapse="','"),"')"))
  ## base or not?  WHO gets the inClause?
  fiveMap <- .makeFiveLetterMapping()
  if(keytype==names(fiveMap)[fiveMap %in% baseFiveCode]){
    subQueryBase <- paste(subQueryBase, inClause)
  }else{
    subQueryAlt <- paste(subQueryAlt, inClause)
  }
  
  ## then extract
  resBase <- dbQuery(dbConn(x), subQueryBase)
  resAlt <- dbQuery(dbConn(x), subQueryAlt)
  ## then merge as an inner join on clust_id for each
  res <- merge(resBase, resAlt, by.x="clust_id", by.y="clust_id")
  res <- res[,c("inp_id.x","inp_id.y")]
  colnames(res) <- c("base.inp_id",fiveCode)
  res
}



## This calls .extractWithSimpleInpQuery for each table and merges results
.collateInpQueryResults <- function(x, tables, keys, keytype, fiveMap,
                                    baseFiveCode, baseSpecies){  
  mergeID <- "base.inp_id"
  res <- data.frame()
  for(i in seq_len(length(tables))){
    if(i==1){
      if(tables[i]==baseSpecies){
        ## This means that my keytype == baseSpecies
        res <- keys(x, keytype=toupper(baseSpecies))
        res <- res[res %in% keys]
        res <- as.data.frame(res)
        colnames(res) <- mergeID
      }else{ ## Otherwise, we must query the DB
        res <- .extractWithSimpleInpQuery(x, tables[i], keys, keytype,
                                          baseFiveCode, fiveMap)
      }
    }else{
      if(i==2){ ## on 2nd pass, set up these vals
        mergeKeys <- res[[mergeID]] 
        mkeytype <- baseSpecies
      }
      if(tables[i] == baseSpecies){
        ## This means that one of later cols == baseSpecies
        res <- cbind(res, res[mergeID])
        colnames(res)[dim(res)[2]] <- baseFiveCode
      }else{
        res <- merge(res,
                     .extractWithSimpleInpQuery(x, tables[i], mergeKeys,
                                                mkeytype, baseFiveCode,
                                                fiveMap),
                     by.x=mergeID, by.y=mergeID,
                     all.x=TRUE, all.y=TRUE)
      }
    }
  }
  ## last thing is to put baseFiveCode instead of "base.inp_id"
  colnames(res)[colnames(res) == "base.inp_id"] <- baseFiveCode
  res
}


## function for making select happen
.selectInp <- function(x, keys, cols, keytype){
  ## check that the keys are of the correct keytype.
  .testIfKeysAreOfProposedKeytype(x, keys, keytype)
  ## filter out keys that are not legit (just from the DB query)
  ktKeys = keys(x, keytype=keytype)
  qkeys <- keys[keys %in% ktKeys]
  
  ## now I need to go through each table, and for each I want to extract the
  ## missing piece with a SIMPLE query (basically get ONE thing), and then
  ## append it onto the results
  baseSpecies <- .getBaseSpecies(x)  
  fiveMap <- .makeFiveLetterMapping()
  baseFiveCode <- .getBaseFiveCode(baseSpecies)

  ## collate possible types (type must ALWAYS be in front)
  tables <-unique(c(keytype,cols[!(cols %in% keytype)]))
  
  res <- .collateInpQueryResults(x, .UCToStandard(tables), qkeys, keytype,
                                 fiveMap, baseFiveCode, baseSpecies)

  ## Setup to call .resort
  ## reqCols must have exactly the same stuff as in tables, but in same format
  ## as header from res
  reqCols <- fiveMap[toupper(names(fiveMap)) %in% tables]

  ## colType is the table abbreviation that matches the initial keytype.
  colType <- fiveMap[toupper(names(fiveMap)) %in% keytype]
  
  ## now drop any unrequested cols
  res <- res[,reqCols,drop=FALSE]
  ## And then resort/tidy etc.
  res <- .resort(res, keys, jointype=colType, reqCols=reqCols)

  ## Then match to the colnames
  colnames(res) <- toupper(names(fiveMap)[match(colnames(res),fiveMap)])
 
  ## return results
  res            
}


## select method
setMethod("select", "InparanoidDb",
    function(x, keys, cols, keytype){
          if (missing(keytype)) keytype <- "ENTREZID"
          .selectInp(x, keys, cols, keytype)
        }
)






## test:
## library(hom.Hs.inp.db); i <- hom.Hs.inp.db;  cols(i); keytypes(i); k= head(keys(i, "MUS_MUSCULUS"));

## select(i, keys=k, cols=c("APIS_MELLIFERA","AEDES_AEGYPTI"), keytype="MUS_MUSCULUS")

## debug(AnnotationDbi:::.selectInp)

## debug(AnnotationDbi:::.extractWithSimpleInpQuery)

## debug(AnnotationDbi:::.collateInpQueryResults)

## working on keys - fixed
## debug(AnnotationDbi:::.keysInp)
## res2 <- head(keys(i, keytype="HOMO_SAPIENS"))



## TODO: make a slew of unit tests similar to those for ReactomeDb





## this still no worky (and maybe it really shouldn't - because the use of
## humans as a keytype means I have to start with the table (in their list)
## that has the most human keys mapped and then go to the next and the next
## etc.  In that case, the path I choose for them would influence the output.
## I don't think we want that kind of responsibility...  For other keys, it's
## OK, because it is a human centered DB, and the human IDs are therefore
## natural as a universal key.  But when human IDs are the central ID And ALSO
## the keytype - this creates a problem because that 1st step can change all
## of the results...
## So I am pretty confident that I want to ban the baseSpecies from the keytypes.
## So TODO?: drop baseSpecies from the keytypes?
## PROBLEM with this idea: HOMO_SAPIENS is a valid keytype for keys()! (and
## really should be)
## For now, I think it's OK if this works for keys (the answer is legit).  But
## it will still not be listed as a legit keytype (even while one of the
## methods works).

## As for cols, I should be able to still have baseSpecies be a valid value
## for cols.







## library(hom.Hs.inp.db); i <- hom.Hs.inp.db;  cols(i); keytypes(i); k= head(keys(i, "MUS_MUSCULUS"));


## now this works
## select(i, keys=k, cols=c("APIS_MELLIFERA","HOMO_SAPIENS"), keytype="MUS_MUSCULUS")



## And this too.
## hk <- head(head(keys(i, keytype="HOMO_SAPIENS")))
## select(i, keys=hk, cols=c("APIS_MELLIFERA","MUS_MUSCULUS"), keytype="HOMO_SAPIENS")



