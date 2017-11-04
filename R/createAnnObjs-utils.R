### All the createAnnObjs.*_DB() functions currently support the same DB
### schema version (of course, each function support its own schema).
DBSCHEMAVERSION <- "2.1"

checkDBSCHEMA <- function(dbconn, DBSCHEMA)
{
    schema <- dbmeta(dbconn, "DBSCHEMA")
    if (schema != DBSCHEMA)
        stop("invalid DB schema (found ", schema, ", expected ", DBSCHEMA, ")")
    version <- dbmeta(dbconn, "DBSCHEMAVERSION")
    if (version != DBSCHEMAVERSION)
        stop("invalid DB schema version (found ", version, ", expected ", DBSCHEMAVERSION, ")")
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### 
###

Go3tablenames <- function(all=FALSE)
{
    tablenames <- c("go_bp", "go_cc", "go_mf")
    if (all)
        tablenames <- paste0(tablenames, "_all")
    names(tablenames) <- c("BP", "CC", "MF")
    tablenames
}

makeGo3L2Rchain <- function(L2Rchain, tablename, ontology)
{
    chainlen <- length(L2Rchain)
    L2Rchain[[chainlen]]@tablename <- tablename
    L2Rchain[[chainlen]]@Rattribnames["Ontology"] <- paste0("'", ontology, "'")
    L2Rchain
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
###

### If 'envir' is not NULL then the created objects are assigned to it.
### TODO: The function should check for name clashes
createAnnObjs <- function(class, seeds, seed0, envir=NULL)
{
    if (is.null(envir))
        envir <- new.env(hash=TRUE, parent=emptyenv())
    for (seed in seeds) {
        seed$Class <- class
        for (slot in names(seed0)) {
            if (is.null(seed[slot][[1]]))
                seed[[slot]] <- seed0[[slot]]
        }
        envir[[seed$objName]] <- do.call(new, seed)
    }
    envir
}

## Function to get all these create statements out of the namespace.
createAnnObjs.SchemaChoice = function(schema, prefix, target, dbconn, datacache){
    FUN <- get(paste0("createAnnObjs.", schema))
    FUN(prefix, target, dbconn, datacache)
}

createAnnDbBimap <- function(seed, seed0)
{
    for (slot in names(seed0)) {
        if (is.null(seed[slot][[1]]))
            seed[[slot]] <- seed0[[slot]]
    }
    L2Rchain <- seed$L2Rchain
    seed$L2Rchain <- lapply(L2Rchain, function(L2Rlink) do.call("L2Rlink", L2Rlink))
    do.call(new, seed)
}

createAnnDbBimaps <- function(seeds, seed0, envir=NULL)
{
    if (is.null(envir))
        envir <- new.env(hash=TRUE, parent=emptyenv())
    for (seed in seeds)
        envir[[seed$objName]] <- createAnnDbBimap(seed, seed0)
    envir
}




### 3 special maps that are not AnnDbBimap objects (just named vectors).

createCHRLENGTHS <- function(dbconn, dbname="")
{
    if(dbname==""){ ##The usual case
        data <- dbGetTable(dbconn, "chrlengths")
    }else{ ##For when people are doing a "CROSS" joined package, we want data from the "org" DB
        data <- dbGetTable(dbconn, paste0(.mangleDBName(dbname),".chrlengths"))        
    }
    CHRLENGTHS <- data[["length"]]
    names(CHRLENGTHS) <- data[["chromosome"]]        
    CHRLENGTHS
}

createREJECTORF <- function(dbconn)
{
    data <- dbGetTable(dbconn, "reject_orf")
    data[["systematic_name"]]
}

createMAPCOUNTS <- function(dbconn, prefix)
{
    data <- dbGetTable(dbconn, "map_counts", "WHERE map_name != 'TOTAL' ORDER BY map_name")
    if(dim(data)[1]!=0){
        MAPCOUNTS <- data[["count"]]
        names(MAPCOUNTS) <- paste0(prefix, data[["map_name"]])
        return(MAPCOUNTS)
    }else{
        return(integer())
    }
}

### Rename all objects in the 'envir' environment by prefixing them
### with 'prefix'. The function is dumb i.e. it doesn't check for (neither
### doesn't try to avoid) possible name clashes. Note that those issues
### could be easily avoided by assigning the renamed objects to a separate
### environment but...
prefixAnnObjNames <- function(envir, prefix)
{
    keys <- ls(envir, all.names=TRUE)
    for (key in keys) {
        new_key <- paste0(prefix, key)
        envir[[new_key]] <- envir[[key]]
    }
    remove(list=keys, envir=envir) # remove old keys
    envir
}



### Populate the huge list of tables neede by the homology packages.
makeSeedList <- function(species, fields)
{
    INPARANOID_DB_AnnDbBimap_seeds <- list()
    
    for(i in 1:length(fields)){
       INPARANOID_DB_AnnDbBimap_seeds[[i]] <- list(                                   
                objName=toupper(fields[i]),
                Class="InpAnnDbBimap",
                L2Rchain=list(          
                  list(
                       tablename=names(fields)[i],
                       Lcolname="inp_id",
                       Rcolname="clust_id",
                       filter=as.character(paste0("{seed_status}='100%' AND ", "{species}=","'",species,"'"))
                       ),
                  list(
                       tablename=names(fields)[i],
                       Lcolname="clust_id",
                       Rcolname="inp_id",
                       filter=as.character(paste0("{seed_status}='100%' AND ","{species}=","'",fields[i],"'"))
                       )
                  )
           )
    }

    INPARANOID_DB_AnnDbBimap_seeds   
}


#####################################################################
## Below are helper functions to define helpful startup messages for
## especially ornery packages:

## TODO: for these organsism add a waring about Blast2GO (being stale)
## taxIDList = c("Canis_familiaris"="9615",
##                 "Sus_Scrofa"="9823",
##                 "Anopheles_gambiae"="180454",
##                 "Xenopus_laevis"="8355",
##                 "Macaca_mulatta"="9544",
##                 "Pan_troglodytes"="9598")
## ##                 "Escherichia_coli_K12"="511145")



choosePackage <- function(pkgType){
## defines a blackList to choose the type of message based on the pkg name.
## If you are not in the blackList, then we don't need a message.
## Default fallthrough message means there is no message.
  type <- switch(EXPR = pkgType,           
                 "ecoli2.db" = "partial",
                 "hugene10stprobeset.db" = "exon_probeset",
                #"hugene10stv1cdf" = "unsupported", ## not AnnotationDbi
                 "hugene11stprobeset.db" = "exon_probeset",
                 "lumiHumanAll.db" = "lumi",
                 "lumiHumanIDMapping.db" = "lumi",
                 "lumiMouseAll.db" = "lumi",
                 "lumiMouseIDMapping.db" = "lumi",
                 "lumiRatAll.db" = "lumi",
                 "lumiRatIDMapping.db" = "lumi",
                 "mogene10stprobeset.db" = "exon_probeset",
                 "mogene11stprobeset.db" = "exon_probeset",
                 "ragene10stprobeset.db" = "exon_probeset",
                 "ragene11stprobeset.db" = "exon_probeset",
		 "HuExExonProbesetLocation" = "hg19",
		 "GGHumanMethCancerPanelv1.db" = "deprecated",
		 "HuExExonProbesetLocationHg18" = "deprecatedRaffaeleCalogero",
		 "HuExExonProbesetLocationHg19" = "deprecatedRaffaeleCalogero",
		 "illuminaHumanv1BeadID.db" = "deprecatedMarkDunning",
		 "illuminaHumanv2BeadID.db" = "deprecatedMarkDunning",
		 "illuminaHumanv3BeadID.db" = "deprecatedMarkDunning",
		 "illuminaHumanv4BeadID.db" = "deprecatedMarkDunning",
		 "illuminaMousev1BeadID.db" = "deprecatedMarkDunning",
		 "illuminaMousev1p1BeadID.db" = "deprecatedMarkDunning",
		 "illuminaMousev2BeadID.db" = "deprecatedMarkDunning",
		 "illuminaRatv1BeadID.db" = "deprecatedMarkDunning",
                 ## Simon, Ferrari and Favero can get a pass till next time
                 ## Pass given on 11/8/10
                 ## "hs25kresogen.db" = "deprecatedSimonDeBernard",
                 ## "mm24kresogen.db" = "deprecatedSimonDeBernard",
                 "org.Cf.eg" = "B2GOstale",
                 "org.Ss.eg" = "B2GOstale",
                 "org.Ag.eg" = "B2GOstale",
                 "org.Xl.eg" = "B2GOstale",
                 "org.Mmu.eg" = "B2GOstale",
                 "org.Pt.eg" = "B2GOstale",
                 "canine.db" = "B2GOstale",
                 "canine2.db" = "B2GOstale",
                 "xlaevis.db" = "B2GOstale",
                 ## End of the B2GO warnings (remove once problem is resolved)
                 "org.Sco.eg.db" = "deprecated",
                 "KEGG.db" = "keggstale",
                 "NO_MESSAGE_TYPE"
                )
}


## This function defines stock messages that can be associated with certain
## packages.
## It can also be called directly by people who are outside of the system but
## want a stock message
annotMessage <- function(msgType, pkgType){
  msg <- switch(EXPR = msgType,           
                "deprecated" = paste("\n",pkgType,"is an older package, and", 
	 	  "the package contributor no longer supports it. Please", 
		  "be advised that this package is now out of date and should",
		  "not occur in future bioconductor releases."),
                "deprecatedSimonDeBernard" = paste("\n",pkgType,"is",
		  "an older package, and the package contributor no",
		  "longer supports it. Please be advised that this",
		  "package is now out of date and should not occur in",
		  "future bioconductor releases."),
		"deprecatedRaffaeleCalogero" = paste("\n",pkgType,"is",
		  "an older package, and the package contributor no",
		  "longer supports it. Please be advised that this",
		  "package is now out of date and should not occur in",
		  "future bioconductor releases.  For a more current",
		  "package containing the same kind of data, please use",
		  "the HuExExonProbesetLocation package instead"),
		"deprecatedMarkDunning" = paste("\n",pkgType,"is an",
		  "older package, and the package contributor no longer",
		  "supports it. Please be advised that this package is",
		  "now out of date and should not occur in future",
		  "bioconductor releases.  For a more current package",
		  "containing the same kind of data, please use the",
		  "appropriate illumina_XXX_.db packages instead of the",
		  "illumina_XXX_BeadID.db packages and make use of the new",
		  "illumina_XXX_ARRAYADDRESS mapping to convert between", 
		  "BeadIDs and Illumina IDs. \n\n  *_XXX_ here refers to the",
  		  "species and version for this platform"),
                "keggstale" = paste("\n",pkgType,"contains mappings based on",
                  " older",
                  "data because the original resource was removed from the",
                  "the public domain before the most recent update was",
                  "produced. This package should now be considered deprecated",
                  "and future versions of Bioconductor may not have it",
                  "available.  Users who want more current data are encouraged",
                  "to look at the KEGGREST or reactome.db packages"),
                "B2GOstale" = paste("\n",pkgType,"contains GO mappings based",
                  "on older data",
                  "because the Blast2GO data resource was removed from",
                  "the public domain just before the most recent update was",
                  "produced. We are working on an alternative means to get",
                  "this kind of data before the next release."),
                "exon_probeset" = paste("\n",pkgType,"is based on exon",
                  "probesets. For a more gene-centric view, use the",
                  "transcriptcluster version of this package."),
                "unsupported" = paste("\n The data",
                  "in this package are based on mapping files labeled as",
                  "'unsupported' by their source."),
                "lumi" = paste("\n",pkgType,"is using or is likely to",
                  "need access to special nuID identifiers.  Users can learn",
                  "about these identifiers from vignette documentation",
                  "provided with the lumi package."),
                "partial" = paste("\n",pkgType,"is providing annotations",
                  "for only one of the species that are supported by this",
                  "platform. You may want to get other annotations from other",
                  "sources/packages in order to cover all the species that",
                  "are represented by probes on this platform."),
		"hg19" = paste("\n The",pkgType,"package was build to match",
 		   "the HG19 build."),
                "NO_MESSAGE_TYPE" = ""
                )

  if(msg != ""){
    msg <- paste0("\n", paste(strwrap(msg, exdent=2), collapse="\n"),
                  "\n")
  }
}

annoStartupMessages <- function(pkgType){
  msgType <- choosePackage(pkgType)
  annotMessage(msgType, pkgType)
}


## function for name-mangling (or not name-mangling depending on how people
## want their DB objects named).  Basically this code exists so that I can
## change this without having to alter a ton of files whenever someone decides
## that they don't like our name convention.
dbObjectName <- function(pkgname, dbType){
  ## if(dbType=="ChipDb"){
  ##   names <- strsplit(pkgname, split="\\.")
  ##   newName <- paste(paste(unlist(names)[1],collapse="_"),
  ##                    dbType,sep="_")    
  ## }else{
  ##   names <- strsplit(pkgname, split="\\.")
  ##   newName <- paste(paste(unlist(names)[c(2,3)],collapse="_"),
  ##                    dbType,sep="_")
  ## }
  ## newName
  
  ## For now we will just use the package name
  pkgname
}
