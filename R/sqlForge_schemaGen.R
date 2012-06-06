
#presently this is the formula for HUMANCHIP_DB
popHUMANCHIPDB <- function(affy,
                           prefix,
                           fileName,
                           chipMapSrc = system.file("extdata", "chipmapsrc_human.sqlite", package="human.db0"),
                           chipSrc = system.file("extdata", "chipsrc_human.sqlite", package="human.db0"),
                           metaDataSrc,
                           otherSrc=character(0),
                           baseMapType="gbNRef",
                           outputDir=".",
                           printSchema=FALSE){

    if(affy==TRUE){
        getMapForBiocChipPkg(
                             csvFileName=fileName,
                             pkgName=prefix,
                             chipMapSrc=chipMapSrc,
                             chipSrc=chipSrc,
                             otherSrc=otherSrc,
                             baseMapType=baseMapType,
                             outputDir=outputDir
                             )
    }
    else if(affy==FALSE){
        getMapForOtherChipPkg(
                              filePath=fileName,
                              pkgName=prefix,
                              chipMapSrc=chipMapSrc,
                              chipSrc=chipSrc,
                              otherSrc=otherSrc,
                              baseMapType=baseMapType,
                              outputDir=outputDir                                
                              )
    }

    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="probes","coreID"="probe_id","suffix"="PROBE","org"="human","cntrTab"="genes", "prefix"=prefix, "outDir"=outputDir, "Db_type"="ChipDb")    
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )
    
    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)
    appendGenes(db, subStrs=subStrs, printSchema=printSchema)
    appendProbes(db, subStrs=subStrs, printSchema=printSchema)
    appendGeneInfo(db, subStrs=subStrs, printSchema=printSchema)

    appendChromosomes(db, subStrs=subStrs, printSchema=printSchema)
    appendCytogenicLocs(db, subStrs=subStrs, printSchema=printSchema)
    appendOmim(db, subStrs=subStrs, printSchema=printSchema)
    appendRefseq(db, subStrs=subStrs, printSchema=printSchema)
    appendPubmed(db, subStrs=subStrs, printSchema=printSchema)
    appendUnigene(db, subStrs=subStrs, printSchema=printSchema)
    appendChrlengths(db, subStrs=subStrs, printSchema=printSchema)
    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)
    appendChromsomeLocs(db, subStrs=subStrs, printSchema=printSchema)
    appendPfam(db, subStrs=subStrs, printSchema=printSchema)
    appendProsite(db, subStrs=subStrs, printSchema=printSchema)
    appendAlias(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsembl(db, subStrs=subStrs, printSchema=printSchema)
    appendUniprot(db, subStrs=subStrs, printSchema=printSchema)

    appendPostMeta(db, subStrs=subStrs)
    
    simplifyProbes(db, subStrs=subStrs)
    dropRedundantTables(db, subStrs=subStrs)    
    dbDisconnect(db)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)
}


#This is the formula for HUMAN_DB  
popHUMANDB <- function(prefix,
                       chipSrc = system.file("extdata", "chipsrc_human.sqlite", package="human.db0"),
                       metaDataSrc,
                       outputDir=".",
                       printSchema=FALSE){

    makeUniversalMapping(pkgName=prefix,
                         chipSrc=chipSrc,
                         outputDir=outputDir)

    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="genes","coreID"="gene_id","suffix"="EG","org"="human","cntrTab"="genes", "prefix"=prefix, "outDir"=outputDir, "Db_type"="OrgDb")
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )

    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)
    appendGenes(db, subStrs=subStrs, printSchema=printSchema)
    appendGeneInfo(db, subStrs=subStrs, printSchema=printSchema)

    appendChromosomes(db, subStrs=subStrs, printSchema=printSchema)
    appendAccessions(db, subStrs=subStrs, printSchema=printSchema)
    appendCytogenicLocs(db, subStrs=subStrs, printSchema=printSchema)
    appendOmim(db, subStrs=subStrs, printSchema=printSchema)
    appendRefseq(db, subStrs=subStrs, printSchema=printSchema)
    appendPubmed(db, subStrs=subStrs, printSchema=printSchema)
    appendUnigene(db, subStrs=subStrs, printSchema=printSchema)
    appendChrlengths(db, subStrs=subStrs, printSchema=printSchema)
    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)
    appendChromsomeLocs(db, subStrs=subStrs, printSchema=printSchema)
    appendPfam(db, subStrs=subStrs, printSchema=printSchema)
    appendProsite(db, subStrs=subStrs, printSchema=printSchema)
    appendAlias(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsembl(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsembl2NCBI(db, subStrs=subStrs, printSchema=printSchema)
    appendNCBI2Ensembl(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsemblProt(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsemblTrans(db, subStrs=subStrs, printSchema=printSchema)
    appendUniprot(db, subStrs=subStrs, printSchema=printSchema)
    appendUCSCGenes(db, subStrs=subStrs, printSchema=printSchema)
    makeGOViews(db)
    
    appendPostMeta(db, subStrs=subStrs)
    
    dbDisconnect(db)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)
}



#This is the formula for MOUSECHIP_DB
popMOUSECHIPDB <- function(affy,
                           prefix,
                           fileName,
                           chipMapSrc = system.file("extdata", "chipmapsrc_mouse.sqlite", package="mouse.db0"),
                           chipSrc = system.file("extdata", "chipsrc_mouse.sqlite", package="mouse.db0"),
                           metaDataSrc,
                           otherSrc=character(0),
                           baseMapType="gbNRef",
                           outputDir=".",
                           printSchema=FALSE){

    if(affy==TRUE){
        getMapForBiocChipPkg(
                             csvFileName=fileName,
                             pkgName=prefix,
                             chipMapSrc=chipMapSrc,
                             chipSrc=chipSrc,
                             otherSrc=otherSrc,
                             baseMapType=baseMapType,
                             outputDir=outputDir
                             )
    }
    else if(affy==FALSE){
        getMapForOtherChipPkg(
                              filePath=fileName,
                              pkgName=prefix,
                              chipMapSrc=chipMapSrc,
                              chipSrc=chipSrc,
                              otherSrc=otherSrc,
                              baseMapType=baseMapType,
                              outputDir=outputDir                                
                              )
    }

    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="probes","coreID"="probe_id","suffix"="PROBE","org"="mouse","cntrTab"="genes", "prefix"=prefix, "outDir"=outputDir, "Db_type"="ChipDb")
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )
    
    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)
    appendGenes(db, subStrs=subStrs, printSchema=printSchema)
    appendProbes(db, subStrs=subStrs, printSchema=printSchema)
    appendGeneInfo(db, subStrs=subStrs, printSchema=printSchema)

    appendChromosomes(db, subStrs=subStrs, printSchema=printSchema)
    appendCytogenicLocs(db, subStrs=subStrs, printSchema=printSchema)
    appendRefseq(db, subStrs=subStrs, printSchema=printSchema)
    appendPubmed(db, subStrs=subStrs, printSchema=printSchema)
    appendUnigene(db, subStrs=subStrs, printSchema=printSchema)
    appendChrlengths(db, subStrs=subStrs, printSchema=printSchema)
    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)
    appendChromsomeLocs(db, subStrs=subStrs, printSchema=printSchema)
    appendPfam(db, subStrs=subStrs, printSchema=printSchema)
    appendProsite(db, subStrs=subStrs, printSchema=printSchema)
    appendAlias(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsembl(db, subStrs=subStrs, printSchema=printSchema)
    appendUniprot(db, subStrs=subStrs, printSchema=printSchema)
    appendMGI(db, subStrs=subStrs, printSchema=printSchema)
    
    appendPostMeta(db, subStrs=subStrs)
    
    simplifyProbes(db, subStrs=subStrs)
    dropRedundantTables(db, subStrs=subStrs)
        
    dbDisconnect(db)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)
}


#This is the formula for MOUSE_DB  
popMOUSEDB <- function(prefix,
                       chipSrc = system.file("extdata", "chipsrc_mouse.sqlite", package="mouse.db0"),
                       metaDataSrc,
                       outputDir=".",
                       printSchema=FALSE){

    makeUniversalMapping(pkgName=prefix,
                         chipSrc=chipSrc,
                         outputDir=outputDir)

    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="genes","coreID"="gene_id","suffix"="EG","org"="mouse","cntrTab"="genes", "prefix"=prefix, "outDir"=outputDir, "Db_type"="OrgDb")
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )

    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)
    appendGenes(db, subStrs=subStrs, printSchema=printSchema)
    appendGeneInfo(db, subStrs=subStrs, printSchema=printSchema)

    appendChromosomes(db, subStrs=subStrs, printSchema=printSchema)
    appendAccessions(db, subStrs=subStrs, printSchema=printSchema)
    appendCytogenicLocs(db, subStrs=subStrs, printSchema=printSchema)
    appendRefseq(db, subStrs=subStrs, printSchema=printSchema)
    appendPubmed(db, subStrs=subStrs, printSchema=printSchema)
    appendUnigene(db, subStrs=subStrs, printSchema=printSchema)
    appendChrlengths(db, subStrs=subStrs, printSchema=printSchema)
    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)
    appendChromsomeLocs(db, subStrs=subStrs, printSchema=printSchema)
    appendPfam(db, subStrs=subStrs, printSchema=printSchema)
    appendProsite(db, subStrs=subStrs, printSchema=printSchema)
    appendAlias(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsembl(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsembl2NCBI(db, subStrs=subStrs, printSchema=printSchema)
    appendNCBI2Ensembl(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsemblProt(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsemblTrans(db, subStrs=subStrs, printSchema=printSchema)
    appendUniprot(db, subStrs=subStrs, printSchema=printSchema)
    appendUCSCGenes(db, subStrs=subStrs, printSchema=printSchema)
    appendMGI(db, subStrs=subStrs, printSchema=printSchema)
    makeGOViews(db)
    
    appendPostMeta(db, subStrs=subStrs)
    
    dbDisconnect(db)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)    
}



#presently this is the formula for RATCHIP_DB
popRATCHIPDB <- function(affy,
                         prefix,
                         fileName,
                         chipMapSrc = system.file("extdata", "chipmapsrc_rat.sqlite", package="rat.db0"),
                         chipSrc = system.file("extdata", "chipsrc_rat.sqlite", package="rat.db0"),
                         metaDataSrc,
                         otherSrc=character(0),
                         baseMapType="gbNRef",
                         outputDir=".",
                         printSchema=FALSE){

    if(affy==TRUE){
        getMapForBiocChipPkg(
                             csvFileName=fileName,
                             pkgName=prefix,
                             chipMapSrc=chipMapSrc,
                             chipSrc=chipSrc,
                             otherSrc=otherSrc,
                             baseMapType=baseMapType,
                             outputDir=outputDir
                             )
    }
    else if(affy==FALSE){
        getMapForOtherChipPkg(
                              filePath=fileName,
                              pkgName=prefix,
                              chipMapSrc=chipMapSrc,
                              chipSrc=chipSrc,
                              otherSrc=otherSrc,
                              baseMapType=baseMapType,
                              outputDir=outputDir                                
                              )
    }

    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="probes","coreID"="probe_id","suffix"="PROBE","org"="rat","cntrTab"="genes", "prefix"=prefix, "outDir"=outputDir, "Db_type"="ChipDb")    
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )    
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )
    
    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)
    appendGenes(db, subStrs=subStrs, printSchema=printSchema)
    appendProbes(db, subStrs=subStrs, printSchema=printSchema)
    appendGeneInfo(db, subStrs=subStrs, printSchema=printSchema)

    appendChromosomes(db, subStrs=subStrs, printSchema=printSchema)
    appendCytogenicLocs(db, subStrs=subStrs, printSchema=printSchema)
    appendRefseq(db, subStrs=subStrs, printSchema=printSchema)
    appendPubmed(db, subStrs=subStrs, printSchema=printSchema)
    appendUnigene(db, subStrs=subStrs, printSchema=printSchema)
    appendChrlengths(db, subStrs=subStrs, printSchema=printSchema)
    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)
    appendChromsomeLocs(db, subStrs=subStrs, printSchema=printSchema)
    appendPfam(db, subStrs=subStrs, printSchema=printSchema)
    appendProsite(db, subStrs=subStrs, printSchema=printSchema)
    appendAlias(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsembl(db, subStrs=subStrs, printSchema=printSchema)
    appendUniprot(db, subStrs=subStrs, printSchema=printSchema)

    appendPostMeta(db, subStrs=subStrs)
    
    simplifyProbes(db, subStrs=subStrs)
    dropRedundantTables(db, subStrs=subStrs)
        
    dbDisconnect(db)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)    
}

#This is the formula for RAT_DB  
popRATDB <- function(prefix,
                     chipSrc = system.file("extdata", "chipsrc_rat.sqlite", package="rat.db0"),
                     metaDataSrc,
                     outputDir=".",
                     printSchema=FALSE){

    makeUniversalMapping(pkgName=prefix,
                         chipSrc=chipSrc,
                         outputDir=outputDir)

    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="genes","coreID"="gene_id","suffix"="EG","org"="rat","cntrTab"="genes", "prefix"=prefix, "outDir"=outputDir, "Db_type"="OrgDb")
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )

    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)
    appendGenes(db, subStrs=subStrs, printSchema=printSchema)
    appendGeneInfo(db, subStrs=subStrs, printSchema=printSchema)

    appendChromosomes(db, subStrs=subStrs, printSchema=printSchema)
    appendAccessions(db, subStrs=subStrs, printSchema=printSchema)
    appendCytogenicLocs(db, subStrs=subStrs, printSchema=printSchema)
    appendRefseq(db, subStrs=subStrs, printSchema=printSchema)
    appendPubmed(db, subStrs=subStrs, printSchema=printSchema)
    appendUnigene(db, subStrs=subStrs, printSchema=printSchema)
    appendChrlengths(db, subStrs=subStrs, printSchema=printSchema)
    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)
    appendChromsomeLocs(db, subStrs=subStrs, printSchema=printSchema)
    appendPfam(db, subStrs=subStrs, printSchema=printSchema)
    appendProsite(db, subStrs=subStrs, printSchema=printSchema)
    appendAlias(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsembl(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsembl2NCBI(db, subStrs=subStrs, printSchema=printSchema)
    appendNCBI2Ensembl(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsemblProt(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsemblTrans(db, subStrs=subStrs, printSchema=printSchema)
    appendUniprot(db, subStrs=subStrs, printSchema=printSchema)
    makeGOViews(db)
    
    appendPostMeta(db, subStrs=subStrs)
    
    dbDisconnect(db)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)    
}




#presently this is the formula for ARABIDOPSISCHIP_DB
popARABIDOPSISCHIPDB <- function(affy,
                                 prefix,
                                 fileName="myFile.txt",
                                 chipMapSrc = system.file("extdata", "chipmapsrc_arabidopsis.sqlite", package="arabidopsis.db0"),
                                 chipSrc = system.file("extdata", "chipsrc_arabidopsis.sqlite", package="arabidopsis.db0"),
                                 metaDataSrc,
                                 outputDir=".",
                                 printSchema=FALSE){

    #This function needs some more work if we want it to be able to have other people use it
    #specifically the code for the next line must be generalized.
    getMapForArabidopsisChipPkg(affy = affy,
                                fileName = fileName,
                                pkgName = prefix,
                                chipMapSrc = chipMapSrc,
                                outputDir = outputDir)
    
    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="probes","coreID"="probe_id","suffix"="PROBE","org"="arabidopsis","cntrTab"="genes", "prefix"=prefix, "outDir"=outputDir, "Db_type"="ChipDb")    
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )
    
    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)
    appendArabidopsisGenes(db, subStrs=subStrs, printSchema=printSchema)           ##Arabidopsis requires a custom function.
    appendArabidopsisEntrezGenes(db, subStrs=subStrs, printSchema=printSchema)     ##Arabidopsis requires a custom function.
    appendArabidopsisProbes(db, subStrs=subStrs, printSchema=printSchema)          ##Arabidopsis requires a custom function.
    appendArabidopsisGeneInfo(db, subStrs=subStrs, printSchema=printSchema)        ##Arabidopsis requires a custom function.

    appendPubmed(db, subStrs=subStrs, printSchema=printSchema)
    appendChrlengths(db, subStrs=subStrs, printSchema=printSchema)
    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)
    appendChromsomeLocs(db, subStrs=subStrs, printSchema=printSchema)
    
    appendAraCyc(db, subStrs=subStrs, printSchema=printSchema)
    appendAraCycEnzyme(db, subStrs=subStrs, printSchema=printSchema)
    
    appendPostMeta(db, subStrs=subStrs)
    
    simplifyArabidopsisProbes(db, subStrs=subStrs)
    dropRedundantTables(db, subStrs=subStrs)
        
    dbDisconnect(db, subStrs=subStrs, printSchema=printSchema)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)    
}


#presently this is the formula for ARABIDOPSIS_DB
popARABIDOPSISDB <- function(prefix,
                             chipSrc = system.file("extdata", "chipsrc_arabidopsis.sqlite", package="arabidopsis.db0"),
                             metaDataSrc,
                             outputDir=".",
                             printSchema=FALSE){

    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="genes","coreID"="gene_id","suffix"="TAIR","org"="arabidopsis","cntrTab"="genes", "prefix"=prefix, "outDir"=outputDir, "Db_type"="OrgDb")    
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )

    sqliteQuickSQL(db, "CREATE TABLE probe_map (probe_id TEXT, gene_id TEXT, accession TEXT);")
    
    sqliteQuickSQL(db, "CREATE TABLE metadata (name VARCHAR(80) PRIMARY KEY, value VARCHAR(255) );")
    sqliteQuickSQL(db, paste("INSERT INTO metadata VALUES ('PKGNAME', '", prefix, "');", sep="", collapse=""))
    
    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)
    appendArabidopsisGenes(db, subStrs=subStrs, printSchema=printSchema)        ##Arabidopsis requires a custom function.
    appendArabidopsisEntrezGenes(db, subStrs=subStrs, printSchema=printSchema)  ##Arabidopsis requires a custom function.
    appendArabidopsisGeneInfo(db, subStrs=subStrs, printSchema=printSchema)     ##Arabidopsis requires a custom function.

    appendPubmed(db, subStrs=subStrs, printSchema=printSchema)
    appendChrlengths(db, subStrs=subStrs, printSchema=printSchema)
    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)
    appendChromsomeLocs(db, subStrs=subStrs, printSchema=printSchema)
    appendRefseq(db, subStrs=subStrs, printSchema=printSchema)
    appendAraCyc(db, subStrs=subStrs, printSchema=printSchema)
    appendAraCycEnzyme(db, subStrs=subStrs, printSchema=printSchema)
    appendPostMeta(db, subStrs=subStrs)
    makeGOViews(db)
    
    dbDisconnect(db, subStrs=subStrs, printSchema=printSchema)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)    
}







#presently this is the formula for FLYCHIP_DB
popFLYCHIPDB <- function(affy,
                         prefix,
                         fileName,
                         chipMapSrc = system.file("extdata", "chipmapsrc_fly.sqlite", package="fly.db0"),
                         chipSrc = system.file("extdata", "chipsrc_fly.sqlite", package="fly.db0"),
                         metaDataSrc,
                         otherSrc=character(0),
                         baseMapType="gbNRef",
                         outputDir=".",
                         printSchema=FALSE){

    if(affy==TRUE){
        getMapForBiocChipPkg(
                             csvFileName=fileName,
                             pkgName=prefix,
                             chipMapSrc=chipMapSrc,
                             chipSrc=chipSrc,
                             otherSrc=otherSrc,
                             baseMapType=baseMapType,
                             outputDir=outputDir
                             )
    }
    else if(affy==FALSE){
        getMapForOtherChipPkg(
                              filePath=fileName,
                              pkgName=prefix,
                              chipMapSrc=chipMapSrc,
                              chipSrc=chipSrc,
                              otherSrc=otherSrc,
                              baseMapType=baseMapType,
                              outputDir=outputDir                                
                              )
    }

    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="probes","coreID"="probe_id","suffix"="PROBE","org"="fly","cntrTab"="genes", "prefix"=prefix, "outDir"=outputDir, "Db_type"="ChipDb")    
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )    
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )
    
    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)
    appendGenes(db, subStrs=subStrs, printSchema=printSchema)
    appendProbes(db, subStrs=subStrs, printSchema=printSchema)
    appendGeneInfo(db, subStrs=subStrs, printSchema=printSchema)

    appendChromosomes(db, subStrs=subStrs, printSchema=printSchema)
    appendCytogenicLocs(db, subStrs=subStrs, printSchema=printSchema)
    appendRefseq(db, subStrs=subStrs, printSchema=printSchema)
    appendPubmed(db, subStrs=subStrs, printSchema=printSchema)
    appendUnigene(db, subStrs=subStrs, printSchema=printSchema)
    appendChrlengths(db, subStrs=subStrs, printSchema=printSchema)
    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)
    appendChromsomeLocs(db, subStrs=subStrs, printSchema=printSchema)
    appendAlias(db, subStrs=subStrs, printSchema=printSchema)
    appendUniprot(db, subStrs=subStrs, printSchema=printSchema)
    appendFlyBase(db, subStrs=subStrs, printSchema=printSchema)
    appendFlyBaseCG(db, subStrs=subStrs, printSchema=printSchema)
    
    appendPostMeta(db, subStrs=subStrs)
    
    simplifyProbes(db, subStrs=subStrs)
    dropRedundantTables(db, subStrs=subStrs)
        
    dbDisconnect(db)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)    
}




#This is the formula for FLY_DB  
popFLYDB <- function(prefix,
                     chipSrc = system.file("extdata", "chipsrc_fly.sqlite", package="fly.db0"),
                     metaDataSrc,
                     outputDir=".",
                     printSchema=FALSE){

    makeUniversalMapping(pkgName=prefix,
                         chipSrc=chipSrc,
                         outputDir=outputDir)

    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="genes","coreID"="gene_id","suffix"="EG","org"="fly","cntrTab"="genes", "prefix"=prefix, "outDir"=outputDir, "Db_type"="OrgDb")
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )    
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )

    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)
    appendGenes(db, subStrs=subStrs, printSchema=printSchema)
    appendGeneInfo(db, subStrs=subStrs, printSchema=printSchema)

    appendChromosomes(db, subStrs=subStrs, printSchema=printSchema)
    appendAccessions(db, subStrs=subStrs, printSchema=printSchema)
    appendCytogenicLocs(db, subStrs=subStrs, printSchema=printSchema)
    appendRefseq(db, subStrs=subStrs, printSchema=printSchema)
    appendPubmed(db, subStrs=subStrs, printSchema=printSchema)
    appendUnigene(db, subStrs=subStrs, printSchema=printSchema)
    appendChrlengths(db, subStrs=subStrs, printSchema=printSchema)
    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)
    appendChromsomeLocs(db, subStrs=subStrs, printSchema=printSchema)
    appendAlias(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsembl(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsemblProt(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsemblTrans(db, subStrs=subStrs, printSchema=printSchema)
    appendUniprot(db, subStrs=subStrs, printSchema=printSchema)
    appendFlyBase(db, subStrs=subStrs, printSchema=printSchema)
    appendFlyBaseCG(db, subStrs=subStrs, printSchema=printSchema)
    appendFlyBaseProt(db, subStrs=subStrs, printSchema=printSchema)
    makeGOViews(db)
    
    appendPostMeta(db, subStrs=subStrs)
    
    dbDisconnect(db)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)    
}





#presently this is the formula for YEASTCHIP_DB
#Like popARABIDOPSISCHIDB(), popYEASTCHIPDB() is only really ready to run with affy chips though we can update this in future as needed.
popYEASTCHIPDB <- function(affy,
                           prefix,
                           fileName,
                           chipSrc = system.file("extdata", "chipsrc_yeast.sqlite", package="yeast.db0"),
                           metaDataSrc,
                           outputDir=".",
                           printSchema=FALSE){

    #the following function makes a decision based on the value in the affy parameter:
    getMapForYeastChipPkg(affy, fileName=fileName, pkgName=prefix, outputDir=outputDir)    
    
    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="probes","coreID"="probe_id","suffix"="PROBE","org"="yeast","cntrTab"="sgd", "prefix"=prefix, "outDir"=outputDir, "Db_type"="ChipDb")    
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )
    
    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)

    appendYeastSGD(db, subStrs=subStrs, printSchema=printSchema)
    appendYeastProbes(db, subStrs=subStrs, printSchema=printSchema)
    appendYeastOrphanMeta(db, subStrs=subStrs)
    appendYeastChromosomeFeatures(db, subStrs=subStrs, printSchema=printSchema)
    appendYeastAlias(db, subStrs=subStrs, printSchema=printSchema)
    
    appendPubmed(db, subStrs=subStrs, printSchema=printSchema)
    appendChrlengths(db, subStrs=subStrs, printSchema=printSchema)
    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsembl(db, subStrs=subStrs, printSchema=printSchema)
    
    appendPostMeta(db, subStrs=subStrs)
    
    simplifyYeastProbes(db, subStrs=subStrs)
    dropRedundantTables(db, subStrs=subStrs)
        
    dbDisconnect(db)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)    
}





#presently this is the formula for YEAST_DB
popYEASTDB <- function(prefix,
                       chipSrc = system.file("extdata", "chipsrc_yeast.sqlite", package="yeast.db0"),
                       metaDataSrc,
                       outputDir=".",
                       printSchema=FALSE){
    
    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="sgd","coreID"="systematic_name","suffix"="ORF","org"="yeast","cntrTab"="sgd", "prefix"=prefix, "outDir"=outputDir, "Db_type"="OrgDb")    
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )

    sqliteQuickSQL(db, "CREATE TABLE probe_map (probe_id TEXT, gene_id TEXT, accession TEXT);")

    sqliteQuickSQL(db, "CREATE TABLE metadata (name VARCHAR(80) PRIMARY KEY, value VARCHAR(255) );")
    sqliteQuickSQL(db, paste("INSERT INTO metadata VALUES ('PKGNAME', '", prefix, "');", sep="", collapse=""))
    
    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)

    appendYeastSGD(db, subStrs=subStrs, printSchema=printSchema)
    appendYeastOrphanMeta(db, subStrs=subStrs)
    appendYeastChromosomeFeatures(db, subStrs=subStrs, printSchema=printSchema)
    appendYeastAlias(db, subStrs=subStrs, printSchema=printSchema)
    
    appendPubmed(db, subStrs=subStrs, printSchema=printSchema)
    appendChrlengths(db, subStrs=subStrs, printSchema=printSchema)
    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)
    appendYeastPfam(db, subStrs=subStrs, printSchema=printSchema)
    appendYeastSmart(db, subStrs=subStrs, printSchema=printSchema)
    appendYeastInterpro(db, subStrs=subStrs, printSchema=printSchema)
    appendYeastRejectORF(db, subStrs=subStrs, printSchema=printSchema)
    appendYeastGene2Systematic(db, subStrs=subStrs, printSchema=printSchema)

    ##Append the 3 tables that come from the NCBI data
    appendExternalEG(db, subStrs=subStrs, printSchema=printSchema)  ##needed to cover case where genes table is not central... 
    appendEnsembl(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsembl2NCBI(db, subStrs=subStrs, printSchema=printSchema)
    appendNCBI2Ensembl(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsemblProt(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsemblTrans(db, subStrs=subStrs, printSchema=printSchema)
    appendUniprot(db, subStrs=subStrs, printSchema=printSchema)
    appendRefseq(db, subStrs=subStrs, printSchema=printSchema)
    makeGOViews(db)
    
    appendPostMeta(db, subStrs=subStrs)
    
    dbDisconnect(db)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)    
}


#This is the formula for MALARIA_DB  
popMALARIADB <- function(prefix,
                        chipSrc = system.file("extdata", "chipsrc_malaria.sqlite", package="malaria.db0"),
                        metaDataSrc,
                        outputDir=".",
                        printSchema=FALSE){

    makeUniversalMapping(pkgName=prefix,
                         chipSrc=chipSrc,
                         outputDir=outputDir)

    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="genes","coreID"="gene_id","suffix"="ORF","org"="malaria","cntrTab"="genes", "prefix"=prefix, "outDir"=outputDir, "Db_type"="OrgDb")
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )

    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)
    appendGenes(db, subStrs=subStrs, printSchema=printSchema)
    appendGeneInfo(db, subStrs=subStrs, printSchema=printSchema)

    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)

    appendAlias(db, subStrs=subStrs, printSchema=printSchema)    
    makeGOViews(db)
    
    appendPostMeta(db, subStrs=subStrs)
    
    dbDisconnect(db)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)
}


#presently this is the formula for ZEBRAFISHCHIP_DB
popZEBRAFISHCHIPDB <- function(affy,
                           prefix,
                           fileName,
                           chipMapSrc = system.file("extdata", "chipmapsrc_zebrafish.sqlite", package="zebrafish.db0"),
                           chipSrc = system.file("extdata", "chipsrc_zebrafish.sqlite", package="zebrafish.db0"),
                           metaDataSrc,
                           otherSrc=character(0),
                           baseMapType="gbNRef",
                           outputDir=".",
                           printSchema=FALSE){

    if(affy==TRUE){
        getMapForBiocChipPkg(
                             csvFileName=fileName,
                             pkgName=prefix,
                             chipMapSrc=chipMapSrc,
                             chipSrc=chipSrc,
                             otherSrc=otherSrc,
                             baseMapType=baseMapType,
                             outputDir=outputDir
                             )
    }
    else if(affy==FALSE){
        getMapForOtherChipPkg(
                              filePath=fileName,
                              pkgName=prefix,
                              chipMapSrc=chipMapSrc,
                              chipSrc=chipSrc,
                              otherSrc=otherSrc,
                              baseMapType=baseMapType,
                              outputDir=outputDir                                
                              )
    }

    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="probes","coreID"="probe_id","suffix"="PROBE","org"="zebrafish","cntrTab"="genes", "prefix"=prefix, "outDir"=outputDir, "Db_type"="ChipDb")    
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )
    
    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)
    appendGenes(db, subStrs=subStrs, printSchema=printSchema)
    appendProbes(db, subStrs=subStrs, printSchema=printSchema)
    appendGeneInfo(db, subStrs=subStrs, printSchema=printSchema)

    appendChromosomes(db, subStrs=subStrs, printSchema=printSchema)
    appendRefseq(db, subStrs=subStrs, printSchema=printSchema)
    appendPubmed(db, subStrs=subStrs, printSchema=printSchema)
    appendUnigene(db, subStrs=subStrs, printSchema=printSchema)
    appendChrlengths(db, subStrs=subStrs, printSchema=printSchema)
    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)
    appendChromsomeLocs(db, subStrs=subStrs, printSchema=printSchema)
    appendPfam(db, subStrs=subStrs, printSchema=printSchema)
    appendProsite(db, subStrs=subStrs, printSchema=printSchema)
    appendAlias(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsembl(db, subStrs=subStrs, printSchema=printSchema)
    appendUniprot(db, subStrs=subStrs, printSchema=printSchema)
    appendZfin(db, subStrs=subStrs, printSchema=printSchema)

    appendPostMeta(db, subStrs=subStrs)
    
    simplifyProbes(db, subStrs=subStrs)
    dropRedundantTables(db, subStrs=subStrs)
        
    dbDisconnect(db)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)
}


#This is the formula for ZEBRAFISH_DB  
popZEBRAFISHDB <- function(prefix,
                       chipSrc = system.file("extdata", "chipsrc_zebrafish.sqlite", package="zebrafish.db0"),
                       metaDataSrc,
                       outputDir=".",
                       printSchema=FALSE){

    makeUniversalMapping(pkgName=prefix,
                         chipSrc=chipSrc,
                         outputDir=outputDir)

    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="genes","coreID"="gene_id","suffix"="EG","org"="zebrafish","cntrTab"="genes", "prefix"=prefix, "outDir"=outputDir, "Db_type"="OrgDb")
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )

    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)
    appendGenes(db, subStrs=subStrs, printSchema=printSchema)
    appendGeneInfo(db, subStrs=subStrs, printSchema=printSchema)

    appendChromosomes(db, subStrs=subStrs, printSchema=printSchema)
    appendAccessions(db, subStrs=subStrs, printSchema=printSchema)
    appendRefseq(db, subStrs=subStrs, printSchema=printSchema)
    appendPubmed(db, subStrs=subStrs, printSchema=printSchema)
    appendUnigene(db, subStrs=subStrs, printSchema=printSchema)
    appendChrlengths(db, subStrs=subStrs, printSchema=printSchema)
    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)
    appendChromsomeLocs(db, subStrs=subStrs, printSchema=printSchema)
    appendPfam(db, subStrs=subStrs, printSchema=printSchema)
    appendProsite(db, subStrs=subStrs, printSchema=printSchema)
    appendAlias(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsembl(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsembl2NCBI(db, subStrs=subStrs, printSchema=printSchema)
    appendNCBI2Ensembl(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsemblProt(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsemblTrans(db, subStrs=subStrs, printSchema=printSchema)
    appendUniprot(db, subStrs=subStrs, printSchema=printSchema)
    appendZfin(db, subStrs=subStrs, printSchema=printSchema)
    makeGOViews(db)
    
    appendPostMeta(db, subStrs=subStrs)
    
    dbDisconnect(db)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)
}




#presently this is the formula for ECOLICHIP_DB
popECOLICHIPDB <- function(affy,
                           prefix,
                           fileName,
                           chipMapSrc = system.file("extdata", "chipmapsrc_ecoliK12.sqlite", package="ecoliK12.db0"),
                           chipSrc = system.file("extdata", "chipsrc_ecoliK12.sqlite", package="ecoliK12.db0"),
                           metaDataSrc,
                           otherSrc=character(0),
                           baseMapType="gbNRef",
                           outputDir=".",
                           printSchema=FALSE){

    if(affy==TRUE){
        getMapForBiocChipPkg(
                             csvFileName=fileName,
                             pkgName=prefix,
                             chipMapSrc=chipMapSrc,
                             chipSrc=chipSrc,
                             otherSrc=otherSrc,
                             baseMapType=baseMapType,
                             outputDir=outputDir
                             )
    }
    else if(affy==FALSE){
        getMapForOtherChipPkg(
                              filePath=fileName,
                              pkgName=prefix,
                              chipMapSrc=chipMapSrc,
                              chipSrc=chipSrc,
                              otherSrc=otherSrc,
                              baseMapType=baseMapType,
                              outputDir=outputDir                                
                              )
    }

    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="probes","coreID"="probe_id","suffix"="PROBE","org"="ecoli","cntrTab"="genes", "prefix"=prefix, "outDir"=outputDir, "Db_type"="ChipDb")    
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )
    
    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)
    appendGenes(db, subStrs=subStrs, printSchema=printSchema)
    appendProbes(db, subStrs=subStrs, printSchema=printSchema)
    appendGeneInfo(db, subStrs=subStrs, printSchema=printSchema)

    appendRefseq(db, subStrs=subStrs, printSchema=printSchema)
    appendPubmed(db, subStrs=subStrs, printSchema=printSchema)
    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)
    appendAlias(db, subStrs=subStrs, printSchema=printSchema)

    appendPostMeta(db, subStrs=subStrs)
    
    simplifyProbes(db, subStrs=subStrs)
    dropRedundantTables(db, subStrs=subStrs)
        
    dbDisconnect(db)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)
}


#This is the formula for ECOLI_DB  
popECOLIDB <- function(prefix,
                       chipSrc = system.file("extdata", "chipsrc_ecoliK12.sqlite", package="ecoliK12.db0"),
                       metaDataSrc,
                       outputDir=".",
                       printSchema=FALSE){

    makeUniversalMapping(pkgName=prefix,
                         chipSrc=chipSrc,
                         outputDir=outputDir)

    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="genes","coreID"="gene_id","suffix"="EG","org"="ecoli","cntrTab"="genes", "prefix"=prefix, "outDir"=outputDir, "Db_type"="OrgDb")
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )

    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)
    appendGenes(db, subStrs=subStrs, printSchema=printSchema)
    appendGeneInfo(db, subStrs=subStrs, printSchema=printSchema)

    appendAccessions(db, subStrs=subStrs, printSchema=printSchema)
    appendRefseq(db, subStrs=subStrs, printSchema=printSchema)
    appendPubmed(db, subStrs=subStrs, printSchema=printSchema)
    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)
    appendAlias(db, subStrs=subStrs, printSchema=printSchema)
    makeGOViews(db)
    
    appendPostMeta(db, subStrs=subStrs)
    
    dbDisconnect(db)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)
}










#presently this is the formula for CANINECHIP_DB
popCANINECHIPDB <- function(affy,
                           prefix,
                           fileName,
                           chipMapSrc = system.file("extdata", "chipmapsrc_canine.sqlite", package="canine.db0"),
                           chipSrc = system.file("extdata", "chipsrc_canine.sqlite", package="canine.db0"),
                           metaDataSrc,
                           otherSrc=character(0),
                           baseMapType="gbNRef",
                           outputDir=".",
                           printSchema=FALSE){

    if(affy==TRUE){
        getMapForBiocChipPkg(
                             csvFileName=fileName,
                             pkgName=prefix,
                             chipMapSrc=chipMapSrc,
                             chipSrc=chipSrc,
                             otherSrc=otherSrc,
                             baseMapType=baseMapType,
                             outputDir=outputDir
                             )
    }
    else if(affy==FALSE){
        getMapForOtherChipPkg(
                              filePath=fileName,
                              pkgName=prefix,
                              chipMapSrc=chipMapSrc,
                              chipSrc=chipSrc,
                              otherSrc=otherSrc,
                              baseMapType=baseMapType,
                              outputDir=outputDir                                
                              )
    }

    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="probes","coreID"="probe_id","suffix"="PROBE","org"="canine","cntrTab"="genes", "prefix"=prefix, "outDir"=outputDir, "Db_type"="ChipDb")    
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )
    
    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)
    appendGenes(db, subStrs=subStrs, printSchema=printSchema)
    appendProbes(db, subStrs=subStrs, printSchema=printSchema)
    appendGeneInfo(db, subStrs=subStrs, printSchema=printSchema)

    appendChromosomes(db, subStrs=subStrs, printSchema=printSchema)
    appendRefseq(db, subStrs=subStrs, printSchema=printSchema)
    appendPubmed(db, subStrs=subStrs, printSchema=printSchema)
    appendUnigene(db, subStrs=subStrs, printSchema=printSchema)
    appendChrlengths(db, subStrs=subStrs, printSchema=printSchema)
    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)
    appendChromsomeLocs(db, subStrs=subStrs, printSchema=printSchema)
    appendAlias(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsembl(db, subStrs=subStrs, printSchema=printSchema)
    appendUniprot(db, subStrs=subStrs, printSchema=printSchema)

    appendPostMeta(db, subStrs=subStrs)
    
    simplifyProbes(db, subStrs=subStrs)
    dropRedundantTables(db, subStrs=subStrs)
        
    dbDisconnect(db)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)
}


#This is the formula for CANINE_DB  
popCANINEDB <- function(prefix,
                       chipSrc = system.file("extdata", "chipsrc_canine.sqlite", package="canine.db0"),
                       metaDataSrc,
                       outputDir=".",
                       printSchema=FALSE){

    makeUniversalMapping(pkgName=prefix,
                         chipSrc=chipSrc,
                         outputDir=outputDir)

    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="genes","coreID"="gene_id","suffix"="EG","org"="canine","cntrTab"="genes", "prefix"=prefix, "outDir"=outputDir, "Db_type"="OrgDb")
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )

    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)
    appendGenes(db, subStrs=subStrs, printSchema=printSchema)
    appendGeneInfo(db, subStrs=subStrs, printSchema=printSchema)

    appendChromosomes(db, subStrs=subStrs, printSchema=printSchema)
    appendAccessions(db, subStrs=subStrs, printSchema=printSchema)
    appendRefseq(db, subStrs=subStrs, printSchema=printSchema)
    appendPubmed(db, subStrs=subStrs, printSchema=printSchema)
    appendUnigene(db, subStrs=subStrs, printSchema=printSchema)
    appendChrlengths(db, subStrs=subStrs, printSchema=printSchema)
    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)
    appendChromsomeLocs(db, subStrs=subStrs, printSchema=printSchema)
    appendAlias(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsembl(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsembl2NCBI(db, subStrs=subStrs, printSchema=printSchema)
    appendNCBI2Ensembl(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsemblProt(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsemblTrans(db, subStrs=subStrs, printSchema=printSchema)
    appendUniprot(db, subStrs=subStrs, printSchema=printSchema)
    makeGOViews(db)
    
    appendPostMeta(db, subStrs=subStrs)
    
    dbDisconnect(db)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)
}




#presently this is the formula for BOVINECHIP_DB
popBOVINECHIPDB <- function(affy,
                           prefix,
                           fileName,
                           chipMapSrc = system.file("extdata", "chipmapsrc_bovine.sqlite", package="bovine.db0"),
                           chipSrc = system.file("extdata", "chipsrc_bovine.sqlite", package="bovine.db0"),
                           metaDataSrc,
                           otherSrc=character(0),
                           baseMapType="gbNRef",
                           outputDir=".",
                           printSchema=FALSE){

    if(affy==TRUE){
        getMapForBiocChipPkg(
                             csvFileName=fileName,
                             pkgName=prefix,
                             chipMapSrc=chipMapSrc,
                             chipSrc=chipSrc,
                             otherSrc=otherSrc,
                             baseMapType=baseMapType,
                             outputDir=outputDir
                             )
    }
    else if(affy==FALSE){
        getMapForOtherChipPkg(
                              filePath=fileName,
                              pkgName=prefix,
                              chipMapSrc=chipMapSrc,
                              chipSrc=chipSrc,
                              otherSrc=otherSrc,
                              baseMapType=baseMapType,
                              outputDir=outputDir                                
                              )
    }

    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="probes","coreID"="probe_id","suffix"="PROBE","org"="bovine","cntrTab"="genes", "prefix"=prefix, "outDir"=outputDir, "Db_type"="ChipDb")    
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )
    
    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)
    appendGenes(db, subStrs=subStrs, printSchema=printSchema)
    appendProbes(db, subStrs=subStrs, printSchema=printSchema)
    appendGeneInfo(db, subStrs=subStrs, printSchema=printSchema)

    appendChromosomes(db, subStrs=subStrs, printSchema=printSchema)
    appendRefseq(db, subStrs=subStrs, printSchema=printSchema)
    appendPubmed(db, subStrs=subStrs, printSchema=printSchema)
    appendUnigene(db, subStrs=subStrs, printSchema=printSchema)
    appendChrlengths(db, subStrs=subStrs, printSchema=printSchema)
    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)
    appendChromsomeLocs(db, subStrs=subStrs, printSchema=printSchema)
    appendPfam(db, subStrs=subStrs, printSchema=printSchema)
    appendProsite(db, subStrs=subStrs, printSchema=printSchema)
    appendAlias(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsembl(db, subStrs=subStrs, printSchema=printSchema)
    appendUniprot(db, subStrs=subStrs, printSchema=printSchema)

    appendPostMeta(db, subStrs=subStrs)
    
    simplifyProbes(db, subStrs=subStrs)
    dropRedundantTables(db, subStrs=subStrs)
        
    dbDisconnect(db)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)
}


#This is the formula for BOVINE_DB  
popBOVINEDB <- function(prefix,
                       chipSrc = system.file("extdata", "chipsrc_bovine.sqlite", package="bovine.db0"),
                       metaDataSrc,
                       outputDir=".",
                       printSchema=FALSE){

    makeUniversalMapping(pkgName=prefix,
                         chipSrc=chipSrc,
                         outputDir=outputDir)

    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="genes","coreID"="gene_id","suffix"="EG","org"="bovine","cntrTab"="genes", "prefix"=prefix, "outDir"=outputDir, "Db_type"="OrgDb")
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )

    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)
    appendGenes(db, subStrs=subStrs, printSchema=printSchema)
    appendGeneInfo(db, subStrs=subStrs, printSchema=printSchema)

    appendChromosomes(db, subStrs=subStrs, printSchema=printSchema)
    appendAccessions(db, subStrs=subStrs, printSchema=printSchema)
    appendRefseq(db, subStrs=subStrs, printSchema=printSchema)
    appendPubmed(db, subStrs=subStrs, printSchema=printSchema)
    appendUnigene(db, subStrs=subStrs, printSchema=printSchema)
    appendChrlengths(db, subStrs=subStrs, printSchema=printSchema)
    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)
    appendChromsomeLocs(db, subStrs=subStrs, printSchema=printSchema)
    appendPfam(db, subStrs=subStrs, printSchema=printSchema)
    appendProsite(db, subStrs=subStrs, printSchema=printSchema)
    appendAlias(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsembl(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsembl2NCBI(db, subStrs=subStrs, printSchema=printSchema)
    appendNCBI2Ensembl(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsemblProt(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsemblTrans(db, subStrs=subStrs, printSchema=printSchema)
    appendUniprot(db, subStrs=subStrs, printSchema=printSchema)
    makeGOViews(db)
    
    appendPostMeta(db, subStrs=subStrs)
    
    dbDisconnect(db)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)
}




#presently this is the formula for WORMCHIP_DB
popWORMCHIPDB <- function(affy,
                           prefix,
                           fileName,
                           chipMapSrc = system.file("extdata", "chipmapsrc_worm.sqlite", package="worm.db0"),
                           chipSrc = system.file("extdata", "chipsrc_worm.sqlite", package="worm.db0"),
                           metaDataSrc,
                           otherSrc=character(0),
                           baseMapType="gbNRef",
                           outputDir=".",
                           printSchema=FALSE){

    if(affy==TRUE){
        getMapForBiocChipPkg(
                             csvFileName=fileName,
                             pkgName=prefix,
                             chipMapSrc=chipMapSrc,
                             chipSrc=chipSrc,
                             otherSrc=otherSrc,
                             baseMapType=baseMapType,
                             outputDir=outputDir
                             )
    }
    else if(affy==FALSE){
        getMapForOtherChipPkg(
                              filePath=fileName,
                              pkgName=prefix,
                              chipMapSrc=chipMapSrc,
                              chipSrc=chipSrc,
                              otherSrc=otherSrc,
                              baseMapType=baseMapType,
                              outputDir=outputDir                                
                              )
    }

    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="probes","coreID"="probe_id","suffix"="PROBE","org"="worm","cntrTab"="genes", "prefix"=prefix, "outDir"=outputDir, "Db_type"="ChipDb")    
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )
    
    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)
    appendGenes(db, subStrs=subStrs, printSchema=printSchema)
    appendProbes(db, subStrs=subStrs, printSchema=printSchema)
    appendGeneInfo(db, subStrs=subStrs, printSchema=printSchema)

    appendChromosomes(db, subStrs=subStrs, printSchema=printSchema)
    appendRefseq(db, subStrs=subStrs, printSchema=printSchema)
    appendPubmed(db, subStrs=subStrs, printSchema=printSchema)
    appendUnigene(db, subStrs=subStrs, printSchema=printSchema)
    appendChrlengths(db, subStrs=subStrs, printSchema=printSchema)
    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)
    appendChromsomeLocs(db, subStrs=subStrs, printSchema=printSchema)
    appendAlias(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsembl(db, subStrs=subStrs, printSchema=printSchema)
    appendUniprot(db, subStrs=subStrs, printSchema=printSchema)
    appendWormbase(db, subStrs=subStrs, printSchema=printSchema)

    appendPostMeta(db, subStrs=subStrs)
    
    simplifyProbes(db, subStrs=subStrs)
    dropRedundantTables(db, subStrs=subStrs)
        
    dbDisconnect(db)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)
}


#This is the formula for WORM_DB  
popWORMDB <- function(prefix,
                       chipSrc = system.file("extdata", "chipsrc_worm.sqlite", package="worm.db0"),
                       metaDataSrc,
                       outputDir=".",
                       printSchema=FALSE){

    makeUniversalMapping(pkgName=prefix,
                         chipSrc=chipSrc,
                         outputDir=outputDir)

    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="genes","coreID"="gene_id","suffix"="EG","org"="worm","cntrTab"="genes", "prefix"=prefix, "outDir"=outputDir, "Db_type"="OrgDb")
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )

    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)
    appendGenes(db, subStrs=subStrs, printSchema=printSchema)
    appendGeneInfo(db, subStrs=subStrs, printSchema=printSchema)

    appendChromosomes(db, subStrs=subStrs, printSchema=printSchema)
    appendAccessions(db, subStrs=subStrs, printSchema=printSchema)
    appendRefseq(db, subStrs=subStrs, printSchema=printSchema)
    appendPubmed(db, subStrs=subStrs, printSchema=printSchema)
    appendUnigene(db, subStrs=subStrs, printSchema=printSchema)
    appendChrlengths(db, subStrs=subStrs, printSchema=printSchema)
    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)
    appendChromsomeLocs(db, subStrs=subStrs, printSchema=printSchema)
    appendAlias(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsembl(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsemblProt(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsemblTrans(db, subStrs=subStrs, printSchema=printSchema)
    appendUniprot(db, subStrs=subStrs, printSchema=printSchema)
    appendWormbase(db, subStrs=subStrs, printSchema=printSchema)
    makeGOViews(db)
    
    appendPostMeta(db, subStrs=subStrs)
    
    dbDisconnect(db)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)
}








#presently this is the formula for PIGCHIP_DB
popPIGCHIPDB <- function(affy,
                           prefix,
                           fileName,
                           chipMapSrc = system.file("extdata", "chipmapsrc_pig.sqlite", package="pig.db0"),
                           chipSrc = system.file("extdata", "chipsrc_pig.sqlite", package="pig.db0"),
                           metaDataSrc,
                           otherSrc=character(0),
                           baseMapType="gbNRef",
                           outputDir=".",
                           printSchema=FALSE){

    if(affy==TRUE){
        getMapForBiocChipPkg(
                             csvFileName=fileName,
                             pkgName=prefix,
                             chipMapSrc=chipMapSrc,
                             chipSrc=chipSrc,
                             otherSrc=otherSrc,
                             baseMapType=baseMapType,
                             outputDir=outputDir
                             )
    }
    else if(affy==FALSE){
        getMapForOtherChipPkg(
                              filePath=fileName,
                              pkgName=prefix,
                              chipMapSrc=chipMapSrc,
                              chipSrc=chipSrc,
                              otherSrc=otherSrc,
                              baseMapType=baseMapType,
                              outputDir=outputDir                                
                              )
    }

    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="probes","coreID"="probe_id","suffix"="PROBE","org"="pig","cntrTab"="genes", "prefix"=prefix, "outDir"=outputDir, "Db_type"="ChipDb")    
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )
    
    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)
    appendGenes(db, subStrs=subStrs, printSchema=printSchema)
    appendProbes(db, subStrs=subStrs, printSchema=printSchema)
    appendGeneInfo(db, subStrs=subStrs, printSchema=printSchema)

    appendChromosomes(db, subStrs=subStrs, printSchema=printSchema)
    appendRefseq(db, subStrs=subStrs, printSchema=printSchema)
    appendPubmed(db, subStrs=subStrs, printSchema=printSchema)
    appendUnigene(db, subStrs=subStrs, printSchema=printSchema)
    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)
    appendAlias(db, subStrs=subStrs, printSchema=printSchema)
##     appendEnsembl(db, subStrs=subStrs, printSchema=printSchema)
    appendUniprot(db, subStrs=subStrs, printSchema=printSchema)

    appendPostMeta(db, subStrs=subStrs)
    
    simplifyProbes(db, subStrs=subStrs)
    dropRedundantTables(db, subStrs=subStrs)
        
    dbDisconnect(db)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)
}


#This is the formula for PIG_DB  
popPIGDB <- function(prefix,
                       chipSrc = system.file("extdata", "chipsrc_pig.sqlite", package="pig.db0"),
                       metaDataSrc,
                       outputDir=".",
                       printSchema=FALSE){

    makeUniversalMapping(pkgName=prefix,
                         chipSrc=chipSrc,
                         outputDir=outputDir)

    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="genes","coreID"="gene_id","suffix"="EG","org"="pig","cntrTab"="genes", "prefix"=prefix, "outDir"=outputDir, "Db_type"="OrgDb")
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )

    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)
    appendGenes(db, subStrs=subStrs, printSchema=printSchema)
    appendGeneInfo(db, subStrs=subStrs, printSchema=printSchema)

    appendChromosomes(db, subStrs=subStrs, printSchema=printSchema)
    appendAccessions(db, subStrs=subStrs, printSchema=printSchema)
    appendRefseq(db, subStrs=subStrs, printSchema=printSchema)
    appendPubmed(db, subStrs=subStrs, printSchema=printSchema)
    appendUnigene(db, subStrs=subStrs, printSchema=printSchema)
    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)
    appendAlias(db, subStrs=subStrs, printSchema=printSchema)
##     appendEnsembl(db, subStrs=subStrs, printSchema=printSchema)
##     appendEnsemblProt(db, subStrs=subStrs, printSchema=printSchema)
##     appendEnsemblTrans(db, subStrs=subStrs, printSchema=printSchema)
    appendUniprot(db, subStrs=subStrs, printSchema=printSchema)
    makeGOViews(db)
    
    appendPostMeta(db, subStrs=subStrs)
    
    dbDisconnect(db)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)
}






#presently this is the formula for CHICKENCHIP_DB
popCHICKENCHIPDB <- function(affy,
                           prefix,
                           fileName,
                           chipMapSrc = system.file("extdata", "chipmapsrc_chicken.sqlite", package="chicken.db0"),
                           chipSrc = system.file("extdata", "chipsrc_chicken.sqlite", package="chicken.db0"),
                           metaDataSrc,
                           otherSrc=character(0),
                           baseMapType="gbNRef",
                           outputDir=".",
                           printSchema=FALSE){

    if(affy==TRUE){
        getMapForBiocChipPkg(
                             csvFileName=fileName,
                             pkgName=prefix,
                             chipMapSrc=chipMapSrc,
                             chipSrc=chipSrc,
                             otherSrc=otherSrc,
                             baseMapType=baseMapType,
                             outputDir=outputDir
                             )
    }
    else if(affy==FALSE){
        getMapForOtherChipPkg(
                              filePath=fileName,
                              pkgName=prefix,
                              chipMapSrc=chipMapSrc,
                              chipSrc=chipSrc,
                              otherSrc=otherSrc,
                              baseMapType=baseMapType,
                              outputDir=outputDir                                
                              )
    }

    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="probes","coreID"="probe_id","suffix"="PROBE","org"="chicken","cntrTab"="genes", "prefix"=prefix, "outDir"=outputDir, "Db_type"="ChipDb")    
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )
    
    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)
    appendGenes(db, subStrs=subStrs, printSchema=printSchema)
    appendProbes(db, subStrs=subStrs, printSchema=printSchema)
    appendGeneInfo(db, subStrs=subStrs, printSchema=printSchema)

    appendChromosomes(db, subStrs=subStrs, printSchema=printSchema)
    appendRefseq(db, subStrs=subStrs, printSchema=printSchema)
    appendPubmed(db, subStrs=subStrs, printSchema=printSchema)
    appendUnigene(db, subStrs=subStrs, printSchema=printSchema)
    appendChrlengths(db, subStrs=subStrs, printSchema=printSchema)
    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)
    appendChromsomeLocs(db, subStrs=subStrs, printSchema=printSchema)
    appendPfam(db, subStrs=subStrs, printSchema=printSchema)
    appendProsite(db, subStrs=subStrs, printSchema=printSchema)
    appendAlias(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsembl(db, subStrs=subStrs, printSchema=printSchema)
    appendUniprot(db, subStrs=subStrs, printSchema=printSchema)

    appendPostMeta(db, subStrs=subStrs)
    
    simplifyProbes(db, subStrs=subStrs)
    dropRedundantTables(db, subStrs=subStrs)
        
    dbDisconnect(db)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)
}



#This is the formula for CHICKEN_DB  
popCHICKENDB <- function(prefix,
                       chipSrc = system.file("extdata", "chipsrc_chicken.sqlite", package="chicken.db0"),
                       metaDataSrc,
                       outputDir=".",
                       printSchema=FALSE){

    makeUniversalMapping(pkgName=prefix,
                         chipSrc=chipSrc,
                         outputDir=outputDir)

    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="genes","coreID"="gene_id","suffix"="EG","org"="chicken","cntrTab"="genes", "prefix"=prefix, "outDir"=outputDir, "Db_type"="OrgDb")
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )

    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)
    appendGenes(db, subStrs=subStrs, printSchema=printSchema)
    appendGeneInfo(db, subStrs=subStrs, printSchema=printSchema)

    appendChromosomes(db, subStrs=subStrs, printSchema=printSchema)
    appendAccessions(db, subStrs=subStrs, printSchema=printSchema)
    appendRefseq(db, subStrs=subStrs, printSchema=printSchema)
    appendPubmed(db, subStrs=subStrs, printSchema=printSchema)
    appendUnigene(db, subStrs=subStrs, printSchema=printSchema)
    appendChrlengths(db, subStrs=subStrs, printSchema=printSchema)
    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)
    appendChromsomeLocs(db, subStrs=subStrs, printSchema=printSchema)
    appendPfam(db, subStrs=subStrs, printSchema=printSchema)
    appendProsite(db, subStrs=subStrs, printSchema=printSchema)
    appendAlias(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsembl(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsembl2NCBI(db, subStrs=subStrs, printSchema=printSchema)
    appendNCBI2Ensembl(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsemblProt(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsemblTrans(db, subStrs=subStrs, printSchema=printSchema)
    appendUniprot(db, subStrs=subStrs, printSchema=printSchema)
    makeGOViews(db)
    
    appendPostMeta(db, subStrs=subStrs)
    
    dbDisconnect(db)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)
}













##This is the formula for CHIMP_DB  
popCHIMPDB <- function(prefix,
                       chipSrc = system.file("extdata", "chipsrc_chimp.sqlite", package="chimp.db0"),
                       metaDataSrc,
                       outputDir=".",
                       printSchema=FALSE){

    makeUniversalMapping(pkgName=prefix,
                         chipSrc=chipSrc,
                         outputDir=outputDir)

    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="genes","coreID"="gene_id","suffix"="EG","org"="chimp","cntrTab"="genes", "prefix"=prefix, "outDir"=outputDir, "Db_type"="OrgDb")
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )

    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)
    appendGenes(db, subStrs=subStrs, printSchema=printSchema)
    appendGeneInfo(db, subStrs=subStrs, printSchema=printSchema)

    appendChromosomes(db, subStrs=subStrs, printSchema=printSchema)
    appendAccessions(db, subStrs=subStrs, printSchema=printSchema)
    appendRefseq(db, subStrs=subStrs, printSchema=printSchema)
    appendPubmed(db, subStrs=subStrs, printSchema=printSchema)
##     appendUnigene(db, subStrs=subStrs, printSchema=printSchema)
    appendChrlengths(db, subStrs=subStrs, printSchema=printSchema)
    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)
    appendChromsomeLocs(db, subStrs=subStrs, printSchema=printSchema)
##     appendPfam(db, subStrs=subStrs, printSchema=printSchema)
##     appendProsite(db, subStrs=subStrs, printSchema=printSchema)
##     appendAlias(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsembl(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsembl2NCBI(db, subStrs=subStrs, printSchema=printSchema)
    appendNCBI2Ensembl(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsemblProt(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsemblTrans(db, subStrs=subStrs, printSchema=printSchema)
    appendUniprot(db, subStrs=subStrs, printSchema=printSchema)
    makeGOViews(db)
    
    appendPostMeta(db, subStrs=subStrs)
    
    dbDisconnect(db)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)
}


#presently this is the formula for RHESUSCHIP_DB
popRHESUSCHIPDB <- function(affy,
                         prefix,
                         fileName,
                         chipMapSrc = system.file("extdata", "chipmapsrc_rhesus.sqlite", package="rhesus.db0"),
                         chipSrc = system.file("extdata", "chipsrc_rhesus.sqlite", package="rhesus.db0"),
                         metaDataSrc,
                         otherSrc=character(0),
                         baseMapType="gbNRef",
                         outputDir=".",
                         printSchema=FALSE){

    if(affy==TRUE){
        getMapForBiocChipPkg(
                             csvFileName=fileName,
                             pkgName=prefix,
                             chipMapSrc=chipMapSrc,
                             chipSrc=chipSrc,
                             otherSrc=otherSrc,
                             baseMapType=baseMapType,
                             outputDir=outputDir
                             )
    }
    else if(affy==FALSE){
        getMapForOtherChipPkg(
                              filePath=fileName,
                              pkgName=prefix,
                              chipMapSrc=chipMapSrc,
                              chipSrc=chipSrc,
                              otherSrc=otherSrc,
                              baseMapType=baseMapType,
                              outputDir=outputDir                                
                              )
    }

    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="probes","coreID"="probe_id","suffix"="PROBE","org"="rhesus","cntrTab"="genes", "prefix"=prefix, "outDir"=outputDir, "Db_type"="ChipDb")    
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )    
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )
    
    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)
    appendGenes(db, subStrs=subStrs, printSchema=printSchema)
    appendProbes(db, subStrs=subStrs, printSchema=printSchema)
    appendGeneInfo(db, subStrs=subStrs, printSchema=printSchema)

    appendChromosomes(db, subStrs=subStrs, printSchema=printSchema)
    #appendCytogenicLocs(db, subStrs=subStrs, printSchema=printSchema)
    appendRefseq(db, subStrs=subStrs, printSchema=printSchema)
    appendPubmed(db, subStrs=subStrs, printSchema=printSchema)
    appendUnigene(db, subStrs=subStrs, printSchema=printSchema)
    appendChrlengths(db, subStrs=subStrs, printSchema=printSchema)
    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)
    appendChromsomeLocs(db, subStrs=subStrs, printSchema=printSchema)
    #appendPfam(db, subStrs=subStrs, printSchema=printSchema)
    #appendProsite(db, subStrs=subStrs, printSchema=printSchema)
    appendAlias(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsembl(db, subStrs=subStrs, printSchema=printSchema)
    appendUniprot(db, subStrs=subStrs, printSchema=printSchema)

    appendPostMeta(db, subStrs=subStrs)
    
    simplifyProbes(db, subStrs=subStrs)
    dropRedundantTables(db, subStrs=subStrs)
        
    dbDisconnect(db)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)    
}


#This is the formula for RHESUS_DB  
popRHESUSDB <- function(prefix,
                       chipSrc = system.file("extdata", "chipsrc_rhesus.sqlite", package="rhesus.db0"),
                       metaDataSrc,
                       outputDir=".",
                       printSchema=FALSE){

    makeUniversalMapping(pkgName=prefix,
                         chipSrc=chipSrc,
                         outputDir=outputDir)

    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="genes","coreID"="gene_id","suffix"="EG","org"="rhesus","cntrTab"="genes", "prefix"=prefix, "outDir"=outputDir, "Db_type"="OrgDb")
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )

    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)
    appendGenes(db, subStrs=subStrs, printSchema=printSchema)
    appendGeneInfo(db, subStrs=subStrs, printSchema=printSchema)

    appendChromosomes(db, subStrs=subStrs, printSchema=printSchema)
    appendAccessions(db, subStrs=subStrs, printSchema=printSchema)
    appendRefseq(db, subStrs=subStrs, printSchema=printSchema)
    appendPubmed(db, subStrs=subStrs, printSchema=printSchema)
##     appendUnigene(db, subStrs=subStrs, printSchema=printSchema)
    appendChrlengths(db, subStrs=subStrs, printSchema=printSchema)
    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)
    appendChromsomeLocs(db, subStrs=subStrs, printSchema=printSchema)
##     appendPfam(db, subStrs=subStrs, printSchema=printSchema)
##     appendProsite(db, subStrs=subStrs, printSchema=printSchema)
##     appendAlias(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsembl(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsembl2NCBI(db, subStrs=subStrs, printSchema=printSchema)
    appendNCBI2Ensembl(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsemblProt(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsemblTrans(db, subStrs=subStrs, printSchema=printSchema)
    appendUniprot(db, subStrs=subStrs, printSchema=printSchema)
    makeGOViews(db)
    
    appendPostMeta(db, subStrs=subStrs)
    
    dbDisconnect(db)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)
}




#This is the formula for ANOPHELES_DB  
popANOPHELESDB <- function(prefix,
                       chipSrc = system.file("extdata", "chipsrc_anopheles.sqlite", package="anopheles.db0"),
                       metaDataSrc,
                       outputDir=".",
                       printSchema=FALSE){

    makeUniversalMapping(pkgName=prefix,
                         chipSrc=chipSrc,
                         outputDir=outputDir)

    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="genes","coreID"="gene_id","suffix"="EG","org"="anopheles","cntrTab"="genes", "prefix"=prefix, "outDir"=outputDir, "Db_type"="OrgDb")
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )

    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)
    appendGenes(db, subStrs=subStrs, printSchema=printSchema)
    appendGeneInfo(db, subStrs=subStrs, printSchema=printSchema)

    appendChromosomes(db, subStrs=subStrs, printSchema=printSchema)
    appendAccessions(db, subStrs=subStrs, printSchema=printSchema)
    appendRefseq(db, subStrs=subStrs, printSchema=printSchema)
    appendPubmed(db, subStrs=subStrs, printSchema=printSchema)
    appendUnigene(db, subStrs=subStrs, printSchema=printSchema)
    appendChrlengths(db, subStrs=subStrs, printSchema=printSchema)
    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)
##     appendChromsomeLocs(db, subStrs=subStrs, printSchema=printSchema)
##     appendPfam(db, subStrs=subStrs, printSchema=printSchema)
##     appendProsite(db, subStrs=subStrs, printSchema=printSchema)
##     appendAlias(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsembl(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsembl2NCBI(db, subStrs=subStrs, printSchema=printSchema)
    appendNCBI2Ensembl(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsemblProt(db, subStrs=subStrs, printSchema=printSchema)
    appendEnsemblTrans(db, subStrs=subStrs, printSchema=printSchema)
    appendUniprot(db, subStrs=subStrs, printSchema=printSchema)
    makeGOViews(db)
    
    appendPostMeta(db, subStrs=subStrs)
    
    dbDisconnect(db)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)
}





#This is the formula for XENOPUSCHIP_DB  
popXENOPUSCHIPDB <- function(affy,
                           prefix,
                           fileName,
                           chipMapSrc = system.file("extdata", "chipmapsrc_xenopus.sqlite", package="xenopus.db0"),
                           chipSrc = system.file("extdata", "chipsrc_xenopus.sqlite", package="xenopus.db0"),
                           metaDataSrc,
                           otherSrc=character(0),
                           baseMapType="gbNRef",
                           outputDir=".",
                           printSchema=FALSE){

    if(affy==TRUE){
        getMapForBiocChipPkg(
                             csvFileName=fileName,
                             pkgName=prefix,
                             chipMapSrc=chipMapSrc,
                             chipSrc=chipSrc,
                             otherSrc=otherSrc,
                             baseMapType=baseMapType,
                             outputDir=outputDir
                             )
    }
    else if(affy==FALSE){
        getMapForOtherChipPkg(
                              filePath=fileName,
                              pkgName=prefix,
                              chipMapSrc=chipMapSrc,
                              chipSrc=chipSrc,
                              otherSrc=otherSrc,
                              baseMapType=baseMapType,
                              outputDir=outputDir                                
                              )
    }

    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="probes","coreID"="probe_id","suffix"="PROBE","org"="xenopus","cntrTab"="genes", "prefix"=prefix, "outDir"=outputDir, "Db_type"="ChipDb")    
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )
    
    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)
    appendGenes(db, subStrs=subStrs, printSchema=printSchema)
    appendProbes(db, subStrs=subStrs, printSchema=printSchema)
    appendGeneInfo(db, subStrs=subStrs, printSchema=printSchema)

    appendChromosomes(db, subStrs=subStrs, printSchema=printSchema)
    appendRefseq(db, subStrs=subStrs, printSchema=printSchema)
    appendPubmed(db, subStrs=subStrs, printSchema=printSchema)
    appendUnigene(db, subStrs=subStrs, printSchema=printSchema)
##     appendChrlengths(db, subStrs=subStrs, printSchema=printSchema)
    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)
##     appendChromsomeLocs(db, subStrs=subStrs, printSchema=printSchema)
##     appendPfam(db, subStrs=subStrs, printSchema=printSchema)
##     appendProsite(db, subStrs=subStrs, printSchema=printSchema)
##     appendAlias(db, subStrs=subStrs, printSchema=printSchema)
##     appendEnsembl(db, subStrs=subStrs, printSchema=printSchema)
##     appendEnsembl2NCBI(db, subStrs=subStrs, printSchema=printSchema)
##     appendNCBI2Ensembl(db, subStrs=subStrs, printSchema=printSchema)
##     appendEnsemblProt(db, subStrs=subStrs, printSchema=printSchema)
##     appendEnsemblTrans(db, subStrs=subStrs, printSchema=printSchema)
    appendUniprot(db, subStrs=subStrs, printSchema=printSchema)
    
    appendPostMeta(db, subStrs=subStrs)
    
    simplifyProbes(db, subStrs=subStrs)
    dropRedundantTables(db, subStrs=subStrs)

    dbDisconnect(db)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)
}




#This is the formula for XENOPUS_DB  
popXENOPUSDB <- function(prefix,
                       chipSrc = system.file("extdata", "chipsrc_xenopus.sqlite", package="xenopus.db0"),
                       metaDataSrc,
                       outputDir=".",
                       printSchema=FALSE){

    makeUniversalMapping(pkgName=prefix,
                         chipSrc=chipSrc,
                         outputDir=outputDir)

    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="genes","coreID"="gene_id","suffix"="EG","org"="xenopus","cntrTab"="genes", "prefix"=prefix, "outDir"=outputDir, "Db_type"="OrgDb")
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )

    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)
    appendGenes(db, subStrs=subStrs, printSchema=printSchema)
    appendGeneInfo(db, subStrs=subStrs, printSchema=printSchema)

    appendChromosomes(db, subStrs=subStrs, printSchema=printSchema)
    appendAccessions(db, subStrs=subStrs, printSchema=printSchema)
    appendRefseq(db, subStrs=subStrs, printSchema=printSchema)
    appendPubmed(db, subStrs=subStrs, printSchema=printSchema)
    appendUnigene(db, subStrs=subStrs, printSchema=printSchema)
##     appendChrlengths(db, subStrs=subStrs, printSchema=printSchema)
    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)
##     appendChromsomeLocs(db, subStrs=subStrs, printSchema=printSchema)
##     appendPfam(db, subStrs=subStrs, printSchema=printSchema)
##     appendProsite(db, subStrs=subStrs, printSchema=printSchema)
##     appendAlias(db, subStrs=subStrs, printSchema=printSchema)
##     appendEnsembl(db, subStrs=subStrs, printSchema=printSchema)
##     appendEnsembl2NCBI(db, subStrs=subStrs, printSchema=printSchema)
##     appendNCBI2Ensembl(db, subStrs=subStrs, printSchema=printSchema)
##     appendEnsemblProt(db, subStrs=subStrs, printSchema=printSchema)
##     appendEnsemblTrans(db, subStrs=subStrs, printSchema=printSchema)
    appendUniprot(db, subStrs=subStrs, printSchema=printSchema)
    makeGOViews(db)
    
    appendPostMeta(db, subStrs=subStrs)
    
    dbDisconnect(db)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)    
}












#This is the formula for YEASTNCBI_DB  
popYEASTNCBIDB <- function(prefix,
                       chipSrc = system.file("extdata", "chipsrc_yeastNCBI.sqlite", package="yeastNCBI.db0"),
                       metaDataSrc,
                       outputDir=".",
                       printSchema=FALSE){

    makeUniversalMapping(pkgName=prefix,
                         chipSrc=chipSrc,
                         outputDir=outputDir)

    #define the substitution needed by the support functions.
    subStrs <- c("coreTab"="genes","coreID"="gene_id","suffix"="EG","org"="human","cntrTab"="genes", "prefix"=prefix, "outDir"=outputDir, "Db_type"="OrgDb")
    require("RSQLite")
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = file.path(outputDir, paste(prefix,".sqlite", sep="")) )
    sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipSrc,"' AS anno;",sep="") )

    appendPreMeta(db, subStrs=subStrs, printSchema=printSchema, metaDataSrc=metaDataSrc)
    appendGenes(db, subStrs=subStrs, printSchema=printSchema)
    appendGeneInfo(db, subStrs=subStrs, printSchema=printSchema)

    appendChromosomes(db, subStrs=subStrs, printSchema=printSchema)
    appendAccessions(db, subStrs=subStrs, printSchema=printSchema)
##     appendCytogenicLocs(db, subStrs=subStrs, printSchema=printSchema)
##     appendOmim(db, subStrs=subStrs, printSchema=printSchema)
    appendRefseq(db, subStrs=subStrs, printSchema=printSchema)
    appendPubmed(db, subStrs=subStrs, printSchema=printSchema)
##     appendUnigene(db, subStrs=subStrs, printSchema=printSchema)
    appendChrlengths(db, subStrs=subStrs, printSchema=printSchema)
    appendGO(db, subStrs=subStrs, printSchema=printSchema)
    appendGOALL(db, subStrs=subStrs, printSchema=printSchema) 
    appendKEGG(db, subStrs=subStrs, printSchema=printSchema)
    appendEC(db, subStrs=subStrs, printSchema=printSchema)
##     appendChromsomeLocs(db, subStrs=subStrs, printSchema=printSchema)
##     appendPfam(db, subStrs=subStrs, printSchema=printSchema)
##     appendProsite(db, subStrs=subStrs, printSchema=printSchema)
    appendAlias(db, subStrs=subStrs, printSchema=printSchema)
##     appendEnsembl(db, subStrs=subStrs, printSchema=printSchema)
##     appendEnsemblProt(db, subStrs=subStrs, printSchema=printSchema)
##     appendEnsemblTrans(db, subStrs=subStrs, printSchema=printSchema)

    appendYeastNCBILocusTags(db, subStrs=subStrs, printSchema=printSchema)
    appendYeastNCBISGD(db, subStrs=subStrs, printSchema=printSchema)
    appendUniprot(db, subStrs=subStrs, printSchema=printSchema)
    makeGOViews(db)
    
    appendPostMeta(db, subStrs=subStrs)
    
    dbDisconnect(db)
    
    analyzeVacuumDisconnect(drv,outputDir,prefix)
}







available.db0pkgs <- function()
{
  url <-
  "http://bioconductor.org/packages/release/data/annotation/src/contrib"
  pkgs <- available.packages(url)[, "Package"]
  pkgs <- pkgs[substr(pkgs, nchar(pkgs)-3, nchar(pkgs)) == ".db0"]
  names(pkgs) <- NULL
  pkgs
}

available.dbschemas <- function(){
  ##need a generic path
  path = system.file(package="AnnotationDbi")
  path = paste(path,"/DBschemas/schemas_1.0", sep = "")
  list = list.files(path)
  list = list[grep("_DB.sql", list)]
  list = gsub(".sql","",list)  
  list
}

available.chipdbschemas <- function(){
  list = available.dbschemas()
  list = list[grep("CHIP_DB", list)]
  list
}




##Generate a schema for an installed package.  Why installed?  Because I will
##be the primary user of this, and it is good for me to have to install at
##least one example of each type of supported package while testing.  Also,
##this means that these things will always be in a standardized place which
##will make it easier to script this stuff.  Also, this will ensure that
##updating the schemas and bumping AnnotationDbi will be one of the last
##things I do.  This function ONLY works with SQLite DBs (all we use)
generate.schema <- function(name = "HUMANCHIP_DB", pkg = "hgu95av2.db", path = "." ){
    pkgname = sub(".db",".sqlite",pkg)
    dbFile = system.file("extdata",pkgname,package=pkg)
    drv = dbDriver("SQLite")
    con = dbConnect(drv, dbname=dbFile)
    sql = "SELECT * FROM sqlite_master;"
    res = dbGetQuery(con, sql)
    ##Now we have to format it nicely...
    res = res[,5]
    res = res[!is.na(res)]
    ##Add semicolons to the end of the elements in res
    res = gsub("$",";",res,perl=TRUE)
    
    ##Insert some lines right before the end index:
    indNums = grep("INDEX",res) #number to insert lines before.
    nonIndNums = grep("INDEX",res, invert=TRUE)
    ##Tnen use paste to insert the comments
    indexComment = "\n-- Explicit index creation on the referencing column of all the foreign keys.\n-- Note that this is only needed for SQLite: PostgreSQL and MySQL create those\n-- indexes automatically."

    res = c(res[nonIndNums],indexComment,res[indNums])
    
    ##Then write it out to a file.
    write(res, file = paste( name,".sql", sep=""))
    ##Then disconnect (so there won't be limits on how many times we can call this)
    dbDisconnect(con)
}



analyzeVacuumDisconnect <- function(drv,outputDir,prefix){
  db <- dbConnect(drv, dbname = file.path(outputDir,
                  paste(prefix,".sqlite", sep="")) )
  sqliteQuickSQL(db, "ANALYZE;")
  sqliteQuickSQL(db, "VACUUM;")
  dbDisconnect(db)  
}



#Test formula to just see if my generic functions work:

makeFooPkg <- function(){

    subStrs <- c("coreTab"="probes","coreID"="probe_id","suffix"="PROBE","org"="human","cntrTab"="genes", "prefix"="pkgFoo", "outDir"=".")    
    require("RSQLite")
    drv <- dbDriver("SQLite")

    db <- dbConnect(drv,dbname=paste(subStrs[["prefix"]],".sqlite", sep="")) 
    createCntrTableGeneric(db, subStrs, printSchema=FALSE, table="genes", field="gene_id",fileName="fakeyIDs.txt")
    dbDisconnect(db)

    db <- dbConnect(drv,dbname=paste(subStrs[["prefix"]],".sqlite", sep=""))
    appendProbesGeneric(db, subStrs, printSchema=FALSE, table="probes", matchID="gene_id", field="probe_id", fileName="fakeyProbes.txt", mapCounts="PROBECOUNT")
    dbDisconnect(db)

    db <- dbConnect(drv,dbname=paste(subStrs[["prefix"]],".sqlite", sep=""))
    appendGeneric(db, subStrs, printSchema=FALSE, table="refseq", matchID="gene_id", field="refseq_id", fileName="fakeyRefSeqs.txt", mapCounts="FOOCOUNT")
    dbDisconnect(db)
}
