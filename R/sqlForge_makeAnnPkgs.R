
##TODO: make these so that they look for the correct kind of chipSrc and chipMapSrc files...  Probably this should be tied to these databases being downloadable to a standard place using biocLite.  For now, its a parameter, but there can be a default location added later.

makeHUMANCHIP_DB <- function(affy,
                             prefix,
                             fileName,
                             otherSrc = character(0),
                             chipMapSrc,
                             chipSrc,
                             baseMapType,
                             outputDir = ".",
                             version,
                             manufacturer = "Manufacturer not specified",
                             chipName = "ChipName not specified",
                             manufacturerUrl = "Manufacturer Url not specified"){
    
    metaDataSrc <- c(DBSCHEMA="HUMANCHIP_DB",
                     ORGANISM="Homo sapiens",
                     SPECIES="Human",
                     MANUFACTURER=manufacturer,
                     CHIPNAME=chipName,
                     MANUFACTURERURL=manufacturerUrl)

    popHUMANCHIPDB(affy = affy,
                   prefix = prefix,
                   fileName = fileName,
                   chipMapSrc = chipMapSrc,
                   chipSrc = chipSrc,
                   metaDataSrc = metaDataSrc,
                   otherSrc = otherSrc,
                   baseMapType=baseMapType,
                   outputDir=outputDir,
                   printSchema=FALSE)

    seed <- new("AnnDbPkgSeed",
                Package= paste(prefix,".db",sep=""),
                Version=version,
                PkgTemplate="HUMANCHIP.DB",
                AnnObjPrefix=prefix
                )

    makeAnnDbPkg(seed, paste(outputDir,"/", prefix,".sqlite", sep=""), dest_dir = outputDir)

}


makeMOUSECHIP_DB <- function(affy,
                             prefix,
                             fileName,
                             otherSrc = character(0),
                             chipMapSrc,
                             chipSrc,
                             baseMapType,
                             outputDir = ".",
                             version,
                             manufacturer = "Manufacturer not specified",
                             chipName = "ChipName not specified",
                             manufacturerUrl = "Manufacturer Url not specified"){
    
    metaDataSrc <- c(DBSCHEMA="MOUSECHIP_DB",
                     ORGANISM="Mus musculus",
                     SPECIES="Mouse",
                     MANUFACTURER=manufacturer,
                     CHIPNAME=chipName,
                     MANUFACTURERURL=manufacturerUrl)

    popMOUSECHIPDB(affy = affy,
                   prefix = prefix,
                   fileName = fileName,
                   chipMapSrc = chipMapSrc,
                   chipSrc = chipSrc,
                   metaDataSrc = metaDataSrc,
                   otherSrc = otherSrc,
                   baseMapType=baseMapType,
                   outputDir=outputDir,
                   printSchema=FALSE)

    seed <- new("AnnDbPkgSeed",
                Package= paste(prefix,".db",sep=""),
                Version=version,
                PkgTemplate="MOUSECHIP.DB",
                AnnObjPrefix=prefix
                )

    makeAnnDbPkg(seed, paste(outputDir,"/", prefix,".sqlite", sep=""), dest_dir = outputDir)

}


makeRATCHIP_DB <- function(affy,
                           prefix,
                           fileName,
                           otherSrc = character(0),
                           chipMapSrc,
                           chipSrc,
                           baseMapType,
                           outputDir = ".",
                           version,
                           manufacturer = "Manufacturer not specified",
                           chipName = "ChipName not specified",
                           manufacturerUrl = "Manufacturer Url not specified"){
    
    metaDataSrc <- c(DBSCHEMA="RATCHIP_DB",
                     ORGANISM="Rattus norvegicus",
                     SPECIES="Rat",
                     MANUFACTURER=manufacturer,
                     CHIPNAME=chipName,
                     MANUFACTURERURL=manufacturerUrl)

    popRATCHIPDB(affy = affy,
                 prefix = prefix,
                 fileName = fileName,
                 chipMapSrc = chipMapSrc,
                 chipSrc = chipSrc,
                 metaDataSrc = metaDataSrc,
                 otherSrc = otherSrc,
                 baseMapType=baseMapType,
                 outputDir=outputDir,
                 printSchema=FALSE)

    seed <- new("AnnDbPkgSeed",
                Package= paste(prefix,".db",sep=""),
                Version=version,
                PkgTemplate="RATCHIP.DB",
                AnnObjPrefix=prefix
                )

    makeAnnDbPkg(seed, paste(outputDir,"/", prefix,".sqlite", sep=""), dest_dir = outputDir)

}



makeFLYCHIP_DB <- function(affy,
                           prefix,
                           fileName,
                           otherSrc = character(0),
                           chipMapSrc,
                           chipSrc,
                           baseMapType,
                           outputDir = ".",
                           version,
                           manufacturer = "Manufacturer not specified",
                           chipName = "ChipName not specified",
                           manufacturerUrl = "Manufacturer Url not specified"){
    
    metaDataSrc <- c(DBSCHEMA="FLYCHIP_DB",
                     ORGANISM="Drosophila melanogaster",
                     SPECIES="Fly",
                     MANUFACTURER=manufacturer,
                     CHIPNAME=chipName,
                     MANUFACTURERURL=manufacturerUrl)

    popFLYCHIPDB(affy = affy,
                 prefix = prefix,
                 fileName = fileName,
                 chipMapSrc = chipMapSrc,
                 chipSrc = chipSrc,
                 metaDataSrc = metaDataSrc,
                 otherSrc = otherSrc,
                 baseMapType=baseMapType,
                 outputDir=outputDir,
                 printSchema=FALSE)

    seed <- new("AnnDbPkgSeed",
                Package= paste(prefix,".db",sep=""),
                Version=version,
                PkgTemplate="FLYCHIP.DB",
                AnnObjPrefix=prefix
                )

    makeAnnDbPkg(seed, paste(outputDir,"/", prefix,".sqlite", sep=""), dest_dir = outputDir)

}



makeARABIDOPSISCHIP_DB <- function(affy,
                                   prefix,
                                   fileName,
                                   chipMapSrc,
                                   chipSrc,
                                   outputDir = ".",
                                   version,
                                   manufacturer = "Manufacturer not specified",
                                   chipName = "ChipName not specified",
                                   manufacturerUrl = "Manufacturer Url not specified"){
    
    metaDataSrc <- c(DBSCHEMA="ARABIDOPSISCHIP_DB",
                     ORGANISM="Arabidopsis thaliana",
                     SPECIES="Arabidosis",
                     MANUFACTURER=manufacturer,
                     CHIPNAME=chipName,
                     MANUFACTURERURL=manufacturerUrl)

    popARABIDOPSISCHIPDB(affy = affy,
                         prefix = prefix,
                         fileName = fileName,
                         chipMapSrc = chipMapSrc,
                         chipSrc = chipSrc,
                         metaDataSrc = metaDataSrc,
                         outputDir = outputDir,
                         printSchema = FALSE)

    seed <- new("AnnDbPkgSeed",
                Package= paste(prefix,".db",sep=""),
                Version=version,
                PkgTemplate="ARABIDOPSISCHIP.DB",
                AnnObjPrefix=prefix
                )

    makeAnnDbPkg(seed, paste(outputDir,"/", prefix,".sqlite", sep=""), dest_dir = outputDir)

}


makeYEASTCHIP_DB <- function(affy,
                             prefix,
                             fileName,
                             chipSrc,
                             outputDir = ".",
                             version,
                             manufacturer = "Manufacturer not specified",
                             chipName = "ChipName not specified",
                             manufacturerUrl = "Manufacturer Url not specified"){
    
    metaDataSrc <- c(DBSCHEMA="YEASTCHIP_DB",
                     ORGANISM="Saccharomyces cerevisiae",
                     SPECIES="Yeast",
                     MANUFACTURER=manufacturer,
                     CHIPNAME=chipName,
                     MANUFACTURERURL=manufacturerUrl)

    popYEASTCHIPDB(affy = affy,
                   prefix = prefix,
                   fileName = fileName,
                   chipSrc = chipSrc,
                   metaDataSrc = metaDataSrc,
                   outputDir=outputDir,
                   printSchema=FALSE)

    seed <- new("AnnDbPkgSeed",
                Package= paste(prefix,".db",sep=""),
                Version=version,
                PkgTemplate="YEASTCHIP.DB",
                AnnObjPrefix=prefix
                )

    makeAnnDbPkg(seed, paste(outputDir,"/", prefix,".sqlite", sep=""), dest_dir = outputDir)

}







