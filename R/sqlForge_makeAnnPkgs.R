
##TODO: make these so that they look for the correct kind of chipSrc and chipMapSrc files...  Probably this should be tied to these databases being downloadable to a standard place using biocLite.  For now, its a parameter, but there can be a default location added later.

makeHUMANCHIP_DB <- function(affy,
                             prefix,
                             fileName,
                             otherSrc = character(0),
                             chipMapSrc = system.file("extdata", "chipmapsrc_human.sqlite", package="human.db0"),
                             chipSrc = system.file("extdata", "chipsrc_human.sqlite", package="human.db0"),
                             baseMapType,
                             outputDir = ".",
                             version,
                             manufacturer = "Manufacturer not specified",
                             chipName = "ChipName not specified",
                             manufacturerUrl = "Manufacturer Url not specified",
                             author = "Marc Carlson, Seth Falcon, Herve Pages, Nianhua Li",
                             maintainer = "Biocore Data Team <biocannotation@lists.fhcrc.org>"){

    if(outputDir!="." && file.access(outputDir)[[1]]!=0){stop("Selected outputDir '", outputDir,"' does not exist.")}
    
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
                Author=author,
                Maintainer=maintainer,
                PkgTemplate="HUMANCHIP.DB",
                AnnObjPrefix=prefix
                )

    makeAnnDbPkg(seed, paste(outputDir,"/", prefix,".sqlite", sep=""), dest_dir = outputDir)

}


makeMOUSECHIP_DB <- function(affy,
                             prefix,
                             fileName,
                             otherSrc = character(0),
                             chipMapSrc = system.file("extdata", "chipmapsrc_mouse.sqlite", package="mouse.db0"),
                             chipSrc = system.file("extdata", "chipsrc_mouse.sqlite", package="mouse.db0"),
                             baseMapType,
                             outputDir = ".",
                             version,
                             manufacturer = "Manufacturer not specified",
                             chipName = "ChipName not specified",
                             manufacturerUrl = "Manufacturer Url not specified",
                             author = "Marc Carlson, Seth Falcon, Herve Pages, Nianhua Li",
                             maintainer = "Biocore Data Team <biocannotation@lists.fhcrc.org>"){

    if(outputDir!="." && file.access(outputDir)[[1]]!=0){stop("Selected outputDir '", outputDir,"' does not exist.")}
    
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
                Author=author,
                Maintainer=maintainer,
                PkgTemplate="MOUSECHIP.DB",
                AnnObjPrefix=prefix
                )

    makeAnnDbPkg(seed, paste(outputDir,"/", prefix,".sqlite", sep=""), dest_dir = outputDir)

}


makeRATCHIP_DB <- function(affy,
                           prefix,
                           fileName,
                           otherSrc = character(0),
                           chipMapSrc = system.file("extdata", "chipmapsrc_rat.sqlite", package="rat.db0"),
                           chipSrc = system.file("extdata", "chipsrc_rat.sqlite", package="rat.db0"),
                           baseMapType,
                           outputDir = ".",
                           version,
                           manufacturer = "Manufacturer not specified",
                           chipName = "ChipName not specified",
                           manufacturerUrl = "Manufacturer Url not specified",
                           author = "Marc Carlson, Seth Falcon, Herve Pages, Nianhua Li",
                           maintainer = "Biocore Data Team <biocannotation@lists.fhcrc.org>"){

    if(outputDir!="." && file.access(outputDir)[[1]]!=0){stop("Selected outputDir '", outputDir,"' does not exist.")}
    
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
                Author=author,
                Maintainer=maintainer,
                PkgTemplate="RATCHIP.DB",
                AnnObjPrefix=prefix
                )

    makeAnnDbPkg(seed, paste(outputDir,"/", prefix,".sqlite", sep=""), dest_dir = outputDir)

}



makeFLYCHIP_DB <- function(affy,
                           prefix,
                           fileName,
                           otherSrc = character(0),
                           chipMapSrc = system.file("extdata", "chipmapsrc_fly.sqlite", package="fly.db0"),
                           chipSrc = system.file("extdata", "chipsrc_fly.sqlite", package="fly.db0"),
                           baseMapType,
                           outputDir = ".",
                           version,
                           manufacturer = "Manufacturer not specified",
                           chipName = "ChipName not specified",
                           manufacturerUrl = "Manufacturer Url not specified",
                           author = "Marc Carlson, Seth Falcon, Herve Pages, Nianhua Li",
                           maintainer = "Biocore Data Team <biocannotation@lists.fhcrc.org>"){

    if(outputDir!="." && file.access(outputDir)[[1]]!=0){stop("Selected outputDir '", outputDir,"' does not exist.")}
    
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
                Author=author,
                Maintainer=maintainer,
                PkgTemplate="FLYCHIP.DB",
                AnnObjPrefix=prefix
                )

    makeAnnDbPkg(seed, paste(outputDir,"/", prefix,".sqlite", sep=""), dest_dir = outputDir)

}



makeARABIDOPSISCHIP_DB <- function(affy,
                                   prefix,
                                   fileName = "myFile.txt",
                                   chipMapSrc = system.file("extdata", "chipmapsrc_arabidopsis.sqlite", package="arabidopsis.db0"),
                                   chipSrc = system.file("extdata", "chipsrc_arabidopsis.sqlite", package="arabidopsis.db0"),
                                   outputDir = ".",
                                   version,
                                   manufacturer = "Manufacturer not specified",
                                   chipName = "ChipName not specified",
                                   manufacturerUrl = "Manufacturer Url not specified",
                                   author = "Marc Carlson, Seth Falcon, Herve Pages, Nianhua Li",
                                   maintainer = "Biocore Data Team <biocannotation@lists.fhcrc.org>"){

    if(outputDir!="." && file.access(outputDir)[[1]]!=0){stop("Selected outputDir '", outputDir,"' does not exist.")}
    
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
                Author=author,
                Maintainer=maintainer,
                PkgTemplate="ARABIDOPSISCHIP.DB",
                AnnObjPrefix=prefix
                )

    makeAnnDbPkg(seed, paste(outputDir,"/", prefix,".sqlite", sep=""), dest_dir = outputDir)

}


makeYEASTCHIP_DB <- function(affy,
                             prefix,
                             fileName,
                             chipSrc = system.file("extdata", "chipsrc_yeast.sqlite", package="yeast.db0"),
                             outputDir = ".",
                             version,
                             manufacturer = "Manufacturer not specified",
                             chipName = "ChipName not specified",
                             manufacturerUrl = "Manufacturer Url not specified",
                             author = "Marc Carlson, Seth Falcon, Herve Pages, Nianhua Li",
                             maintainer = "Biocore Data Team <biocannotation@lists.fhcrc.org>"){

    if(outputDir!="." && file.access(outputDir)[[1]]!=0){stop("Selected outputDir '", outputDir,"' does not exist.")}
    
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
                Author=author,
                Maintainer=maintainer,
                PkgTemplate="YEASTCHIP.DB",
                AnnObjPrefix=prefix
                )

    makeAnnDbPkg(seed, paste(outputDir,"/", prefix,".sqlite", sep=""), dest_dir = outputDir)

}







