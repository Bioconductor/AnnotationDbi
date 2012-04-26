
##TODO: make these so that they look for the correct kind of chipSrc and chipMapSrc files...  Probably this should be tied to these databases being downloadable to a standard place using biocLite.  For now, its a parameter, but there can be a default location added later.


makeHUMANCHIP_DB <- function(...){
    .Defunct("makeDBPackage", package="AnnotationDbi", msg = "This is no longer the function to use for making chip packages. Please use makeDBPackage() instead.")
}
makeMOUSECHIP_DB <- function(...){
    .Defunct("makeDBPackage", package="AnnotationDbi", msg = "This is no longer the function to use for making chip packages. Please use makeDBPackage() instead.")
}
makeRATCHIP_DB <- function(...){
    .Defunct("makeDBPackage", package="AnnotationDbi", msg = "This is no longer the function to use for making chip packages. Please use makeDBPackage() instead.")
}
makeFLYCHIP_DB <- function(...){
    .Defunct("makeDBPackage", package="AnnotationDbi", msg = "This is no longer the function to use for making chip packages. Please use makeDBPackage() instead.")
}
makeYEASTCHIP_DB <- function(...){
    .Defunct("makeDBPackage", package="AnnotationDbi", msg = "This is no longer the function to use for making chip packages. Please use makeDBPackage() instead.")
}

makeZEBRAFISHCHIP_DB <- function(...){
    .Defunct("makeDBPackage", package="AnnotationDbi", msg = "This is no longer the function to use for making chip packages. Please use makeDBPackage() instead.")
}

makeECOLICHIP_DB <- function(...){
    .Defunct("makeDBPackage", package="AnnotationDbi", msg = "This is no longer the function to use for making chip packages. Please use makeDBPackage() instead.")
}

makeCANINECHIP_DB <- function(...){
    .Defunct("makeDBPackage", package="AnnotationDbi", msg = "This is no longer the function to use for making chip packages. Please use makeDBPackage() instead.")
}

makeBOVINECHIP_DB <- function(...){
    .Defunct("makeDBPackage", package="AnnotationDbi", msg = "This is no longer the function to use for making chip packages. Please use makeDBPackage() instead.")
}

makeWORMCHIP_DB <- function(...){
    .Defunct("makeDBPackage", package="AnnotationDbi", msg = "This is no longer the function to use for making chip packages. Please use makeDBPackage() instead.")
}

makePIGCHIP_DB <- function(...){
    .Defunct("makeDBPackage", package="AnnotationDbi", msg = "This is no longer the function to use for making chip packages. Please use makeDBPackage() instead.")
}

makeCHICKENCHIP_DB <- function(...){
    .Defunct("makeDBPackage", package="AnnotationDbi", msg = "This is no longer the function to use for making chip packages. Please use makeDBPackage() instead.")
}

makeARABIDOPSISCHIP_DB <- function(...){
    .Defunct("makeDBPackage", package="AnnotationDbi", msg = "This is no longer the function to use for making chip packages. Please use makeDBPackage() instead.")
}

makeRHESUSCHIP_DB <- function(...){
    .Deprecated("makeDBPackage", package="AnnotationDbi", msg = "This is no longer the function to use for making chip packages. Please use makeDBPackage() instead.")
    makeDBPackage("RHESUSCHIP_DB", ...)    
}

.makeHUMANCHIP_DB <- function(affy,
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


.makeMOUSECHIP_DB <- function(affy,
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


.makeRATCHIP_DB <- function(affy,
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



.makeFLYCHIP_DB <- function(affy,
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



.makeARABIDOPSISCHIP_DB <- function(affy,
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


.makeYEASTCHIP_DB <- function(affy,
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



.makeZEBRAFISHCHIP_DB <- function(affy,
                             prefix,
                             fileName,
                             otherSrc = character(0),
                             chipMapSrc = system.file("extdata", "chipmapsrc_zebrafish.sqlite", package="zebrafish.db0"),
                             chipSrc = system.file("extdata", "chipsrc_zebrafish.sqlite", package="zebrafish.db0"),
                             baseMapType,
                             outputDir = ".",
                             version,
                             manufacturer = "Manufacturer not specified",
                             chipName = "ChipName not specified",
                             manufacturerUrl = "Manufacturer Url not specified",
                             author = "Marc Carlson, Seth Falcon, Herve Pages, Nianhua Li",
                             maintainer = "Biocore Data Team <biocannotation@lists.fhcrc.org>"){

    if(outputDir!="." && file.access(outputDir)[[1]]!=0){stop("Selected outputDir '", outputDir,"' does not exist.")}
    
    metaDataSrc <- c(DBSCHEMA="ZEBRAFISHCHIP_DB",
                     ORGANISM="Danio rerio",
                     SPECIES="Zebrafish",
                     MANUFACTURER=manufacturer,
                     CHIPNAME=chipName,
                     MANUFACTURERURL=manufacturerUrl)

    popZEBRAFISHCHIPDB(affy = affy,
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
                PkgTemplate="ZEBRAFISHCHIP.DB",
                AnnObjPrefix=prefix
                )

    makeAnnDbPkg(seed, paste(outputDir,"/", prefix,".sqlite", sep=""), dest_dir = outputDir)

}



.makeECOLICHIP_DB <- function(affy,
                             prefix,
                             fileName,
                             otherSrc = character(0),
                             chipMapSrc = system.file("extdata", "chipmapsrc_ecoliK12.sqlite", package="ecoliK12.db0"),
                             chipSrc = system.file("extdata", "chipsrc_ecoliK12.sqlite", package="ecoliK12.db0"),
                             baseMapType,
                             outputDir = ".",
                             version,
                             manufacturer = "Manufacturer not specified",
                             chipName = "ChipName not specified",
                             manufacturerUrl = "Manufacturer Url not specified",
                             author = "Marc Carlson, Seth Falcon, Herve Pages, Nianhua Li",
                             maintainer = "Biocore Data Team <biocannotation@lists.fhcrc.org>"){

    if(outputDir!="." && file.access(outputDir)[[1]]!=0){stop("Selected outputDir '", outputDir,"' does not exist.")}
    
    metaDataSrc <- c(DBSCHEMA="ECOLICHIP_DB",
                     ORGANISM="Escherichia coli",
                     SPECIES="E coli",
                     MANUFACTURER=manufacturer,
                     CHIPNAME=chipName,
                     MANUFACTURERURL=manufacturerUrl)
    
    popECOLICHIPDB(affy = affy,
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
                PkgTemplate="ECOLICHIP.DB",
                AnnObjPrefix=prefix
                )

    makeAnnDbPkg(seed, paste(outputDir,"/", prefix,".sqlite", sep=""), dest_dir = outputDir)

}




.makeCANINECHIP_DB <- function(affy,
                             prefix,
                             fileName,
                             otherSrc = character(0),
                             chipMapSrc = system.file("extdata", "chipmapsrc_canine.sqlite", package="canine.db0"),
                             chipSrc = system.file("extdata", "chipsrc_canine.sqlite", package="canine.db0"),
                             baseMapType,
                             outputDir = ".",
                             version,
                             manufacturer = "Manufacturer not specified",
                             chipName = "ChipName not specified",
                             manufacturerUrl = "Manufacturer Url not specified",
                             author = "Marc Carlson, Seth Falcon, Herve Pages, Nianhua Li",
                             maintainer = "Biocore Data Team <biocannotation@lists.fhcrc.org>"){

    if(outputDir!="." && file.access(outputDir)[[1]]!=0){stop("Selected outputDir '", outputDir,"' does not exist.")}
    
    metaDataSrc <- c(DBSCHEMA="CANINECHIP_DB",
                     ORGANISM="Canis familiaris",
                     SPECIES="Canine",
                     MANUFACTURER=manufacturer,
                     CHIPNAME=chipName,
                     MANUFACTURERURL=manufacturerUrl)

    popCANINECHIPDB(affy = affy,
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
                PkgTemplate="CANINECHIP.DB",
                AnnObjPrefix=prefix
                )

    makeAnnDbPkg(seed, paste(outputDir,"/", prefix,".sqlite", sep=""), dest_dir = outputDir)

}



.makeBOVINECHIP_DB <- function(affy,
                             prefix,
                             fileName,
                             otherSrc = character(0),
                             chipMapSrc = system.file("extdata", "chipmapsrc_bovine.sqlite", package="bovine.db0"),
                             chipSrc = system.file("extdata", "chipsrc_bovine.sqlite", package="bovine.db0"),
                             baseMapType,
                             outputDir = ".",
                             version,
                             manufacturer = "Manufacturer not specified",
                             chipName = "ChipName not specified",
                             manufacturerUrl = "Manufacturer Url not specified",
                             author = "Marc Carlson, Seth Falcon, Herve Pages, Nianhua Li",
                             maintainer = "Biocore Data Team <biocannotation@lists.fhcrc.org>"){

    if(outputDir!="." && file.access(outputDir)[[1]]!=0){stop("Selected outputDir '", outputDir,"' does not exist.")}
    
    metaDataSrc <- c(DBSCHEMA="BOVINECHIP_DB",
                     ORGANISM="Bos taurus",
                     SPECIES="Bovine",
                     MANUFACTURER=manufacturer,
                     CHIPNAME=chipName,
                     MANUFACTURERURL=manufacturerUrl)

    popBOVINECHIPDB(affy = affy,
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
                PkgTemplate="BOVINECHIP.DB",
                AnnObjPrefix=prefix
                )

    makeAnnDbPkg(seed, paste(outputDir,"/", prefix,".sqlite", sep=""), dest_dir = outputDir)

}



.makeWORMCHIP_DB <- function(affy,
                             prefix,
                             fileName,
                             otherSrc = character(0),
                             chipMapSrc = system.file("extdata", "chipmapsrc_worm.sqlite", package="worm.db0"),
                             chipSrc = system.file("extdata", "chipsrc_worm.sqlite", package="worm.db0"),
                             baseMapType,
                             outputDir = ".",
                             version,
                             manufacturer = "Manufacturer not specified",
                             chipName = "ChipName not specified",
                             manufacturerUrl = "Manufacturer Url not specified",
                             author = "Marc Carlson, Seth Falcon, Herve Pages, Nianhua Li",
                             maintainer = "Biocore Data Team <biocannotation@lists.fhcrc.org>"){

    if(outputDir!="." && file.access(outputDir)[[1]]!=0){stop("Selected outputDir '", outputDir,"' does not exist.")}
    
    metaDataSrc <- c(DBSCHEMA="WORMCHIP_DB",
                     ORGANISM="Caenorhabditis elegans",
                     SPECIES="Worm",
                     MANUFACTURER=manufacturer,
                     CHIPNAME=chipName,
                     MANUFACTURERURL=manufacturerUrl)

    popWORMCHIPDB(affy = affy,
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
                PkgTemplate="WORMCHIP.DB",
                AnnObjPrefix=prefix
                )

    makeAnnDbPkg(seed, paste(outputDir,"/", prefix,".sqlite", sep=""), dest_dir = outputDir)

}


.makePIGCHIP_DB <- function(affy,
                             prefix,
                             fileName,
                             otherSrc = character(0),
                             chipMapSrc = system.file("extdata", "chipmapsrc_pig.sqlite", package="pig.db0"),
                             chipSrc = system.file("extdata", "chipsrc_pig.sqlite", package="pig.db0"),
                             baseMapType,
                             outputDir = ".",
                             version,
                             manufacturer = "Manufacturer not specified",
                             chipName = "ChipName not specified",
                             manufacturerUrl = "Manufacturer Url not specified",
                             author = "Marc Carlson, Seth Falcon, Herve Pages, Nianhua Li",
                             maintainer = "Biocore Data Team <biocannotation@lists.fhcrc.org>"){

    if(outputDir!="." && file.access(outputDir)[[1]]!=0){stop("Selected outputDir '", outputDir,"' does not exist.")}
    
    metaDataSrc <- c(DBSCHEMA="PIGCHIP_DB",
                     ORGANISM="Sus scrofa",
                     SPECIES="Pig",
                     MANUFACTURER=manufacturer,
                     CHIPNAME=chipName,
                     MANUFACTURERURL=manufacturerUrl)

    popPIGCHIPDB(affy = affy,
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
                PkgTemplate="PIGCHIP.DB",
                AnnObjPrefix=prefix
                )

    makeAnnDbPkg(seed, paste(outputDir,"/", prefix,".sqlite", sep=""), dest_dir = outputDir)

}


.makeCHICKENCHIP_DB <- function(affy,
                             prefix,
                             fileName,
                             otherSrc = character(0),
                             chipMapSrc = system.file("extdata", "chipmapsrc_chicken.sqlite", package="chicken.db0"),
                             chipSrc = system.file("extdata", "chipsrc_chicken.sqlite", package="chicken.db0"),
                             baseMapType,
                             outputDir = ".",
                             version,
                             manufacturer = "Manufacturer not specified",
                             chipName = "ChipName not specified",
                             manufacturerUrl = "Manufacturer Url not specified",
                             author = "Marc Carlson, Seth Falcon, Herve Pages, Nianhua Li",
                             maintainer = "Biocore Data Team <biocannotation@lists.fhcrc.org>"){

    if(outputDir!="." && file.access(outputDir)[[1]]!=0){stop("Selected outputDir '", outputDir,"' does not exist.")}
    
    metaDataSrc <- c(DBSCHEMA="CHICKENCHIP_DB",
                     ORGANISM="Gallus gallus",
                     SPECIES="Chicken",
                     MANUFACTURER=manufacturer,
                     CHIPNAME=chipName,
                     MANUFACTURERURL=manufacturerUrl)

    popCHICKENCHIPDB(affy = affy,
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
                PkgTemplate="CHICKENCHIP.DB",
                AnnObjPrefix=prefix
                )

    makeAnnDbPkg(seed, paste(outputDir,"/", prefix,".sqlite", sep=""), dest_dir = outputDir)

}


.makeXENOPUSCHIP_DB <- function(affy,
                            prefix,
                            fileName,
                            otherSrc = character(0),
                            chipMapSrc = system.file("extdata", "chipmapsrc_xenopus.sqlite", package="xenopus.db0"),
                            chipSrc = system.file("extdata", "chipsrc_xenopus.sqlite", package="xenopus.db0"),
                            baseMapType,
                            outputDir = ".",
                            version,
                            manufacturer = "Manufacturer not specified",
                            chipName = "ChipName not specified",
                            manufacturerUrl = "Manufacturer Url not specified",
                            author = "Marc Carlson, Seth Falcon, Herve Pages, Nianhua Li",
                            maintainer = "Biocore Data Team <biocannotation@lists.fhcrc.org>"){

    if(outputDir!="." && file.access(outputDir)[[1]]!=0){stop("Selected outputDir '", outputDir,"' does not exist.")}
    
    metaDataSrc <- c(DBSCHEMA="XENOPUSCHIP_DB",
                     ORGANISM="Xenopus laevis",
                     SPECIES="Xenopus",
                     MANUFACTURER=manufacturer,
                     CHIPNAME=chipName,
                     MANUFACTURERURL=manufacturerUrl)

    popXENOPUSCHIPDB(affy = affy,
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
                PkgTemplate="XENOPUSCHIP.DB",
                AnnObjPrefix=prefix
                )

    makeAnnDbPkg(seed, paste(outputDir,"/", prefix,".sqlite", sep=""), dest_dir = outputDir)

}


.makeRHESUSCHIP_DB <- function(affy,
                           prefix,
                           fileName,
                           otherSrc = character(0),
                           chipMapSrc = system.file("extdata", "chipmapsrc_rhesus.sqlite", package="rhesus.db0"),
                           chipSrc = system.file("extdata", "chipsrc_rhesus.sqlite", package="rhesus.db0"),
                           baseMapType,
                           outputDir = ".",
                           version,
                           manufacturer = "Manufacturer not specified",
                           chipName = "ChipName not specified",
                           manufacturerUrl = "Manufacturer Url not specified",
                           author = "Marc Carlson, Seth Falcon, Herve Pages, Nianhua Li",
                           maintainer = "Biocore Data Team <biocannotation@lists.fhcrc.org>"){

    if(outputDir!="." && file.access(outputDir)[[1]]!=0){stop("Selected outputDir '", outputDir,"' does not exist.")}
    
    metaDataSrc <- c(DBSCHEMA="RHESUSCHIP_DB",
                     ORGANISM="Macaca mulatta",
                     SPECIES="Rhesus",
                     MANUFACTURER=manufacturer,
                     CHIPNAME=chipName,
                     MANUFACTURERURL=manufacturerUrl)

    popRHESUSCHIPDB(affy = affy,
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
                PkgTemplate="RHESUSCHIP.DB",
                AnnObjPrefix=prefix
                )

    makeAnnDbPkg(seed, paste(outputDir,"/", prefix,".sqlite", sep=""), dest_dir = outputDir)

}
