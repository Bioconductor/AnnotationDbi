wrapBaseDBPackages <- function (dbPath = "/mnt/cpb_anno/mcarlson/proj/sqliteGen/nli/annosrc/db/",
                                destDir = ".",
                                version = "1.1.1"){

  #human.db0
  vals <- list(ANNOBJPREFIX="human",
               ORGANISM="Homo sapiens",
               SPECIES="human",
               AUTHOR="Marc Carlson, Nianhua Li, Herve Pages",
               PKGVERSION=version,
               LIC="The Artistic License, Version 2.0",
               BIOCVIEWS="AnnotationData, Homo_sapiens",
               ANNDBIVERSION="1.1.15")
  pkgName= paste(vals[["ANNOBJPREFIX"]],".db0",sep="")
  template=paste(system.file(package = "AnnotationDbi"), "/AnnDbPkg-templates/BASEPKG.DB", sep="")

  chipSrcFile = paste(dbPath,"chipsrc_human.sqlite",sep="")
  chipMapSrcFile = paste(dbPath,"chipmapsrc_human.sqlite",sep="")

  makeBasePackage(chipSrcFile, chipMapSrcFile, pkgName, destDir, template, vals)

  
  #mouse.db0 
  vals <- list(ANNOBJPREFIX="mouse",
               ORGANISM="Mus musculus",
               SPECIES="mouse",
               AUTHOR="Marc Carlson, Nianhua Li, Herve Pages",
               PKGVERSION=version,
               LIC="The Artistic License, Version 2.0",
               BIOCVIEWS="AnnotationData, Mus_musculus",
               ANNDBIVERSION="1.1.15")
  pkgName= paste(vals[["ANNOBJPREFIX"]],".db0",sep="")
  template=paste(system.file(package = "AnnotationDbi"), "/AnnDbPkg-templates/BASEPKG.DB", sep="")

  chipSrcFile = paste(dbPath,"chipsrc_mouse.sqlite",sep="")
  chipMapSrcFile = paste(dbPath,"chipmapsrc_mouse.sqlite",sep="")

  makeBasePackage(chipSrcFile, chipMapSrcFile, pkgName, destDir, template, vals)


  #rat.db0 
  vals <- list(ANNOBJPREFIX="rat",
               ORGANISM="Rattus norvegicus",
               SPECIES="rat",
               AUTHOR="Marc Carlson, Nianhua Li, Herve Pages",
               PKGVERSION=version,
               LIC="The Artistic License, Version 2.0",
               BIOCVIEWS="AnnotationData, Rattus_norvegicus",
               ANNDBIVERSION="1.1.15")
  pkgName= paste(vals[["ANNOBJPREFIX"]],".db0",sep="")
  template=paste(system.file(package = "AnnotationDbi"), "/AnnDbPkg-templates/BASEPKG.DB", sep="")

  chipSrcFile = paste(dbPath,"chipsrc_rat.sqlite",sep="")
  chipMapSrcFile = paste(dbPath,"chipmapsrc_rat.sqlite",sep="")

  makeBasePackage(chipSrcFile, chipMapSrcFile, pkgName, destDir, template, vals)


  #fly.db0 
  vals <- list(ANNOBJPREFIX="fly",
               ORGANISM="Drosophila melanogaster",
               SPECIES="fly",
               AUTHOR="Marc Carlson, Nianhua Li, Herve Pages",
               PKGVERSION=version,
               LIC="The Artistic License, Version 2.0",
               BIOCVIEWS="AnnotationData, Drosophila_melanogaster",
               ANNDBIVERSION="1.1.15")
  pkgName= paste(vals[["ANNOBJPREFIX"]],".db0",sep="")
  template=paste(system.file(package = "AnnotationDbi"), "/AnnDbPkg-templates/BASEPKG.DB", sep="")

  chipSrcFile = paste(dbPath,"chipsrc_fly.sqlite",sep="")
  chipMapSrcFile = paste(dbPath,"chipmapsrc_fly.sqlite",sep="")

  makeBasePackage(chipSrcFile, chipMapSrcFile, pkgName, destDir, template, vals)


  #yeast.db0 
  vals <- list(ANNOBJPREFIX="yeast",
               ORGANISM="Saccharomyces cerevisiae",
               SPECIES="yeast",
               AUTHOR="Marc Carlson, Nianhua Li, Herve Pages",
               PKGVERSION=version,
               LIC="The Artistic License, Version 2.0",
               BIOCVIEWS="AnnotationData, Saccharomyces_cerevisiae",
               ANNDBIVERSION="1.1.15")
  pkgName= paste(vals[["ANNOBJPREFIX"]],".db0",sep="")
  template=paste(system.file(package = "AnnotationDbi"), "/AnnDbPkg-templates/BASEPKG.DB", sep="")

  chipSrcFile = paste(dbPath,"chipsrc_yeast.sqlite",sep="")
  chipMapSrcFile = paste(dbPath,"chipmapsrc_yeast.sqlite",sep="")

  makeBasePackage(chipSrcFile, pkgName=pkgName, destDir=destDir, template=template, vals=vals)
  

  #arabidopsis.db0 
  vals <- list(ANNOBJPREFIX="arabidopsis",
               ORGANISM="Arabidopsis thaliana",
               SPECIES="arabidopsis",
               AUTHOR="Marc Carlson, Nianhua Li, Herve Pages",
               PKGVERSION=version,
               LIC="The Artistic License, Version 2.0",
               BIOCVIEWS="AnnotationData, Arabidopsis_thaliana",
               ANNDBIVERSION="1.1.15")
  pkgName= paste(vals[["ANNOBJPREFIX"]],".db0",sep="")
  template=paste(system.file(package = "AnnotationDbi"), "/AnnDbPkg-templates/BASEPKG.DB", sep="")

  chipSrcFile = paste(dbPath,"chipsrc_arabidopsis.sqlite",sep="")
  chipMapSrcFile = paste(dbPath,"chipmapsrc_arabidopsis.sqlite",sep="")

  makeBasePackage(chipSrcFile, chipMapSrcFile, pkgName, destDir, template, vals)  
}

#makeBase Package is just a utility function to minimize code bloat.
makeBasePackage <- function(chipSrcFile = NULL,#No chipSrcFile = no package
                            chipMapSrcFile = NULL,#No chipMapSrcFile is sometimes ok
                            pkgName,
                            destDir,
                            template,
                            vals){
  if(length(chipSrcFile)>0){ 
    createPackage(pkgname=pkgName,
                  destinationDir=destDir,
                  originDir=template,
                  symbolValues=vals)
  
    db_destDir = file.path(destDir,pkgName,"inst/extdata/")
    if (!file.copy(chipSrcFile, db_destDir)){
        stop("cannot copy file '", chipSrcFile, "' to '", db_destDir, "'")
    }
    if(length(chipMapSrcFile) > 0){
      if (!file.copy(chipMapSrcFile, db_destDir)){
          stop("cannot copy file '", chipMapSrcFile, "' to '", db_destDir, "'")
      }
    }
  }
  else{
      stop("Cannot find the chipSrcFile to make the package.")
  }
    
}


