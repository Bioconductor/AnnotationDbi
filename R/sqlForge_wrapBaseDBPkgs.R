wrapBaseDBPackages <- function (dbPath = "/mnt/cpb_anno/mcarlson/proj/mcarlson/sqliteGen/annosrc/db/",
                                destDir = ".",
                                version ){

  #human.db0
  vals <- list(ANNOBJPREFIX="human",
               ORGANISM="Homo sapiens",
               SPECIES="human",
               AUTHOR="Marc Carlson, Nianhua Li, Herve Pages",
               PKGVERSION=version,
               LIC="The Artistic License, Version 2.0",
               BIOCVIEWS="AnnotationData, Homo_sapiens",
               ANNDBIVERSION="1.1.15",
               MAINTAINER="Biocore Team c/o BioC user list <bioconductor@stat.math.ethz.ch>")
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
               ANNDBIVERSION="1.1.15",
               MAINTAINER="Biocore Team c/o BioC user list <bioconductor@stat.math.ethz.ch>")
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
               ANNDBIVERSION="1.1.15",
               MAINTAINER="Biocore Team c/o BioC user list <bioconductor@stat.math.ethz.ch>")
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
               ANNDBIVERSION="1.1.15",
               MAINTAINER="Biocore Team c/o BioC user list <bioconductor@stat.math.ethz.ch>")
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
               ANNDBIVERSION="1.1.15",
               MAINTAINER="Biocore Team c/o BioC user list <bioconductor@stat.math.ethz.ch>")
  pkgName= paste(vals[["ANNOBJPREFIX"]],".db0",sep="")
  template=paste(system.file(package = "AnnotationDbi"), "/AnnDbPkg-templates/BASEPKG.DB", sep="")

  chipSrcFile = paste(dbPath,"chipsrc_yeast.sqlite",sep="")
#  chipMapSrcFile = paste(dbPath,"chipmapsrc_yeast.sqlite",sep="")
  chipMapSrcFile = NULL
  
  makeBasePackage(chipSrcFile, pkgName=pkgName, destDir=destDir, template=template, vals=vals)
  

  #arabidopsis.db0 
  vals <- list(ANNOBJPREFIX="arabidopsis",
               ORGANISM="Arabidopsis thaliana",
               SPECIES="arabidopsis",
               AUTHOR="Marc Carlson, Nianhua Li, Herve Pages",
               PKGVERSION=version,
               LIC="The Artistic License, Version 2.0",
               BIOCVIEWS="AnnotationData, Arabidopsis_thaliana",
               ANNDBIVERSION="1.1.15",
               MAINTAINER="Biocore Team c/o BioC user list <bioconductor@stat.math.ethz.ch>")
  pkgName= paste(vals[["ANNOBJPREFIX"]],".db0",sep="")
  template=paste(system.file(package = "AnnotationDbi"), "/AnnDbPkg-templates/BASEPKG.DB", sep="")

  chipSrcFile = paste(dbPath,"chipsrc_arabidopsis.sqlite",sep="")
  chipMapSrcFile = paste(dbPath,"chipmapsrc_arabidopsis.sqlite",sep="")

  makeBasePackage(chipSrcFile, chipMapSrcFile, pkgName, destDir, template, vals)

  
  #malaria.db0 
  vals <- list(ANNOBJPREFIX="malaria",
               ORGANISM="Plasmodium falciparum",
               SPECIES="malaria",
               AUTHOR="Marc Carlson, Herve Pages",
               PKGVERSION=version,
               LIC="The Artistic License, Version 2.0",
               BIOCVIEWS="AnnotationData, Plasmodium_falciparum",
               ANNDBIVERSION="1.1.28",
               MAINTAINER="Biocore Team c/o BioC user list <bioconductor@stat.math.ethz.ch>")
  pkgName= paste(vals[["ANNOBJPREFIX"]],".db0",sep="")
  template=paste(system.file(package = "AnnotationDbi"), "/AnnDbPkg-templates/BASEPKG.DB", sep="")

  chipSrcFile = paste(dbPath,"chipsrc_malaria.sqlite",sep="")
  chipMapSrcFile = NULL

  makeBasePackage(chipSrcFile, chipMapSrcFile, pkgName, destDir, template, vals)  

 
  #zebrafish.db0 
  vals <- list(ANNOBJPREFIX="zebrafish",
               ORGANISM="Danio rerio",
               SPECIES="zebrafish",
               AUTHOR="Marc Carlson, Herve Pages",
               PKGVERSION=version,
               LIC="The Artistic License, Version 2.0",
               BIOCVIEWS="AnnotationData, Danio_rerio",
               ANNDBIVERSION="1.1.28",
               MAINTAINER="Biocore Team c/o BioC user list <bioconductor@stat.math.ethz.ch>")
  pkgName= paste(vals[["ANNOBJPREFIX"]],".db0",sep="")
  template=paste(system.file(package = "AnnotationDbi"), "/AnnDbPkg-templates/BASEPKG.DB", sep="")

  chipSrcFile = paste(dbPath,"chipsrc_zebrafish.sqlite",sep="")
  chipMapSrcFile = paste(dbPath,"chipmapsrc_zebrafish.sqlite",sep="")

  makeBasePackage(chipSrcFile, chipMapSrcFile, pkgName, destDir, template, vals)  

  
  #canine.db0 
  vals <- list(ANNOBJPREFIX="canine",
               ORGANISM="Canis familiaris",
               SPECIES="canine",
               AUTHOR="Marc Carlson, Herve Pages",
               PKGVERSION=version,
               LIC="The Artistic License, Version 2.0",
               BIOCVIEWS="AnnotationData, Canis_familiaris",
               ANNDBIVERSION="1.1.28",
               MAINTAINER="Biocore Team c/o BioC user list <bioconductor@stat.math.ethz.ch>")
  pkgName= paste(vals[["ANNOBJPREFIX"]],".db0",sep="")
  template=paste(system.file(package = "AnnotationDbi"), "/AnnDbPkg-templates/BASEPKG.DB", sep="")

  chipSrcFile = paste(dbPath,"chipsrc_canine.sqlite",sep="")
  chipMapSrcFile = paste(dbPath,"chipmapsrc_canine.sqlite",sep="")

  makeBasePackage(chipSrcFile, chipMapSrcFile, pkgName, destDir, template, vals)  


  #bovine.db0 
  vals <- list(ANNOBJPREFIX="bovine",
               ORGANISM="Bos taurus",
               SPECIES="bovine",
               AUTHOR="Marc Carlson, Herve Pages",
               PKGVERSION=version,
               LIC="The Artistic License, Version 2.0",
               BIOCVIEWS="AnnotationData, Bos_taurus",
               ANNDBIVERSION="1.1.28",
               MAINTAINER="Biocore Team c/o BioC user list <bioconductor@stat.math.ethz.ch>")
  pkgName= paste(vals[["ANNOBJPREFIX"]],".db0",sep="")
  template=paste(system.file(package = "AnnotationDbi"), "/AnnDbPkg-templates/BASEPKG.DB", sep="")

  chipSrcFile = paste(dbPath,"chipsrc_bovine.sqlite",sep="")
  chipMapSrcFile = paste(dbPath,"chipmapsrc_bovine.sqlite",sep="")

  makeBasePackage(chipSrcFile, chipMapSrcFile, pkgName, destDir, template, vals)  

  
  
  #ecoliK12.db0 
  vals <- list(ANNOBJPREFIX="ecoliK12",
               ORGANISM="Escherichia coli",
               SPECIES="E coli K12 Strain",
               AUTHOR="Marc Carlson, Herve Pages",
               PKGVERSION=version,
               LIC="The Artistic License, Version 2.0",
               BIOCVIEWS="AnnotationData, Escherichia_coli",
               ANNDBIVERSION="1.1.28",
               MAINTAINER="Biocore Team c/o BioC user list <bioconductor@stat.math.ethz.ch>")
  pkgName= paste(vals[["ANNOBJPREFIX"]],".db0",sep="")
  template=paste(system.file(package = "AnnotationDbi"), "/AnnDbPkg-templates/BASEPKG.DB", sep="")

  chipSrcFile = paste(dbPath,"chipsrc_ecoliK12.sqlite",sep="")
  chipMapSrcFile = paste(dbPath,"chipmapsrc_ecoliK12.sqlite",sep="")
  
  makeBasePackage(chipSrcFile, chipMapSrcFile, pkgName, destDir, template, vals)  


  #ecoliSakai.db0 
  vals <- list(ANNOBJPREFIX="ecoliSakai",
               ORGANISM="Escherichia coli",
               SPECIES="E coli Sakai Strain",
               AUTHOR="Marc Carlson, Herve Pages",
               PKGVERSION=version,
               LIC="The Artistic License, Version 2.0",
               BIOCVIEWS="AnnotationData, Escherichia_coli",
               ANNDBIVERSION="1.1.28",
               MAINTAINER="Biocore Team c/o BioC user list <bioconductor@stat.math.ethz.ch>")
  pkgName= paste(vals[["ANNOBJPREFIX"]],".db0",sep="")
  template=paste(system.file(package = "AnnotationDbi"), "/AnnDbPkg-templates/BASEPKG.DB", sep="")

  chipSrcFile = paste(dbPath,"chipsrc_ecoliSakai.sqlite",sep="")
  chipMapSrcFile = paste(dbPath,"chipmapsrc_ecoliSakai.sqlite",sep="")

  makeBasePackage(chipSrcFile, chipMapSrcFile, pkgName, destDir, template, vals)  

  
  #worm.db0 
  vals <- list(ANNOBJPREFIX="worm",
               ORGANISM="Caenorhabditis elegans",
               SPECIES="worm",
               AUTHOR="Marc Carlson, Herve Pages",
               PKGVERSION=version,
               LIC="The Artistic License, Version 2.0",
               BIOCVIEWS="AnnotationData, Caenorhabditis_elegans",
               ANNDBIVERSION="1.1.28",
               MAINTAINER="Biocore Team c/o BioC user list <bioconductor@stat.math.ethz.ch>")
  pkgName= paste(vals[["ANNOBJPREFIX"]],".db0",sep="")
  template=paste(system.file(package = "AnnotationDbi"), "/AnnDbPkg-templates/BASEPKG.DB", sep="")

  chipSrcFile = paste(dbPath,"chipsrc_worm.sqlite",sep="")
  chipMapSrcFile = paste(dbPath,"chipmapsrc_worm.sqlite",sep="")

  makeBasePackage(chipSrcFile, chipMapSrcFile, pkgName, destDir, template, vals)
  
  #pig.db0 
  vals <- list(ANNOBJPREFIX="pig",
               ORGANISM="Sus scrofa",
               SPECIES="pig",
               AUTHOR="Marc Carlson, Herve Pages",
               PKGVERSION=version,
               LIC="The Artistic License, Version 2.0",
               BIOCVIEWS="AnnotationData, Sus_scrofa",
               ANNDBIVERSION="1.1.28",
               MAINTAINER="Biocore Team c/o BioC user list <bioconductor@stat.math.ethz.ch>")
  pkgName= paste(vals[["ANNOBJPREFIX"]],".db0",sep="")
  template=paste(system.file(package = "AnnotationDbi"), "/AnnDbPkg-templates/BASEPKG.DB", sep="")

  chipSrcFile = paste(dbPath,"chipsrc_pig.sqlite",sep="")
  chipMapSrcFile = paste(dbPath,"chipmapsrc_pig.sqlite",sep="")

  makeBasePackage(chipSrcFile, chipMapSrcFile, pkgName, destDir, template, vals)  

  #chicken.db0 
  vals <- list(ANNOBJPREFIX="chicken",
               ORGANISM="Gallus gallus",
               SPECIES="chicken",
               AUTHOR="Marc Carlson, Herve Pages",
               PKGVERSION=version,
               LIC="The Artistic License, Version 2.0",
               BIOCVIEWS="AnnotationData, Gallus_gallus",
               ANNDBIVERSION="1.1.28",
               MAINTAINER="Biocore Team c/o BioC user list <bioconductor@stat.math.ethz.ch>")
  pkgName= paste(vals[["ANNOBJPREFIX"]],".db0",sep="")
  template=paste(system.file(package = "AnnotationDbi"), "/AnnDbPkg-templates/BASEPKG.DB", sep="")

  chipSrcFile = paste(dbPath,"chipsrc_chicken.sqlite",sep="")
  chipMapSrcFile = paste(dbPath,"chipmapsrc_chicken.sqlite",sep="")

  makeBasePackage(chipSrcFile, chipMapSrcFile, pkgName, destDir, template, vals)  

  
##   #yeastNCBI.db0 
##   vals <- list(ANNOBJPREFIX="yeastNCBI",
##                ORGANISM="Saccharomyces cerevisiae",
##                SPECIES="yeast",
##                AUTHOR="Marc Carlson, Nianhua Li, Herve Pages",
##                PKGVERSION=version,
##                LIC="The Artistic License, Version 2.0",
##                BIOCVIEWS="AnnotationData, Saccharomyces_cerevisiae",
##                ANNDBIVERSION="1.1.28",
##                MAINTAINER="Biocore Team c/o BioC user list <bioconductor@stat.math.ethz.ch>")
##   pkgName= paste(vals[["ANNOBJPREFIX"]],".db0",sep="")
##   template=paste(system.file(package = "AnnotationDbi"), "/AnnDbPkg-templates/BASEPKG.DB", sep="")

##   chipSrcFile = paste(dbPath,"chipsrc_yeastNCBI.sqlite",sep="")
##   chipMapSrcFile = paste(dbPath,"chipmapsrc_yeastNCBI.sqlite",sep="")
  
##   makeBasePackage(chipSrcFile, pkgName=pkgName, destDir=destDir, template=template, vals=vals)


  #chimp.db0 
  vals <- list(ANNOBJPREFIX="chimp",
               ORGANISM="Pan troglodytes",
               SPECIES="chimp",
               AUTHOR="Marc Carlson, Herve Pages",
               PKGVERSION=version,
               LIC="The Artistic License, Version 2.0",
               BIOCVIEWS="AnnotationData, Pan_troglodytes",
               ANNDBIVERSION="1.7.0",
               MAINTAINER="Biocore Team c/o BioC user list <bioconductor@stat.math.ethz.ch>")
  pkgName= paste(vals[["ANNOBJPREFIX"]],".db0",sep="")
  template=paste(system.file(package = "AnnotationDbi"), "/AnnDbPkg-templates/BASEPKG.DB", sep="")

  chipSrcFile = paste(dbPath,"chipsrc_chimp.sqlite",sep="")
  chipMapSrcFile = paste(dbPath,"chipmapsrc_chimp.sqlite",sep="")

  makeBasePackage(chipSrcFile, chipMapSrcFile, pkgName, destDir, template, vals)  

  
  #rhesus.db0 
  vals <- list(ANNOBJPREFIX="rhesus",
               ORGANISM="Macaca Mulatta",
               SPECIES="rhesus",
               AUTHOR="Marc Carlson, Herve Pages",
               PKGVERSION=version,
               LIC="The Artistic License, Version 2.0",
               BIOCVIEWS="AnnotationData, Macaca_Mulatta",
               ANNDBIVERSION="1.7.0",
               MAINTAINER="Biocore Team c/o BioC user list <bioconductor@stat.math.ethz.ch>")
  pkgName= paste(vals[["ANNOBJPREFIX"]],".db0",sep="")
  template=paste(system.file(package = "AnnotationDbi"), "/AnnDbPkg-templates/BASEPKG.DB", sep="")

  chipSrcFile = paste(dbPath,"chipsrc_rhesus.sqlite",sep="")
  chipMapSrcFile = paste(dbPath,"chipmapsrc_rhesus.sqlite",sep="")

  makeBasePackage(chipSrcFile, chipMapSrcFile, pkgName, destDir, template, vals)  


  
  #anopheles.db0 
  vals <- list(ANNOBJPREFIX="anopheles",
               ORGANISM="Anopheles gambiae",
               SPECIES="anopheles",
               AUTHOR="Marc Carlson, Herve Pages",
               PKGVERSION=version,
               LIC="The Artistic License, Version 2.0",
               BIOCVIEWS="AnnotationData, Anopheles_gambiae",
               ANNDBIVERSION="1.7.0",
               MAINTAINER="Biocore Team c/o BioC user list <bioconductor@stat.math.ethz.ch>")
  pkgName= paste(vals[["ANNOBJPREFIX"]],".db0",sep="")
  template=paste(system.file(package = "AnnotationDbi"), "/AnnDbPkg-templates/BASEPKG.DB", sep="")

  chipSrcFile = paste(dbPath,"chipsrc_anopheles.sqlite",sep="")
  chipMapSrcFile = paste(dbPath,"chipmapsrc_anopheles.sqlite",sep="")

  makeBasePackage(chipSrcFile, chipMapSrcFile, pkgName, destDir, template, vals)  

  #xenopus.db0 
  vals <- list(ANNOBJPREFIX="xenopus",
               ORGANISM="Xenopus laevis",
               SPECIES="xenopus",
               AUTHOR="Marc Carlson, Herve Pages",
               PKGVERSION=version,
               LIC="The Artistic License, Version 2.0",
               BIOCVIEWS="AnnotationData, Xenopus_laevis",
               ANNDBIVERSION="1.7.0",
               MAINTAINER="Biocore Team c/o BioC user list <bioconductor@stat.math.ethz.ch>")
  pkgName= paste(vals[["ANNOBJPREFIX"]],".db0",sep="")
  template=paste(system.file(package = "AnnotationDbi"), "/AnnDbPkg-templates/BASEPKG.DB", sep="")

  chipSrcFile = paste(dbPath,"chipsrc_xenopus.sqlite",sep="")
  chipMapSrcFile = paste(dbPath,"chipmapsrc_xenopus.sqlite",sep="")

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


