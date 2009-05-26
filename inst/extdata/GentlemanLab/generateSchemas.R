

## ##I want one example from each kind of schema
## pkgs = c("ag.db" = "ARABIDOPSISCHIP_DB",
##          "org.At.tair.db" = "ARABIDOPSIS_DB"
##          "hgu95av2.db"="HUMANCHIP_DB",
##          "org.Hs.eg.db" = "HUMAN_DB",
##          "moe430a.db" = "MOUSECHIP_DB",
##          "org.Mm.eg.db" = "MOUSE_DB",
##          "rae230a.db" = "RATCHIP_DB",
##          "org.Rn.eg.db" = "RAT_DB",
##          "bovine.db" = "BOVINECHIP_DB",
##          "org.Bt.eg.db" = "BOVINE_DB",
##          "canine.db" = "CANINECHIP_DB",
##          "org.Cf.eg.db" = "CANINE_DB",
##          "chicken.db" = "CHICKENCHIP_DB",
##          "org.Gg.eg.db" = "CHICKEN_DB",
## ##         "" = "ECOLICHIP_DB",  ##No example yet - make by hand :(
##          "org.Ec.eg.db" = "ECOLI_DB",
##          "drosgenome1.db" = "FLYCHIP_DB",
##          "org.Dm.eg.db" = "FLY_DB",
##          "org.Pf.plasmo.db" = "MALARIA_DB",
## ##         "" = "PIGCHIP_DB",  ##No example yet - make by hand :(
##          "org.Ss.eg.db" = "PIG_DB",
##          "celegans.db" = "WORMCHIP_DB",
##          "org.Ce.eg.db" = "WORM_DB",
##          "yeast2.db" = "YEASTCHIP_DB",
##          "org.Sc.sgd.db" = "YEAST_DB",
##          "zebrafish.db" = "ZEBRAFISHCHIP_DB",
##          "org.Dr.eg.db" = "ZEBRAFISH_DB",  
##          "GO.db" = "GO_DB",
##          "KEGG.db" = "KEGG_DB",
##          "PFAM.db" = "PFAM_DB",
##          "hom.Hs.inp.db" = "INPARANOIDHOMSA_DB",
##          "hom.Mm.inp.db" = "INPARANOIDMUSMU_DB",
##          "hom.Rn.inp.db" = "INPARANOIDRATNO_DB",
##          "hom.Dm.inp.db" = "INPARANOIDDROME_DB",
##          "hom.Sc.inp.db" = "INPARANOIDSACCE_DB"
##          )









##Optional- depends on circumstances
##Get the latest (this step is slow)
##1st make sure that we have the latest packages installed
##(after they have been pushed)
## source("http://bioconductor.org/biocLite.R")
##biocLite(pkgs)




##Need to put the packages in the latest AnnotationDbi schemas directory
##(source)
schemaDir = "/mnt/cpb_anno/mcarlson/proj/mcarlson/AnnMod/AnnotationDbi/inst/DBschemas/schemas_2.0/"

##Now generate the schemas
library(AnnotationDbi)
library(DBI)
library(RSQLite)



inpPkgs = c(
  "INPARANOIDDROME_DB" = "hom.Dm.inp.db", 
  "INPARANOIDHOMSA_DB" = "hom.Hs.inp.db", 
  "INPARANOIDMUSMU_DB" = "hom.Mm.inp.db", 
  "INPARANOIDRATNO_DB" = "hom.Rn.inp.db", 
  "INPARANOIDSACCE_DB" = "hom.Sc.inp.db")

for(i in seq_len(length(inpPkgs))){
    AnnotationDbi:::generate.schema(names(inpPkgs[i]),inpPkgs[i], path = schemaDir)
}



sbPkgs = c(
  "GO_DB" = "GO.db", 
  "KEGG_DB" = "KEGG.db", 
  "PFAM_DB" = "PFAM.db")

for(i in seq_len(length(sbPkgs))){
    AnnotationDbi:::generate.schema(names(sbPkgs[i]),sbPkgs[i], path = schemaDir)
}



##Cannot do more than 16 at once so split up orgs (for now)
orgPkgs = c(
  "ANOPHELES_DB" = "org.Ag.eg.db", 
  "ARABIDOPSIS_DB" = "org.At.tair.db", 
  "BOVINE_DB" = "org.Bt.eg.db", 
  "CANINE_DB" = "org.Cf.eg.db", 
  "CHICKEN_DB" = "org.Gg.eg.db", 
  "CHIMP_DB" = "org.Pt.eg.db", 
  "ECOLI_DB" = "org.EcK12.eg.db", 
  "FLY_DB" = "org.Dm.eg.db", 
  "HUMAN_DB" = "org.Hs.eg.db")

for(i in seq_len(length(orgPkgs))){
    AnnotationDbi:::generate.schema(names(orgPkgs[i]),orgPkgs[i], path = schemaDir)
}



orgPkgs2 = c(
  "MALARIA_DB" = "org.Pf.plasmo.db", 
  "MOUSE_DB" = "org.Mm.eg.db", 
  "PIG_DB" = "org.Ss.eg.db", 
  "RAT_DB" = "org.Rn.eg.db", 
  "RHESUS_DB" = "org.Mmu.eg.db", 
  "WORM_DB" = "org.Ce.eg.db", 
  "XENOPUS_DB" = "org.Xl.eg.db", 
  "YEAST_DB" = "org.Sc.sgd.db", 
  ## "YEASTNCBI_DB" = "", 
  "ZEBRAFISH_DB" = "org.Dr.eg.db")

for(i in seq_len(length(orgPkgs2))){
    AnnotationDbi:::generate.schema(names(orgPkgs2[i]),orgPkgs2[i], path = schemaDir)
}



chipPkgs = c(
  "ARABIDOPSISCHIP_DB" = "ag.db", 
  "BOVINECHIP_DB" = "bovine.db", 
  "CANINECHIP_DB" = "canine.db", 
  "CHICKENCHIP_DB" = "chicken.db", 
  ## "ECOLICHIP_DB" = "", 
  "FLYCHIP_DB" = "drosgenome1.db", 
  "HUMANCHIP_DB" = "hgu95av2.db", 
  "MOUSECHIP_DB" = "mgu74a.db", 
  ## "PIGCHIP_DB" = "", 
  "RATCHIP_DB" = "rae230a.db", 
  "WORMCHIP_DB" = "celegans.db", 
  "XENOPUSCHIP_DB" = "xlaevis.db",  ##Have to repeat once its installed.
  "YEASTCHIP_DB" = "yeast2.db", 
  "ZEBRAFISHCHIP_DB" = "zebrafish.db")

for(i in seq_len(length(chipPkgs))){
    AnnotationDbi:::generate.schema(names(chipPkgs[i]),chipPkgs[i], path = schemaDir)
}

