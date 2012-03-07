### Wrap all sqlite files found under /mnt/cpb_anno/mcarlson/sanctionedSqlite
### into a .db package.

srcdir <- "/mnt/cpb_anno/mcarlson/sanctionedSqlite"
exclude <- c(
    "YEAST.sqlite",
    "metadatasrc.sqlite",
    "megaGO.sqlite",
    "chipsrc_arabidopsis.sqlite",
    "chipsrc_mouse.sqlite",
    "chipsrc_human.sqlite",
    "chipsrc_fly.sqlite",
    "chipsrc_yeast.sqlite",
    "chipsrc_rat.sqlite",
    "chipmapsrc_arabidopsis.sqlite",
    "chipmapsrc_all.sqlite",
    "chipmapsrc_fly.sqlite",
    "chipmapsrc_human.sqlite",
    "chipmapsrc_mouse.sqlite",
    "chipmapsrc_rat.sqlite",
    "humanCHRLOC.sqlite",
    "mouseCHRLOC.sqlite",
    "ratCHRLOC.sqlite",
    "flyCHRLOC.sqlite"
)
orgs = list.files(srcdir, pattern="^org")
exclude = c(exclude, orgs)
sqlitefiles <- list.files(srcdir, pattern="\\.sqlite$")
sqlitefiles <- sqlitefiles[!(sqlitefiles %in% exclude)]
pkgs <- paste(substr(sqlitefiles, 1, nchar(sqlitefiles)-7), ".db", sep="")

## ## used for testing only
## pkgs <- c("hgu95av2.db","ecoli2.db")

library(AnnotationDbi)
makeAnnDbPkg(pkgs)



## example for testing things:
## With a "AnnDbPkgSeed" object:
## x <- new("AnnDbPkgSeed",
##          Package="KEGG.db",
##          Version="0.0.99",
##          PkgTemplate="KEGG.DB",
##          AnnObjPrefix="KEGG"
##          )
## dbfile <- "/mnt/cpb_anno/mcarlson/sanctionedSqlite/KEGG.sqlite"
## dest_dir="."
## no.man=FALSE
## x <- AnnotationDbi:::initWithDbMetada(x, dbfile)
## x <- AnnotationDbi:::initComputedSlots(x)
## dbfile_basename <- basename(dbfile)
## etc. inside of: setMethod("makeAnnDbPkg", "AnnDbPkgSeed", 
