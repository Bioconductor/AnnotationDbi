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

sqlitefiles <- list.files(srcdir, pattern="^org")
sqlitefiles <- sqlitefiles[!(sqlitefiles %in% exclude)]
pkgs <- paste(substr(sqlitefiles, 1, nchar(sqlitefiles)-7), ".db", sep="")

library(AnnotationDbi)
makeAnnDbPkg(pkgs)

