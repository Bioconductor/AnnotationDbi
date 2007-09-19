datacache <- new.env(hash=TRUE, parent=emptyenv())

@ANNOBJPREFIX@_dbconn <- function() get("dbconn", envir=datacache)

@ANNOBJPREFIX@ORGANISM <- "@ORGANISM@"

.onLoad <- function(libname, pkgname)
{
    require("methods", quietly=TRUE)
    ## Connect to the SQLite DB
    dbfile <- system.file("extdata", "@DBFILE@", package=pkgname, lib.loc=libname)
    assign("dbfile", dbfile, envir=datacache)
    addToNamespaceAndExport("@ANNOBJPREFIX@_dbfile", dbfile, pkgname)
    dbconn <- dbFileConnect(dbfile)
    assign("dbconn", dbconn, envir=datacache)
    ## Create the AnnObj instances
    ann_objs <- createAnnObjs.@DBSCHEMA@("@ANNOBJPREFIX@", "@ANNOBJTARGET@", dbconn, datacache)
    mergeToNamespaceAndExport(ann_objs, pkgname)
}

.onUnload <- function(libpath)
{
    dbFileDisconnect(@ANNOBJPREFIX@_dbconn())
}

