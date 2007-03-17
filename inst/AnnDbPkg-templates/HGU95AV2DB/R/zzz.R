datacache <- new.env(hash=TRUE, parent=emptyenv())

@ANNOBJPREFIX@ORGANISM <- "@ORGANISM@"

.onLoad <- function(libname, pkgname)
{
    require("methods", quietly=TRUE)
    ## Connect to the SQLite DB
    db_file <- system.file("extdata", "@DBFILE@", package=pkgname)
    addToNamespaceAndExport("db_file", db_file, pkgname)
    db_conn <- dbFileConnect(db_file)
    addToNamespaceAndExport("db_conn", db_conn, pkgname)
    ## Create the AnnObj instances
    annobjs <- createAnnObjs.@DBSCHEMA@("@ANNOBJPREFIX@", "@ANNOBJTARGET@", db_conn, datacache)
    mergeToNamespaceAndExport(annobjs, pkgname)
}

.onUnload <- function(libpath)
{
    dbFileDisconnect(db_conn)
}

