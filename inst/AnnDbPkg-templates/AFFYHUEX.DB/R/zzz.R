datacache <- new.env(hash=TRUE, parent=emptyenv())

@ANNOBJPREFIX@ORGANISM <- "@ORGANISM@"

.onLoad <- function(libname, pkgname)
{
    require("methods", quietly=TRUE)
    ## Connect to the SQLite DB
    db_file <- system.file("extdata", "@DBFILE@", package=pkgname, lib.loc=libname)
    addToNamespaceAndExport("@ANNOBJPREFIX@_dbfile", db_file, pkgname)
    db_conn <- dbFileConnect(db_file)
    addToNamespaceAndExport("db_conn", db_conn, pkgname)
    ## Create the AnnObj instances
    ann_objs <- createAnnObjs.SchemaChoice("@DBSCHEMA@", "@ANNOBJPREFIX@", "@ANNOBJTARGET@", dbconn, datacache)
    mergeToNamespaceAndExport(ann_objs, pkgname)
    packageStartupMessage(AnnotationDbi:::annoStartupMessages("@ANNOBJPREFIX@.db"))
}

.onUnload <- function(libpath)
{
    dbFileDisconnect(db_conn)
}

