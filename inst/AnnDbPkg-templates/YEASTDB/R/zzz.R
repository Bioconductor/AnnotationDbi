datacache <- new.env(hash=TRUE, parent=emptyenv())

@OBJPREFIX@ORGANISM <- "@ORGANISM@"

.onLoad <- function(libname, pkgname)
{
    require("methods", quietly=TRUE)
    ## Connect to the SQLite DB
    db_file <- system.file("extdata", "@DBFILE@", package=pkgname)
    addToNamespaceAndExport("db_file", db_file, pkgname)
    db_conn <- dbFileConnect(db_file)
    addToNamespaceAndExport("db_conn", db_conn, pkgname)
    ## Create the AnnObj instances
    annobjs <- createAnnObjs.@DBSCHEMA@("@OBJPREFIX@", "@OBJTARGET@", db_conn, datacache)
    for (objname in names(annobjs))
        addToNamespaceAndExport(objname, annobjs[[objname]], pkgname=pkgname)
}

.onUnload <- function(libpath)
{
    dbFileDisconnect(db_conn)
}

