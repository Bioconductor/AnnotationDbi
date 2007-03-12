datacache <- new.env(hash=TRUE)

@OBJPREFIX@ORGANISM <- "@ORGANISM@"

.onLoad <- function(libname, pkgname) {
    require("methods", quietly=TRUE)
    ## Establish a connection to the SQLite DB
    initDbConnection()
    ## ... and init the data
    maps <- createAnnObjects.@DBSCHEMA@("@OBJPREFIX@", "@OBJTARGET@", getDb(), datacache)
    ns <- asNamespace(pkgname)
    for (mapname in names(maps)) {
        assign(mapname, maps[[mapname]], envir=ns)
    }
    namespaceExport(ns, names(maps))
}

.onUnload <- function(libpath) {
    closeDb()
}
