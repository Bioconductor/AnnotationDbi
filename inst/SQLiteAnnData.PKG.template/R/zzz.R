.onLoad <- function(libname, pkgname) {
    require("methods", quietly=TRUE)
    ## Establish a connection to the SQLite DB
    initDbConnection()
    ## ... and init all the maps
    maps <- allMaps(getDb())
    names(maps) <- paste("@CHIPSHORTNAME@", names(maps), sep="")
    ns <- asNamespace(pkgname)
    for (mapname in names(maps)) {
        assign(mapname, maps[[mapname]], envir=ns)
    }
    namespaceExport(ns, names(maps))
}

.onUnload <- function(libpath) {
    closeDb()
}
