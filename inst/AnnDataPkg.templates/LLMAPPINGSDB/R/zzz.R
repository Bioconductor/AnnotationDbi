datacache <- new.env(hash=TRUE)

.onLoad <- function(libname, pkgname) {
    require("methods", quietly=TRUE)
    ## Establish a connection to the SQLite DB
    initDbConnection()
    ## ... and init the data
    annobjs <- createAnnObjects.@DBSCHEMA@("@OBJPREFIX@", "@OBJTARGET@", getDb(), datacache)
    ns <- asNamespace(pkgname)
    for (objname in names(annobjs)) {
        assign(objname, annobjs[[objname]], envir=ns)
    }
    namespaceExport(ns, names(annobjs))
}

.onUnload <- function(libpath) {
    closeDb()
}
