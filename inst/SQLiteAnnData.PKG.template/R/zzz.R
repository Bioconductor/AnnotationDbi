.onLoad <- function(libname, pkgname) {
    require("methods", quietly=TRUE)
    ## Establish a connection to the SQLite DB
    initDbConnection()
}

.onUnload <- function(libpath) {
    closeDb()
}
