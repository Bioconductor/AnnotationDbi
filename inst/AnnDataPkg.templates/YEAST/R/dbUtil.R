## Most of this file belongs in an AnnotationDBI or similarly named package.
## However, the first bits that reference the 'globals' env probably need
## to be generated and part of the actual data package.  The exception is
## globals$DEBUG which really belongs to the interface layer.

## We use a package-global environment to store table names, DB path,
## and the connection to the DB itself.  Using an env is key because it
## allows us to establish the DB con inside .onLoad via the
## initDbConnection() function.
globals <- new.env(parent=emptyenv())
globals$DEBUG <- TRUE
globals$DATA_DB_PATH <- system.file("extdata", "@DBFILE@",
                                        package="@PKGNAME@")

initDbConnection <- function() {
    globals$dbCon <- dbConnect(dbDriver("SQLite"),
                               dbname=globals$DATA_DB_PATH,
				cache_size=64000, synchronous=0)
    globals$dbCon
}
    
getDb  <- function() {
    if (!is.null(globals$dbCon))
      return(globals$dbCon)
    initDbConnection()
}

closeDb <- function() {
    ## FIXME: check for valid connection?
    dbDisconnect(globals$dbCon)
    remove(dbCon, envir=globals)
}
