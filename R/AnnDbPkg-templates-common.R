### The functions below are shared across all SQLite-based ann data packages.
### They're centralized here and used in the templates of the ann data
### packages instead of being redefined over and over in every templates.

### Used at load time (in .onLoad).
dbFileConnect <- function(dbfile)
{
    ## This is a protection against dbConnect() working even with non-existing
    ## files (for our use case, the .sqlite file _must_ exist):
    if (!file.exists(dbfile))
        stop("DB file '", dbfile, "' not found")
    ## We should not need to explicitly library(RSQLite) because it's in
    ## Depends and Imports but this seems to make 'R CMD check hgu95av2.db' happier.
    library(RSQLite)
    dbConnect(SQLite(), dbname=dbfile, cache_size=64000, synchronous=0,
              flags=SQLITE_RO)
}

### Used at unload time (in .onUnload).
dbFileDisconnect <- function(dbconn)
{
    dbDisconnect(dbconn)
}

### Used at load time (in .onLoad) by the SQLite-based ann data package to
### dynamically add exported symbols to its namespace environment.

addToNamespaceAndExport <- function(x, value, pkgname)
{
    ns <- asNamespace(pkgname)
    assign(x, value, envir=ns)
    namespaceExport(ns, x)
}

mergeToNamespaceAndExport <- function(envir, pkgname)
{
    keys <- ls(envir, all.names=TRUE)
    for (key in keys)
        addToNamespaceAndExport(key, envir[[key]], pkgname)
}

