### The functions below are shared across all SQLite-based ann data packages.
### They're centralized here and used in the templates of the ann data
### packages instead of being redefined over and over in every templates.

### Used at load time (in .onLoad).
dbFileConnect <- function(db_file)
{
    ## We should not need to explicitly library(RSQLite) because it's in
    ## Depends and Imports but this seems to make 'R CMD hgu95av2db' happy.
    library(RSQLite)
    dbConnect(SQLite(), dbname=db_file, cache_size=64000, synchronous=0)
}

### Used at unload time (in .onUnload).
dbFileDisconnect <- function(db_conn)
{
    dbDisconnect(db_conn)
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

