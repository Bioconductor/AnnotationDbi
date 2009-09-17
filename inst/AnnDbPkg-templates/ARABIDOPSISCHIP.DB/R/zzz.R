datacache <- new.env(hash=TRUE, parent=emptyenv())

@ANNOBJPREFIX@ <- function() showQCData("@ANNOBJPREFIX@", datacache)
@ANNOBJPREFIX@_dbconn <- function() dbconn(datacache)
@ANNOBJPREFIX@_dbfile <- function() dbfile(datacache)
@ANNOBJPREFIX@_dbschema <- function(file="", show.indices=FALSE) dbschema(datacache, file=file, show.indices=show.indices)
@ANNOBJPREFIX@_dbInfo <- function() dbInfo(datacache)

@ANNOBJPREFIX@ORGANISM <- "@ORGANISM@"

.onLoad <- function(libname, pkgname)
{
    require("methods", quietly=TRUE)
    ## Connect to the SQLite DB
    dbfile <- system.file("extdata", "@DBFILE@", package=pkgname, lib.loc=libname)
    assign("dbfile", dbfile, envir=datacache)
    dbconn <- dbFileConnect(dbfile)
    assign("dbconn", dbconn, envir=datacache)
    ## Create the AnnObj instances
    ann_objs <- createAnnObjs.SchemaChoice("@DBSCHEMA@", "@ANNOBJPREFIX@", "@ANNOBJTARGET@", dbconn, datacache)
    mergeToNamespaceAndExport(ann_objs, pkgname)

    msg <- paste("\n Warning: All Arabidopsis packages now require you",
                 "to use the toggleProbes() method in order to expose",
                 "multiple mappings.")
    msg <- paste("\n",paste(strwrap(msg, exdent=2),collapse="\n"),
                 "\n",sep="")
    packageStartupMessage(msg)
    
}

.onUnload <- function(libpath)
{
    dbFileDisconnect(@ANNOBJPREFIX@_dbconn())
}

