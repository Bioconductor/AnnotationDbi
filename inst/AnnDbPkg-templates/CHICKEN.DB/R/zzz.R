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

    ## Create the OrgDb object
    sPkgname <- sub(".db$","",pkgname)
    txdb <- loadDb(system.file("extdata", paste(sPkgname,
      ".sqlite",sep=""), package=pkgname, lib.loc=libname),
                   packageName=pkgname)    
    dbNewname <- AnnotationDbi:::dbObjectName(pkgname,"OrgDb")
    ns <- asNamespace(pkgname)
    assign(dbNewname, txdb, envir=ns)
    namespaceExport(ns, dbNewname)
        
    ## Create the AnnObj instances
    ann_objs <- createAnnObjs.SchemaChoice("@DBSCHEMA@", "@ANNOBJPREFIX@", "@ANNOBJTARGET@", dbconn, datacache)
    mergeToNamespaceAndExport(ann_objs, pkgname)
    packageStartupMessage(AnnotationDbi:::annoStartupMessages("@ANNOBJPREFIX@"))
}

.onUnload <- function(libpath)
{
    dbFileDisconnect(@ANNOBJPREFIX@_dbconn())
}

