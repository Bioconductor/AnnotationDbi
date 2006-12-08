###
### The makeDataPackage method for SQLiteAnnDataPkgSeed objects
###
setMethod(Biobase::makeDataPackage,
    signature(object="SQLiteAnnDataPkgSeed"),
    function(object, author, email, packageName, packageVersion,
             license, biocViews, filePath, sqliteFilePath, quiet=FALSE,
             unlink=FALSE)
    {
        if (missing(email) || !is.character(email)
         || length(email) != 1 || grep("@", email) != 1)
            stop("invalid email address")
        extdataDir <- file.path(filePath, packageName, "inst", "extdata")
        dbFileName <- paste(object@chipShortName, "sqlite", sep=".")
        syms <- list(CHIPSHORTNAME=object@chipShortName,
                     ORGANISM=object@organism,
                     SPECIES=object@species,
                     MANUF=object@manufacturer,
                     CHIPNAME=object@chipName,
                     MANUFURL=object@manufacturerUrl,
                     AUTHOR=author,
                     AUTHOREMAIL=email,
                     PKGNAME=packageName,
                     VERSION=packageVersion,
                     LIC=license,
                     BIOCVIEWS=biocViews,
                     DBFILE=dbFileName)
        templateDir <- system.file("SQLiteAnnData.PKG.template",
                                   package="AnnotationDbi")
        createPackage(pkgname=packageName, destinationDir=filePath,
                      originDir=templateDir,
                      symbolValues=syms, quiet=quiet, unlink=unlink)
        dbFilePath <- file.path(extdataDir, dbFileName)
        if (!dir.create(dbFilePath, showWarnings=FALSE, recursive=TRUE))
          stop("unable to create directory: ", dbFilePath)
        if (!file.copy(sqliteFilePath, dbFilePath))
          stop("unable to copy SQLite DB to destination")
        return(invisible(TRUE))
    }
)

### Just for testing (not exported). Call with
###   AnnotationDbi:::make_hgu95av2db("/tmp", "path/to/hgu95av2.sqlite")
make_hgu95av2db <- function(filePath, sqliteFilePath, ...)
{
    pkgseed <- new("SQLiteAnnDataPkgSeed",
                   chipShortName="hgu95av2",
                   organism="Homo sapiens",
                   species="Human",
                   manufacturer="Affymetrix",
                   chipName="Human Genome U95 Set",
                   manufacturerUrl="http://www.affymetrix.com/support/technical/byproduct.affx?product=hgu95")
    makeDataPackage(pkgseed, "Nianhua Li, Seth Falcon", "biocannotation@lists.fhcrc.org",
                    "hgu95av2db", "1.13.999", "LGPL",
                    "AnnotationData, hgu95av2, AffymetrixChip, Homo_sapiens",
                    filePath, sqliteFilePath, ...)
}

