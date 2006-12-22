###
### The makeDataPackage method for SQLiteAnnDataPkgSeed objects
###
setMethod(Biobase::makeDataPackage,
    signature(object="SQLiteAnnDataPkgSeed"),
    function(object, author, email, packageName, packageVersion,
             license, biocViews, filePath,
             sqliteFilePath, RSQLiteVersion, unlink=FALSE, quiet=FALSE)
    {
        if (missing(email) || !is.character(email)
         || length(email) != 1 || grep("@", email) != 1)
            stop("invalid email address")
        extdataDir <- file.path(filePath, packageName, "inst", "extdata")
        dbFileName <- paste(object@chipShortName, "sqlite", sep=".")
        if (missing(RSQLiteVersion)) {
            cran.pkgs <- available.packages(contrib.url("http://cran.fhcrc.org"))
            if (nrow(cran.pkgs) == 0)
                stop("unable to retrieve version of last available RSQLite package from CRAN, please provide a 'RSQLiteVersion' arg")
            RSQLiteVersion <- cran.pkgs['RSQLite', 'Version']
        }
        syms <- list(CHIPSHORTNAME=object@chipShortName,
                     ORGANISM=object@organism,
                     SPECIES=object@species,
                     MANUF=object@manufacturer,
                     CHIPNAME=object@chipName,
                     MANUFURL=object@manufacturerUrl,
                     AUTHOR=author,
                     AUTHOREMAIL=email,
                     VERSION=packageVersion,
                     LIC=license,
                     BIOCVIEWS=biocViews,
                     DBFILE=dbFileName,
                     RSQLITEVERSION=RSQLiteVersion)
        templateDir <- system.file("AnnDataPkg.templates",
                                   object@templateName,
                                   package="AnnotationDbi")
        createPackage(pkgname=packageName, destinationDir=filePath,
                      originDir=templateDir,
                      symbolValues=syms, unlink=unlink, quiet=quiet)
        dbFilePath <- file.path(extdataDir, dbFileName)
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
                   templateName="hgu95av2db",
                   chipShortName="hgu95av2",
                   organism="Homo sapiens",
                   species="Human",
                   manufacturer="Affymetrix",
                   chipName="Human Genome U95 Set",
                   manufacturerUrl="http://www.affymetrix.com/support/technical/byproduct.affx?product=hgu95")
    makeDataPackage(pkgseed, "Nianhua Li, Seth Falcon", "biocannotation@lists.fhcrc.org",
                    "hgu95av2db", "1.13.9999", "LGPL",
                    "AnnotationData, hgu95av2, AffymetrixChip, Homo_sapiens",
                    filePath, sqliteFilePath, ...)
}

