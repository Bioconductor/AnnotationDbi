###
### The makeDataPackage method for AnnDataPkgSeed objects
###
setMethod(Biobase::makeDataPackage,
    signature(object="AnnDataPkgSeed"),
    function(object, author, email, packageName, packageVersion,
             license, biocViews, filePath,
             srcSQLiteFilePath, RSQLiteVersion, unlink=FALSE, quiet=FALSE)
    {
        if (!file.exists(srcSQLiteFilePath))
            stop("'", srcSQLiteFilePath, "': No such file or directory")
        if (missing(email) || !is.character(email)
         || length(email) != 1 || grep("@", email) != 1)
            stop("invalid email address")
        extdataDir <- file.path(filePath, packageName, "inst", "extdata")
        dbFileName <- paste(object@chipShortName, ".sqlite", sep="")
        if (missing(RSQLiteVersion)) {
            cran.pkgs <- available.packages(contrib.url("http://cran.fhcrc.org"))
            if (nrow(cran.pkgs) == 0)
                stop("unable to retrieve version of last available RSQLite package from CRAN, please provide a 'RSQLiteVersion' arg")
            RSQLiteVersion <- cran.pkgs['RSQLite', 'Version']
        }
        AnnDbiVersion <- installed.packages()['AnnotationDbi','Version']
        syms <- list(CHIPSHORTNAME=object@chipShortName,
                     DBSCHEMA=object@dbSchema,
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
                     RSQLITEVERSION=RSQLiteVersion,
                     ANNDBIVERSION=AnnDbiVersion)
        templateDir <- system.file("AnnDataPkg.templates",
                                   object@templateName,
                                   package="AnnotationDbi")
        createPackage(pkgname=packageName, destinationDir=filePath,
                      originDir=templateDir,
                      symbolValues=syms, unlink=unlink, quiet=quiet)
        destSQLiteFilePath <- file.path(extdataDir, dbFileName)
        if (!file.copy(srcSQLiteFilePath, destSQLiteFilePath))
            stop("cannot copy file '", srcSQLiteFilePath, "' to '", destSQLiteFilePath, "'")
        return(invisible(TRUE))
    }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Just for testing (not exported).
### Typical use:
###   library(AnnotationDbi)
###   AnnotationDbi:::make_hgu95av2db(".", "data/hgu95av2.sqlite")
###   AnnotationDbi:::make_yeast2db(".", "data/yeast2.sqlite")
###   AnnotationDbi:::make_ygs98db(".", "data/ygs98.sqlite")
###   AnnotationDbi:::make_agdb(".", "data/ag.sqlite")
###   AnnotationDbi:::make_ath1121501db(".", "data/ath1121501.sqlite")
### or to make them all:
###   AnnotationDbi:::make_all("data", "lastbuilds")

make_hgu95av2db <- function(filePath, srcSQLiteFilePath, ...)
{
    pkgseed <- new("AnnDataPkgSeed",
        chipShortName="hgu95av2",
        templateName="HGU95AV2DB",
        dbSchema="HGU95AV2DB",
        organism="Homo sapiens",
        species="Human",
        manufacturer="Affymetrix",
        chipName="Human Genome U95 Set",
        manufacturerUrl="http://www.affymetrix.com/support/technical/byproduct.affx?product=hgu95"
    )
    author <- "Nianhua Li, Seth Falcon"
    email <- "biocannotation@lists.fhcrc.org"
    packageName <- "hgu95av2db"
    packageVersion <- "1.13.900"
    license <- "LGPL"
    biocViews <- "AnnotationData, AffymetrixChip, Homo_sapiens, hgu95av2"
    makeDataPackage(pkgseed, author, email, packageName, packageVersion,
                    license, biocViews, filePath, srcSQLiteFilePath, ...)
}

test_hgu95av2db <- function(verbose=FALSE)
{
    compareAnnDataIn2Pkgs.HGU95AV2DB("hgu95av2", "hgu95av2db", "hgu95av2", verbose=verbose)
}

make_yeast2db <- function(filePath, srcSQLiteFilePath, ...)
{
    pkgseed <- new("AnnDataPkgSeed",
        chipShortName="yeast2",
        templateName="HGU95AV2DB",
        dbSchema="YEAST2DB",
        organism="Saccharomyces cerevisiae",
        species="Yeast",
        manufacturer="Affymetrix",
        chipName="Yeast Genome 2.0 Array",
        manufacturerUrl="http://www.affymetrix.com/support/technical/byproduct.affx?product=yeast-20"
    )
    author <- "Ting-Yuan Liu, ChenWei Lin, Seth Falcon, Jianhua Zhang, James W. MacDonald"
    email <- "biocannotation@lists.fhcrc.org"
    packageName <- "yeast2db"
    packageVersion <- "1.13.900"
    license <- "LGPL"
    biocViews <- "AnnotationData, AffymetrixChip, Saccharomyces_cerevisiae, yeast2"
    makeDataPackage(pkgseed, author, email, packageName, packageVersion,
                    license, biocViews, filePath, srcSQLiteFilePath, ...)
}

test_yeast2db <- function(verbose=FALSE)
{
    compareAnnDataIn2Pkgs.YEAST2DB("yeast2", "yeast2db", "yeast2", verbose=verbose)
}

make_ygs98db <- function(filePath, srcSQLiteFilePath, ...)
{
    pkgseed <- new("AnnDataPkgSeed",
        chipShortName="ygs98",
        templateName="HGU95AV2DB",
        dbSchema="YEAST2DB",
        organism="Saccharomyces cerevisiae",
        species="Yeast",
        manufacturer="Affymetrix",
        chipName="Yeast Genome S98 Array",
        manufacturerUrl="http://www.affymetrix.com/support/technical/byproduct.affx?product=yeast"
    )
    author <- "Ting-Yuan Liu, ChenWei Lin, Seth Falcon, Jianhua Zhang, James W. MacDonald"
    email <- "biocannotation@lists.fhcrc.org"
    packageName <- "ygs98db"
    packageVersion <- "1.13.900"
    license <- "LGPL"
    biocViews <- "AnnotationData, AffymetrixChip, Saccharomyces_cerevisiae, ygs98"
    makeDataPackage(pkgseed, author, email, packageName, packageVersion,
                    license, biocViews, filePath, srcSQLiteFilePath, ...)
}

test_ygs98db <- function(verbose=FALSE)
{
    compareAnnDataIn2Pkgs.YEAST2DB("ygs98", "ygs98db", "ygs98", verbose=verbose)
}

make_agdb <- function(filePath, srcSQLiteFilePath, ...)
{
    pkgseed <- new("AnnDataPkgSeed",
        chipShortName="ag",
        templateName="HGU95AV2DB",
        dbSchema="AGDB",
        organism="Arabidopsis thaliana",
        species="Arabidopsis",
        manufacturer="Affymetrix",
        chipName="Arabidopsis Genome Array",
        manufacturerUrl="http://www.affymetrix.com/support/technical/byproduct.affx?product=atgenome1"
    )
    author <- "Ting-Yuan Liu, ChenWei Lin, Seth Falcon, Jianhua Zhang, James W. MacDonald"
    email <- "biocannotation@lists.fhcrc.org"
    packageName <- "agdb"
    packageVersion <- "1.13.900"
    license <- "LGPL"
    biocViews <- "AnnotationData, AffymetrixChip, Arabidopsis_thaliana, ag"
    makeDataPackage(pkgseed, author, email, packageName, packageVersion,
                    license, biocViews, filePath, srcSQLiteFilePath, ...)
}

test_agdb <- function(verbose=FALSE)
{
    probes <- c("17096_s_at", "17097_at", "17098_s_at")
    compareAnnDataIn2Pkgs.AGDB("ag", "agdb", "ag", probes=probes, verbose=verbose)
}

make_ath1121501db <- function(filePath, srcSQLiteFilePath, ...)
{
    pkgseed <- new("AnnDataPkgSeed",
        chipShortName="ath1121501",
        templateName="HGU95AV2DB",
        dbSchema="AGDB",
        organism="Arabidopsis thaliana",
        species="Arabidopsis",
        manufacturer="Affymetrix",
        chipName="Arabidopsis ATH1 Genome Array",
        manufacturerUrl="http://www.affymetrix.com/support/technical/byproduct.affx?product=arab"
    )
    author <- "Ting-Yuan Liu, ChenWei Lin, Seth Falcon, Jianhua Zhang, James W. MacDonald"
    email <- "biocannotation@lists.fhcrc.org"
    packageName <- "ath1121501db"
    packageVersion <- "1.13.900"
    license <- "LGPL"
    biocViews <- "AnnotationData, AffymetrixChip, Arabidopsis_thaliana, ath1121501"
    makeDataPackage(pkgseed, author, email, packageName, packageVersion,
                    license, biocViews, filePath, srcSQLiteFilePath, ...)
}

test_ath1121501db <- function(verbose=FALSE)
{
    compareAnnDataIn2Pkgs.AGDB("ath1121501", "ath1121501db", "ath1121501", verbose=verbose)
}

make_all <- function(srcDir=".", destDir=".")
{
    chips <- c(
        "hgu95av2",
        "yeast2",
        "ygs98",
        "ag",
        "ath1121501"
    )
    for (chip in chips) {
        funcname <- paste("make_", chip, "db", sep="")
        srcSQLiteFilePath <- file.path(srcDir, paste(chip, ".sqlite", sep=""))
        do.call(funcname, list(destDir, srcSQLiteFilePath))
    }
}

