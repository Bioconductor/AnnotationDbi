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
        dbFileName <- paste(object@mapPrefix, ".sqlite", sep="")
        if (missing(RSQLiteVersion)) {
            cran.pkgs <- available.packages(contrib.url("http://cran.fhcrc.org"))
            if (nrow(cran.pkgs) == 0)
                stop("unable to retrieve version of last available RSQLite package from CRAN, please provide a 'RSQLiteVersion' arg")
            RSQLiteVersion <- cran.pkgs['RSQLite', 'Version']
        }
        AnnDbiVersion <- installed.packages()['AnnotationDbi','Version']
        syms <- list(DBSCHEMA=object@dbSchema,
                     MAPPREFIX=object@mapPrefix,
                     MAPTARGET=object@mapTarget,
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
        template.path <- system.file("AnnDataPkg.templates",
                                     object@pkg.template,
                                     package="AnnotationDbi")
        createPackage(pkgname=packageName, destinationDir=filePath,
                      originDir=template.path,
                      symbolValues=syms, unlink=unlink, quiet=quiet)
        destSQLiteFilePath <- file.path(extdataDir, dbFileName)
        if (!file.copy(srcSQLiteFilePath, destSQLiteFilePath))
            stop("cannot copy file '", srcSQLiteFilePath, "' to '", destSQLiteFilePath, "'")
        return(invisible(TRUE))
    }
)


### =========================================================================
### Make and test SQLite-based annotation packages
### -------------------------------------------------------------------------
### Just for testing (not exported). See at the bottom of this file for
### typical use of the make_*db functions.


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### HGU95AV2DB schema

make_hgu95av2db <- function(filePath, srcSQLiteFilePath, ...)
{
    pkgseed <- new("AnnDataPkgSeed",
        pkg.template="HGU95AV2DB",
        dbSchema="HGU95AV2DB",
        mapPrefix="hgu95av2",
        mapTarget="chip hgu95av2",
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### YEAST2DB schema

make_yeast2db <- function(filePath, srcSQLiteFilePath, ...)
{
    pkgseed <- new("AnnDataPkgSeed",
        pkg.template="HGU95AV2DB",
        dbSchema="YEAST2DB",
        mapPrefix="yeast2",
        mapTarget="chip yeast2",
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
        pkg.template="HGU95AV2DB",
        dbSchema="YEAST2DB",
        mapPrefix="ygs98",
        mapTarget="chip ygs98",
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### AGDB schema

make_agdb <- function(filePath, srcSQLiteFilePath, ...)
{
    pkgseed <- new("AnnDataPkgSeed",
        pkg.template="HGU95AV2DB",
        dbSchema="AGDB",
        mapPrefix="ag",
        mapTarget="chip ag",
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
    #probes <- c("17096_s_at", "17097_at", "17098_s_at")
    #compareAnnDataIn2Pkgs.AGDB("ag", "agdb", "ag", probes=probes, verbose=verbose)
    compareAnnDataIn2Pkgs.AGDB("ag", "agdb", "ag", verbose=verbose)
}

make_ath1121501db <- function(filePath, srcSQLiteFilePath, ...)
{
    pkgseed <- new("AnnDataPkgSeed",
        pkg.template="HGU95AV2DB",
        dbSchema="AGDB",
        mapPrefix="ath1121501",
        mapTarget="chip ath1121501",
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### LLMAPPINGSDB schema

make_humanLLMappingsdb <- function(filePath, srcSQLiteFilePath, ...)
{
    pkgseed <- new("AnnDataPkgSeed",
        pkg.template="LLMAPPINGSDB",
        dbSchema="LLMAPPINGSDB",
        mapPrefix="ath1121501",
        mapTarget="human LocusLink ids",
        organism="NA",
        species="NA",
        manufacturer="NA",
        chipName="NA",
        manufacturerUrl="NA"
    )
    author <- "Ting-Yuan Liu, ChenWei Lin, Seth Falcon, Jianhua Zhang, James W. MacDonald"
    email <- "biocannotation@lists.fhcrc.org"
    packageName <- "humanLLMappingsdb"
    packageVersion <- "1.13.900"
    license <- "LGPL"
    biocViews <- "AnnotationData, Homo_sapiens, humanLLMappings"
    makeDataPackage(pkgseed, author, email, packageName, packageVersion,
                    license, biocViews, filePath, srcSQLiteFilePath, ...)
}

test_humanLLMappingsdb <- function(verbose=FALSE)
{
    compareAnnDataIn2Pkgs.LLMAPPINGSDB("humanLLMappings", "humanLLMappingsdb",
                                       "humanLLMappings", verbose=verbose)
}

make_mouseLLMappingsdb <- function(filePath, srcSQLiteFilePath, ...)
{
    pkgseed <- new("AnnDataPkgSeed",
        pkg.template="LLMAPPINGSDB",
        dbSchema="LLMAPPINGSDB",
        mapPrefix="mouseLLMappings",
        mapTarget="mouse LocusLink ids",
        organism="NA",
        species="NA",
        manufacturer="NA",
        chipName="NA",
        manufacturerUrl="NA"
    )
    author <- "Ting-Yuan Liu, ChenWei Lin, Seth Falcon, Jianhua Zhang, James W. MacDonald"
    email <- "biocannotation@lists.fhcrc.org"
    packageName <- "mouseLLMappingsdb"
    packageVersion <- "1.13.900"
    license <- "LGPL"
    biocViews <- "AnnotationData, Mus_musculus, mouseLLMappings"
    makeDataPackage(pkgseed, author, email, packageName, packageVersion,
                    license, biocViews, filePath, srcSQLiteFilePath, ...)
}

test_mouseLLMappingsdb <- function(verbose=FALSE)
{
    compareAnnDataIn2Pkgs.LLMAPPINGSDB("mouseLLMappings", "mouseLLMappingsdb",
                                       "mouseLLMappings", verbose=verbose)
}

make_ratLLMappingsdb <- function(filePath, srcSQLiteFilePath, ...)
{
    pkgseed <- new("AnnDataPkgSeed",
        pkg.template="LLMAPPINGSDB",
        dbSchema="LLMAPPINGSDB",
        mapPrefix="ratLLMappings",
        mapTarget="rat LocusLink ids",
        organism="NA",
        species="NA",
        manufacturer="NA",
        chipName="NA",
        manufacturerUrl="NA"
    )
    author <- "Ting-Yuan Liu, ChenWei Lin, Seth Falcon, Jianhua Zhang, James W. MacDonald"
    email <- "biocannotation@lists.fhcrc.org"
    packageName <- "ratLLMappingsdb"
    packageVersion <- "1.13.900"
    license <- "LGPL"
    biocViews <- "AnnotationData, Rattus_norvegicus, ratLLMappings"
    makeDataPackage(pkgseed, author, email, packageName, packageVersion,
                    license, biocViews, filePath, srcSQLiteFilePath, ...)
}

test_ratLLMappingsdb <- function(verbose=FALSE)
{
    compareAnnDataIn2Pkgs.LLMAPPINGSDB("ratLLMappings", "ratLLMappingsdb",
                                       "ratLLMappings", verbose=verbose)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### YEASTDB schema

make_YEASTdb <- function(filePath, srcSQLiteFilePath, ...)
{
    pkgseed <- new("AnnDataPkgSeed",
        pkg.template="YEASTDB",
        dbSchema="YEASTDB",
        mapPrefix="YEAST",
        mapTarget="YEAST",
        organism="Saccharomyces cerevisiae",
        species="Yeast",
        manufacturer="NA",
        chipName="NA",
        manufacturerUrl="NA"
    )
    author <- "Ting-Yuan Liu, ChenWei Lin, Seth Falcon, Jianhua Zhang, James W. MacDonald"
    email <- "biocannotation@lists.fhcrc.org"
    packageName <- "YEASTdb"
    packageVersion <- "1.13.900"
    license <- "LGPL"
    biocViews <- "AnnotationData, Saccharomyces_cerevisiae"
    makeDataPackage(pkgseed, author, email, packageName, packageVersion,
                    license, biocViews, filePath, srcSQLiteFilePath, ...)
}

test_YEASTdb <- function(verbose=FALSE)
{
    compareAnnDataIn2Pkgs.YEASTDB("YEAST", "YEASTdb", "YEAST", verbose=verbose)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Typical use of the make_*db functions:
###   library(AnnotationDbi)
###   AnnotationDbi:::make_hgu95av2db(".", "data/hgu95av2.sqlite")
###   AnnotationDbi:::make_yeast2db(".", "data/yeast2.sqlite")
###   AnnotationDbi:::make_ygs98db(".", "data/ygs98.sqlite")
###   AnnotationDbi:::make_agdb(".", "data/ag.sqlite")
###   AnnotationDbi:::make_ath1121501db(".", "data/ath1121501.sqlite")
###   AnnotationDbi:::make_humanLLMappingsdb(".", "data/humanLLMappings.sqlite")
###   AnnotationDbi:::make_mouseLLMappingsdb(".", "data/mouseLLMappings.sqlite")
###   AnnotationDbi:::make_ratLLMappingsdb(".", "data/ratLLMappings.sqlite")
###   AnnotationDbi:::make_YEASTdb(".", "data/YEAST.sqlite")
### or to make them all at once:
###   AnnotationDbi:::make_all("data", "lastbuilds")

make_all <- function(srcDir=".", destDir=".")
{
    library("AnnotationDbi")
    prefixes <- c(
        "hgu95av2",
        "yeast2",
        "ygs98",
        "ag",
        "ath1121501",
        "humanLLMappings",
        "mouseLLMappings",
        "ratLLMappings",
        "YEAST"
    )
    for (prefix in prefixes) {
        funcname <- paste("make_", prefix, "db", sep="")
        srcSQLiteFilePath <- file.path(srcDir, paste(prefix, ".sqlite", sep=""))
        do.call(funcname, list(destDir, srcSQLiteFilePath))
    }
}

