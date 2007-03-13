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
        dbFileName <- paste(object@objNamePrefix, ".sqlite", sep="")
        if (missing(RSQLiteVersion)) {
            cran.pkgs <- available.packages(contrib.url("http://cran.fhcrc.org"))
            if (nrow(cran.pkgs) == 0)
                stop("unable to retrieve version of last available RSQLite package from CRAN, please provide a 'RSQLiteVersion' arg")
            RSQLiteVersion <- cran.pkgs['RSQLite', 'Version']
        }
        AnnDbiVersion <- installed.packages()['AnnotationDbi','Version']
        syms <- list(DBSCHEMA=object@dbSchema,
                     OBJPREFIX=object@objNamePrefix,
                     OBJTARGET=object@objTarget,
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
### HGU95AV2_DB schema

make_hgu95av2db <- function(version, filePath, srcSQLiteFilePath, ...)
{
    pkgseed <- new("AnnDataPkgSeed",
        pkg.template="HGU95AV2DB",
        dbSchema="HGU95AV2_DB",
        objNamePrefix="hgu95av2",
        objTarget="chip hgu95av2",
        organism="Homo sapiens",
        species="Human",
        manufacturer="Affymetrix",
        chipName="Human Genome U95 Set",
        manufacturerUrl="http://www.affymetrix.com/support/technical/byproduct.affx?product=hgu95"
    )
    author <- "Nianhua Li, Seth Falcon"
    email <- "biocannotation@lists.fhcrc.org"
    packageName <- "hgu95av2db"
    license <- "LGPL"
    biocViews <- "AnnotationData, AffymetrixChip, Homo_sapiens, hgu95av2"
    makeDataPackage(pkgseed, author, email, packageName, version,
                    license, biocViews, filePath, srcSQLiteFilePath, ...)
}

test_hgu95av2db <- function(verbose=FALSE)
{
    compareAnnDataIn2Pkgs.HGU95AV2_DB("hgu95av2", "hgu95av2db", "hgu95av2", verbose=verbose)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### YEAST2_DB schema

make_yeast2db <- function(version, filePath, srcSQLiteFilePath, ...)
{
    pkgseed <- new("AnnDataPkgSeed",
        pkg.template="HGU95AV2DB",
        dbSchema="YEAST2_DB",
        objNamePrefix="yeast2",
        objTarget="chip yeast2",
        organism="Saccharomyces cerevisiae",
        species="Yeast",
        manufacturer="Affymetrix",
        chipName="Yeast Genome 2.0 Array",
        manufacturerUrl="http://www.affymetrix.com/support/technical/byproduct.affx?product=yeast-20"
    )
    author <- "Ting-Yuan Liu, ChenWei Lin, Seth Falcon, Jianhua Zhang, James W. MacDonald"
    email <- "biocannotation@lists.fhcrc.org"
    packageName <- "yeast2db"
    license <- "LGPL"
    biocViews <- "AnnotationData, AffymetrixChip, Saccharomyces_cerevisiae, yeast2"
    makeDataPackage(pkgseed, author, email, packageName, version,
                    license, biocViews, filePath, srcSQLiteFilePath, ...)
}

test_yeast2db <- function(verbose=FALSE)
{
    compareAnnDataIn2Pkgs.YEAST2_DB("yeast2", "yeast2db", "yeast2", verbose=verbose)
}

make_ygs98db <- function(version, filePath, srcSQLiteFilePath, ...)
{
    pkgseed <- new("AnnDataPkgSeed",
        pkg.template="HGU95AV2DB",
        dbSchema="YEAST2_DB",
        objNamePrefix="ygs98",
        objTarget="chip ygs98",
        organism="Saccharomyces cerevisiae",
        species="Yeast",
        manufacturer="Affymetrix",
        chipName="Yeast Genome S98 Array",
        manufacturerUrl="http://www.affymetrix.com/support/technical/byproduct.affx?product=yeast"
    )
    author <- "Ting-Yuan Liu, ChenWei Lin, Seth Falcon, Jianhua Zhang, James W. MacDonald"
    email <- "biocannotation@lists.fhcrc.org"
    packageName <- "ygs98db"
    license <- "LGPL"
    biocViews <- "AnnotationData, AffymetrixChip, Saccharomyces_cerevisiae, ygs98"
    makeDataPackage(pkgseed, author, email, packageName, version,
                    license, biocViews, filePath, srcSQLiteFilePath, ...)
}

test_ygs98db <- function(verbose=FALSE)
{
    compareAnnDataIn2Pkgs.YEAST2_DB("ygs98", "ygs98db", "ygs98", verbose=verbose)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### AG_DB schema

make_agdb <- function(version, filePath, srcSQLiteFilePath, ...)
{
    pkgseed <- new("AnnDataPkgSeed",
        pkg.template="HGU95AV2DB",
        dbSchema="AG_DB",
        objNamePrefix="ag",
        objTarget="chip ag",
        organism="Arabidopsis thaliana",
        species="Arabidopsis",
        manufacturer="Affymetrix",
        chipName="Arabidopsis Genome Array",
        manufacturerUrl="http://www.affymetrix.com/support/technical/byproduct.affx?product=atgenome1"
    )
    author <- "Ting-Yuan Liu, ChenWei Lin, Seth Falcon, Jianhua Zhang, James W. MacDonald"
    email <- "biocannotation@lists.fhcrc.org"
    packageName <- "agdb"
    license <- "LGPL"
    biocViews <- "AnnotationData, AffymetrixChip, Arabidopsis_thaliana, ag"
    makeDataPackage(pkgseed, author, email, packageName, version,
                    license, biocViews, filePath, srcSQLiteFilePath, ...)
}

test_agdb <- function(verbose=FALSE)
{
    #probes <- c("17096_s_at", "17097_at", "17098_s_at")
    #compareAnnDataIn2Pkgs.AG_DB("ag", "agdb", "ag", probes=probes, verbose=verbose)
    compareAnnDataIn2Pkgs.AG_DB("ag", "agdb", "ag", verbose=verbose)
}

make_ath1121501db <- function(version, filePath, srcSQLiteFilePath, ...)
{
    pkgseed <- new("AnnDataPkgSeed",
        pkg.template="HGU95AV2DB",
        dbSchema="AG_DB",
        objNamePrefix="ath1121501",
        objTarget="chip ath1121501",
        organism="Arabidopsis thaliana",
        species="Arabidopsis",
        manufacturer="Affymetrix",
        chipName="Arabidopsis ATH1 Genome Array",
        manufacturerUrl="http://www.affymetrix.com/support/technical/byproduct.affx?product=arab"
    )
    author <- "Ting-Yuan Liu, ChenWei Lin, Seth Falcon, Jianhua Zhang, James W. MacDonald"
    email <- "biocannotation@lists.fhcrc.org"
    packageName <- "ath1121501db"
    license <- "LGPL"
    biocViews <- "AnnotationData, AffymetrixChip, Arabidopsis_thaliana, ath1121501"
    makeDataPackage(pkgseed, author, email, packageName, version,
                    license, biocViews, filePath, srcSQLiteFilePath, ...)
}

test_ath1121501db <- function(verbose=FALSE)
{
    compareAnnDataIn2Pkgs.AG_DB("ath1121501", "ath1121501db", "ath1121501", verbose=verbose)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### LLMAPPINGS_DB schema

make_humanLLMappingsdb <- function(version, filePath, srcSQLiteFilePath, ...)
{
    pkgseed <- new("AnnDataPkgSeed",
        pkg.template="LLMAPPINGSDB",
        dbSchema="LLMAPPINGS_DB",
        objNamePrefix="ath1121501",
        objTarget="human LocusLink ids",
        organism="NA",
        species="NA",
        manufacturer="NA",
        chipName="NA",
        manufacturerUrl="NA"
    )
    author <- "Ting-Yuan Liu, ChenWei Lin, Seth Falcon, Jianhua Zhang, James W. MacDonald"
    email <- "biocannotation@lists.fhcrc.org"
    packageName <- "humanLLMappingsdb"
    license <- "LGPL"
    biocViews <- "AnnotationData, Homo_sapiens, humanLLMappings"
    makeDataPackage(pkgseed, author, email, packageName, version,
                    license, biocViews, filePath, srcSQLiteFilePath, ...)
}

test_humanLLMappingsdb <- function(verbose=FALSE)
{
    compareAnnDataIn2Pkgs.LLMAPPINGS_DB("humanLLMappings", "humanLLMappingsdb",
                                       "humanLLMappings", verbose=verbose)
}

make_mouseLLMappingsdb <- function(version, filePath, srcSQLiteFilePath, ...)
{
    pkgseed <- new("AnnDataPkgSeed",
        pkg.template="LLMAPPINGSDB",
        dbSchema="LLMAPPINGS_DB",
        objNamePrefix="mouseLLMappings",
        objTarget="mouse LocusLink ids",
        organism="NA",
        species="NA",
        manufacturer="NA",
        chipName="NA",
        manufacturerUrl="NA"
    )
    author <- "Ting-Yuan Liu, ChenWei Lin, Seth Falcon, Jianhua Zhang, James W. MacDonald"
    email <- "biocannotation@lists.fhcrc.org"
    packageName <- "mouseLLMappingsdb"
    license <- "LGPL"
    biocViews <- "AnnotationData, Mus_musculus, mouseLLMappings"
    makeDataPackage(pkgseed, author, email, packageName, version,
                    license, biocViews, filePath, srcSQLiteFilePath, ...)
}

test_mouseLLMappingsdb <- function(verbose=FALSE)
{
    compareAnnDataIn2Pkgs.LLMAPPINGS_DB("mouseLLMappings", "mouseLLMappingsdb",
                                       "mouseLLMappings", verbose=verbose)
}

make_ratLLMappingsdb <- function(version, filePath, srcSQLiteFilePath, ...)
{
    pkgseed <- new("AnnDataPkgSeed",
        pkg.template="LLMAPPINGSDB",
        dbSchema="LLMAPPINGS_DB",
        objNamePrefix="ratLLMappings",
        objTarget="rat LocusLink ids",
        organism="NA",
        species="NA",
        manufacturer="NA",
        chipName="NA",
        manufacturerUrl="NA"
    )
    author <- "Ting-Yuan Liu, ChenWei Lin, Seth Falcon, Jianhua Zhang, James W. MacDonald"
    email <- "biocannotation@lists.fhcrc.org"
    packageName <- "ratLLMappingsdb"
    license <- "LGPL"
    biocViews <- "AnnotationData, Rattus_norvegicus, ratLLMappings"
    makeDataPackage(pkgseed, author, email, packageName, version,
                    license, biocViews, filePath, srcSQLiteFilePath, ...)
}

test_ratLLMappingsdb <- function(verbose=FALSE)
{
    compareAnnDataIn2Pkgs.LLMAPPINGS_DB("ratLLMappings", "ratLLMappingsdb",
                                        "ratLLMappings", verbose=verbose)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### YEAST_DB schema

make_YEASTdb <- function(version, filePath, srcSQLiteFilePath, ...)
{
    pkgseed <- new("AnnDataPkgSeed",
        pkg.template="YEASTDB",
        dbSchema="YEAST_DB",
        objNamePrefix="YEAST",
        objTarget="YEAST",
        organism="Saccharomyces cerevisiae",
        species="Yeast",
        manufacturer="NA",
        chipName="NA",
        manufacturerUrl="NA"
    )
    author <- "Ting-Yuan Liu, ChenWei Lin, Seth Falcon, Jianhua Zhang, James W. MacDonald"
    email <- "biocannotation@lists.fhcrc.org"
    packageName <- "YEASTdb"
    license <- "LGPL"
    biocViews <- "AnnotationData, Saccharomyces_cerevisiae"
    makeDataPackage(pkgseed, author, email, packageName, version,
                    license, biocViews, filePath, srcSQLiteFilePath, ...)
}

test_YEASTdb <- function(verbose=FALSE)
{
    compareAnnDataIn2Pkgs.YEAST_DB("YEAST", "YEASTdb", "YEAST", verbose=verbose)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### AFFYHUEX_DB schema

make_huex10stv2db <- function(version, filePath, srcSQLiteFilePath, ...)
{
    pkgseed <- new("AnnDataPkgSeed",
        pkg.template="AFFYHUEXDB",
        dbSchema="AFFYHUEX_DB",
        objNamePrefix="huex10stv2",
        objTarget="exon array HuEx-1_0-st-v2",
        organism="Homo sapiens",
        species="Human",
        manufacturer="Affymetrix",
        chipName="HuEx-1_0-st-v2",
        manufacturerUrl="NA"
    )
    author <- "Herve Pages"
    email <- "biocannotation@lists.fhcrc.org"
    packageName <- "huex10stv2db"
    license <- "LGPL"
    biocViews <- "AnnotationData, AffymetrixChip, Homo_sapiens, huex10stv2"
    makeDataPackage(pkgseed, author, email, packageName, version,
                    license, biocViews, filePath, srcSQLiteFilePath, ...)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Typical use of the make_*db functions:
###   library(AnnotationDbi)
###   AnnotationDbi:::make_hgu95av2db("1.13.92", ".", "data/hgu95av2.sqlite")
###   AnnotationDbi:::make_yeast2db("1.13.92", ".", "data/yeast2.sqlite")
###   AnnotationDbi:::make_ygs98db("1.13.92", ".", "data/ygs98.sqlite")
###   AnnotationDbi:::make_agdb("1.13.92", ".", "data/ag.sqlite")
###   AnnotationDbi:::make_ath1121501db("1.13.92", ".", "data/ath1121501.sqlite")
###   AnnotationDbi:::make_humanLLMappingsdb("1.13.92", ".", "data/humanLLMappings.sqlite")
###   AnnotationDbi:::make_mouseLLMappingsdb("1.13.92", ".", "data/mouseLLMappings.sqlite")
###   AnnotationDbi:::make_ratLLMappingsdb("1.13.92", ".", "data/ratLLMappings.sqlite")
###   AnnotationDbi:::make_YEASTdb("1.13.92", ".", "data/YEAST.sqlite")
###   AnnotationDbi:::make_huex10stv2db("1.13.92", ".", "data/huex10stv2.sqlite")
### or to make them all at once:
###   AnnotationDbi:::make_all("1.13.92", "data", "lastbuilds")

make_all <- function(version, srcDir=".", destDir=".")
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
        do.call(funcname, list(version, destDir, srcSQLiteFilePath))
    }
}

