### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setClass(
    "AnnDataPkgSeed",
    representation(
        pkg.template="character",    # e.g. "HGU95AV2DB"
        dbSchema="character",        # e.g. "HGU95AV2_DB"
        objNamePrefix="character",   # e.g. "hgu95av2"
        objTarget="character",       # e.g. "chip hgu95av2"
        organism="character",
        species="character",
        manufacturer="character",
        chipName="character",
        manufacturerUrl="character"
    )
)

###
### The makeDataPackage method for AnnDataPkgSeed objects
###
setMethod(Biobase::makeDataPackage,
    signature(object="AnnDataPkgSeed"),
    function(object, author, email, packageName, packageVersion,
             license, biocViews, filePath,
             srcSQLiteFilePath, unlink=FALSE, quiet=FALSE)
    {
        if (!file.exists(srcSQLiteFilePath))
            stop("'", srcSQLiteFilePath, "': No such file or directory")
        if (missing(email) || !is.character(email)
         || length(email) != 1 || grep("@", email) != 1)
            stop("invalid email address")
        extdataDir <- file.path(filePath, packageName, "inst", "extdata")
        dbFileName <- paste(object@objNamePrefix, ".sqlite", sep="")
        ## RSQLiteVersion is not used anymore
        #if (missing(RSQLiteVersion)) {
        #    cran.pkgs <- available.packages(contrib.url("http://cran.fhcrc.org"))
        #    if (nrow(cran.pkgs) == 0)
        #        stop("unable to retrieve version of last available RSQLite package from CRAN, please provide a 'RSQLiteVersion' arg")
        #    RSQLiteVersion <- cran.pkgs['RSQLite', 'Version']
        #}
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
removeCommentsInFile <- function(infile, outfile)
{
    if (file.exists(outfile))
        stop("file '", outfile, "' already exists")
    outfile <- file(outfile, "w")
    #on.exit(close(outfile)) # doesn't seem to work
    infile <- file(infile, "r")
    #on.exit(close(infile))
    while (TRUE) {
        text <- readLines(infile, n=1)
        if (length(text) == 0)
            break
        if (substr(text, 1, 1) != "#")
            writeLines(text, outfile)
    }
    close(infile)
    close(outfile)
}

### 'pkgname' can be a regular expression.
### Typical use:
###   > library(AnnotationDbi)
###   > AnnotationDbi:::makeSQLiteAnnDataPkg("hgu95av2db")
### or to make all the packages:
###   > AnnotationDbi:::makeSQLiteAnnDataPkg(".*")
###
makeSQLiteAnnDataPkg <- function(pkgname, dest_dir=".")
{
    master_file <- "SQLITE-ANN-DATA-PKGS.TXT"
    master_filepath <- system.file("extdata", master_file,
                                   package="AnnotationDbi")
    tmp_file <- paste(master_file, "tmp", sep=".")
    removeCommentsInFile(master_filepath, tmp_file)
    master <- read.dcf(tmp_file)
    file.remove(tmp_file)
    master <- master[grep(pkgname, master[ ,"Package"]), , drop=FALSE]
    for (i in seq_len(nrow(master))) {
        pkginfo <- master[i, ]
        pkgname <- pkginfo["Package"]
        version <- pkginfo["Version"]
        db_file <- pkginfo["DBfile"]
        pkgseed <- new("AnnDataPkgSeed",
            pkg.template=pkginfo["PkgTemplate"],
            dbSchema=pkginfo["DBschema"],
            objNamePrefix=pkginfo["AnnObjectPrefix"],
            objTarget=pkginfo["AnnObjectTarget"],
            organism=pkginfo["organism"],
            species=pkginfo["species"],
            manufacturer=pkginfo["manufacturer"],
            chipName=pkginfo["chipName"],
            manufacturerUrl=pkginfo["manufacturerUrl"]
        )
        author <- "Nianhua Li, Seth Falcon, Herve Pages"
        email <- "biocannotation@lists.fhcrc.org"
        license <- "LGPL"
        biocViews <- paste("AnnotationData", pkginfo["biocViews"], sep=", ")
        makeDataPackage(pkgseed, author, email, pkgname, version,
            license, biocViews, dest_dir, db_file)
    }
}


### =========================================================================
### Test SQLite-based annotation packages
### -------------------------------------------------------------------------

### HGU95AV2_DB schema
test_hgu95av2db <- function(verbose=FALSE)
{
    compareAnnDataIn2Pkgs.HGU95AV2_DB("hgu95av2", "hgu95av2db", "hgu95av2", verbose=verbose)
}

### YEAST2_DB schema
test_yeast2db <- function(verbose=FALSE)
{
    compareAnnDataIn2Pkgs.YEAST2_DB("yeast2", "yeast2db", "yeast2", verbose=verbose)
}
test_ygs98db <- function(verbose=FALSE)
{
    compareAnnDataIn2Pkgs.YEAST2_DB("ygs98", "ygs98db", "ygs98", verbose=verbose)
}

### AG_DB schema
test_agdb <- function(verbose=FALSE)
{
    #probes <- c("17096_s_at", "17097_at", "17098_s_at")
    #compareAnnDataIn2Pkgs.AG_DB("ag", "agdb", "ag", probes=probes, verbose=verbose)
    compareAnnDataIn2Pkgs.AG_DB("ag", "agdb", "ag", verbose=verbose)
}
test_ath1121501db <- function(verbose=FALSE)
{
    compareAnnDataIn2Pkgs.AG_DB("ath1121501", "ath1121501db", "ath1121501", verbose=verbose)
}

### YEAST_DB schema
test_YEASTdb <- function(verbose=FALSE)
{
    compareAnnDataIn2Pkgs.YEAST_DB("YEAST", "YEASTdb", "YEAST", verbose=verbose)
}

### LLMAPPINGS_DB schema
test_humanLLMappingsdb <- function(verbose=FALSE)
{
    compareAnnDataIn2Pkgs.LLMAPPINGS_DB("humanLLMappings", "humanLLMappingsdb",
                                       "humanLLMappings", verbose=verbose)
}
test_mouseLLMappingsdb <- function(verbose=FALSE)
{
    compareAnnDataIn2Pkgs.LLMAPPINGS_DB("mouseLLMappings", "mouseLLMappingsdb",
                                       "mouseLLMappings", verbose=verbose)
}
test_ratLLMappingsdb <- function(verbose=FALSE)
{
    compareAnnDataIn2Pkgs.LLMAPPINGS_DB("ratLLMappings", "ratLLMappingsdb",
                                        "ratLLMappings", verbose=verbose)
}

