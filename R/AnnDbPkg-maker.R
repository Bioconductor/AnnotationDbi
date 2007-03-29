### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setClass(
    "AnnDbPkgSeed",
    representation(
        pkgTemplate="character",     # e.g. "HGU95AV2DB"
        dbSchema="character",        # e.g. "HGU95AV2_DB"
        annObjPrefix="character",    # e.g. "hgu95av2"
        annObjTarget="character",       # e.g. "chip hgu95av2"
        organism="character",
        species="character",
        manufacturer="character",
        chipName="character",
        manufacturerUrl="character"
    )
)

###
### The makeDataPackage method for AnnDbPkgSeed objects
###
setMethod(Biobase::makeDataPackage,
    signature(object="AnnDbPkgSeed"),
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
        dbFileName <- paste(object@annObjPrefix, ".sqlite", sep="")
        ## RSQLiteVersion is not used anymore
        #if (missing(RSQLiteVersion)) {
        #    cran.pkgs <- available.packages(contrib.url("http://cran.fhcrc.org"))
        #    if (nrow(cran.pkgs) == 0)
        #        stop("unable to retrieve version of last available RSQLite package from CRAN, please provide a 'RSQLiteVersion' arg")
        #    RSQLiteVersion <- cran.pkgs['RSQLite', 'Version']
        #}
        AnnDbiVersion <- installed.packages()['AnnotationDbi','Version']
        syms <- list(DBSCHEMA=object@dbSchema,
                     ANNOBJPREFIX=object@annObjPrefix,
                     ANNOBJTARGET=object@annObjTarget,
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
        template.path <- system.file("AnnDbPkg-templates",
                                     object@pkgTemplate,
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
###   > makeAnnDbPkg("hgu95av2db")
### or to make all the packages:
###   > makeAnnDbPkg(".*")
###
makeAnnDbPkg <- function(pkgname, dest_dir=".")
{
    index_file <- "ANNDBPKG-INDEX.TXT"
    index_path <- system.file("extdata", index_file,
                              package="AnnotationDbi")
    tmp_file <- paste(index_file, "tmp", sep=".")
    removeCommentsInFile(index_path, tmp_file)
    index <- read.dcf(tmp_file)
    file.remove(tmp_file)
    pkgname <- paste("^", pkgname, "$", sep="")
    subindex <- index[grep(pkgname, index[ , "Package"]), , drop=FALSE]
    for (i in seq_len(nrow(subindex))) {
        pkginfo <- subindex[i, ]
        pkgname <- pkginfo["Package"]
        version <- pkginfo["Version"]
        db_file <- pkginfo["DBfile"]
        pkgseed <- new("AnnDbPkgSeed",
            pkgTemplate=pkginfo["PkgTemplate"],
            dbSchema=pkginfo["DBschema"],
            annObjPrefix=pkginfo["AnnObjPrefix"],
            annObjTarget=pkginfo["AnnObjTarget"],
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

