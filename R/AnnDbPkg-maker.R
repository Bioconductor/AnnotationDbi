#############################################################################
#############################################################################
###
### AnnDbPkg-maker.R file
###
#############################################################################


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "AnnDbPkgSeed" class.
###

setClass(
    "AnnDbPkgSeed",
    representation(
        Package="character",            # e.g. "hgu133a2db"
        Version="character",            # e.g. "0.0.99"
        License="character", 
        Author="character", 
        Maintainer="character", 
        PkgTemplate="character",        # e.g. "HGU95AV2DB"
        DBschema="character",           # e.g. "HGU95AV2_DB"
        AnnObjPrefix="character",       # e.g. "hgu133a2"
        AnnObjTarget="character",       # e.g. "chip hgu133a2"
        organism="character",           # e.g. "Homo sapiens"
        species="character",            # e.g. "Human"
        manufacturer="character",       # e.g. "Affymetrix"
        chipName="character",           # e.g. "Human Genome U133A 2.0 Array"
        manufacturerUrl="character",    # e.g. "http://www.affymetrix.com/support/technical/byproduct.affx?product=hgu133-20"
        biocViews="character"
    ),
    prototype(
        #License="The Artistic License, Version 2.0",
        License="LGPL",
        Author="Nianhua Li, Seth Falcon, Herve Pages",
        Maintainer="Biocore Data Team <biocannotation@lists.fhcrc.org>",
        DBschema=as.character(NA),
        AnnObjPrefix=as.character(NA),
        AnnObjTarget=as.character(NA),
        organism=as.character(NA),
        species=as.character(NA),
        manufacturer=as.character(NA),
        chipName=as.character(NA),
        manufacturerUrl=as.character(NA),
        biocViews=as.character(NA)
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Some helper functions.
###

initComputedSlots <- function(x)
{
    if (is.na(x@AnnObjTarget)
     && !is.na(x@AnnObjPrefix)) {
        x@AnnObjTarget <- paste("chip", x@AnnObjPrefix)
    }
    if (is.na(x@biocViews)
     && !is.na(x@AnnObjPrefix)
     && !is.na(x@organism)
     && !is.na(x@manufacturer)) {
        chip_view <- paste(x@manufacturer, "Chip", sep="")
        org_view <- chartr(" ", "_", x@organism)
        x@biocViews <- paste("AnnotationData", chip_view, org_view,
                             x@AnnObjPrefix, sep=", ")
    }
    x
}

initWithDbMetada <- function(x, db_file)
{
    metadata2slot <- c(
        DBSCHEMA="DBschema",
        ORGANISM="organism",
        SPECIES="species",
        MANUFACTURER="manufacturer",
        CHIPNAME="chipName",
        MANUFACTURERURL="manufacturerUrl"
    )
    db_conn <- dbFileConnect(db_file)
    on.exit(dbFileDisconnect(db_conn))
    metadata <- dbGetTable(db_conn, "metadata")
    row.names(metadata) <- metadata$name
    for (i in seq_len(length(metadata2slot))) {
        slot_name <- metadata2slot[i]
        metadata_name <- names(metadata2slot)[i]
        val <- metadata[metadata_name, "value"]
        if (!is.na(slot(x, slot_name)) && slot(x, slot_name) != val)
            warning(metadata_name, " specified in '", db_file, "' (\"", val, "\") ",
                    "doesn't match 'x@", slot_name, "' (\"", slot(x, slot_name), "\")")
        slot(x, slot_name) <- val
    }
    x
}

removeCommentsFromFile <- function(infile, outfile)
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "initialize" method for AnnDbPkgSeed objects.
###

setMethod("initialize", "AnnDbPkgSeed",
    function(.Object, ...)
    {
        .Object <- callNextMethod()
        initComputedSlots(.Object)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "makeAnnDbPkg" new generic.
###

setGeneric("makeAnnDbPkg", function(x, db_file, dest_dir=".") standardGeneric("makeAnnDbPkg"))

setMethod("makeAnnDbPkg", "AnnDbPkgSeed",
    function(x, db_file, dest_dir=".")
    {
        x <- initWithDbMetada(x, db_file)
        x <- initComputedSlots(x)
        db_file_basename <- basename(db_file)
        if (db_file_basename != paste(x@AnnObjPrefix, ".sqlite", sep=""))
            stop("'", db_file, "': File name doesn't match 'x@AnnObjPrefix' (", x@AnnObjPrefix, ")")
        template_path <- system.file("AnnDbPkg-templates",
                                     x@PkgTemplate,
                                     package="AnnotationDbi")
        ann_dbi_version <- installed.packages()['AnnotationDbi','Version']
        symvals <- list(
            DBSCHEMA=x@DBschema,
            ANNOBJPREFIX=x@AnnObjPrefix,
            ANNOBJTARGET=x@AnnObjTarget,
            ORGANISM=x@organism,
            SPECIES=x@species,
            MANUF=x@manufacturer,
            CHIPNAME=x@chipName,
            MANUFURL=x@manufacturerUrl,
            AUTHOR=x@Author,
            VERSION=x@Version,
            LIC=x@License,
            BIOCVIEWS=x@biocViews,
            DBFILE=db_file_basename,
            ANNDBIVERSION=ann_dbi_version
        )
        createPackage(x@Package,
                      destinationDir=dest_dir,
                      originDir=template_path,
                      symbolValues=symvals)
        dest_db_file <- file.path(dest_dir, x@Package, "inst", "extdata", db_file_basename)
        if (!file.copy(db_file, dest_db_file))
            stop("cannot copy file '", db_file, "' to '", dest_db_file, "'")
        return(invisible(TRUE))
    }
)

setMethod("makeAnnDbPkg", "list",
    function(x, db_file, dest_dir=".")
    {
        x$Class <- "AnnDbPkgSeed"
        y <- do.call("new", x)
        makeAnnDbPkg(y, db_file, dest_dir)
    }
)

### 'x' can be a regular expression.
### Typical use:
###   > library(AnnotationDbi)
###   > makeAnnDbPkg("hgu133a2db")
### or to make all the packages:
###   > makeAnnDbPkg(".*")
###
setMethod("makeAnnDbPkg", "character",
    function(x, db_file, dest_dir=".")
    {
        if (missing(db_file))
            db_file <- system.file("extdata", "ANNDBPKG-INDEX.TXT",
                                   package="AnnotationDbi")
        tmp_file <- file.path(dest_dir, paste(basename(db_file), "tmp", sep="."))
        removeCommentsFromFile(db_file, tmp_file)
        index <- read.dcf(tmp_file)
        file.remove(tmp_file)
        pkgname <- paste("^", x, "$", sep="")
        subindex <- index[grep(pkgname, index[ , "Package"]), , drop=FALSE]
        for (i in seq_len(nrow(subindex))) {
            y <- subindex[i, ]
            y <- as.list(y[!is.na(y)])
            db_file <- y[["DBfile"]]
            y <- y[names(y) != "DBfile"]
            makeAnnDbPkg(y, db_file, dest_dir)
        }
    }
)

