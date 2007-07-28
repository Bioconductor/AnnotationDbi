#############################################################################
#############################################################################
###
### AnnDbPkg-maker.R file
###
#############################################################################
#############################################################################


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "AnnDbPkgSeed" class.
###

setClass(
    "AnnDbPkgSeed",
    representation(
        Package="character",            # e.g. "hgu133a2.db"
        Version="character",            # e.g. "0.0.99"
        License="character", 
        Author="character", 
        Maintainer="character", 
        PkgTemplate="character",        # e.g. "HUMANCHIP.DB"
        DBschema="character",           # e.g. "HUMANCHIP_DB"
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
        Author="Marc Carlson, Seth Falcon, Herve Pages, Nianhua Li",
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
    if (any(duplicated(metadata$name))) {
        stop("col \"name\" in \"metadata\" table has duplicated values\n",
             "  (this would never happen if \"name\" was defined as a PRIMARY KEY!)")
    }
    row.names(metadata) <- metadata$name
    for (i in seq_len(length(metadata2slot))) {
        metadata_name <- names(metadata2slot)[i]
        if (!(metadata_name %in% row.names(metadata))) {
            if (metadata_name == "DBSCHEMA")
                stop("'DBSCHEMA' not found in \"metadata\" table")
            next
        }
        slot_name <- metadata2slot[i]
        val <- metadata[metadata_name, "value"]
        if (is.na(slot(x, slot_name))) {
            slot(x, slot_name) <- val
            next
        }
        if (slot(x, slot_name) != val)
            stop(metadata_name, " specified in '", db_file, "' (\"", val, "\") ",
                 "doesn't match 'x@", slot_name, "' (\"", slot(x, slot_name), "\")")
    }
    x
}

initWithDbDoc <- function(db_file)
{
    db_conn <- dbFileConnect(db_file)
    on.exit(dbFileDisconnect(db_conn))
    map_metadata <- dbGetTable(db_conn, "map_metadata")
    map_metadata
}

getSymbolValuesForManPages <- function(map_names, db_file)
{
    map_metadata <- initWithDbDoc(db_file)
    map_source <- lapply(map_names,
                         function(this_map)
                         {
                             map_index <- which(map_metadata$map_name == this_map)
                             if (length(map_index) > 0) {
                                 this_source <- paste(
                                     map_metadata[map_index, "source_name"],
                                     "(",
                                     map_metadata[map_index, "source_url"],
                                     ") on",
                                     map_metadata[map_index, "source_date"],
                                     sep=" ", collapse=" and ")
                             } else {
                                 this_source <- ""
                             }
                             this_source
                         })
    map_source <- sub("_", "\\\\_", map_source)
    names(map_source) <- paste(map_names, "SOURCE", sep="")
    map_source
}

removeCommentsFromFile <- function(infile, outfile)
{
    if (!is.character(infile) || length(infile) != 1 || is.na(infile))
        stop("'infile' must be a character string naming a file")
    if (!is.character(outfile) || length(outfile) != 1 || is.na(outfile))
        stop("'outfile' must be a character string naming a file")
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

loadAnnDbPkgIndex <- function(file)
{
    if (missing(file)) {
        file <- system.file("extdata", "ANNDBPKG-INDEX.TXT",
                            package="AnnotationDbi")
    } else {
        if (!is.character(file) || length(file) != 1 || is.na(file))
            stop("'file' must be a character string naming a file")
    }
    tmp_file <- file.path(tempdir(), paste(basename(file), "tmp", sep="."))
    removeCommentsFromFile(file, tmp_file)
    index <- read.dcf(tmp_file)
    file.remove(tmp_file)
    index
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

setGeneric("makeAnnDbPkg", signature="x",
    function(x, db_file, dest_dir=".", ...) standardGeneric("makeAnnDbPkg")
)

setMethod("makeAnnDbPkg", "AnnDbPkgSeed",
    function(x, db_file, dest_dir=".", ...)
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
            PKGVERSION=x@Version,
            LIC=x@License,
            BIOCVIEWS=x@biocViews,
            DBFILE=db_file_basename,
            ANNDBIVERSION=ann_dbi_version
        )
        man_dir <- file.path(template_path, "man")
        if (file.exists(man_dir)) {
            doc_template_names <- list.files(man_dir, "\\.Rd$")
            is_static <- doc_template_names %in% c("db_conn.Rd", "db_file.Rd")
            doc_template_names <- doc_template_names[!is_static]
            map_names <- sub("\\.Rd$", "", doc_template_names)
            if (length(map_names) != 0)
                symvals <- c(symvals, getSymbolValuesForManPages(map_names, db_file))
        }
        if (any(duplicated(names(symvals)))) {
            str(symvals)
            stop("'symvals' contains duplicated symbols (see above)")
        }
        createPackage(x@Package,
                      destinationDir=dest_dir,
                      originDir=template_path,
                      symbolValues=symvals)
        ## rename Rd files
        if (file.exists(man_dir) && length(doc_template_names) != 0) {
            doc_path <- file.path(dest_dir, x@Package, "man")
            from_doc_names <- paste(doc_path, doc_template_names, sep=.Platform$file.sep)
            to_doc_names <- paste(x@AnnObjPrefix, doc_template_names, sep="")
            to_doc_names <- paste(doc_path, to_doc_names, sep=.Platform$file.sep)
            mapply(file.rename, from_doc_names, to_doc_names)
	}

	dest_db_file <- file.path(dest_dir, x@Package, "inst", "extdata", db_file_basename)
        if (!file.copy(db_file, dest_db_file))
            stop("cannot copy file '", db_file, "' to '", dest_db_file, "'")
        return(invisible(TRUE))
    }
)

setMethod("makeAnnDbPkg", "list",
    function(x, db_file, dest_dir=".", ...)
    {
        x$Class <- "AnnDbPkgSeed"
        y <- do.call("new", x)
        makeAnnDbPkg(y, db_file, dest_dir)
    }
)

### 'x' can be a regular expression.
### Typical use:
###   > library(AnnotationDbi)
###   > makeAnnDbPkg(c("hgu95av2.db", "hgu133a2.db"))
### or to make all the packages:
###   > makeAnnDbPkg(".*") # a character vector of length 1 is treated as a
###                        # regular expression
###
setMethod("makeAnnDbPkg", "character",
    function(x, db_file, dest_dir=".", ...)
    {
        if (missing(db_file)) {
            file <- system.file("extdata", "ANNDBPKG-INDEX.TXT",
                                package="AnnotationDbi")
        }
        index <- loadAnnDbPkgIndex(db_file)
        if (length(x) != 1) {
            ii <- match(x, index[ , "Package"])
            if (any(is.na(ii)))
                stop("packages ", paste(x[is.na(ii)], collapse=", "), " not in ", db_file)
            index <- index[ii, , drop=FALSE]
        } else if (!is.na(x) && x != "") {
            pkgname <- paste("^", x, "$", sep="")
            ii <- grep(pkgname, index[ , "Package"])
            index <- index[ii, , drop=FALSE]
        }
        filter <- list(...)
        for (j in seq_len(length(filter))) {
            colname <- names(filter)[j]
            if (!(colname %in% colnames(index)))
                stop("unknown field '", colname, "'")
            colvals <- filter[[j]]
            if (!is.character(colvals))
                stop("extra arg values must be of type character")
            index <- index[index[ , colname] %in% colvals, , drop=FALSE]
        }
        pkgnames_in1string <- paste(index[, "Package"], collapse=", ")
        cat(nrow(index), " package(s) to make: ", pkgnames_in1string, "\n", sep="")
        for (i in seq_len(nrow(index))) {
            y <- index[i, ]
            y <- as.list(y[!is.na(y)])
            cat("[", i, "/", nrow(index), "] making package ", y[["Package"]], ": ", sep="")
            db_file <- y[["DBfile"]]
            y <- y[names(y) != "DBfile"]
            makeAnnDbPkg(y, db_file, dest_dir)
        }
        cat("DONE (", nrow(index), " package(s) made under the ", dest_dir, " directory)\n", sep="")
    }
)

