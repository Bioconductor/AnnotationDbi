### =========================================================================
### Low-level API for AnnDbObj objects
### ----------------------------------
###
### The "AnnDbObj" class is a general purpose container for SQLite-based
### annotation data (refer to AllClasses.R for the definition of the "AnnDbObj"
### class and its derived classes).
###
### This file defines and implements the low-level API for AnnDbObj objects.
### This API consists of the following set of generics:
###
###   1) Generics that do _not_ access the database:
###        revmap,
###        db,
###        left.db_table, right.db_table,
###        left.colname, right.colname,
###        tagnames,
###        colnames,
###        left.filter, right.filter,
###        show
###
###   2) Generics that access the database:
###        toTable, as.data.frame, nrow,
###        left.names, right.names, names,
###        left.length, right.length, length,
###        as.character,
###        toList,
###        mapped.left.names, count.mapped.left.names,
###        mapped.right.names, count.mapped.right.names,
###        mapped.names, count.mapped.names,
###        is.na
###      NB: "names", "length", "mapped.names" and "count.mapped.names" are
###      "oriented" methods i.e. they give a different result on a map and its
###      "reverse" map (this result depends on the orientation of the map).
###      For each of these methods, there are 2 "unoriented" methods: a left
###      method and a right method.
###
### The environment-like API for AnnDbMap objects (ls, mget, etc...) is defined
### in the AnnDbMap-envirAPI.R file.
###
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "revmap" new generic.
###
### I would have liked to use "reverse" instead of "revmap" but "reverse" is
### already defined as a generic in Biostrings and it seems that the second
### of the 2 packages to be loaded breaks the generic and attached methods
### defined in the first. Don't know how to deal with this situation :-/
### The "rev" generic defined in package:base doesn't work neither because
### we want to be able to use a different signature (2 args).
###
### Note that this generic does _not_ query the database!
###

.revmap <- function(class, x, objName)
{
    if (is.null(objName))
        objName <- paste("revmap(", x@objName, ")", sep="")
    else
        objName <- as.character(objName)
    new(class, x, objName=objName)
}

setMethod("revmap", "AtomicAnnDbMap",
    function(x, objName=NULL) .revmap("RevAtomicAnnDbMap", x, objName)
)
setMethod("revmap", "RevAtomicAnnDbMap",
    function(x, objName=NULL) stop("already a reverse map")
)

setMethod("revmap", "GoAnnDbMap",
    function(x, objName=NULL) .revmap("RevGoAnnDbMap", x, objName)
)
setMethod("revmap", "RevGoAnnDbMap",
    function(x, objName=NULL) stop("already a reverse map")
)

setMethod("revmap", "Go3AnnDbMap",
    function(x, objName=NULL) .revmap("RevGo3AnnDbMap", x, objName)
)
setMethod("revmap", "RevGo3AnnDbMap",
    function(x, objName=NULL) stop("already a reverse map")
)

setMethod("revmap", "environment",
    function(x, objName=NULL) l2e(reverseSplit(as.list(x)))
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Generics that return meta information about a given map:
###     db,
###     left.db_table, right.db_table,
###     left.colname, right.colname,
###     tagnames,
###     colnames,
###     left.filter, right.filter
###
### Note that these generics do _not_ query the database!
###

setMethod("db", "AnnDbObj", function(object) object@conn)

setMethod("left.db_table", "AnnDbMap",
    function(x) L2Rpath.leftmostTable(x@L2Rpath))
setMethod("right.db_table", "AnnDbMap",
    function(x) L2Rpath.rightmostTable(x@L2Rpath))
setMethod("right.db_table", "Go3AnnDbMap", function(x) x@rightTables)

setMethod("left.colname", "AnnDbMap",
    function(x) L2Rpath.leftmostColname(x@L2Rpath))
setMethod("right.colname", "AnnDbMap",
    function(x) L2Rpath.rightmostColname(x@L2Rpath))

setMethod("tagnames", "AnnDbMap",
    function(x) L2Rpath.tagnames(x@L2Rpath))
setMethod("tagnames", "Go3AnnDbMap",
    function(x) c(L2Rpath.tagnames(x@L2Rpath), "Ontology"))

setMethod("colnames", "AnnDbMap",
    function(x, do.NULL=TRUE, prefix="col")
        c(left.colname(x), right.colname(x), tagnames(x))
)

setMethod("left.filter", "AnnDbMap",
    function(x) L2Rpath.leftmostFilter(x@L2Rpath))
setMethod("right.filter", "AnnDbMap",
    function(x) L2Rpath.rightmostFilter(x@L2Rpath))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "toTable" new generic.
###
### Note that because we use the same JOIN for a map and its corresponding
### "reverse" map (this is made possible thanks to the use of INNER joins),
### then the result returned by "toTable" or "nrow" does not depend on the
### orientation (direct/reverse) of the map which is a nice property
### (e.g. 'nrow(map) == nrow(revmap(map))').
###
### The 'left.names' and 'right.names' args can be one of the following:
###   - NULL: the arg is ignored.
###   - A NA-free character vector: only the rows with a "left name" (1st
###     field) matching one of the names in 'left.names' and a "right name"
###     (2nd field) matching one of the names in 'right.names' are
###     retrieved.
### Note that the 'left.names' and 'right.names' args are _not_ checked i.e.
### only NULL and NA-free character vectors are guaranted to work properly.
###

setMethod("toTable", "AnnDbTable",
    function(x, left.names=NULL, verbose=FALSE)
    {
        dbRawAnnDbMapToTable(db(x), left.db_table(x), left.colname(x), left.names,
                                    NULL, NULL, NULL,
                                    x@showCols, x@from, verbose)
    }
)

setMethod("toTable", "AnnDbMap",
    function(x, left.names=NULL, right.names=NULL, extra.colnames=NULL, verbose=FALSE)
    {
        dbSelectFromL2Rpath(db(x), x@L2Rpath, left.names, right.names,
                                   extra.colnames, verbose)
    }
)

### This method needs to retrieve and bind data from the 3 GO tables.
### Binding the results of the 3 SELECTs can be done early in SQLite with
### a UNION:
###   dbGetQuery("query1 UNION query2 UNION query3")
### or later in R with rbind():
###   rbind(dbGetQuery("query1"), dbGetQuery("query2"), dbGetQuery("query3"))
### Surprisingly the latter is almost twice faster than the former!
setMethod("toTable", "Go3AnnDbMap",
    function(x, left.names=NULL, right.names=NULL, extra.colnames=NULL, verbose=FALSE)
    {
        extra.colnames <- c("evidence", extra.colnames)
        getPartialSubmap <- function(ontology)
        {
            table <- right.db_table(x)[ontology]
            L2Rpath <- x@L2Rpath
            L2Rpath[[length(L2Rpath)]]@table <- table
            data <- dbSelectFromL2Rpath(db(x), L2Rpath, left.names, right.names,
                                               extra.colnames, verbose)
            if (nrow(data) != 0)
                data[["Ontology"]] <- ontology
            data
        }
        rbind(getPartialSubmap("BP"),
              getPartialSubmap("CC"),
              getPartialSubmap("MF"))
    }
)

### "as.data.frame" is equivalent to "toTable". Might be deprecated soon.
setMethod("as.data.frame", "AnnDbObj",
    function(x, row.names=NULL, optional=FALSE,
             left.names=NULL, right.names=NULL, ...)
    {
        if (missing(left.names))
            left.names <- row.names
        if (missing(right.names)) {
            if (missing(optional))
                return(toTable(x, left.names, ...))
            if (!identical(optional, FALSE))
                right.names <- optional
        }
        toTable(x, left.names, right.names=right.names, ...)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "nrow" new generic.
###
### Conceptual definition (for AnnDbMap object x):
###     nrow(x) :== nrow(toTable(x))
###
### Since "toTable" is unoriented, then "nrow" is unoriented too.
###

setMethod("nrow", "AnnDbTable",
    function(x)
    {
        dbCountRawAnnDbMapRows(db(x), left.db_table(x), left.colname(x), NULL, NULL, x@from)
    }
)

setMethod("nrow", "AnnDbMap",
    function(x) dbCountRowsFromL2Rpath(db(x), x@L2Rpath)
)

setMethod("nrow", "Go3AnnDbMap",
    function(x)
    {
        countRows <- function(ontology)
        {
            table <- right.db_table(x)[ontology]
            L2Rpath <- x@L2Rpath
            L2Rpath[[length(L2Rpath)]]@table <- table
            dbCountRowsFromL2Rpath(db(x), x@L2Rpath)
        }
        countRows("BP") + countRows("CC") + countRows("MF")
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "left.names", "right.names" and "names" generics.
###

setMethod("left.names", "AnnDbObj",
    function(x)
    {
        dbUniqueVals(db(x), left.db_table(x), left.colname(x),
                            left.filter(x), x@datacache)
    }
)

setMethod("right.names", "AnnDbMap",
    function(x)
    {
        dbUniqueVals(db(x), right.db_table(x), right.colname(x),
                            right.filter(x), x@datacache)
    }
)

setMethod("right.names", "Go3AnnDbMap",
    function(x)
    {
        getNames <- function(ontology)
        {
            table <- right.db_table(x)[ontology]
            dbUniqueVals(db(x), table, "go_id", right.filter(x), x@datacache)
        }
        ## Because a given go_id can only belong to 1 of the 3 ontologies...
        ## (if not, then apply unique to this result)
        c(getNames("BP"), getNames("CC"), getNames("MF"))
    }
)

setMethod("names", "AnnDbObj", function(x) left.names(x))
setMethod("names", "RevAnnDbMap", function(x) right.names(x))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "left.length", "right.length" and "length" generic.
###
### Conceptual definitions (for AnnDbMap object x):
###     left.length(x) :== length(left.names(x))
###     right.length(x) :== length(right.names(x))
###

setMethod("left.length", "AnnDbObj",
    function(x)
    {
        dbCountUniqueVals(db(x), left.db_table(x), left.colname(x),
                                 left.filter(x), x@datacache)
    }
)

setMethod("right.length", "AnnDbMap",
    function(x)
    {
        dbCountUniqueVals(db(x), right.db_table(x), right.colname(x),
                                 right.filter(x), x@datacache)
    }
)

setMethod("right.length", "Go3AnnDbMap",
    function(x)
    {
        countNames <- function(ontology)
        {
            table <- right.db_table(x)[ontology]
            dbCountUniqueVals(db(x), table, "go_id", right.filter(x), x@datacache)
        }
        ## Because a given go_id can only belong to 1 of the 3 ontologies...
        countNames("BP") + countNames("CC") + countNames("MF")
    }
)

setMethod("length", "AnnDbObj", function(x) left.length(x))
setMethod("length", "RevAnnDbMap", function(x) right.length(x))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" methods.
###
### Note that this generic does _not_ query the database!
###

setMethod("show", "AnnDbTable",
    function(object)
    {
        cat(object@objName, " table for ", object@objTarget,
            " (object of class \"", class(object), "\")\n", sep="")
    }
)

setMethod("show", "AnnDbMap",
    function(object)
    {
        cat(object@objName, " map for ", object@objTarget,
            " (object of class \"", class(object), "\")\n", sep="")
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "as.character" generic.
###
### For untagged Reverse/AtomicAnnDbMap obects only!
###

### R doesn't let me add a 'names' arg here:
###  Error in rematchDefinition(definition, fdef, mnames, fnames, signature) :
###          methods can add arguments to the generic only if '...' is an argument to the generic
setMethod("as.character", "AtomicAnnDbMap",
    function(x)
    {
        if (length(tagnames(x)) != 0)
            stop("AtomicAnnDbMap object with tags cannot be coerced to a character vector")
        data <- toTable(x)
        ans <- data[[2]] # could also use [[right.colname(x)]]
        if (!is.character(ans))
            ans <- as.character(ans)
        names(ans) <- data[[1]] # could also use [[left.colname(x)]]
        if (any(duplicated(names(ans))))
            warning("returned vector has duplicated names")
        ans
    }
)

setMethod("as.character", "RevAtomicAnnDbMap",
    function(x)
    {
        if (length(tagnames(x)) != 0)
            stop("cannot coerce to character an AtomicAnnDbMap object with tags")
        data <- toTable(x)
        ans <- data[[1]] # could also use [[left.colname(x)]]
        if (!is.character(ans))
            ans <- as.character(ans)
        names(ans) <- data[[2]] # could also use [[right.colname(x)]]
        if (any(duplicated(names(ans))))
            warning("returned vector has duplicated names")
        ans
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "toList" new generic.
###
### The "toList" methods below have a 'names' arg.
### The 'names' arg. can be one of the following:
###   - NULL: all map elements are extracted (equivalent to passing
###           'names=names(x)' but more efficient).
###   - A NA-free character vector: the names of the returned list will
###     be those passed in 'names' in the same order. Names that are not
###     in the map are associated with NAs. Note that, unlike "mget",
###     "toList" doesn't treat differently names that are not in the map
###     from names that are in the map but associated with NAs.
###   - A NA-free numeric vector: for conveniency 'toList(x, 1:3)' is a
###     shortcut for 'toList(x, names(x)[1:3])'. This is identical to
###     'toList(x)[1:3]' but much faster.
### Note that the 'names' arg. is _not_ checked i.e. only NULL, NA-free
### character vectors and NA-free numeric vectors are guaranted to work.
###

.checkNamesExist <- function(names, all.names)
{
    if (!is.null(names)) {
        not_found <- which(!(names %in% all.names))
        if (length(not_found) != 0)
            stop("value for '", names[not_found[1]], "' not found")
    }
}

alignAnnList <- function(x, names)
{
    y <- l2e(x)
    name2val <- function(name)
    {
        val <- y[[name]]
        if (is.null(val)) {
            val <- NA
        } else if (class(val) == "data.frame") {
            row.names(val) <- NULL
        }
        val
    }
    names(names) <- names
    lapply(names, name2val)
}

setMethod("toList", "AnnDbMap",
    function(x, names=NULL)
    {
        if (!is.null(names) && length(names) == 0)
            return(list())
        data0 <- toTable(x, left.names=names)
        if (nrow(data0) == 0) {
            ann_list <- list()
        } else {
            ## Just to make sure that toTable() is not broken
            if (!identical(names(data0)[1:2], c(left.colname(x), right.colname(x))))
                stop("annotationDbi internal problem, please report to the maintainer")
            data1 <- data0[ , -1]
            if (length(x@rightColType) == 1
             && typeof(data1[[1]]) != x@rightColType) {
                converter <- get(paste("as.", x@rightColType, sep=""))
                data1[[1]] <- converter(data1[[1]])
            }
            ann_list <- split(data1, data0[[1]])
        }
        if (is.null(names))
            names <- names(x)
        alignAnnList(ann_list, names)
    }
)

setMethod("toList", "RevAnnDbMap",
    function(x, names=NULL)
    {
        if (!is.null(names) && length(names) == 0)
            return(list())
        data0 <- toTable(x, right.names=names)
        if (!is.null(names) && !all(names %in% data0[[2]])) # could also use [[right.colname(x)]]
            .checkNamesExist(names, names(x))
        if (nrow(data0) == 0) {
            ann_list <- list()
        } else {
            ## Just to make sure that toTable() is not broken
            if (!identical(names(data0)[1:2], c(left.colname(x), right.colname(x))))
                stop("annotationDbi internal problem, please report to the maintainer")
            data2 <- data0[ , -2]
            ann_list <- split(data2, data0[[2]])
        }
        if (is.null(names))
            names <- names(x)
        alignAnnList(ann_list, names)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "mapped names" family of generics:
###   - mapped.left.names, count.mapped.left.names
###   - mapped.right.names, count.mapped.right.names
###   - mapped.names, count.mapped.names
###
### Conceptual definitions (for AnnDbMap object x):
###
###     mapped.left.names(x) :== unique values in left col (col 1) of
###                              toTable(x)
###     count.mapped.left.names(x) :== length(mapped.left.names(x))
###
###     mapped.right.names(x) :== unique values in right col (col 2) of
###                               toTable(x)
###     count.mapped.right.names(x) :== length(mapped.right.names(x))
###
### Note that all "right names" should be mapped to a "left name" hence
### mapped.right.names(x) should be the same as right.names(x) (something
### worth checking in a test unit).
###

### For an AtomicAnnDbMap, x@replace.single and x@replace.multiple will be
### ignored, hence will give wrong results if one of those 2 fields has a
### non-default value like silly maps ENTREZID and MULTIHIT in AG_DB schema.
### But who cares, those maps are silly anyway...
setMethod("mapped.left.names", "AnnDbMap",
    function(x) dbUniqueMappedVals(db(x), x@L2Rpath, x@datacache)
)
setMethod("count.mapped.left.names", "AnnDbMap",
    function(x) dbCountUniqueMappedVals(db(x), x@L2Rpath, x@datacache)
)

setMethod("mapped.right.names", "AnnDbMap",
    function(x) dbUniqueMappedVals(db(x), L2Rpath.rev(x@L2Rpath), x@datacache)
)
setMethod("count.mapped.right.names", "AnnDbMap",
    function(x) dbCountUniqueMappedVals(db(x), L2Rpath.rev(x@L2Rpath), x@datacache)
)

setMethod("mapped.left.names", "Go3AnnDbMap",
    function(x)
    {
        getMappedNames <- function(ontology)
        {
            table <- right.db_table(x)[ontology]
            L2Rpath <- x@L2Rpath
            L2Rpath[[length(L2Rpath)]]@table <- table
            dbUniqueMappedVals(db(x), L2Rpath, x@datacache)
        }
        names1 <- getMappedNames("BP")
        names2 <- getMappedNames("CC")
        names3 <- getMappedNames("MF")
        unique(c(names1, names2, names3))
    }
)
setMethod("count.mapped.left.names", "Go3AnnDbMap",
    function(x) length(mapped.left.names(x))
)

setMethod("mapped.right.names", "Go3AnnDbMap",
    function(x)
    {
        getMappedNames <- function(ontology)
        {
            table <- right.db_table(x)[ontology]
            L2Rpath <- x@L2Rpath
            L2Rpath[[length(L2Rpath)]]@table <- table
            dbUniqueMappedVals(db(x), L2Rpath.rev(L2Rpath), x@datacache)
        }
        names1 <- getMappedNames("BP")
        names2 <- getMappedNames("CC")
        names3 <- getMappedNames("MF")
        ## Because a given go_id can only belong to 1 of the 3 ontologies...
        ## (if not, then apply unique to this result)
        c(names1, names2, names3)
    }
)
setMethod("count.mapped.right.names", "Go3AnnDbMap",
    function(x)
    {
        countMappedNames <- function(ontology)
        {
            table <- right.db_table(x)[ontology]
            L2Rpath <- x@L2Rpath
            L2Rpath[[length(L2Rpath)]]@table <- table
            dbCountUniqueMappedVals(db(x), L2Rpath.rev(L2Rpath), x@datacache)
        }
        ## Because a given go_id can only belong to 1 of the 3 ontologies...
        countMappedNames("BP") + countMappedNames("CC") + countMappedNames("MF")
    }
)

setMethod("mapped.names", "AnnDbMap", function(x) mapped.left.names(x))
setMethod("mapped.names", "RevAnnDbMap", function(x) mapped.right.names(x))
setMethod("count.mapped.names", "AnnDbMap", function(x) count.mapped.left.names(x))
setMethod("count.mapped.names", "RevAnnDbMap", function(x) count.mapped.right.names(x))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "is.na" generic.
###
### 'is.na(x)' is a named logical vector that associates each name in the map
### with TRUE except for those names that are actually mapped to something
### (other than an NA).
###

setMethod("is.na", "AnnDbMap",
    function(x)
    {
        mapped_names <- mapped.names(x)
        names <- names(x)
        ans <- !(names %in% mapped_names)
        names(ans) <- names
        ans
    }
)

