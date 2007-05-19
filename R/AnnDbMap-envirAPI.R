### =========================================================================
### Environment-like API for AnnDbMap objects
### -----------------------------------------
###
### This file defines an environment-like API for the AnnDbMap objects (ls,
### mget, eapply, get, exists, [[ and $) for backward compatibility with the
### classic envir-based annotation maps.
### This environment-like API is defined on top of the low-level API for
### AnnDbObj objects (refer to AnnDbObj-lowAPI.R for the definition of this
### low-level API).
###
### Notes:
###   - The "length" method is not redefined here since it is considered to
###     belong to the low-level API.
###   - The "sample" method is not part of the "real" environment API.
###
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Helper functions.
###

.checkNamesAreStrings <- function(names)
{
    if (is.null(names) || !is.character(names) || any(is.na(names)))
        stop("invalid first argument")
}

### Re-order and format the list 'ann_list' as follow:
###   > ann_list <- list(aa=1, b=2, c=3)
###   > names <- c("a", "c", "d")
###   > .formatAnnList(ann_list, names)
### ... must return 'list(a=NA, c=3, d=NA)'
### Note that the returned list must have exactly the names in 'names' (in the
### same order).
.formatAnnList <- function(ann_list, names, replace.single=NULL, replace.multiple=NULL)
{
    if (length(ann_list))
        ann_list <- l2e(ann_list)
    doReplaceSingle <- length(replace.single) != 0L
    doReplaceMultiple <- length(replace.multiple) != 0L
    formatVal <- function(key)
    {
        val <- ann_list[[key]]
        lval <- length(val)
        if (lval == 1L) {
            if (doReplaceMultiple)
              return(replace.multiple)
        } else if (lval > 1L) {
            if (doReplaceMultiple)
              return(replace.multiple)
        } else {                        # lval == 0
            val <- NA
        }
        val
    }
    names(names) <- names
    lapply(names, formatVal)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The ".foldFlatMap" helper function: folds and formats a "flat" map.
###
### Folding a map is the action of converting its "flat" representation (as
### returned by toTable()) to a list of right objects for a mono-valued map,
### and to a list of lists of right objects for a multi-valued map.
### By default (if arg 'FUN' is omitted), each right object itself is stored
### in a named list.
###
### Example:
###
###   Conceptual map M:
###     3 objects on the left: a, b, c
###     2 objects on the right: A, B
###     Mapping:
###       a --> A, B
###       b --> A
###       c --> B
###     In addition, the right objects have 1 tag, which is a numeric vector:
###       A is tagged with c(1.2, 0.9)
###       B is tagged with NA
###
###   toTable(x) queries the database and returns "flat" representations of
###   maps. For M it would return something like this:
###
###     col1   col2   Tag
###     a      A      1.2
###     a      A      0.9
###     a      B       NA
###     b      A      1.2
###     b      A      0.9
###     c      B       NA
###
###   A natural way of representing objects A and B in R is to use lists:
###     - object A: list(col2="A", Tag=c(1.2, 0.9)) 
###     - object B: list(col2="B", Tag=NA)
###
###   A natural way of representing map M in R is to use a named list with 3
###   elements (one for each left object) where:
###     - the "a" element is a named list of 2 elements, one named "A" and
###       containing the representation of A, and one named "B" and
###       containing the representation of B,
###     - the "b" element is a named list of 1 element, named "A" and
###       containing the representation of A,
###     - the "c" element is a named list of 1 element, named "B" and
###       containing the representation of B.
###   We call this the "folded" representation of map M.
###   .foldFlatMap() does the conversion from "flattened" to "folded".
###
### 'FUN' is the formatting function that is applied on-the-fly on each
### object during the "folding" process. If 'FUN' is omitted then
### 'function(...) list(...)' is used so no information is lost and the
### formatting can be done later with:
###
###   > ann_list <- .foldFlatMap(table0, names)
###   > ann_list <- lapply(ann_list, function(val)
###                                  lapply(val, function(x) do.call(FUN, x)))
###
### The result will be the same as with just:
###
###   > ann_list <- .foldFlatMap(table0, names, FUN)
###
### but the latter will be faster.
###
### 3 types are supported:
###   type 1: mono-valued map
###   type 2: multi-valued map containing 1 right object per row (i.e. all
###           right objects are stored in a single row, this can only be known
###           in advance if we know that their tags are mono-valued)
###   type 3: multi-valued map where more than 1 row can be needed to store
###           a given right object (this can happen only if at least one of the
###           tags are multi-valued)
### In the example above, map M is of type 3.
###

.foldFlatMap <- function(table0, names, type, FUN)
{
    ann_list <- as.list(rep(as.character(NA), length(names)))
    names(ann_list) <- names
    if (nrow(table0) == 0)
        return(ann_list)
    if (missing(FUN))
        FUN <- function(...) list(...)
    ## First slicing (top level)
    slicing_one <- lapply(2:length(table0), function(j) split(table0[[j]], table0[[1]]))
    names(slicing_one) <- names(table0)[-1]
    mapped_names <- names(slicing_one[[1]])
    ii <- match(mapped_names, names, nomatch=0L)
    for (i1 in seq_len(length(ii))) {
        i2 <- ii[i1]
        slice_one <- lapply(slicing_one, function(col) col[[i1]])
        if (type == 1) {
            ann_list[[i2]] <- do.call(FUN, slice_one)
            next
        }
        if (type == 2 || !any(duplicated(slice_one[[1]]))) {
            ann_list[[i2]] <- do.call("mapply", c(FUN=FUN, slice_one, SIMPLIFY=FALSE))
            next
        }
        ## Sub-slicing
        slicing_two <- lapply(2:length(slice_one), function(j) split(slice_one[[j]], slice_one[[1]]))
        slicing_two <- c(list(names(slicing_two[[1]])), slicing_two)
        names(slicing_two) <- names(slice_one)
        ann_list[[i2]] <- do.call("mapply", c(FUN=FUN, slicing_two, SIMPLIFY=FALSE))
    }
    ann_list
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "ls" new generic.
###

setMethod("ls", signature(name="AnnDbMap"),
    function(name, pos, envir, all.names, pattern) names(name)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "as.list" generic.
###

setMethod("as.list", "AtomicAnnDbMap",
    function(x, names=NULL)
    {
        if (!is.null(names) && length(names) == 0)
            return(list())
        data0 <- toTable(x, left.names=names)
        if (nrow(data0) == 0) {
            ann_list <- list()
        } else {
            ## We must use 'data0[[2]]' instead of 'data0[[right.colname(x)]]'
            ## because 'data0' names are not necessarily unique e.g.:
            ##   > toTable(GOBPPARENTS, "GO:0000001")
            ##          go_id      go_id Evidence
            ##   1 GO:0000001 GO:0048308      isa
            ##   2 GO:0000001 GO:0048311      isa
            right_col <- data0[[2]]
            if (length(x@rightColType) == 1
             && typeof(right_col) != x@rightColType) {
                converter <- get(paste("as.", x@rightColType, sep=""))
                right_col <- converter(right_col)
            }
            if (length(data0) == 3)
                names(right_col) <- data0[[3]]
            ann_list <- split(right_col, data0[[1]])
        }
        if (is.null(names))
            names <- names(x)
        .formatAnnList(ann_list, names, x@replace.single, x@replace.multiple)
    }
)

setMethod("as.list", "RevAtomicAnnDbMap",
    function(x, names=NULL)
    {
        if (!is.null(names) && length(names) == 0)
            return(list())
        data0 <- toTable(x, right.names=names)
        if (nrow(data0) == 0) {
            ann_list <- list()
        } else {
            left_col <- data0[[1]]
            if (length(data0) == 3)
                names(left_col) <- data0[[3]]
            ann_list <- split(left_col, data0[[2]])
        }
        if (is.null(names))
            names <- names(x)
        .formatAnnList(ann_list, names, x@replace.single, x@replace.multiple)
    }
)

setMethod("as.list", "IpiAnnDbMap",
    function(x, names=NULL)
    {
        if (!is.null(names) && length(names) == 0)
            return(list())
        data0 <- toTable(x, left.names=names)
        if (nrow(data0) == 0) {
            ann_list <- list()
        } else {
            tag_col <- data0[[3]]
            names(tag_col) <- data0[[2]]
            ann_list <- split(tag_col, data0[[1]])
        }
        if (is.null(names))
            names <- names(x)
        .formatAnnList(ann_list, names)
    }
)

### This new version (0.0.27) is 3 times faster than previous version (0.0.26):
### Old version:
###   > system.time(aa <- as.list(hgu95av2GO))
###      user  system elapsed
###    76.968   5.692  85.080
### New version:
###   > system.time(aa <- as.list(hgu95av2GO))
###      user  system elapsed
###    25.305   1.012  27.658
### Reference (envir-based):
###   > system.time(aa <- as.list(hgu95av2GO))
###      user  system elapsed
###     4.456   0.228   4.953
setMethod("as.list", "GoAnnDbMap",
    function(x, names=NULL)
    {
        if (!is.null(names) && length(names) == 0)
            return(list())
        data0 <- toTable(x, left.names=names)
        if (is.null(names))
            names <- names(x)
        ann_list <- as.list(rep(as.character(NA), length(names)))
        names(ann_list) <- names
        if (nrow(data0) != 0) {
            makeGONodeList <- function(GOIDs, Evidences, Ontologies)
            {
                mapply(function(gid, evi, ont)
                       list(GOID=gid, Evidence=evi, Ontology=ont),
                       GOIDs, Evidences, Ontologies, SIMPLIFY=FALSE)
            }
            GOIDs <- split(data0[["go_id"]], data0[[1]])
            Evidences <- split(data0[["Evidence"]], data0[[1]])
            Ontologies <- split(data0[["Ontology"]], data0[[1]])
            ## The 'GOIDs', 'Evidences' and 'Ontologies' lists have the same
            ## names in the same order.
            mapped_names <- names(GOIDs)
            ii <- match(mapped_names, names, nomatch=0L)
            for (i1 in seq_len(length(ii))) {
                i2 <- ii[i1]
                ## 'mapped_names' should always be a subset of 'names'
                ## hence 'i2 == 0L' should never happen. So maybe we should
                ## raise something like "AnnotationDbi internal error" instead
                ## of just ignoring this...
                if (i2 == 0L) next 
                ann_list[[i2]] <- makeGONodeList(GOIDs[[i1]], Evidences[[i1]],
                                                 Ontologies[[i1]])
            }
        }
        ann_list
    }
)

.RevGoAsList <- function(x, names=NULL)
{
    if (!is.null(names) && length(names) == 0)
        return(list())
    data0 <- toTable(x, right.names=names)
    if (!is.null(names) && !all(names %in% data0[[2]]))
        .checkNamesExist(names, names(x))
    if (nrow(data0) == 0)
        return(list())
    left_col <- data0[[1]]
    names(left_col) <- data0[["Evidence"]]
    ann_list <- split(left_col, data0[["go_id"]])
    if (is.null(names))
        names <- names(x)
    .formatAnnList(ann_list, names)
}
    
setMethod("as.list", "RevGoAnnDbMap",
    function(x, names=NULL) .RevGoAsList(x, names)
)
setMethod("as.list", "RevGo3AnnDbMap",
    function(x, names=NULL) .RevGoAsList(x, names)
)

### Formatting the right objects with 'makeGONode' instead of just using the
### default formatting provided by .foldFlatMap() (the default is to create a
### list for each object) makes things _much_ slower:
###  > system.time(aa <- .foldFlatMap(toTable(GOTERM), names(GOTERM), type=1))
###     user  system elapsed 
###    1.988   0.020   2.005 
###  > system.time(bb <- as.list(GOTERM))
###     user  system elapsed 
###   27.710   0.048  27.777
### Why is the S4 initialization mechanism so slow?
setMethod("as.list", "GONodeAnnDbMap",
    function(x, names=NULL)
    {
        if (!is.null(names) && length(names) == 0)
            return(list())
        table0 <- toTable(x, left.names=names)
        if (is.null(names))
            names <- names(x)
        makeGONode <- function(go_id, Term, Ontology, Definition, ...)
        {
            new("GONode", GOID=go_id[1],
                          Term=Term[1],
                          Ontology=Ontology[1],
                          Definition=Definition[1],
                          ...)
        }
        .foldFlatMap(table0, names, type=1, makeGONode)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "mget" new generic.
###
### 'mget(x, map)' vs 'as.list(map, names=x)':
###   1. mget checks its 'x' arg. and gracefully fails if it's not of
###      the expected type (i.e. NULL or NA-free character vector),
###   2. mget will error on the first string in 'x' not in 'names(map)',
###      as.list will accept those strings and map them to NAs.
###   3. if 'x' is a subset of 'names(map)', then 'mget(x, map)'
###      is identical to 'as.list(map, names=x)'.
###   4. 'mget(names(map), map)' is identical to 'as.list(map)'.
###      Note that for a real "environment", 'as.list(envir)' is not identical
###      to 'mget(ls(envir), envir)': the 2 lists have the same elements but
###      not necesarily in the same order!
### NB: .checkNamesExist() is defined in the AnnDbObj-lowAPI.R file.

setMethod("mget", signature(envir="AnnDbMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        .checkNamesAreStrings(x)
        .checkNamesExist(x, names(envir))
        as.list(envir, names=x)
    }
)

setMethod("mget", signature(envir="RevAnnDbMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        .checkNamesAreStrings(x)
        as.list(envir, names=x)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "eapply" new generic.
###

setMethod("eapply", signature(env="AnnDbMap"),
    function(env, FUN, ..., all.names)
    {
        lapply(as.list(env), FUN)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "get" new generic.
###
### We want this:
###   get("1027_at", envir=hgu95av2GO)
### and this
###   get("1027_at", hgu95av2GO)
### to work so we need to dispatch on the 'pos' arg too.
do_get <- function(what, map) mget(what[1], map)[[1]]

setMethod("get", signature(envir="AnnDbMap"),
    function(x, pos, envir, mode, inherits)
    {
        do_get(x, envir)
    }
)

setMethod("get", signature(pos="AnnDbMap", envir="missing"),
    function(x, pos, envir, mode, inherits)
    {
        do_get(x, pos)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "exists" new generic.
###
### We want this:
###   exists("1027_at", envir=hgu95av2GO)
### and this
###   exists("1027_at", hgu95av2GO)
### to work so we need to dispatch on the 'where' arg too.
do_exists <- function(x, map) x %in% names(map)

setMethod("exists", signature(envir="AnnDbMap"),
    function(x, where, envir, frame, mode, inherits)
    {
        do_exists(x, envir)
    }
)

setMethod("exists", signature(where="AnnDbMap", envir="missing"),
    function(x, where, envir, frame, mode, inherits)
    {
        do_exists(x, where)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "[[" and "$" generics.
###

setMethod("[[", "AnnDbMap",
    function(x, i, j, ...)
    {
        # 'x' is guaranteed to be a "AnnDbMap" object (if it's not, then the
        # method dispatch algo will not call this method in the first place),
        # so nargs() is guaranteed to be >= 1
        if (nargs() >= 3)
            stop("too many subscripts")
        subscripts <- list(...)
        if (!missing(i))
            subscripts$i <- i
        if (!missing(j))
            subscripts$j <- j
        # At this point, 'subscripts' should be guaranteed
        # to be of length <= 1
        if (length(subscripts) == 0)
            stop("no index specified")
        i <- subscripts[[1]]
        if (length(i) < 1)
            stop("attempt to select less than one element")
        if (length(i) > 1)
            stop("attempt to select more than one element")
        if (!is.character(i) || is.na(i))
            stop("wrong argument for subsetting an object of class ", sQuote(class(x)))
        get(i, envir=x)
    }
)

setMethod("$", "AnnDbMap", function(x, name) x[[name]])


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "sample" new generic.
###

setMethod("sample", "AnnDbMap",
    function(x, size, replace=FALSE, prob=NULL)
    {
        names <- ls(x)
        as.list(x, names=names[sample(length(names), size, replace, prob)])
    }
)

