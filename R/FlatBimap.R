### =========================================================================
### FlatBimap objects
### -----------------
###


### Possible col metanames are: "Lkeyname", "Rkeyname" and "tagname"
### There must be exactly 1 "Lkeyname" and 1 "Rkeyname" col.
### There can be 0 or 1 "tagname" col.
setClass("FlatBimap",
    contains="Bimap",
    representation(
        colmetanames="character",   # must have a length <= ncol of data slot
        direction="integer",
        data="data.frame",
        Lkeys="character",
        Rkeys="character",
        ifnotfound="list"
    ),
    prototype(
        direction=1L,               # left-to-right by default
        Lkeys=as.character(NA),
        Rkeys=as.character(NA),
        ifnotfound=list()           # empty list => raise an error on first key not found
    )
)

setMethod("initialize", "FlatBimap",
    function(.Object, colmetanames, direction, data, Lkeys, Rkeys)
    {
        if (!is.character(colmetanames)
         || any(duplicated(colmetanames))
         || !all(colmetanames %in% c("Lkeyname", "Rkeyname", "tagname"))
         || !all(c("Lkeyname", "Rkeyname") %in% colmetanames))
            stop("invalid col metanames")
        if (ncol(data) < length(colmetanames))
            stop("FlatBimap object has not enough columns")
        .Object@colmetanames <- colmetanames
        if (!missing(direction))
            .Object@direction <- .normalize.direction(direction)
        .Object@data <- data
        .Object@Lkeys <- Lkeys
        .Object@Rkeys <- Rkeys
        if (any(duplicated(Rattribnames(.Object))))
           stop("duplicated Rattrib names")
        .Object
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Implementation of the Bimap interface.
###

setMethod("colmetanames", "FlatBimap",
    function(x) x@colmetanames)

setMethod("colnames", "FlatBimap",
    function(x, do.NULL=TRUE, prefix="col") colnames(x@data))

setReplaceMethod("Rattribnames", "FlatBimap",
    function(x, value)
    {
        colnames0 <- colnames(x@data)
        if (!is.null(value) && !is.character(value))
            stop("Rattrib names must be a character vector or NULL")
        if (!all(value %in% Rattribnames(x)))
            stop("invalid Rattrib names")
        if (any(duplicated(value)))
            stop("can't assign duplicated Rattrib names")
        ii <- c(seq_along(colmetanames(x)), match(value, colnames0))
        x@data <- x@data[ii]
        if (length(ii) < length(colnames0))
            x@data <- unique(x@data)
        ## Needed because subsetting a data frame can change the names
        ## of its cols (for the duplicated names)
        colnames(x@data) <- colnames0[ii]
        x
    }
)

setMethod("direction", "FlatBimap",
    function(x) x@direction)
setReplaceMethod("direction", "FlatBimap",
    function(x, value)
    {
        x@direction <- .normalize.direction(value)
        x
    }
)

setMethod("mappedLkeys", "FlatBimap",
    function(x) unique(x@data[[match("Lkeyname", x@colmetanames)]]))

setMethod("mappedRkeys", "FlatBimap",
    function(x) unique(x@data[[match("Rkeyname", x@colmetanames)]]))

setMethod("Lkeys", "FlatBimap",
    function(x)
    {
        if (length(x@Lkeys) == 1 && is.na(x@Lkeys))
            return(mappedLkeys(x))
        x@Lkeys
    }
)

setMethod("Rkeys", "FlatBimap",
    function(x)
    {
        if (length(x@Rkeys) == 1 && is.na(x@Rkeys))
            return(mappedRkeys(x))
        x@Rkeys
    }
)

.checkKeys <- function(keys, valid.keys, ifnotfound)
{
    if (!is.character(keys))
        stop("the keys must be character strings")
    if (length(ifnotfound) == 0) {
        not_found <- which(!(keys %in% valid.keys))
        if (length(not_found) != 0)
            stop("invalid key \"", keys[not_found[1]], "\"")
    }
}

setReplaceMethod("Lkeys", "FlatBimap",
    function(x, value)
    {
        if (!is.null(value)) {
            .checkKeys(value, Lkeys(x), x@ifnotfound)
            x@Lkeys <- value
        }
        x
    }
)

setReplaceMethod("Rkeys", "FlatBimap",
    function(x, value)
    {
        if (!is.null(value)) {
            .checkKeys(value, Rkeys(x), x@ifnotfound)
            x@Rkeys <- value
        }
        x
    }
)

setMethod("subset", "FlatBimap",
    function(x, Lkeys=NULL, Rkeys=NULL, ...)
    {
        lii <- rii <- TRUE
        Lkeys(x) <- Lkeys
        Rkeys(x) <- Rkeys
        if (!is.null(Lkeys))
            lii <- x@data[[1]] %in% Lkeys
        if (!is.null(Rkeys))
            rii <- x@data[[2]] %in% Rkeys
        cn <- colnames(x@data)
        x@data <- x@data[lii & rii, ]
        colnames(x@data) <- cn
        x
    }
)

setMethod("nrow", "FlatBimap",
    function(x) nrow(x@data))

setMethod("links", "FlatBimap",
    function(x)
    {
        Rattribnames(x) <- NULL
        x@data
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Other convenience methods.
###

setMethod("head", "FlatBimap",
    function(x, n=10, ...)
    {
        c <- colnames(x)
        y <- head(x@data, n, ...)
        if (!identical(colnames(y), c))
            colnames(y) <- c
        y
    }
)

setMethod("tail", "FlatBimap",
    function(x, n=10, ...)
    {
        c <- colnames(x)
        y <- tail(x@data, n, ...)
        if (!identical(colnames(y), c))
            colnames(y) <- c
        y
    }
)

setMethod("show", "FlatBimap",
    function(object)
    {
        cat("\"", class(object), "\" object:\n|\n", sep="")
        Bimap.summary(object)
        cat("\ndata:\n")
        if (nrow(object) <= 20) {
            show(object@data)
        } else {
            show(head(object))
            cat("...\n")
            cat("(", nrow(object), " rows)\n", sep="")
        }
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "toLList" and "toRList" methods (Bimap interface).
###

.alignAnnList <- function(ann_list, keys)
{
    y <- l2e(ann_list)
    key2val <- function(key)
    {
        val <- y[[key]]
        if (is.null(val)) {
            val <- NA
        } else {
            if (class(val) == "data.frame")
                row.names(val) <- NULL
        }
        val
    }
    names(keys) <- keys
    lapply(keys, key2val)
}

setMethod("toLList", "FlatBimap",
    function(x, keys=NULL)
    {
        if (!is.null(keys))
            x <- subset(x, Lkeys=keys, Rkeys=NULL)
        if (nrow(x@data) == 0)
            ann_list <- list()
        else
            ann_list <- split(x@data[ , -1], x@data[[1]])
        .alignAnnList(ann_list, Lkeys(x))
    }
)

setMethod("toRList", "FlatBimap",
    function(x, keys=NULL)
    {
        if (!is.null(keys))
            x <- subset(x, Lkeys=NULL, Rkeys=keys)
        if (nrow(x@data) == 0)
            ann_list <- list()
        else
            ann_list <- split(x@data[ , -2], x@data[[2]])
        .alignAnnList(ann_list, Rkeys(x))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### 2 functions for folding and formatting FlatBimap objects.
###
### WORK IN PROGRESS...
###

### The "foldListOfAtomicVectors" function.
foldListOfAtomicVectors <- function(x, direction, FUN)
{
    stop("NOT READY YET, SORRY")
}

### The "foldListOfLists" function.
###
### Folding a bimap is the action of converting its "flat" representation
### (as a FlatBimap object) to a list of right objects for a mono-valued map,
### and to a list of lists of right objects for a multi-valued map.
### By default (if arg 'FUN' is omitted), each right object itself is stored
### in a named list.
###
### Back to the M bimap example given at the top of this file:
###   
###   A natural way of representing objects A, B, C in R is to use lists:
###     - object A: list("A", c(1.2, 0.9)) 
###     - object B: list("B", character(0))
###     - object C: list("C", -1:1)
###
###   And a natural way of representing map M in R is to use a named list
###   of 4 elements (one for each left object) where:
###     - the "a" element is a named list of 2 elements, one named "A" and
###       containing the representation of A, and one named "B" and
###       containing the representation of B,
###     - the "b" element is a named list of 1 element, named "A" and
###       containing the representation of A,
###     - the "c" element is NA
###     - the "d" element is a named list of 1 element, named "C" and
###       containing the representation of C.
###
###   foldListOfLists() does this conversion from "flat" to "list".
###
### Arguments:
###
### 'direction': 1 for left-to-right and -1 for right-to-left
###
### 'mode': 3 modes are supported:
###         mode 1: mono-valued map
###         mode 2: multi-valued map containing 1 right object per row (i.e.
###           all right objects are stored in a single row, this can only be
###           known in advance if we know that their tags are mono-valued)
###         mode 3: multi-valued map where more than 1 row can be needed to
###           store a given right object (this can happen only if at least
###           one of the tags are multi-valued)
###         In our example above, bimap M is of mode 3.
###
### 'FUN': a formatting function applied on-the-fly on each object during
###        the "folding" process. If 'FUN' is omitted then
###        'function(...) list(...)' is used so no information is lost and
###        the formatting can always be done later with:
###
###        > y <- foldListOfLists(x)
###        > y <- lapply(y, function(val)
###                           lapply(val, function(x) do.call(myfun, x)))
###
###        The result will be the same as with just:
###
###        > y <- foldListOfLists(x, FUN=myfun)
###
###        but the latter will be faster.
###
### WARNING: only folding in direction 1 is currently working.
### TODO: make it work with from="Rkeyname"!
foldListOfLists <- function(x, direction, mode, FUN)
{
    keys <- from.keys(x, direction)
    ans <- as.list(rep(as.character(NA), length(keys)))
    names(ans) <- keys
    if (nrow(x) == 0)
        return(ans)
    if (direction == -1)
        stop("right-to-left folding not yet ready, sorry")
    if (missing(FUN))
        FUN <- function(...) list(...)
    ## First slicing (top level)
    slicer1 <- from.colpos(x, direction)
    slicer2 <- to.colpos(x, direction)
    if (slicer2 > slicer1)
        slicer2 <- slicer2 - 1
    keepcols <- seq_len(length(x@data))[- slicer1]
    slicing_one <- lapply(keepcols,
                     function(j) split(x@data[[j]], x@data[[slicer1]]))
    names(slicing_one) <- names(x@data)[- slicer1]
    mapped_keys <- names(slicing_one[[1]])
    ii <- match(mapped_keys, keys)
    for (i1 in seq_len(length(ii))) {
        i2 <- ii[i1]
        slice_one <- lapply(slicing_one, function(col) col[[i1]])
        if (mode == 1) {
            ans[[i2]] <- do.call(FUN, slice_one)
            next
        }
        if (mode == 2 || !any(duplicated(slice_one[[1]]))) {
            ans[[i2]] <- do.call("mapply",
                                 c(FUN=FUN, slice_one, SIMPLIFY=FALSE))
            next
        }
        ## Sub-slicing
        keepcols <- seq_len(length(slice_one))[- slicer2]
        slicing_two <- lapply(keepcols,
                         function(j) split(slice_one[[j]], slice_one[[slicer2]]))
        slicing_two <- c(list(names(slicing_two[[1]])), slicing_two)
        names(slicing_two) <- c(names(slice_one)[slicer2], names(slice_one)[- slicer2])
        ans[[i2]] <- do.call("mapply",
                             c(FUN=FUN, slicing_two, SIMPLIFY=FALSE))
    }
    ans
}

