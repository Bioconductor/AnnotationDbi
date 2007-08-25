### =========================================================================
### FlatBimap objects
### -----------------
###


### Possible col labels are: "left", "right", "tag", "Lattrib", "Rattrib"
### There must be exactly 1 "left" and 1 "right" col.
### There can be 0 or 1 "tag" col.
### There can be any number of "Lattrib", "Rattrib" or unlabelled (NA) cols.
setClass("FlatBimap",
    contains="Bimap",
    representation(
        collabels="character",      # must have the same length as the 'data' slot
        direction="integer",
        data="data.frame",
        left.keys="character",
        right.keys="character",
        ifnotfound="list"
    ),
    prototype(
        direction=1L,               # left-to-right by default
        left.keys=as.character(NA),
        right.keys=as.character(NA),
        ifnotfound=list()           # empty list => raise an error on first key not found
    )
)

setMethod("initialize", "FlatBimap",
    function(.Object, collabels, direction, data, left.keys, right.keys)
    {
        if (missing(collabels)) {
            collabels <- rep(as.character(NA), ncol(data))
            collabels[1] <- "left"
            collabels[2] <- "right"
        }
        .Object@collabels <- collabels
        if (!missing(direction))
            .Object@direction <- .normalize.direction(direction)
        .Object@data <- data
        if (length(left.keys) != 1 || !is.na(left.keys))
            .Object@left.keys <- left.keys
        if (length(right.keys) != 1 || !is.na(right.keys))
            .Object@right.keys <- right.keys
        .Object
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Implementation of the Bimap interface.
###

setMethod("collabels", "FlatBimap",
    function(x) x@collabels)

setMethod("colnames", "FlatBimap",
    function(x, do.NULL=TRUE, prefix="col") colnames(x@data))

setMethod("direction", "FlatBimap",
    function(x) x@direction)
setReplaceMethod("direction", "FlatBimap",
    function(x, value)
    {
        x@direction <- .normalize.direction(value)
        x
    }
)

setMethod("left.keys", "FlatBimap",
    function(x) x@left.keys)
setMethod("right.keys", "FlatBimap",
    function(x) x@right.keys)

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

setReplaceMethod("left.keys", "FlatBimap",
    function(x, value)
    {
        if (!is.null(value)) {
            .checkKeys(value, left.keys(x), x@ifnotfound)
            x@left.keys <- value
        }
        x
    }
)

setReplaceMethod("right.keys", "FlatBimap",
    function(x, value)
    {
        if (!is.null(value)) {
            .checkKeys(value, right.keys(x), x@ifnotfound)
            x@right.keys <- value
        }
        x
    }
)

setMethod("subset", "FlatBimap",
    function(x, left.keys=NULL, right.keys=NULL, ...)
    {
        lii <- rii <- TRUE
        left.keys(x) <- left.keys
        right.keys(x) <- right.keys
        if (!is.null(left.keys))
            lii <- x@data[[1]] %in% left.keys
        if (!is.null(right.keys))
            rii <- x@data[[2]] %in% right.keys
        cn <- colnames(x@data)
        x@data <- x@data[lii & rii, ]
        colnames(x@data) <- cn
        x
    }
)

setMethod("left.mappedKeys", "FlatBimap",
    function(x) unique(x@data[[match("left", x@collabels)]]))

setMethod("right.mappedKeys", "FlatBimap",
    function(x) unique(x@data[[match("right", x@collabels)]]))

setMethod("nrow", "FlatBimap",
    function(x) nrow(x@data))

setMethod("links", "FlatBimap",
    function(x)
    {
        j <- c(match("left", x@collabels), match("right", x@collabels))
        unique(x@data[ , j])
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
        cat("\"", class(object), "\" object:\n\n", sep="")
        c2l <- data.frame(COLNAME=colnames(object), LABEL=object@collabels)
        show(c2l)
        direction <- names(.DIRECTION_STR2INT)[.DIRECTION_STR2INT == direction(object)]
        cat("\ndirection: ", direction, sep="")
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
### The "left.toList" and "right.toList" methods (Bimap interface).
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

setMethod("left.toList", "FlatBimap",
    function(x, keys=NULL)
    {
        if (!is.null(keys))
            x <- subset(x, left.keys=keys, right.keys=NULL)
        if (nrow(x@data) == 0)
            ann_list <- list()
        else
            ann_list <- split(x@data[ , -1], x@data[[1]])
        .alignAnnList(ann_list, left.keys(x))
    }
)

setMethod("right.toList", "FlatBimap",
    function(x, keys=NULL)
    {
        if (!is.null(keys))
            x <- subset(x, left.keys=NULL, right.keys=keys)
        if (nrow(x@data) == 0)
            ann_list <- list()
        else
            ann_list <- split(x@data[ , -2], x@data[[2]])
        .alignAnnList(ann_list, right.keys(x))
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
### TODO: make it work with from="right"!
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

