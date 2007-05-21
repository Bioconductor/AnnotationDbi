### =========================================================================
### The bimap concept, the BimapAPI0 interface and the FlatBimap objects
### --------------------------------------------------------------------
###
### Example of a bimap M:
###
###   4 objects on the left (left names): a, b, c, d
###   2 objects on the right (right names): A, B, C
###
###   Links:
###      a <--> A
###      a <--> B
###      b <--> A
###      d <--> C
###
### The "flat" representation of M looks like a data frame:
###
###   left  right
###      a      A 
###      a      B
###      b      A
###      d      C
###
### If in addition the right objects have 1 multi-valued attribute, for
### example, a numeric vector:
###   A <-- c(1.2, 0.9)
###   B <-- character(0)
###   C <-- -1:1
###
### then the "flat" representation of M becomes:
###
###   left  right  Rattrib
###      a      A      1.2
###      a      A      0.9
###      a      B       NA
###      b      A      1.2
###      b      A      0.9
###      d      C       -1
###      d      C        0
###      d      C        1
###
### Note that now the number of rows is greater than the number of links!
###
### -------------------------------------------------------------------------



### =========================================================================
### The "BimapAPI0" interface
### -------------------------
###
### This is the common interface to FlatBimap and to AnnDbMap objects.
### The "flatten" method defined in AnnDbObj-lowAPI.R plays a central role:
### it transforms a AnnDbMap object into a FlatBimap.
###
### The BimapAPI0 interface must always satisfy Property0:
###   if x is a AnnDbMap object, f1 a BimapAPI0 method for FlatBimap objects
###   and f2 the corresponding method for AnnDbMap objects then f2(x) is
###   expected to return _exactly_ the same thing as f1(flatten(x)).
### 
### The checkProperty0() function (AnnDbPkg-checker.R file) checks that
### Property0 is satisfied on all the AnnDbMap objects of a given package.
###


### KEEP THIS IN SYNC WITH THE STATE OF AFFAIRS! Only methods of the first and
### second group go here.
BimapAPI0_methods <- c(
    ## GROUP 1: 8 methods that _must_ be defined for the FlatBimap objects and
    ## the AnnDbMap objects (or extensions)
    "collabels",
    "colnames",
    "left.names", "right.names",
    "left.mappedNames", "right.mappedNames",
    "nrow",
    "links",
    ## GROUP 2: Methods for which a default is provided (below) but that are
    ## redefined for the AnnDbMap objects to obtain better performance
    "left.length", "right.length",
    "count.left.mappedNames", "count.right.mappedNames",
    "count.links",
    "left.colname", "right.colname"
)
 
setClass("BimapAPI0", representation("VIRTUAL"))


setMethod("left.length", "BimapAPI0",
    function(x) length(left.names(x)))
setMethod("right.length", "BimapAPI0",
    function(x) length(right.names(x)))

setMethod("count.left.mappedNames", "BimapAPI0",
    function(x) length(left.mappedNames(x)))
setMethod("count.right.mappedNames", "BimapAPI0",
    function(x) length(right.mappedNames(x)))

setMethod("count.links", "BimapAPI0",
    function(x) nrow(links(x)))

setMethod("ncol", "BimapAPI0",
    function(x) length(colnames(x)))

### left-to-right: direction =  1
### right-to-left: direction = -1
setMethod("from.colpos", "BimapAPI0",
    function(x, direction)
    {
        if (direction == 1) side = "left" else side = "right"
        match(side, collabels(x))
    }
)
setMethod("to.colpos", "BimapAPI0",
    function(x, direction) from.colpos(x, - direction))

setMethod("left.colname", "BimapAPI0",
    function(x) colnames(x)[from.colpos(x,  1)])
setMethod("right.colname", "BimapAPI0",
    function(x) colnames(x)[from.colpos(x, -1)])

### FIXME
### For now, tags and attributes are all mixed together which bad and will
### cause problems when reversing some maps or when plugging maps together.
### We need to make the distinction between tags and attributes but this
### will require to change the current L2Rbrick class and make things more
### complicated...
setMethod("tags.colpos", "BimapAPI0",
    function(x) seq_len(ncol(x))[-c(from.colpos(x, 1), from.colpos(x, -1))])

setMethod("from.names", "BimapAPI0",
    function(x, direction) if (direction == 1) left.names(x) else right.names(x))
setMethod("to.names", "BimapAPI0",
    function(x, direction) from.names(x, - direction))

setMethod("dim", "BimapAPI0",
    function(x) c(nrow(x), ncol(x)))



### =========================================================================
### The "FlatBimap" class
### ---------------------
###


### Possible col labels are: "left", "right", "tag", "Lattrib", "Rattrib"
### There must be exactly 1 "left" and 1 "right" col.
### There can be 0 or 1 "tag" col.
### There can be any number of "Lattrib", "Rattrib" or unlabelled (NA) cols.
setClass("FlatBimap",
    contains="BimapAPI0",
    representation(
        collabels="character",   # must have the same length as the 'data' slot
        data="data.frame",
        left.names="character",
        right.names="character"
    ),
    prototype(
        left.names=as.character(NA),
        right.names=as.character(NA)
    )
)

setMethod("initialize", "FlatBimap",
    function(.Object, collabels, data, left.names, right.names)
    {
        if (missing(collabels)) {
            collabels <- rep(NA, ncol(data))
            collabels[1] <- "left"
            collabels[2] <- "right"
        }
        .Object@collabels <- collabels
        .Object@data <- data
        if (length(left.names) != 1 || !is.na(left.names))
            .Object@left.names <- left.names
        if (length(right.names) != 1 || !is.na(right.names))
            .Object@right.names <- right.names
        .Object
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Required methods of the BimapAPI0 interface.
###

setMethod("collabels", "FlatBimap",
    function(x) x@collabels)

setMethod("colnames", "FlatBimap",
    function(x, do.NULL=TRUE, prefix="col") colnames(x@data))

setMethod("left.names", "FlatBimap",
    function(x) x@left.names)
setMethod("right.names", "FlatBimap",
    function(x) x@right.names)

setMethod("left.mappedNames", "FlatBimap",
    function(x) unique(x@data[[match("left", x@collabels)]]))
setMethod("right.mappedNames", "FlatBimap",
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
### 2 functions for folding and formatting FlatBimap objects.
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
### 'direction': 1 for left-to-right and 2 for right-to-left
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
    names <- from.names(x, direction)
    ans <- as.list(rep(as.character(NA), length(names)))
    names(ans) <- names
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
    mapped_names <- names(slicing_one[[1]])
    ii <- match(mapped_names, names)
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

