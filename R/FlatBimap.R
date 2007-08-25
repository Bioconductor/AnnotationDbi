### =========================================================================
### The bimap concept, the BimapAPI0 interface and the FlatBimap objects
### --------------------------------------------------------------------
###
### Example of a bimap M:
###
###   4 objects on the left (left keys): a, b, c, d
###   2 objects on the right (right keys): A, B, C
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
### A FlatBimap object is a bimap whose data (left keys, right keys and
### links) are stored in memory (in a data frame for the links).
### A AnnDbBimap object is a bimap whose data are stored in a data base.
### Conceptually, a FlatBimap and a AnnDbBimap object are the same (only
### their internal representation differ) so it's natural to try to define
### a set of methods that make sense for both (so they can be manipulated
### in a similar way).
### However, there is an important asymetry between these two classes:
###   A AnnDbBimap object can be converted into a FlatBimap object
###   but a FlatBimap object can't be converted into an AnnDbBimap
###   object (well, in theory maybe it could be, but for now the data bases
###   we use to store the data of the AnnDbBimap objects are treated as
###   read-only). This conversion from AnnDbBimap to FlatBimap is performed
###   by the "flatten" generic function (with methods for AnnDbBimap objects
###   only).
### The "BimapAPI0" interface is the common interface between FlatBimap and
### AnnDbBimap objects. The "flatten" generic (with methods for AnnDbBimap
### objects only) and the "subset" generic (with methods for AnnDbBimap and
### FlatBimap objects) play the following central roles:
###   1. When used with no extra argument, flatten(x) converts AnnDbBimap
###      object x into FlatBimap object y with no loss of information.
###   2. If x is an AnnDbBimap object and f a BimapAPI0 generic, then f is
###      expected "to do the same thing" for AnnDbBimap and FlatBimap objects.
###      More precisely, this means that for any AnnDbBimap object x, we
###      expect f(x) to be identical to f(flatten(x)). We call this property
###      Property0.
### The checkProperty0() function (AnnDbPkg-checker.R file) checks that
### Property0 is satisfied on all the AnnDbBimap objects defined in a given
### package.
### Both AnnDbBimap and FlatBimap objects have a read-only semantic: the user
### can subset them but cannot change their data.
### More about "subset" coming soon...
###


### KEEP THIS IN SYNC WITH THE STATE OF AFFAIRS! Only methods of the first and
### second group go here.
BimapAPI0_methods <- c(
    ## GROUP 1: 8 methods that _must_ be defined for FlatBimap objects
    ## _and_ AnnDbBimap objects
    "collabels",
    "colnames",
    "direction",
    "direction<-",
    "left.keys", "right.keys",
    "left.keys<-", "right.keys<-",
    "subset",
    "left.mappedKeys", "right.mappedKeys",
    "nrow",
    "links",
    "left.toList", "right.toList",
    ## GROUP 2: Methods for which a default is provided (in this file) but
    ## some of them are redefined for AnnDbBimap objects to obtain better
    ## performance
    "left.length", "right.length",
    "count.left.mappedKeys", "count.right.mappedKeys",
    "count.links",
    "left.colname", "right.colname",
    ## GROUP 3: Directed methods (i.e. what they return depends on the
    ## direction of the map). All what they do is to dispatch on the
    ## corresponding undirected method according to the value of direction(x)
    "keys",
    "length",
    "mappedKeys",
    "count.mappedKeys",
    "toList"
)

### A virtual class with no slot (a kind of equivalent to what is called an
### "interface" in Java)
setClass("BimapAPI0", representation("VIRTUAL"))

setMethod("left.length", "BimapAPI0",
    function(x) length(left.keys(x)))
setMethod("right.length", "BimapAPI0",
    function(x) length(right.keys(x)))

setMethod("count.left.mappedKeys", "BimapAPI0",
    function(x) length(left.mappedKeys(x)))
setMethod("count.right.mappedKeys", "BimapAPI0",
    function(x) length(right.mappedKeys(x)))

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
### For now, tags and attributes are all mixed together which is bad and will
### cause problems when reversing some maps or when plugging maps together.
### We need to make a clear distinction between tags and attributes but this
### will require to change the current L2Rbrick class and make things more
### complicated...
setMethod("tags.colpos", "BimapAPI0",
    function(x) seq_len(ncol(x))[-c(from.colpos(x, 1), from.colpos(x, -1))])

setMethod("from.keys", "BimapAPI0",
    function(x, direction) if (direction == 1) left.keys(x) else right.keys(x))
setMethod("to.keys", "BimapAPI0",
    function(x, direction) from.keys(x, - direction))

setMethod("dim", "BimapAPI0",
    function(x) c(nrow(x), ncol(x)))

### Directed methods

.DIRECTION_STR2INT <- c("left-to-right"=1L, "right-to-left"=-1L, "undirected"=0L)

.normalize.direction <- function(direction)
{
    if ((!is.numeric(direction) && !is.character(direction))
     || length(direction) != 1 || is.na(direction))
        stop("'direction' must be a single (non-NA) integer or string")
    if (is.character(direction)) {
        direction <- match.arg(tolower(direction), names(.DIRECTION_STR2INT))
        return(do.call("switch", c(EXPR=direction, as.list(.DIRECTION_STR2INT))))
    }
    if (!(direction %in% .DIRECTION_STR2INT))
        stop("when a numeric value, 'direction' should be one of 1, -1 or 0")
    as.integer(direction)
}

setMethod("keys", "BimapAPI0",
    function(x)
        switch(as.character(direction(x)),
                "1"=left.keys(x),
               "-1"=right.keys(x),
                    stop("keys() is undefined for an undirected bimap"))
)
setMethod("length", "BimapAPI0",
    function(x)
        switch(as.character(direction(x)),
                "1"=left.length(x),
               "-1"=right.length(x),
                    stop("length() is undefined for an undirected bimap"))
)
setMethod("mappedKeys", "BimapAPI0",
    function(x)
        switch(as.character(direction(x)),
                "1"=left.mappedKeys(x),
               "-1"=right.mappedKeys(x),
                    stop("mappedKeys() is undefined for an undirected bimap"))
)
setMethod("count.mappedKeys", "BimapAPI0",
    function(x)
        switch(as.character(direction(x)),
                "1"=count.left.mappedKeys(x),
               "-1"=count.right.mappedKeys(x),
                    stop("count.mappedKeys() is undefined for an undirected bimap"))
)
setMethod("toList", "BimapAPI0",
    function(x, keys=NULL)
        switch(as.character(direction(x)),
                "1"=left.toList(x, keys),
               "-1"=right.toList(x, keys),
                    stop("toList() is undefined for an undirected bimap"))
)



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
### Required methods of the BimapAPI0 interface.
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
    function(x, left.keys=NULL, right.keys=NULL)
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
### The "left.toList" and "right.toList" methods.
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

