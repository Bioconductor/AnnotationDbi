### =========================================================================
### Formatting a Bimap as a list or character vector
### ------------------------------------------------
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "from.colpos" and "to.colpos" methods.
###
### NOTE: Not sure these methods are really needed. Need to check. At best
### they need to be revisited and probably reworked. They are currently not
### exported.
###

setMethod("from.colpos", "Bimap",
    function(x)
    {
        if (direction(x) == 1) side = "Lkeyname" else side = "Rkeyname"
        match(side, colmetanames(x))
    }
)

setMethod("to.colpos", "Bimap",
    function(x)
        from.colpos(revmap(x)))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Convenience functions used in this file only (not exported)
###

### Re-order and format list 'x' as follow:
###   > x <- list(aa=1, b=2:-1, c=3)
###   > names <- c("a", "c", "d")
###   > .formatList(x, names)
### ... must return 'list(a=NA, c=3, d=NA)'
### Note that the returned list must have exactly the names in 'names' (in the
### same order).
.formatList <- function(x, names, replace.single=NULL, replace.multiple=NULL)
{
    if (length(x) != 0)
        x <- l2e(x)
    doReplaceSingle <- length(replace.single) != 0L
    doReplaceMultiple <- length(replace.multiple) != 0L
    formatVal <- function(key)
    {
        val <- x[[key]]
        lval <- length(val)
        if (lval == 1L) {
            if (doReplaceMultiple)
              return(replace.multiple)
        } else if (lval > 1L) {
            if (doReplaceMultiple)
              return(replace.multiple)
        } else {
            # lval == 0
            val <- NA
        }
        val
    }
    names(names) <- names
    lapply(names, formatVal)
}

### The ".toListOfLists" function.
###
### This function converts FlatBimap object 'x' to a list of right objects
### if is 'x' a mono-valued (many-to-one) map, or to a list of lists of right
### objects if 'x' is a multi-valued map.
### By default (if arg 'FUN' is omitted), each right object itself is stored
### in a named list.
###
### Back to the bimap B1 given in the Bimap.R file: 
###
###   A natural way of representing objects A, B, C in R is to use lists:
###     - object A: list("A", c(1.2, 0.9))
###     - object B: list("B", character(0))
###     - object C: list("C", -1:1)
###
###   And a natural way for representing map B1 in R is to use a named list
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
###   .toListOfLists() does this conversion from "flat" to "list".
###
### Arguments:
###
### 'mode': 3 modes are supported:
###         mode 1: mono-valued map
###         mode 2: multi-valued map containing 1 right object per row (i.e.
###           all right objects are stored in a single row, this can only be
###           known in advance if we know that their tags are mono-valued)
###         mode 3: multi-valued map where more than 1 row can be needed to
###           store a given right object (this can happen only if at least
###           one of the tags are multi-valued)
###         In our example above, bimap B1 is of mode 3.
###
### 'FUN': a formatting function applied on-the-fly on each object during
###        the "folding" process. If 'FUN' is omitted then
###        'function(...) list(...)' is used so no information is lost and
###        the formatting can always be done later with:
###
###        > y <- .toListOfLists(x)
###        > y <- lapply(y, function(val)
###                           lapply(val, function(x) do.call(myfun, x)))
###
###        The result will be the same as with just:
###
###        > y <- .toListOfLists(x, FUN=myfun)
###
###        but the latter will be faster.
###
### TODO: make it work for "L <- R" maps
###
.toListOfLists <- function(x, mode, FUN)
{
    keys <- keys(x)
    ans <- as.list(rep(as.character(NA), length(keys)))
    names(ans) <- keys
    if (nrow(x) == 0)
        return(ans)
    if (direction(x) == -1)
        stop("right-to-left folding not yet ready, sorry")
    if (missing(FUN))
        FUN <- function(...) list(...)
    ## First slicing (top level)
    slicer1 <- from.colpos(x)
    slicer2 <- to.colpos(x)
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "fromCol", "toCol" and "tagCol" methods.
###
### Note that we must use 'x@data[[2]]' instead of 'x@data[[Rkeyname(x)]]'
### because 'x' colnames are not necessarily unique e.g.:
###   > head(flatten(subset(GOBPPARENTS, "GO:0000001")), 2)
###          go_id      go_id Evidence
###   1 GO:0000001 GO:0048308      isa
###   2 GO:0000001 GO:0048311      isa
###
### TODO: Make these methods use colmetanames(x) to find the positions of the
### left, right and tag cols in x@data instead of hardcoding this with
### x@data[[1]], x@data[[2]] and x@data[[3]].
###

setMethod("fromCol", "Bimap",
    function(x)
    {
        if (direction(x) == 1)
            x@data[[1]]
        else
            x@data[[2]]
    }
)

setMethod("toCol", "Bimap",
    function(x)
    {
        if (direction(x) == 1)
            x@data[[2]]
        else
            x@data[[1]]
    }
)

setMethod("tagCol", "Bimap",
    function(x)
    {
        if (is.na(tagname(x)))
            return(NULL)
        x@data[[3]]
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "as.list" methods.
###

setMethod("as.list", "Bimap",
    function(x, ...)
    {
        Rattribnames(x) <- NULL
        toCol <- toCol(x)
        names(toCol) <- tagCol(x)
        y <- split(toCol, fromCol(x))
        .formatList(y, keys(x))
    }
)

setMethod("as.list", "AnnDbBimap",
    function(x, ...)
    {
        Rattribnames(x) <- NULL
        y <- flatten(x, fromKeys.only=TRUE)
        as.list(y)
    }
)

### Needed to deal with the PFAM and PROSITE maps.
### Their colnames are:
###   PFAM: "probe_id", "ipi_id", "PfamId"
###   PROSITE: "probe_id", "ipi_id", "PrositeId"
setMethod("as.list", "IpiAnnDbMap",
    function(x, ...)
    {
        y <- flatten(x, fromKeys.only=TRUE)
        toCol <- y@data[[3]]
        names(toCol) <- toCol(y)
        z <- split(toCol, fromCol(y))
        .formatList(z, keys(y))
    }
)

setMethod("as.list", "AgiAnnDbMap",
    function(x, ...)
    {
        y <- flatten(x, fromKeys.only=TRUE)
        z <- split(toCol(y), fromCol(y))
        .formatList(z, keys(y), x@replace.single, x@replace.multiple)
    }
)

setMethod("as.list", "GoAnnDbBimap",
    function(x, ...)
    {
        y <- flatten(x, fromKeys.only=TRUE)
        if (direction(y) == -1)
            return(as.list(y))
        keys <- keys(y)
        ann_list <- as.list(rep(as.character(NA), length(keys)))
        names(ann_list) <- keys
        if (nrow(y@data) != 0) {
            makeGOList <- function(GOIDs, Evidences, Ontologies)
            {
                mapply(function(gid, evi, ont)
                       list(GOID=gid, Evidence=evi, Ontology=ont),
                       GOIDs, Evidences, Ontologies, SIMPLIFY=FALSE)
            }
            fromCol <- fromCol(y)
            GOIDs <- split(y@data[["go_id"]], fromCol)
            Evidences <- split(y@data[["Evidence"]], fromCol)
            Ontologies <- split(y@data[["Ontology"]], fromCol)
            ## The 'GOIDs', 'Evidences' and 'Ontologies' lists have the same
            ## names in the same order.
            mapped_keys <- names(GOIDs)
            ii <- match(mapped_keys, keys, nomatch=0L)
            for (i1 in seq_len(length(ii))) {
                i2 <- ii[i1]
                ## 'mapped_keys' should always be a subset of 'keys'
                ## hence 'i2 == 0L' should never happen. So maybe we should
                ## raise something like "AnnotationDbi internal error" instead
                ## of just ignoring this...
                if (i2 == 0L) next
                ann_list[[i2]] <- makeGOList(GOIDs[[i1]], Evidences[[i1]],
                                                 Ontologies[[i1]])
            }
        }
        ann_list
    }
)

### Formatting the right objects with 'makeGONode' instead of just using the
### default formatting provided by .toListOfLists() (the default is to create
### a list for each object) makes things _much_ slower:
###  > x <- flatten(GOTERM)
###  > system.time(y <- .toListOfLists(x, "Lkeyname", mode=1))
###     user  system elapsed
###    1.888   0.016   1.905
###  > system.time(y <- .toListOfLists(x, "Lkeyname", mode=1, FUN=makeGONode))
###     user  system elapsed
###   20.893   0.072  21.066
### Why is the S4 initialization mechanism so slow?
setMethod("as.list", "GOTermsAnnDbBimap",
    function(x, ...)
    {
        y <- flatten(x, fromKeys.only=TRUE)
        makeGONode <- function(go_id, Term, Ontology, Definition, ...)
        {
            new("GOTerms", GOID=go_id[1],
                           Term=Term[1],
                           Ontology=Ontology[1],
                           Definition=Definition[1],
                           ...)
        }
        .toListOfLists(y, mode=1, makeGONode)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "toLList", "toRList" and "toList" methods.
###
### NOTE: These methods are not exported yet (will be soon).
### TODO: Define these methods for FlatBimap objects.
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
    function(x)
    {
        if (nrow(x@data) == 0)
            ann_list <- list()
        else
            ann_list <- split(x@data[ , -1], x@data[[1]])
        .alignAnnList(ann_list, Lkeys(x))
    }
)

setMethod("toLList", "AnnDbBimap",
    function(x)
    {
        toLList(flatten(x, fromKeys.only=TRUE))
    }
)

setMethod("toLList", "AnnDbMap",
    function(x)
    {
        y <- flatten(x, fromKeys.only=TRUE)
        if (length(x@rightColType) == 1
         && typeof(y@data[[2]]) != x@rightColType) {
                converter <- get(paste("as.", x@rightColType, sep=""))
                y@data[[2]] <- converter(y@data[[2]])
        }
        toLList(y)
    }
)

setMethod("toRList", "FlatBimap",
    function(x)
    {
        if (nrow(x@data) == 0)
            ann_list <- list()
        else
            ann_list <- split(x@data[ , -2], x@data[[2]])
        .alignAnnList(ann_list, Rkeys(x))
    }
)

setMethod("toRList", "AnnDbBimap",
    function(x)
    {
        toRList(flatten(x, fromKeys.only=TRUE))
    }
)

setMethod("toRList", "AnnDbMap",
    function(x)
    {
        stop("toRList() is not supported for an \"", class(x), "\" object")
    }
)

setMethod("toList", "Bimap",
    function(x)
        switch(as.character(direction(x)),
                "1"=toLList(x),
               "-1"=toRList(x),
                    stop("toList() is undefined for an undirected bimap"))
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "as.character" methods.
###
### Only defined for untagged AnnDbBimap objects for now.
###
### TODO: Extend to FlatBimap objects.
###

### R doesn't let me add a 'keys' arg here:
###  Error in rematchDefinition(definition, fdef, mnames, fnames, signature) :
###          methods can add arguments to the generic only if '...' is an argument to the generic
setMethod("as.character", "AnnDbBimap",
    function(x)
    {
        if (!is.na(tagname(x)))
            stop("AnnDbBimap object with tags cannot be coerced to a character vector")
        Rattribnames(x) <- NULL
        y <- flatten(x, fromKeys.only=TRUE)
        ans <- toCol(y)
        if (!is.character(ans))
            ans <- as.character(ans)
        names(ans) <- fromCol(y)
        if (any(duplicated(names(ans))))
            warning("returned vector has duplicated names")
        ans
    }
)

