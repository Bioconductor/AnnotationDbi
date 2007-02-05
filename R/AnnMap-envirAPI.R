### =========================================================================
### Environment-like API for AnnMap objects
### ---------------------------------------
###
### This file defines an environment-like API for the AnnMap objects (ls,
### mget, eapply, get, exists, [[ and $) for backward compatibility with the
### classic envir-based annotation maps.
###
### This environment-like API is defined on top of the low-level API for
### AnnMap objects defined in the AnnMap-objects.R file (refer to this file
### too for the definition of the "AnnMap" class and subclasses).
###
### Note that the length and lapply methods are not redefined here since they
### are considered to belong to the low-level API.
###
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "ls" new generic.
###

setMethod("ls", signature(name="AnnMap"),
    function(name, pos, envir, all.names, pattern) names(name)
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
### NB: .checkNamesAreStrings() and .checkNamesExist() are defined in
###     the AnnMap-objects.R file.

setMethod("mget", signature(envir="AnnMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        .checkNamesAreStrings(x)
        .checkNamesExist(x, names(envir))
        as.list(envir, names=x)
    }
)

setMethod("mget", signature(envir="ReverseAnnMap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        .checkNamesAreStrings(x)
        as.list(envir, names=x)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "eapply" new generic.
###

setMethod("eapply", signature(env="AnnMap"),
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

setMethod("get", signature(envir="AnnMap"),
    function(x, pos, envir, mode, inherits)
    {
        mget(x[1], envir)[[1]]
    }
)

setMethod("get", signature(pos="AnnMap", envir="missing"),
    function(x, pos, envir, mode, inherits)
    {
        get(x, envir=pos)
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

setMethod("exists", signature(envir="AnnMap"),
    function(x, where, envir, frame, mode, inherits)
    {
        x %in% names(envir)
    }
)

setMethod("exists", signature(where="AnnMap", envir="missing"),
    function(x, where, envir, frame, mode, inherits)
    {
        exists(x, envir=where)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "[[" and "$" generics.
###

setMethod("[[", "AnnMap",
    function(x, i, j, ...)
    {
        # 'x' is guaranteed to be a "AnnMap" object (if it's not, then the
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

setMethod("$", "AnnMap", function(x, name) x[[name]])

