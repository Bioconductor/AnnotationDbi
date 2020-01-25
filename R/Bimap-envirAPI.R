### =========================================================================
### Environment-like API for AnnDbBimap objects
### -------------------------------------------
###
### This file defines an environment-like API for the AnnDbBimap objects (ls,
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
### The "ls" new generic.
###
.ls <- function(name, pattern, sorted){
        keys <- keys(name)
        if (!missing(pattern))
            keys <- keys[grep(pattern, keys)]
        if (sorted)
            keys <- sort(keys)
        keys
}

setMethod("ls", signature(name="Bimap"),
    function(name, pos=-1L, envir=as.environment(pos), all.names=FALSE,
             pattern, sorted=TRUE){
        if (!missing(pos))
          warning("ignoring 'pos' argument")
        if (!missing(envir))
          warning("ignoring 'envir' argument")
        if (!missing(all.names))
          warning("ignoring 'all.names' argument")
        .ls(name, pattern, sorted)
    }
)




### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### "mget" methods.
###
### 'mget(x, map)' vs 'as.list(map)':
###   1. mget checks its 'x' arg. and gracefully fails if it's not of
###      the expected type (i.e. NULL or NA-free character vector),
###   2. 'mget(keys(map), map)' is identical to 'as.list(map)'.
###      Note that for a real "environment", 'as.list(envir)' is not identical
###      to 'mget(ls(envir), envir)': the 2 lists have the same elements but
###      not necesarily in the same order!

setMethod("mget", signature(x="ANY", envir="Bimap"),
    function(x, envir, mode, ifnotfound, inherits)
    {
        if (missing(ifnotfound)) {
            ifnotfound <- list()
        } else {
            if (!is.vector(ifnotfound) || length(ifnotfound) != 1 || !is.na(ifnotfound))
                stop("only NA is currently supported for 'ifnotfound'")
            ifnotfound <- as.list(ifnotfound)
        }
        keys0 <- keys(envir)
        envir@ifnotfound <- ifnotfound
        keys(envir) <- x
        ans <- as.list(envir)
        if (length(ifnotfound) == 0)
            return(ans)
        ans[!(names(ans) %in% keys0)] <- ifnotfound
        ans
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "eapply" new generic.
###

setMethod("eapply", signature(env="Bimap"),
    function(env, FUN, ..., all.names =  FALSE, USE.NAMES = TRUE)
    {
        lapply(as.list(env), FUN, ...)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### "get" methods.
###
### We want this:
###   get("1027_at", envir=hgu95av2GO)
### and this
###   get("1027_at", hgu95av2GO)
### to work so we need to dispatch on the 'pos' arg too.
###

.get <- function(what, map) mget(what[1], map)[[1]]

setMethod("get", signature(x="ANY", pos="ANY", envir="Bimap"),
    function(x, pos, envir, mode, inherits)
    {
        .get(x, envir)
    }
)

setMethod("get", signature(x="ANY", pos="Bimap", envir="missing"),
    function(x, pos, envir, mode, inherits)
    {
        .get(x, pos)
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
###
### Like the original "exists" function for environment objects, our "exists"
### methods only use the first element of 'x'. But unlike the original, our
### methods will raise an error if this first element is a character NA.
###

.exists <- function(x, map)
{
    if (!is.character(x) || length(x) == 0 || x[1] %in% c(NA, ""))
        stop("invalid first argument")
    x[1] %in% keys(map)
}

setMethod("exists", signature(x="ANY", where="ANY", envir="Bimap"),
    function(x, where, envir, frame, mode, inherits)
    {
        .exists(x, envir)
    }
)

setMethod("exists", signature(x="ANY", where="Bimap", envir="missing"),
    function(x, where, envir, frame, mode, inherits)
    {
        .exists(x, where)
    }
)




### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "[[" and "$" generics.
###

.doubleBracketSub <- function(x, i, j, ...)
{
      subscripts <- list(...)
      if (!missing(i)) subscripts$i <- i
      if (!missing(j)) subscripts$j <- j
      # At this point, 'subscripts' should be guaranteed
      # to be of length <= 1
      if (length(subscripts) == 0) stop("no index specified")
      i <- subscripts[[1]]
      if (length(i) < 1)
          stop("attempt to select less than one element")
      if (length(i) > 1)
          stop("attempt to select more than one element")
      if (!is.character(i) || is.na(i))
          stop("wrong argument for subsetting an object of class ",
               sQuote(class(x)))
      val <- mget(i, envir=x, ifnotfound=NA)[[1]]
      # We could just do '!(i %in% keys(x))' but checking 'identical(val, NA)'
      # first is very cheap and will fail most of the time thus preventing
      # evaluation of '!(i %in% keys(x))' which is a lot more expensive.
      if (identical(val, NA) && !(i %in% keys(x)))
          val <- NULL
      val
}

setMethod("[[", "Bimap",
    function(x, i, j, ...){
        # 'x' is guaranteed to be a "Bimap" object (if it's not, then the
        # method dispatch algo will not call this method in the first place),
        # so nargs() is guaranteed to be >= 1
        if (nargs() >= 3) stop("too many subscripts")
        .doubleBracketSub(x, i, j, ...)
    }
)




setMethod("$", "Bimap", function(x, name) x[[name]])



### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### "contents" methods.
###
### The "contents" method for environment objects is defined in Biobase.
###

setMethod("contents", "Bimap",
    function(object, all.names)
    {
        if (!missing(all.names))
            warning("ignoring 'all.names' argument")
        as.list(object)
    }
)



### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "sample" new generic.
###

.sample <- function(x, size, replace, prob)
{
    keys <- ls(x)
    keys <- keys[sample(length(keys), size, replace, prob)]
    mget(keys, x)
}
    
setMethod("sample", "Bimap",
    function(x, size, replace=FALSE, prob=NULL)
        .sample(x, size, replace, prob)
)

setMethod("sample", "environment",
    function(x, size, replace=FALSE, prob=NULL)
        .sample(x, size, replace, prob)
)


