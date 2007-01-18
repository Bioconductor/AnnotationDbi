### The identical.uulists function is a replacement for setequal when
### used on "uulists".
### Conceptually, an "uulist" is an unordered, unamed list. 2 uulists
### are identical if they contain the same elements, possibly in
### different orders, with different outer names and with different number
### of occurences for any given element. 2 elements are considered to be
### the same if they are strictly identical (identical(e1, e2) is TRUE).
### setequal(x, y) checks that all elts in x are also in y and that all
### elts in y are also in x. It works for atomic vectors and lists.
### For example, setequal is TRUE for:
###     x <- list(a=2:3, 5)
###     y <- list(c(b='5'), 2:3, 2:3)
### But the comparison between elements ignore their types, their outer names
### ("a" in x) and their inner names ("b" in y)!
### With identical.uulists:
###     x <- list(a=2:3, 5, 5)      # setequal(x, y)  identical.uulists(x, y)
###     y <- list(5, 2:3, 2:3)      #      TRUE             TRUE
###     y <- list(c(b=5), 2:3, 2:3) #      TRUE             FALSE
###     y <- list(5, 2:3, c(2,3))   #      TRUE             FALSE
identical.uulists <- function(x, y)
{
    ## Quick and dirty implementation (inefficient)
    XeltsAreInY <- function(x, y)
    {
        all(sapply(x, function(ex) any(sapply(y, function(ey) identical(ex, ey)))))
    }
    XeltsAreInY(x, y) && XeltsAreInY(y, x)
}

### x and y must be collections.
### A collection can be a NULL, an unamed atomic vectors,
### a named atomic vectors, an unamed lists or a named list.
### Will ignore duplicated elements in the collections.
identical.collections <- function(x, y)
{
    if (typeof(x) != typeof(y))
        return(FALSE)
    nmx <- names(x)
    nmy <- names(y)
    if (is.null(nmx) != is.null(nmy))
        return(FALSE)
    if (is.null(nmx)) {
        if (is.list(x))
            return(identical.uulists(x, y))
        else
            return(setequal(x, y))
    }
    if (!setequal(nmx, nmy))
        return(FALSE)
    nm0 <- unique(nmx)
    for (nm in nm0) {
        x1 <- x[nmx %in% nm]
        y1 <- y[nmy %in% nm]
        if (is.list(x1)) {
            if (!identical.uulists(x1, y1))
                return(FALSE)
        } else {
            if (!setequal(x1, y1))
                return(FALSE)
        }
    }
    return(TRUE)
}

compareAnnDataIn2Pkgs <- function(pkgname1, pkgname2, direct_maps, reverse_maps,
                                  mapprefix="", probes=NULL, verbose=FALSE)
{
    require(pkgname1, character.only=TRUE) || stop(pkgname1, " package needed")
    require(pkgname2, character.only=TRUE) || stop(pkgname2, " package needed")
    getMap <- function(pkgname, mapname)
    {
        get(mapname, envir=as.environment(paste("package", pkgname, sep=":")), inherits=FALSE)
    }
    mismatch_summary <- list()
    for (mapshortname in c(direct_maps, reverse_maps)) {
        mapname <- paste(mapprefix, mapshortname, sep="")
        cat("*** Comparing ", mapname, " maps:\n", sep="")
        map1 <- getMap(pkgname1, mapname)
        cat("***   map1 is ", mapname, " from package ", pkgname1, "\n", sep="")
        map2 <- getMap(pkgname2, mapname)
        cat("***   map2 is ", mapname, " from package ", pkgname2, "\n", sep="")
        ## Compare lengths
        nkeys1 <- length(map1)
        cat("***   length(map1) = ", nkeys1, "\n", sep="")
        nkeys2 <- length(map2)
        cat("***   length(map2) = ", nkeys2, "\n", sep="")
        ## Compare submaps
        if (mapshortname %in% direct_maps && !is.null(probes)) {
            testedkeys <- probes
        } else {
            testedkeys <- intersect(ls(map1), ls(map2))
        }
        cat("***   nb of tested keys = ", length(testedkeys), "\n", sep="")
        submap1 <- mget(testedkeys, envir=map1)
        if (verbose) {
            cat("***   mget(testedkeys, envir=map1):\n", sep="")
            show(submap1)
        }
        submap2 <- mget(testedkeys, envir=map2)
        if (verbose) {
            cat("***   mget(testedkeys, envir=map2):\n", sep="")
            show(submap2)
        }
        OK <- sapply(testedkeys, function(key) identical.collections(submap1[[key]], submap2[[key]]))
        nmis <- sum(!OK)
        cat("***   nb of mismatches = ", nmis, "\n", sep="")
        mismatch_summary[[mapname]] <- nmis
    }
    mismatch_summary <- unlist(mismatch_summary)
    cat("SUMMARY OF MISMATCHES:\n")
    show(mismatch_summary)
    cat("  - nb of map comparisons = ", length(mismatch_summary), "\n", sep="")
    cat("  - nb of PASSED maps = ", sum(mismatch_summary == 0), "\n", sep="")
    cat("  - nb of FAILED maps = ", sum(mismatch_summary != 0), "\n", sep="")
    mismatch_summary
}

