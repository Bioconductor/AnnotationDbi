### =========================================================================
### AnnDbPkg checking functions
### ---------------------------
###
### Some functions for checking SQLite-based ann packages.
###
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Helper functions.
###

### The identical.uulists function is a replacement for setequal when
### used on "uulists".
### Conceptually, a "uulist" is an unordered, unamed list. 2 uulists
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
    ## If x and y have different types _and_ at least one of them is not a
    ## single NA then we return FALSE.
    if (typeof(x) != typeof(y)
     && (length(x) != 1 || !is.na(x) || length(y) != 1 || !is.na(y)))
        return(FALSE)
    ## Now we can assume that x and y have the same type _except_ if
    ## they are both NAs (and in this case we can't just return TRUE
    ## because we need to compare the names too).
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "compareAnnDataIn2Pkgs" function.
###
### "compareAnnDataIn2Pkgs" compares the annotation data between 2 packages.
### We use it to validate our SQLite-based ann packages by comparing each of
### them to its envir-based sibling package.
###

compareAnnDataIn2Pkgs <- function(pkgname1, pkgname2, direct_maps, reverse_maps,
                                  prefix="", probes=NULL, verbose=FALSE)
{
    require(pkgname1, character.only=TRUE) || stop(pkgname1, " package needed")
    require(pkgname2, character.only=TRUE) || stop(pkgname2, " package needed")
    getMap <- function(pkgname, mapname)
    {
        get(mapname, envir=as.environment(paste("package", pkgname, sep=":")), inherits=FALSE)
    }
    mismatch_summary <- list()
    for (mapshortname in c(direct_maps, reverse_maps)) {
        mapname <- paste(prefix, mapshortname, sep="")
        cat("*** Comparing ", mapname, " maps:\n", sep="")
        map1 <- getMap(pkgname1, mapname)
        cat("***   map1 is ", mapname, " from package ", pkgname1, "\n", sep="")
        map2 <- getMap(pkgname2, mapname)
        cat("***   map2 is ", mapname, " from package ", pkgname2, "\n", sep="")
        ## Compare lengths
        nnames1 <- length(map1)
        cat("***   length(map1) = ", nnames1, "\n", sep="")
        nnames2 <- length(map2)
        cat("***   length(map2) = ", nnames2, "\n", sep="")
        ## Compare submaps
        if (mapshortname %in% direct_maps && !is.null(probes)) {
            tested_names <- probes
        } else {
            tested_names <- intersect(ls(map1), ls(map2))
        }
        cat("***   nb of tested names = ", length(tested_names), "\n", sep="")
        submap1 <- mget(tested_names, envir=map1)
        if (verbose) {
            cat("***   mget(tested_names, envir=map1):\n", sep="")
            show(submap1)
        }
        submap2 <- mget(tested_names, envir=map2)
        if (verbose) {
            cat("***   mget(tested_names, envir=map2):\n", sep="")
            show(submap2)
        }
        OK <- sapply(tested_names,
                     function(name) identical.collections(submap1[[name]], submap2[[name]]))
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "checkMAPCOUNTS" function.

checkMAPCOUNTS <- function(pkgname, prefix)
{
    require(pkgname, character.only=TRUE) || stop(pkgname, " package needed")
    getMap <- function(mapname)
    {
        get(mapname, envir=asNamespace(pkgname))
    }
    MAPCOUNTS <- getMap(paste(prefix, "MAPCOUNTS", sep=""))
    for (mapname in names(MAPCOUNTS)) {
        cat("Counting mapped names for map ", mapname, ":\n", sep="")
        map <- getMap(mapname)
        map_length <- length(map)
        cat("  - length(map) = ", map_length, "\n", sep="")
        count0 <- MAPCOUNTS[mapname]
        cat("  - MAPCOUNTS[\"", mapname, "\"] = ", count0, "\n", sep="")
        if (is.numeric(map)) # to deal with the CHRLENGTHS case
            t1 <- system.time(count1 <- sum(!is.na(map)))
        else
            t1 <- system.time(count1 <- count.mapped.names(map))
        cat("  - count1 = ", count1, " (", t1[3], " s)\n", sep="")
        if (count1 != count0)
            stop("count0 and count1 differ")
        if (is.numeric(map))
            next
        t2 <- system.time(count2 <- sum(sapply(toList(map), function(x) length(x)!=1 || !is.na(x))))
        cat("  - count2 = ", count2, " (", t2[3], " s)\n", sep="")
        if (count2 != count1)
            stop("count1 and count2 differ")
    }
}

