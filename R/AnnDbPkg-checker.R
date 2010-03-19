### =========================================================================
### AnnDbPkg checking functions
### ---------------------------
###
### Some functions for checking SQLite-based ann packages.
###
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "checkProperty0" function.
###
### Check that Property0 is satisfied on a given package (see FlatBimap.R
### file for more info).
###

checkProperty0 <- function(pkgname)
{
    mths <- Bimap_methods
    ## Temporary remove these 2 methods, they are not yet implemented
    ## for AnnDbBimap objects
    mths <- mths[!(mths %in% c("links", "count.links"))] # temporary removal
    ## First we check that there is a generic for all the expected Bimap
    ## methods and that this generic has indeed a corresponding method for
    ## Bimap objects.
    for (FUN in mths) {
        if (!isGeneric(FUN))
            stop("AnnotationDbi problem: \"",
                 FUN, "\" should be a generic")
        fdef <- getGeneric(FUN)
        classes <- ls(get(".MTable", envir = environment(fdef)), all.names=TRUE)
        if (!any(sapply(classes, function(class) extends(class, "Bimap"))))
            stop("AnnotationDbi problem: can't find a \"",
                 FUN, "\" method for Bimap objects")
    }
    ## Then we check  
    #pkgenv <- asNamespace(pkgname) # so it works also if the pkg has no NAMESPACE
    pkgenv <- as.environment(paste("package", pkgname, sep=":"))
    for (objname in ls(pkgenv)) {
        x <- pkgenv[[objname]]
        if (!is(x, "AnnDbBimap"))
            next
        cat("Testing Bimap methods on object '", objname, "':\n", sep="")
        for (FUN in mths) {
            cat(" - method: \"", FUN, "\"... ", sep="")
            fdef <- get(FUN)
            y1 <- fdef(x)
            y2 <- fdef(flatten(x))
            if (!identical(y1, y2))
                stop(FUN, "(x) and ", FUN, "(flatten(x)) are not identical")
            cat("OK\n")
        }
    }
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "checkMAPCOUNTS" function.
###
### Typical use:
###   > checkMAPCOUNTS("hgu95av2.db")
###

checkMAPCOUNTS <- function(pkgname)
{
    require(pkgname, character.only=TRUE) || stop(pkgname, " package needed")
    if (substr(pkgname, nchar(pkgname) - 2, nchar(pkgname)) != ".db")
        stop(pkgname, " doesn't look like the name of an SQLite-based annotation package")
    prefix <- substr(pkgname, 1, nchar(pkgname) - 3)
    getMap <- function(mapname) get(mapname, envir=asNamespace(pkgname))
    MAPCOUNTS <- getMap(paste(prefix, "MAPCOUNTS", sep=""))
    for (mapname in names(MAPCOUNTS)) {
        cat("Counting mapped keys for map ", mapname, ":\n", sep="")
        map <- getMap(mapname)
        map_length <- length(map)
        cat("  - length(map) = ", map_length, "\n", sep="")

        ## count0
        count0 <- MAPCOUNTS[mapname]
        cat("  - MAPCOUNTS[\"", mapname, "\"] = ", count0, "\n", sep="")

        if (is(map, "AgiAnnDbMap") && mappedLkeysIsNotAvailable(map)) {
            ## count.mappedkeys and mappedkeys are not available for silly map MULTIHIT
            cat("  - count1 = not available for this map ==> SKIPPED!\n")
            cat("  - count2 = not available for this map ==> SKIPPED!\n")
        } else {
            ## count1
            if (is.vector(map) && is.null(names(map))) # to deal with the REJECTORF case
                t1 <- system.time(count1 <- sum(!is.na(map)))
            else
                t1 <- system.time(count1 <- count.mappedkeys(map))
            cat("  - count1 = ", count1, " (", t1[3], " s)\n", sep="")
            if (count1 != count0)
                stop("count1 and count0 differ")
            if (is.vector(map) && is.null(names(map)))
                next

            ## count2
            t2 <- system.time(count2 <- length(mappedkeys(map)))
            cat("  - count2 = ", count2, " (", t2[3], " s)\n", sep="")
            if (count2 != count0)
                stop("count2 and count0 differ")
            if (is(map, "IpiAnnDbMap") || is(map, "GOTermsAnnDbBimap"))
                next
        }

        ## count3
        if(map_length == 0){
            count3 <- 0
            t3 <- 0
        }else{
            t3 <- system.time(count3 <- sum(sapply(as.list(map), function(x) length(x)!=1 || !is.na(x))))
        }
        cat("  - count3 = ", count3, " (", t3[3], " s)\n", sep="")
        if (count3 != count0)
            stop("count3 and count0 differ")
    }
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "compareAnnDataIn2Pkgs" function.
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

### "compareAnnDataIn2Pkgs" compares the annotation data between 2 packages.
### We use it to validate our SQLite-based ann packages by comparing each of
### them to its envir-based sibling package e.g.:
###     > library(AnnotationDbi)
###     > AnnotationDbi:::compareAnnDataIn2Pkgs("hgu95av2", "hgu95av2.db", "hgu95av2")
###
compareAnnDataIn2Pkgs <- function(pkgname1, pkgname2, prefix, quick=FALSE, verbose=FALSE)
{
    require(pkgname1, character.only=TRUE) || stop(pkgname1, " package needed")
    require(pkgname2, character.only=TRUE) || stop(pkgname2, " package needed")
    getMap <- function(pkgname, mapname)
    {
        get(mapname, envir=as.environment(paste("package", pkgname, sep=":")), inherits=FALSE)
    }
    maps1 <- names(getMap(pkgname1, paste(prefix, "MAPCOUNTS", sep="")))
    maps2 <- names(getMap(pkgname2, paste(prefix, "MAPCOUNTS", sep="")))
    notin2 <- setdiff(maps1, maps2)
    if (length(notin2) != 0) {
        cat("Maps in ", pkgname1, "::", prefix, "MAPCOUNTS",
            " not in ", pkgname2, "::", prefix, "MAPCOUNTS:\n", sep="")
        cat("  ", paste(notin2, collapse=", "), "\n\n", sep="")
    }
    notin1 <- setdiff(maps2, maps1)
    if (length(notin1) != 0) {
        cat("Maps in ", pkgname2, "::", prefix, "MAPCOUNTS",
            " not in ", pkgname1, "::", prefix, "MAPCOUNTS:\n", sep="")
        cat("  ", paste(notin1, collapse=", "), "\n\n", sep="")
    }

    maps <- intersect(maps1, maps2)
    mismatch_summary <- list()
    for (mapname in maps) {
        cat("*** Comparing ", mapname, " maps:\n", sep="")
        map1 <- getMap(pkgname1, mapname)
        cat("***   map1 is ", mapname, " from package ", pkgname1, "\n", sep="")
        map2 <- getMap(pkgname2, mapname)
        cat("***   map2 is ", mapname, " from package ", pkgname2, "\n", sep="")

        ## Compare lengths
        length1 <- length(map1)
        mapped_keys1 <- mappedkeys(map1)
        count1 <- length(mapped_keys1)
        cat("***   length(map1) = ", length1,
            " (", count1, " mapped keys)\n", sep="")

        length2 <- length(map2)
        mapped_keys2 <- mappedkeys(map2)
        count2 <- length(mapped_keys2)
        cat("***   length(map2) = ", length2,
            " (", count2, " mapped keys)\n", sep="")

        common_keys <- intersect(ls(map1), ls(map2))
        common_mapped_keys <- intersect(mapped_keys1, mapped_keys2)
        count3 <- length(common_mapped_keys)
        cat("***   nb of common keys = ", length(common_keys),
            " (", count3, " common mapped keys)\n", sep="")

        if (count3 == 0) {
            cat("*** ==> NOTHING WORTH COMPARING!\n")
            mismatch_summary[[mapname]] <- NA
            next
        }
        if (quick) {
            ## Quick test (on a sample of 50 common mapped keys)
            size <- 50L
            if (size > count3)
                size <- count3
            random_keys <- sample(common_mapped_keys, size)
            submap1 <- mget(random_keys, envir=map1)
            if (!identical(names(submap1), random_keys))
                stop("mget() didn't return the expected keys on map1")
            submap2 <- mget(random_keys, envir=map2)
            if (!identical(names(submap2), random_keys))
                stop("mget() didn't return the expected keys on map2")
            OK <- sapply(random_keys,
                         function(key) identical.collections(map1[[key]], map2[[key]]))
            nmis <- sum(!OK)
            cat("***   nb of mismatches (on a sample of ", size, " keys) = ", nmis, "\n", sep="")
        } else {
            ## Full test (on all common keys)
            OK <- sapply(common_keys,
                         function(key) identical.collections(map1[[key]], map2[[key]]))
            nmis <- sum(!OK)
            cat("***   nb of mismatches = ", nmis, "\n", sep="")
        }
        mismatch_summary[[mapname]] <- nmis
    }
    mismatch_summary <- unlist(mismatch_summary)
    nb_PASSED <- sum(mismatch_summary == 0, na.rm=TRUE)
    nb_FAILED <- length(mismatch_summary) - nb_PASSED
    cat("SUMMARY OF MISMATCHES:\n")
    show(mismatch_summary)
    cat("  - nb of map comparisons = ", length(mismatch_summary), "\n", sep="")
    cat("  - nb of PASSED maps = ", nb_PASSED, "\n", sep="")
    cat("  - nb of FAILED maps = ", nb_FAILED, "\n", sep="")
    mismatch_summary
}

