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
### them to its envir-based sibling package e.g.:
###     > library(AnnotationDbi)
###   HGU95AV2_DB schema
###     > AnnotationDbi:::compareAnnDataIn2Pkgs.HGU95AV2_DB("hgu95av2", "hgu95av2db", "hgu95av2")
###   YEAST2_DB schema
###     > AnnotationDbi:::compareAnnDataIn2Pkgs.YEAST2_DB("yeast2", "yeast2db", "yeast2")
###     > AnnotationDbi:::compareAnnDataIn2Pkgs.YEAST2_DB("ygs98", "ygs98db", "ygs98")
###   AG_DB schema
###     > AnnotationDbi:::compareAnnDataIn2Pkgs.AG_DB("ag", "agdb", "ag")
###     > AnnotationDbi:::compareAnnDataIn2Pkgs.AG_DB("ath1121501", "ath1121501db", "ath1121501")
###   YEAST_DB schema
###     > AnnotationDbi:::compareAnnDataIn2Pkgs.YEAST_DB("YEAST", "YEASTdb", "YEAST")
###   LLMAPPINGS_DB schema
###     > AnnotationDbi:::compareAnnDataIn2Pkgs.LLMAPPINGS_DB("humanLLMappings",
###                                                           "humanLLMappingsdb", "humanLLMappings")
###     > AnnotationDbi:::compareAnnDataIn2Pkgs.LLMAPPINGS_DB("mouseLLMappings",
###                                                           "mouseLLMappingsdb", "mouseLLMappings")
###     > AnnotationDbi:::compareAnnDataIn2Pkgs.LLMAPPINGS_DB("ratLLMappings",
###                                                           "ratLLMappingsdb", "ratLLMappings")
###

compareAnnDataIn2Pkgs <- function(pkgname1, pkgname2, prefix, direct_maps, reverse_maps,
                                  quick=FALSE, verbose=FALSE)
{
    require(pkgname1, character.only=TRUE) || stop(pkgname1, " package needed")
    require(pkgname2, character.only=TRUE) || stop(pkgname2, " package needed")
    getMap <- function(pkgname, mapname)
    {
        get(mapname, envir=as.environment(paste("package", pkgname, sep=":")), inherits=FALSE)
    }
    mappedNames <- function(map)
    {
        if (is.environment(map)) {
            is_na <- eapply(map, function(x) length(x) == 1 && is.na(x))
            mapped_names <- names(is_na)[!as.logical(is_na)]
        } else {
            mapped_names <- mapped.names(map)
        }
        mapped_names
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
        length1 <- length(map1)
        mapped_names1 <- mappedNames(map1)
        count1 <- length(mapped_names1)
        cat("***   length(map1) = ", length1,
            " (", count1, " mapped names)\n", sep="")

        length2 <- length(map2)
        mapped_names2 <- mappedNames(map2)
        count2 <- length(mapped_names2)
        cat("***   length(map2) = ", length2,
            " (", count2, " mapped names)\n", sep="")

        common_names <- intersect(ls(map1), ls(map2))
        common_mapped_names <- intersect(mapped_names1, mapped_names2)
        count3 <- length(common_mapped_names)
        cat("***   nb of common names = ", length(common_names),
            " (", count3, " common mapped names)\n", sep="")

        if (count3 == 0) {
            cat("*** ==> NOTHING WORTH COMPARING!\n")
            mismatch_summary[[mapname]] <- NA
            next
        }
        if (quick) {
            ## Quick test (on a sample of 50 common mapped names)
            size <- 50L
            if (size > count3)
                size <- count3
            random_names <- sample(common_mapped_names, size)
            submap1 <- mget(random_names, envir=map1)
            if (!identical(names(submap1), random_names))
                stop("mget() didn't return the expected names on map1")
            submap2 <- mget(random_names, envir=map2)
            if (!identical(names(submap2), random_names))
                stop("mget() didn't return the expected names on map2")
            OK <- sapply(random_names,
                         function(name) identical.collections(map1[[name]], map2[[name]]))
            nmis <- sum(!OK)
            cat("***   nb of mismatches (on a sample of ", size, " names) = ", nmis, "\n", sep="")
        } else {
            ## Full test (on all common names)
            OK <- sapply(common_names,
                         function(name) identical.collections(map1[[name]], map2[[name]]))
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

        ## count0
        count0 <- MAPCOUNTS[mapname]
        cat("  - MAPCOUNTS[\"", mapname, "\"] = ", count0, "\n", sep="")

        ## count1
        if (is.numeric(map)) # to deal with the CHRLENGTHS case
            t1 <- system.time(count1 <- sum(!is.na(map)))
        else
            t1 <- system.time(count1 <- count.mapped.names(map))
        cat("  - count1 = ", count1, " (", t1[3], " s)\n", sep="")
        if (count1 != count0)
            stop("count1 and count0 differ")
        if (is.numeric(map))
            next

        ## count2
        t2 <- system.time(count2 <- length(mapped.names(map)))
        cat("  - count2 = ", count2, " (", t2[3], " s)\n", sep="")
        if (count2 != count0)
            stop("count2 and count0 differ")
        if (is(map, "IpiAnnDbMap"))
            next

        ## count3
        t3 <- system.time(count3 <- sum(sapply(as.list(map), function(x) length(x)!=1 || !is.na(x))))
        cat("  - count3 = ", count3, " (", t3[3], " s)\n", sep="")
        if (count3 != count0)
            stop("count3 and count0 differ")
    }
}

