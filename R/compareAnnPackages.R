compareAnnPackages <- function(pkgname1, pkgname2, direct_maps, reverse_maps, mapprefix="", probes=NULL)
{
    require(pkgname1, character.only=TRUE) || stop(pkgname1, " package needed")
    require(pkgname2, character.only=TRUE) || stop(pkgname2, " package needed")
    getMap <- function(pkgname, mapname)
    {
        get(mapname, envir=asNamespace(pkgname))
    }
    cmp_submap_summary <- list()
    for (mapshortname in c(direct_maps, reverse_maps)) {
        mapname <- paste(mapprefix, mapshortname, sep="")
        cat("*** Comparing ", mapname, " maps:\n", sep="")
        map1 <- getMap(pkgname1, mapname)
        map2 <- getMap(pkgname2, mapname)
        map1fullname <- paste(pkgname1, "::", mapname, sep="")
        map2fullname <- paste(pkgname2, "::", mapname, sep="")
        ## Compare lengths
        nkeys1 <- length(map1)
        cat("  **1** length(", map1fullname, ") = ", nkeys1, "\n", sep="")
        nkeys2 <- length(map2)
        cat("  **2** length(", map2fullname, ") = ", nkeys2, "\n", sep="")
        ## Compare submaps
        if (mapshortname %in% direct_maps) {
            if (is.null(probes))
                probes <- ls(map1)[1:3]
            submap1 <- mget(probes, envir=map1)
            cat("  **1** mget(probes, envir=", map1fullname, "):\n", sep="")
            show(submap1)
            submap2 <- mget(probes, envir=map2)
            cat("  **2** mget(probes, envir=", map2fullname, "):\n", sep="")
            show(submap2)
        } else {
            keys <- ls(map1)[1:3]
            submap1 <- mget(keys, envir=map1)
            cat("  **1** mget(keys, envir=", map1fullname, "):\n", sep="")
            show(submap1)
            submap2 <- mget(keys, envir=map2)
            cat("  **2** mget(keys, envir=", map2fullname, "):\n", sep="")
            show(submap2)
        }
        cmp_submap_summary[[mapname]] <- identical(submap1, submap2)
    }
    cmp_submap_summary
}
