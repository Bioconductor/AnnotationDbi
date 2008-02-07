###
### Flattening different kinds of environments and maps
###
### A toCV.<colname> function takes a vector (or a single NA) and returns
### a character vector of the same length (or NULL).
### A <colname>_CVL object is a list of character vector or NULL elements.
###

### Flatten the GO envir found in HUMANCHIP, MOUSECHIP and RATCHIP packages.
flatten.GOenvir <- function(env)
{
    ## 'x' is a named list of "GO triplets". The names are the GO ids.
    ## A "GO triplet" is itself a list with 3 elements (GOID, Evidence,
    ## Ontology).
    toCV.go_id <- function(x)
    {
        if (!is.list(x)) {
            if (!isTRUE(is.na(x)))
                warning("right value in GO map is neither a list or a single NA")
            return(NULL)
        }
        names(x)
    }
    toCV.Evidence <- function(x)
    {
        if (!is.list(x)) {
            if (!isTRUE(is.na(x)))
                warning("right value in GO map is neither a list or a single NA")
            return(NULL)
        }
        sapply(x, function(goelt) goelt$Evidence, USE.NAMES=FALSE)
    }
    toCV.Ontology <- function(x)
    {
        if (!is.list(x)) {
            if (!isTRUE(is.na(x)))
                warning("right value in GO map is neither a list or a single NA")
            return(NULL)
        }
        sapply(x, function(goelt) goelt$Ontology, USE.NAMES=FALSE)
    }
    go_id_CVL <- eapply(env, toCV.go_id, all.names=TRUE)
    Evidence_CVL <- eapply(env, toCV.Evidence, all.names=TRUE)
    Ontology_CVL <- eapply(env, toCV.Ontology, all.names=TRUE)

    lens <- sapply(go_id_CVL, length)    
    probe_id <- rep.int(names(go_id_CVL), lens)
    go_id <- unlist(go_id_CVL, recursive=FALSE, use.names=FALSE)
    Evidence <- unlist(Evidence_CVL, recursive=FALSE, use.names=FALSE)
    Ontology <- unlist(Ontology_CVL, recursive=FALSE, use.names=FALSE)
   
    data.frame(probe_id=probe_id, Evidence=Evidence, go_id=go_id,
               Ontology=Ontology, stringsAsFactors=FALSE)
}

### Flatten the GO2PROBE and GO2ALLPROBES envir found in HUMANCHIP, 
### MOUSECHIP and RATCHIP packages.
flatten.GO2PROBEenvir <- function(env)
{
    ## 'x' is a named character vector of probe ids. The names are the
    ## Evidence codes.
    toCV.probe_id <- function(x)
    {
        if (any(is.na(x)))
            warning("left value in GO2PROBE map contains NAs")
        x
    }
    toCV.Evidence <- function(x)
    {
        Evidence <- names(x)
        if (any(is.na(Evidence)))
            warning("left value in GO2PROBE map contains NAs")
        Evidence
    }
    probe_id_CVL <- eapply(env, toCV.probe_id, all.names=TRUE)
    Evidence_CVL <- eapply(env, toCV.Evidence, all.names=TRUE)

    lens <- sapply(probe_id_CVL, length)
    go_id <- rep.int(names(probe_id_CVL), lens)
    probe_id <- unlist(probe_id_CVL, recursive=FALSE, use.names=FALSE)
    Evidence <- unlist(Evidence_CVL, recursive=FALSE, use.names=FALSE)
    
    data.frame(probe_id=probe_id, Evidence=Evidence, go_id=go_id,
               stringsAsFactors=FALSE)
}

### Some sanity checking
checkPROBE2GOmappings <- function(GO, GO2PROBE, GO2ALLPROBES)
{
    cat("Checking PROBE2GO mappings:\n")

    is_mapped <- eapply(GO, function(x) !(is.atomic(x) && is.vector(x) && isTRUE(is.na(x))), all.names=TRUE)
    is_mapped <- unlist(is_mapped, recursive=FALSE, use.names=TRUE)
    mapped_probe_id <- sort(names(is_mapped)[is_mapped])
    cat(length(mapped_probe_id), " probe ids are mapped to at least 1 GO id\n", sep="")

    GOflat <- flatten.GOenvir(GO)
    ii <- order(GOflat$probe_id, GOflat$go_id, GOflat$Evidence)
    GOflat <- GOflat[ii, ]
    if (!identical(unique(GOflat$probe_id), mapped_probe_id))
        stop("unexpected or missing probe ids in flat representation of GO")

    GO2PROBEflat <- flatten.GO2PROBEenvir(GO2PROBE)
    ii <- order(GO2PROBEflat$probe_id, GO2PROBEflat$go_id, GO2PROBEflat$Evidence)
    GO2PROBEflat <- GO2PROBEflat[ii, ]
    if (!identical(unique(GO2PROBEflat$probe_id), mapped_probe_id))
        stop("unexpected or missing probe ids in flat representation of GO2PROBE")

    if (!all(GOflat[ , c("probe_id", "Evidence", "go_id")] == GO2PROBEflat))
        stop("flat representations of GO and GO2PROBE differ")

    GO2ALLPROBESflat <- flatten.GO2PROBEenvir(GO2ALLPROBES)
    ii <- order(GO2ALLPROBESflat$probe_id, GO2ALLPROBESflat$go_id, GO2ALLPROBESflat$Evidence)
    GO2ALLPROBESflat <- GO2ALLPROBESflat[ii, ]
    if (!identical(unique(GO2ALLPROBESflat$probe_id), mapped_probe_id))
        stop("unexpected or missing probe ids in flat representation of GO2ALLPROBESflat")


}

