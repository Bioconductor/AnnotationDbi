###
### Flattening different kinds of environments and maps
###
### A toCV.<colname> function takes a vector (or a single NA) and returns
### a character vector of the same length (or NULL).
### A <colname>_CVL object is a list of character vector or NULL elements.
###

### Flatten the GO envir found in HUMANCHIP and RODENTCHIP packages.
flatten.GOenvir <- function(env)
{
    ## 'x' is a named list of "GO triplets". The names are the GO ids.
    ## A "GO triplet" is itself a list with 3 elements (GOID, Evidence,
    ## Ontology).
    toCV.go_id <- function(x)
    {
        if (!is.list(x)) {
            if (!isTRUE(is.na(x)))
                warning("right value in GO map is neither a list or an NA")
            return(NULL)
        }
        names(x)
    }
    toCV.Evidence <- function(x)
    {
        if (!is.list(x)) {
            if (!isTRUE(is.na(x)))
                warning("right value in GO map is neither a list or an NA")
            return(NULL)
        }
        sapply(x, function(goelt) goelt$Evidence, USE.NAMES=FALSE)
    }
    toCV.Ontology <- function(x)
    {
        if (!is.list(x)) {
            if (!isTRUE(is.na(x)))
                warning("right value in GO map is neither a list or an NA")
            return(NULL)
        }
        sapply(x, function(goelt) goelt$Ontology, USE.NAMES=FALSE)
    }
    go_id_CVL <- eapply(env, toCV.go_id, all.names=TRUE)
    Evidence_CVL <- eapply(env, toCV.Evidence, all.names=TRUE)
    Ontology_CVL <- eapply(env, toCV.Ontology, all.names=TRUE)

    lens <- sapply(go_id_CVL, length)    
    probe_id <- rep(names(go_id_CVL), lens)
    go_id <- unlist(go_id_CVL, recursive=FALSE, use.names=FALSE)
    Evidence <- unlist(Evidence_CVL, recursive=FALSE, use.names=FALSE)
    Ontology <- unlist(Ontology_CVL, recursive=FALSE, use.names=FALSE)
   
    data.frame(probe_id=probe_id, Evidence=Evidence, go_id=go_id,
               Ontology=Ontology, stringsAsFactors=FALSE)
}

### Flatten the GO2PROBE envir found in HUMANCHIP and RODENTCHIP packages.
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
    go_id <- rep(names(probe_id_CVL), lens)
    probe_id <- unlist(probe_id_CVL, recursive=FALSE, use.names=FALSE)
    Evidence <- unlist(Evidence_CVL, recursive=FALSE, use.names=FALSE)
    
    data.frame(probe_id=probe_id, Evidence=Evidence, go_id=go_id,
               stringsAsFactors=FALSE)
}

