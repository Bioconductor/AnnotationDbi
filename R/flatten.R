###
### Flattening different kinds of environments and maps
###

### Flatten the GO envir found in HUMANCHIP and RODENTCHIP packages.
flatten.GOenvir <- function(env)
{
    get.go_id <- function(golist)
    {
        if (!is.list(golist)) {
            if (!isTRUE(is.na(golist)))
                warning("right value in GO map is neither a list or an NA")
            return(NULL)
        }
        names(golist)
    }
    get.Evidence <- function(golist)
    {
        if (!is.list(golist)) {
            if (!isTRUE(is.na(golist)))
                warning("right value in GO map is neither a list or an NA")
            return(NULL)
        }
        sapply(golist, function(goelt) goelt$Evidence, USE.NAMES=FALSE)
    }
    get.Ontology <- function(golist)
    {
        if (!is.list(golist)) {
            if (!isTRUE(is.na(golist)))
                warning("right value in GO map is neither a list or an NA")
            return(NULL)
        }
        sapply(golist, function(goelt) goelt$Ontology, USE.NAMES=FALSE)
    }
    go_id_map <- eapply(env, get.go_id, all.names=TRUE)
    Evidence_map <- eapply(env, get.Evidence, all.names=TRUE)
    Ontology_map <- eapply(env, get.Ontology, all.names=TRUE)

    lens <- sapply(go_id_map, length)    
    probe_id <- rep(names(go_id_map), lens)
    go_id <- unlist(go_id_map, recursive=FALSE, use.names=FALSE)
    Evidence <- unlist(Evidence_map, recursive=FALSE, use.names=FALSE)
    Ontology <- unlist(Ontology_map, recursive=FALSE, use.names=FALSE)
   
    data.frame(probe_id=probe_id, Evidence=Evidence, go_id=go_id,
               Ontology=Ontology, stringsAsFactors=FALSE)
}

### Flatten the GO2PROBE envir found in HUMANCHIP and RODENTCHIP packages.
flatten.GO2PROBEenvir <- function(env)
{
    ## 'probe_id' is a vector of probe ids
    get.probe_id <- function(probe_id)
    {
        if (any(is.na(probe_id)))
            warning("left value in GO2PROBE map contains NAs")
        probe_id
    }
    get.Evidence <- function(probe_id)
    {
        Evidence <- names(probe_id)
        if (any(is.na(Evidence)))
            warning("left value in GO2PROBE map contains NAs")
        Evidence
    }
    probe_id_map <- eapply(env, get.probe_id, all.names=TRUE)
    Evidence_map <- eapply(env, get.Evidence, all.names=TRUE)

    lens <- sapply(probe_id_map, length)
    go_id <- rep(names(probe_id_map), lens)
    probe_id <- unlist(probe_id_map, recursive=FALSE, use.names=FALSE)
    Evidence <- unlist(Evidence_map, recursive=FALSE, use.names=FALSE)
    
    data.frame(probe_id=probe_id, Evidence=Evidence, go_id=go_id,
               stringsAsFactors=FALSE)
}

