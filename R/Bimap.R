### =========================================================================
### The bimap concept and the Bimap interface
### -----------------------------------------


### -------------------------------------------------------------------------
### The bimap concept
### -----------------
###
### Example of a bimap M:
###
###   4 objects on the left (Lkeys): a, b, c, d
###   2 objects on the right (Rkeys): A, B, C
###
###   Links:
###      a <--> A
###      a <--> B
###      b <--> A
###      d <--> C
###
### The "flat" representation of M looks like a data frame:
###
###   left  right
###      a      A 
###      a      B
###      b      A
###      d      C
###
### If in addition the right objects have 1 multi-valued attribute, for
### example, a numeric vector:
###   A <-- c(1.2, 0.9)
###   B <-- character(0)
###   C <-- -1:1
###
### then the "flat" representation of M becomes:
###
###   left  right  Rattrib
###      a      A      1.2
###      a      A      0.9
###      a      B       NA
###      b      A      1.2
###      b      A      0.9
###      d      C       -1
###      d      C        0
###      d      C        1
###
### Note that now the number of rows is greater than the number of links!
###
### -------------------------------------------------------------------------


### -------------------------------------------------------------------------
### The "Bimap" interface
### ---------------------
###
### AnnDbBimap and FlatBimap objects:
###
###    A AnnDbBimap object is a bimap whose data are stored in a data base.
###    A FlatBimap object is a bimap whose data (left keys, right keys and
###    links) are stored in memory (in a data frame for the links).
###    Conceptually, an AnnDbBimap and a FlatBimap object are the same (only
###    their internal representation differ) so it's natural to try to define
###    a set of methods that make sense for both (so they can be manipulated
###    in a similar way). This common interface is the "Bimap" interface.
###
### The "flatten" generic:
###
###      flatten(x) converts AnnDbBimap object x into FlatBimap object y
###      with no loss of information
###
###    Note that a FlatBimap object can't be converted into an AnnDbBimap
###    object (well, in theory maybe it could be, but for now the data bases
###    we use to store the data of the AnnDbBimap objects are treated as
###    read-only). This conversion from AnnDbBimap to FlatBimap is performed
###    by the "flatten" generic function (with methods for AnnDbBimap objects
###    only). 
###    The "flatten" generic (defined for AnnDbBimap objects only) converts
###    an AnnDbBimap object into a FlatBimap object .
###
### Property0:
###
###    The "flatten" generic plays a very useful role when we need to
###    understand or explain exactly what a given Bimap method f will do when
###    applied to an AnnDbBimap object. It's generally easier to explain what
###    it does on a FlatBimap object and then to just say "and it does the
###    same thing on a AnnDbBimap object". This is exactly what Property0
###    says:
###
###      for any AnnDbBimap object x, f(x) is expected to be indentical
###      to f(flatten(x))
###
###    Of course, this implies that the f method for AnnDbBimap objects
###    return the same type of object than the f method for FlatBimap objects.
###    In this sense, the "revmap" and "subset" Bimap methods are particular
###    because they are expected to return an object of the same class as
###    their argument x, so f(x) can't be identical to f(flatten(x)). For
###    these methods, Property0 says:
###
###      for any AnnDbBimap object x, flatten(f(x)) is expected to
###      be identical to f(flatten(x))
###
###    The checkProperty0() function (AnnDbPkg-checker.R file) checks that
###    Property0 is satisfied on all the AnnDbBimap objects defined in a given
###    package (FIXME: checkProperty0 is currently broken).
###
### Finally, note that both AnnDbBimap and FlatBimap objects have a read-only
### semantic: the user can subset them but cannot change their data.
###
### -------------------------------------------------------------------------


### KEEP THIS IN SYNC WITH THE STATE OF AFFAIRS! Only methods of the first and
### second group go here.
Bimap_methods <- c(
    ## GROUP 1: 15 methods that _must_ be defined for FlatBimap objects
    ## _and_ AnnDbBimap objects
    "collabels",
    "colnames",
    "direction",
    "direction<-",
    "Lkeys", "Rkeys",
    "Lkeys<-", "Rkeys<-",
    "subset",
    "mappedLkeys", "mappedRkeys",
    "nrow",
    "links",
    "toLList", "toRList",
    ## GROUP 2: Methods for which a default is provided (in this file) but
    ## some of them are redefined for AnnDbBimap objects to obtain better
    ## performance
    "Lcolname", "Rcolname", "Tcolname", "Rattrib_colnames",
    "revmap",
    "Llength", "Rlength",
    "count.mappedLkeys", "count.mappedRkeys",
    "count.links",
    ## GROUP 3: Directed methods (i.e. what they return depends on the
    ## direction of the map). All what they do is to dispatch on the
    ## corresponding undirected method according to the value of direction(x)
    "keys",
    "length",
    "mappedkeys",
    "count.mappedkeys",
    "toList"
)

### A virtual class with no slot (a kind of Java "interface")
setClass("Bimap", representation("VIRTUAL"))

setMethod("Lcolname", "Bimap",
    function(x)
    {
        colnames <- colnames(x)
        names(colnames) <- collabels(x)
        colnames["Lcolname"]
    }
)
setMethod("Rcolname", "Bimap",
    function(x)
    {
        colnames <- colnames(x)
        names(colnames) <- collabels(x)
        colnames["Rcolname"]
    }
)
setMethod("Tcolname", "Bimap",
    function(x)
    {
        colnames <- colnames(x)
        names(colnames) <- collabels(x)
        colnames["Tcolname"]
    }
)
setMethod("Rattrib_colnames", "Bimap",
    function(x)
    {
        colnames(x)[collabels(x) == "Rattrib_colname"]
    }
)

setMethod("revmap", "Bimap",
    function(x, ...) { direction(x) <- - direction(x); x }
)

setMethod("Llength", "Bimap",
    function(x) length(Lkeys(x)))
setMethod("Rlength", "Bimap",
    function(x) length(Rkeys(x)))

setMethod("count.mappedLkeys", "Bimap",
    function(x) length(mappedLkeys(x)))
setMethod("count.mappedRkeys", "Bimap",
    function(x) length(mappedRkeys(x)))

setMethod("count.links", "Bimap",
    function(x) nrow(links(x)))

setMethod("ncol", "Bimap",
    function(x) length(colnames(x)))

### left-to-right: direction =  1
### right-to-left: direction = -1
setMethod("from.colpos", "Bimap",
    function(x, direction)
    {
        if (direction == 1) side = "Lcolname" else side = "Rcolname"
        match(side, collabels(x))
    }
)
setMethod("to.colpos", "Bimap",
    function(x, direction) from.colpos(x, - direction))

### FIXME
### For now, tags and attributes are all mixed together which is bad and will
### cause problems when reversing some maps or when plugging maps together.
### We need to make a clear distinction between tags and attributes but this
### will require to change the current L2Rlink class and make things more
### complicated...
setMethod("tags.colpos", "Bimap",
    function(x) seq_len(ncol(x))[-c(from.colpos(x, 1), from.colpos(x, -1))])

setMethod("from.keys", "Bimap",
    function(x, direction) if (direction == 1) Lkeys(x) else Rkeys(x))
setMethod("to.keys", "Bimap",
    function(x, direction) from.keys(x, - direction))

setMethod("dim", "Bimap",
    function(x) c(nrow(x), ncol(x)))

### Directed methods

.DIRECTION_STR2INT <- c("L --> R"=1L, "L <-- R"=-1L, "undirected"=0L)

.normalize.direction <- function(direction)
{
    if ((!is.numeric(direction) && !is.character(direction))
     || length(direction) != 1 || is.na(direction))
        stop("'direction' must be a single (non-NA) integer or string")
    if (is.character(direction)) {
        direction <- match.arg(tolower(direction), names(.DIRECTION_STR2INT))
        return(do.call("switch", c(EXPR=direction, as.list(.DIRECTION_STR2INT))))
    }
    if (!(direction %in% .DIRECTION_STR2INT))
        stop("when a numeric value, 'direction' should be one of 1, -1 or 0")
    as.integer(direction)
}

setMethod("keys", "Bimap",
    function(x)
        switch(as.character(direction(x)),
                "1"=Lkeys(x),
               "-1"=Rkeys(x),
                    stop("keys() is undefined for an undirected bimap"))
)
setReplaceMethod("keys", "Bimap",
    function(x, value)
    {
        switch(as.character(direction(x)),
                "1"=`Lkeys<-`(x, value),
               "-1"=`Rkeys<-`(x, value),
                    stop("keys<- is undefined for an undirected bimap"))
    }
)
setMethod("length", "Bimap",
    function(x)
        switch(as.character(direction(x)),
                "1"=Llength(x),
               "-1"=Rlength(x),
                    stop("length() is undefined for an undirected bimap"))
)
setMethod("mappedkeys", "Bimap",
    function(x)
        switch(as.character(direction(x)),
                "1"=mappedLkeys(x),
               "-1"=mappedRkeys(x),
                    stop("mappedkeys() is undefined for an undirected bimap"))
)
setMethod("count.mappedkeys", "ANY",
    function(x)
        switch(as.character(direction(x)),
                "1"=count.mappedLkeys(x),
               "-1"=count.mappedRkeys(x),
                    stop("count.mappedkeys() is undefined for an undirected bimap"))
)
setMethod("toList", "Bimap",
    function(x, keys=NULL)
        switch(as.character(direction(x)),
                "1"=toLList(x, keys),
               "-1"=toRList(x, keys),
                    stop("toList() is undefined for an undirected bimap"))
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Used by the show methods for FlatBimap and AnnDbBimap objects.
###

.key.summary <- function(keys, mapped)
{
    len0 <- length(keys)
    if (len0 > 2)
        keys <- keys[1:2]
    string <- paste(paste("\"", keys, "\"", sep=""), collapse=", ")
    if (len0 > 2) {
        string <- paste(string, ", ... (total=", len0, "/mapped=", mapped, ")", sep="")
    }
    string
}

Bimap.summary <- function(x)
{
    ## Left keys
    cat("| Lcolname: ", Lcolname(x), sep="")
    if (is(x, "AnnDbBimap"))
        cat(" (Ltablename: ", Ltablename(x), ")", sep="")
    cat("\n")
    tmp <- mappedLkeys(x) # just to put them in the cache
    cat("|    Lkeys: ", .key.summary(Lkeys(x), count.mappedLkeys(x)), "\n", sep="")
    cat("|\n")

    if (!is(x, "AnnDbMap")) {
        ## Right keys
        cat("| Rcolname: ", Rcolname(x), sep="")
        if (is(x, "AnnDbBimap"))
            cat(" (Rtablename: ", Rtablename(x), ")", sep="")
        cat("\n")
        tmp <- mappedRkeys(x) # just to put them in the cache
        cat("|    Rkeys: ", .key.summary(Rkeys(x), count.mappedRkeys(x)), "\n", sep="")
        cat("|\n")
    }

    ## Tag
    if (!is.na(Tcolname(x)))
        cat("| Tcolname: ", Tcolname(x), "\n|\n", sep="")

    ## direction
    direction <- names(.DIRECTION_STR2INT)[.DIRECTION_STR2INT == direction(x)]
    cat("| direction: ", direction, "\n", sep="")
}

