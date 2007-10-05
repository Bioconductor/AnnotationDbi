### =========================================================================
### The bimap concept and the Bimap interface
### -----------------------------------------


### The bimap concept
### -----------------
###
### A bimap is made of:
###
###   - 2 sets of objects: the left objects and the right objects. All the
###     objects have a name and this name is unique in each set (i.e. in the
###     left set and in the right set). The names of the left (resp. right)
###     objects are called the left (resp. right) keys or the Lkeys (resp. the
###     Rkeys).
###     
###   - Any number of links (edges) between the left and right objects. Note
###     that the links can be tagged. In our model, for a given bimap, either
###     none or all the links are tagged.
###
### In other words, a bimap is a bipartite graph.
###
### Here are some examples:
###
###   1. bimap B1:
###
###      4 left objects (Lkeys): "a", "b", "c", "d"
###      3 objects on the right (Rkeys): "A", "B", "C"
###
###      Links (edges):
###        "a" <--> "A"
###        "a" <--> "B"
###        "b" <--> "A"
###        "d" <--> "C"
###
###      Note that:
###        - There can be any number of links starting from or ending at a
###          given object.
###        - The links in this example are untagged.
###
###   2. bimap B2:
###
###      4 left objects (Lkeys): "a", "b", "c", "d"
###      3 objects on the right (Rkeys): "A", "B", "C"
###
###      Tagged links (edges):
###        "a" <-"x"-> "A"
###        "a" <-"y"-> "B"
###        "b" <-"x"-> "A"
###        "d" <-"x"-> "C"
###        "d" <-"y"-> "C"
###
###      Note that there are 2 links between objects "d" and "C":
###      1 with tag "x" and 1 with tag "y".
###
###
### Flat representation of a bimap
### ------------------------------
###
### The flat representation of a bimap is a data frame. For example,
### for B1, it is:
###
###   left  right
###      a      A 
###      a      B
###      b      A
###      d      C
###
### If in addition the right objects have 1 multi-valued attribute, for
### example, a numeric vector:
###
###   A <-- c(1.2, 0.9)
###   B <-- character(0)
###   C <-- -1:1
###
### then the flat representation of B1 becomes:
###
###   left  right  Rattrib1
###      a      A       1.2
###      a      A       0.9
###      a      B        NA
###      b      A       1.2
###      b      A       0.9
###      d      C        -1
###      d      C         0
###      d      C         1
###
### Note that now the number of rows is greater than the number of links!
###
###
### The Bimap interface in AnnotationDbi
### ------------------------------------
###
### AnnDbBimap and FlatBimap objects:
###
###    A AnnDbBimap object is a bimap whose data are stored in a data base.
###    A FlatBimap object is a bimap whose data (left keys, right keys and
###    links) are stored in memory (in a data frame for the links).
###    Conceptually, AnnDbBimap and FlatBimap objects are the same (only
###    their internal representation differ) so it's natural to try to define
###    a set of methods that make sense for both (so they can be manipulated
###    in a similar way). This common interface is the Bimap interface.
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
    "colnames",
    "colmetanames",
    "Rattribnames<-",
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
    "Lkeyname", "Rkeyname", "tagname",
    "Rattribnames",
    "revmap",
    "Llength", "Rlength",
    "count.mappedLkeys", "count.mappedRkeys",
    "count.links",
    ## GROUP 3: Directed methods (i.e. what they return depends on the
    ## direction of the map). All what they do is to dispatch on the
    ## corresponding undirected method according to the value of direction(x)
    "keyname",
    "keys",
    "length",
    "mappedkeys",
    "count.mappedkeys",
    "toList",
    "isNA"
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "direction" and "direction<-" methods.
###

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

setMethod("direction", "FlatBimap",
    function(x) x@direction)

setMethod("direction", "AnnDbBimap",
    function(x) x@direction)

setReplaceMethod("direction", "FlatBimap",
    function(x, value)
    {
        x@direction <- .normalize.direction(value)
        x
    }
)

setReplaceMethod("direction", "AnnDbBimap",
    function(x, value)
    {
        direction <- .normalize.direction(value)
        if (direction == 0)
            stop("undirected AnnDbBimap objects are not supported")
        if (direction != x@direction) {
            x@objName <- paste("revmap(", x@objName, ")", sep="")
            x@direction <- direction
        }
        x
    }
)

setReplaceMethod("direction", "AnnDbMap",
    function(x, value)
    {
        stop("changing the direction of an \"", class(x), "\" object is not supported")
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "revmap" methods.
###
### Conceptual definition:
###     revmap(x) is the reverse of map x i.e. the map that provides the
###     reverse lookup (or mapping)
###
### I would have liked to use "reverse" instead of "revmap" but "reverse" is
### already defined as a generic in Biostrings and it seems that the second
### of the 2 packages to be loaded breaks the generic and attached methods
### defined in the first. Don't know how to deal with this situation :-/
###
### The "rev" generic defined in package:base can't be used neither because
### it doesn't allow passing additional arguments to or from methods (i.e. it
### has no '...' arg) and we want to be able to pass the 'objName' arg.
### Other generics defined in package:base where having a '...' arg could be
### useful: "unlist", "t" and "scale" (just in case someone feels brave enough
### to request this on R-devel).
###
### Note that "revmap" for "AnnDbBimap" objects does _not_ query the database!
###

setMethod("revmap", "Bimap",
    function(x) { direction(x) <- - direction(x); x }
)

setMethod("revmap", "AnnDbBimap",
    function(x, objName=NULL)
    {
        x <- callNextMethod(x) # calls "revmap" method for "Bimap" objects
        if (!is.null(objName))
            x@objName <- toString(objName)
        x
    }
)

setMethod("revmap", "environment",
    function(x) l2e(reverseSplit(as.list(x)))
)

setMethod("revmap", "list",
    function(x) reverseSplit(x)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "colnames" and "colmetanames" methods.
###

setMethod("colnames", "FlatBimap",
    function(x, do.NULL=TRUE, prefix="col")
        colnames(x@data)
)

setMethod("colnames", "AnnDbBimap",
    function(x, do.NULL=TRUE, prefix="col")
        L2Rchain.colnames(x@L2Rchain)
)

setMethod("colmetanames", "FlatBimap",
    function(x)
        x@colmetanames
)

setMethod("colmetanames", "AnnDbBimap",
    function(x)
        L2Rchain.colmetanames(x@L2Rchain)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "Lkeyname", "Rkeyname" and "keyname" methods.
###

setMethod("Lkeyname", "Bimap",
    function(x)
    {
        colnames <- colnames(x)
        names(colnames) <- colmetanames(x)
        colnames["Lkeyname"]
    }
)

setMethod("Lkeyname", "AnnDbBimap",
    function(x) L2Rchain.Lkeyname(x@L2Rchain))

setMethod("Rkeyname", "Bimap",
    function(x)
    {
        colnames <- colnames(x)
        names(colnames) <- colmetanames(x)
        colnames["Rkeyname"]
    }
)

setMethod("Rkeyname", "AnnDbBimap",
    function(x) L2Rchain.Rkeyname(x@L2Rchain))

setMethod("keyname", "Bimap",
    function(x)
        switch(as.character(direction(x)),
                "1"=Lkeyname(x),
               "-1"=Rkeyname(x),
                    stop("keyname() is undefined for an undirected bimap"))
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "tagname" methods.
###

setMethod("tagname", "Bimap",
    function(x)
    {
        colnames <- colnames(x)
        names(colnames) <- colmetanames(x)
        colnames["tagname"]
    }
)

setMethod("tagname", "AnnDbBimap",
    function(x) L2Rchain.tagname(x@L2Rchain))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "Rattribnames" and "Rattribnames<-" methods.
###

setMethod("Rattribnames", "Bimap",
    function(x)
    {
        colnames(x)[-seq_along(colmetanames(x))]
    }
)

setMethod("Rattribnames", "AnnDbBimap",
    function(x) L2Rchain.Rattribnames(x@L2Rchain))

setReplaceMethod("Rattribnames", "FlatBimap",
    function(x, value)
    {
        colnames0 <- colnames(x@data)
        if (!is.null(value) && !is.character(value))
            stop("Rattrib names must be a character vector or NULL")
        if (!all(value %in% Rattribnames(x)))
            stop("invalid Rattrib names")
        if (any(duplicated(value)))
            stop("can't assign duplicated Rattrib names")
        ii <- c(seq_along(colmetanames(x)), match(value, colnames0))
        x@data <- x@data[ii]
        if (length(ii) < length(colnames0))
            x@data <- unique(x@data)
        ## Needed because subsetting a data frame can change the names
        ## of its cols (for the duplicated names)
        colnames(x@data) <- colnames0[ii]
        x
    }
)

setReplaceMethod("Rattribnames", "AnnDbBimap",
    function(x, value)
    {
        Rattribnames0 <- Rattribnames(x)
        L2Rchain.Rattribnames(x@L2Rchain) <- value
        if (length(Rattribnames(x)) < length(Rattribnames0))
            x <- as(x, Class="AnnDbBimap", strict=TRUE)
        x
    }
)

setReplaceMethod("Rattribnames", "Go3AnnDbBimap",
    function(x, value)
    {
        stop("can't modify the Rattrib names of a ", class(x), " object")
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "Lkeys", "Rkeys" and "keys" methods.
###

setMethod("Lkeys", "FlatBimap",
    function(x)
    {
        if (length(x@Lkeys) == 1 && is.na(x@Lkeys))
            return(mappedLkeys(x))
        x@Lkeys
    }
)

.inslot.Lkeys <- function(x)
{
    length(x@Lkeys) != 1 || !is.na(x@Lkeys)
}

setMethod("Lkeys", "AnnDbBimap",
    function(x)
    {
        if (.inslot.Lkeys(x))
            return(x@Lkeys)
        dbUniqueVals(dbconn(x), Ltablename(x), Lkeyname(x),
                                Lfilter(x), x@datacache)
    }
)

setMethod("Rkeys", "FlatBimap",
    function(x)
    {
        if (length(x@Rkeys) == 1 && is.na(x@Rkeys))
            return(mappedRkeys(x))
        x@Rkeys
    }
)

.inslot.Rkeys <- function(x)
{
    length(x@Rkeys) != 1 || !is.na(x@Rkeys)
}

setMethod("Rkeys", "AnnDbBimap",
    function(x)
    {
        if (.inslot.Rkeys(x))
            return(x@Rkeys)
        dbUniqueVals(dbconn(x), Rtablename(x), Rkeyname(x),
                                Rfilter(x), x@datacache)
    }
)

setMethod("Rkeys", "Go3AnnDbBimap",
    function(x)
    {
        if (.inslot.Rkeys(x))
            return(x@Rkeys)
        getNames <- function(ontology)
        {
            tablename <- Rtablename(x)[ontology]
            dbUniqueVals(dbconn(x), tablename, "go_id",
                                    Rfilter(x), x@datacache)
        }
        ## Because a given go_id can only belong to 1 of the 3 ontologies...
        ## (if not, then apply unique to this result)
        c(getNames("BP"), getNames("CC"), getNames("MF"))
    }
)

setMethod("Rkeys", "AnnDbMap",
    function(x)
    {
        stop("Rkeys() is not supported for an \"", class(x), "\" object")
    }
)

setMethod("keys", "Bimap",
    function(x)
        switch(as.character(direction(x)),
                "1"=Lkeys(x),
               "-1"=Rkeys(x),
                    stop("keys() is undefined for an undirected bimap"))
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "Lkeys<-", "Rkeys<-" and "keys<-" replacement methods.
###

.checkKeys <- function(keys, valid.keys, ifnotfound)
{
    if (!is.character(keys))
        stop("the keys must be character strings")
    if (length(ifnotfound) == 0) {
        not_found <- which(!(keys %in% valid.keys))
        if (length(not_found) != 0)
            stop("invalid key \"", keys[not_found[1]], "\"")
    }
}

setReplaceMethod("Lkeys", "FlatBimap",
    function(x, value)
    {
        if (!is.null(value)) {
            .checkKeys(value, Lkeys(x), x@ifnotfound)
            if (!is.null(names(value)))
                names(value) <- NULL
            x@Lkeys <- value
            ii <- which(x@data[[1]] %in% value)
            cn <- colnames(x@data)
            x@data <- x@data[ii, ]
            colnames(x@data) <- cn
        }
        x
    }
)

setReplaceMethod("Lkeys", "AnnDbBimap",
    function(x, value)
    {
        if (!is.null(value)) {
            .checkKeys(value, Lkeys(x), x@ifnotfound)
            if (!is.null(names(value)))
                names(value) <- NULL
            x@Lkeys <- value
        }
        x
    }
)

setReplaceMethod("Rkeys", "FlatBimap",
    function(x, value)
    {
        if (!is.null(value)) {
            .checkKeys(value, Rkeys(x), x@ifnotfound)
            if (!is.null(names(value)))
                names(value) <- NULL
            x@Rkeys <- value
            ii <- which(x@data[[2]] %in% value)
            cn <- colnames(x@data)
            x@data <- x@data[ii, ]
            colnames(x@data) <- cn
        }
        x
    }
)

setReplaceMethod("Rkeys", "AnnDbBimap",
    function(x, value)
    {
        if (!is.null(value)) {
            .checkKeys(value, Rkeys(x), x@ifnotfound)
            if (!is.null(names(value)))
                names(value) <- NULL
            x@Rkeys <- value
        }
        x
    }
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "subset" methods.

setMethod("subset", "Bimap",
    function(x, Lkeys=NULL, Rkeys=NULL)
    {
        Lkeys(x) <- Lkeys
        Rkeys(x) <- Rkeys
        x
    }
)

setMethod("subset", "AnnDbBimap",
    function(x, Lkeys=NULL, Rkeys=NULL, objName=NULL)
    {
        ## Call "subset" method for "Bimap" objects
        x <- callNextMethod(x, Lkeys=Lkeys, Rkeys=Rkeys)
        if (!is.null(objName))
            x@objName <- toString(objName)
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "[" methods.
###

### Supported 'i' types: character vector, numeric vector, logical vector,
### NULL and missing.
setMethod("[", "Bimap",
    function(x, i, j, ..., drop)
    {
        if (!missing(j) || length(list(...)) > 0)
            stop("invalid subsetting")
        if (missing(i))
            return(x)
        keys <- keys(x)
        if (is.character(i)) {
            keys(x) <- i
        } else {
            keys(x) <- keys[i]
        }
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "Llength", "Rlength" and "length" methods.
###

setMethod("Llength", "Bimap",
    function(x) length(Lkeys(x)))

setMethod("Llength", "AnnDbBimap",
    function(x)
    {
        if (.inslot.Lkeys(x))
            return(length(x@Lkeys))
        dbCountUniqueVals(dbconn(x), Ltablename(x), Lkeyname(x),
                                     Lfilter(x), x@datacache)
    }
)

setMethod("Rlength", "Bimap",
    function(x) length(Rkeys(x)))

setMethod("Rlength", "AnnDbBimap",
    function(x)
    {
        if (.inslot.Rkeys(x))
            return(length(x@Rkeys))
        dbCountUniqueVals(dbconn(x), Rtablename(x), Rkeyname(x),
                                     Rfilter(x), x@datacache)
    }
)

setMethod("Rlength", "Go3AnnDbBimap",
    function(x)
    {
        if (.inslot.Rkeys(x))
            return(length(x@Rkeys))
        countNames <- function(ontology)
        {
            tablename <- Rtablename(x)[ontology]
            dbCountUniqueVals(dbconn(x), tablename, "go_id", Rfilter(x), x@datacache)
        }
        ## Because a given go_id can only belong to 1 of the 3 ontologies...
        countNames("BP") + countNames("CC") + countNames("MF")
    }
)

setMethod("Rlength", "AnnDbMap",
    function(x)
    {
        stop("Rlength() is not supported for an \"", class(x), "\" object")
    }
)

setMethod("length", "Bimap",
    function(x)
        switch(as.character(direction(x)),
                "1"=Llength(x),
               "-1"=Rlength(x),
                    stop("length() is undefined for an undirected bimap"))
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "isNA" methods.
###

### Like "is.na", "isNA" returns a named logical vector that associates each
### key in the map with TRUE except for those keys that are actually mapped
### to something (other than an NA).
setMethod("isNA", "Bimap",
    function(x)
    {
        mapped_keys <- mappedkeys(x)
        keys <- keys(x)
        ans <- !(keys %in% mapped_keys)
        names(ans) <- keys
        ans
    }
)

### "is.na" on environments has a silly semantic but since it is sealed then
### it can't be redefined.
setMethod("isNA", "environment",
    function(x) is.na(as.list(x, all.names=TRUE))
)

### And for ANY other vector-like object for which an "is.na"
### method is defined (e.g. an environment or a list)
setMethod("isNA", "ANY", function(x) is.na(x))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "mappedLkeys", "mappedRkeys" and "mappedkeys" methods.
###
### Note that for the maps in DB schemas like HUMANCHIP_DB, all the "right
### keys" are expected to be mapped to at least one "left key" hence
### mappedRkeys(x) should be the same as Rkeys(x) for those maps (maybe
### something worth checking in a test unit).
###

setMethod("mappedLkeys", "FlatBimap",
    function(x)
        unique(x@data[[match("Lkeyname", x@colmetanames)]])
)

setMethod("mappedLkeys", "AnnDbBimap",
    function(x)
    {
        dbUniqueMappedKeys(dbconn(x), x@L2Rchain, x@Lkeys, x@Rkeys,
                                      1, x@datacache)
    }
)

setMethod("mappedLkeys", "Go3AnnDbBimap",
    function(x)
    {
        getMappedKeys <- function(ontology)
        {
            tablename <- Rtablename(x)[ontology]
            L2Rchain <- makeGo3L2Rchain(x@L2Rchain, tablename, ontology)
            dbUniqueMappedKeys(dbconn(x), L2Rchain, x@Lkeys, x@Rkeys,
                                          1, x@datacache)
        }
        keys1 <- getMappedKeys("BP")
        keys2 <- getMappedKeys("CC")
        keys3 <- getMappedKeys("MF")
        unique(c(keys1, keys2, keys3))
    }
)

### For an AgiAnnDbMap object (like silly maps ACCNUM and MULTIHIT in
### ARABIDOPSISCHIP_DB), the "mappedLkeys" method for AnnDbBimap objects
### would ignore the x@replace.single and x@replace.multiple slots leading
### to a wrong result when one of those slots is NA.
### But who cares, those maps are silly anyway...
mappedLkeysIsNotAvailable <- function(x)
{
    (length(x@replace.single) == 1 && is.na(x@replace.single)) ||
      (length(x@replace.multiple) == 1 && is.na(x@replace.multiple))
}

setMethod("mappedLkeys", "AgiAnnDbMap",
    function(x)
    {
        if (mappedLkeysIsNotAvailable(x))
            stop("mappedLkeys() is not available for map ", x@objName)
        callNextMethod(x)
    }
)

setMethod("mappedRkeys", "FlatBimap",
    function(x)
        unique(x@data[[match("Rkeyname", x@colmetanames)]])
)

setMethod("mappedRkeys", "AnnDbBimap",
    function(x)
    {
        dbUniqueMappedKeys(dbconn(x), x@L2Rchain, x@Lkeys, x@Rkeys,
                                      -1, x@datacache)
    }
)

setMethod("mappedRkeys", "Go3AnnDbBimap",
    function(x)
    {
        getMappedKeys <- function(ontology)
        {
            tablename <- Rtablename(x)[ontology]
            L2Rchain <- x@L2Rchain
            L2Rchain[[length(L2Rchain)]]@tablename <- tablename
            dbUniqueMappedKeys(dbconn(x), L2Rchain, x@Lkeys, x@Rkeys,
                                          -1, x@datacache)
        }
        keys1 <- getMappedKeys("BP")
        keys2 <- getMappedKeys("CC")
        keys3 <- getMappedKeys("MF")
        ## Because a given go_id can only belong to 1 of the 3 ontologies...
        ## (if not, then apply unique to this result)
        c(keys1, keys2, keys3)
    }
)

setMethod("mappedRkeys", "AnnDbMap",
    function(x)
    {
        stop("mappedRkeys() is not supported for an \"", class(x), "\" object")
    }
)

setMethod("mappedkeys", "Bimap",
    function(x)
        switch(as.character(direction(x)),
                "1"=mappedLkeys(x),
               "-1"=mappedRkeys(x),
                    stop("mappedkeys() is undefined for an undirected bimap"))
)

setMethod("mappedkeys", "environment",
    function(x)
    {
        ## This is needed because the ! operator loses the "names" attributes
        ## when applied on a named vector of length 0
        if (length(x) == 0)
            return(character(0))
        notNA <- !isNA(x)
        names(notNA)[notNA]
    }
)

setMethod("mappedkeys", "vector",
    function(x)
    {
        if (is.null(names(x)))
            stop("mappedkeys() is not defined on an unnamed vector")
        ## This is needed because the ! operator loses the "names" attributes
        ## when applied on a named vector of length 0
        if (length(x) == 0)
            return(character(0))
        notNA <- !isNA(x)
        names(notNA)[notNA]
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "count.mappedLkeys", "count.mappedRkeys" and "count.mappedkeys" methods.
###

setMethod("count.mappedLkeys", "Bimap",
    function(x) length(mappedLkeys(x)))

setMethod("count.mappedLkeys", "AnnDbBimap",
    function(x)
    {
        dbCountUniqueMappedKeys(dbconn(x), x@L2Rchain, x@Lkeys, x@Rkeys,
                                           1, x@datacache)
    }
)

setMethod("count.mappedLkeys", "Go3AnnDbBimap",
    function(x) length(mappedLkeys(x))
)

setMethod("count.mappedLkeys", "AgiAnnDbMap",
    function(x)
    {
        if (mappedLkeysIsNotAvailable(x))
            stop("count.mappedLkeys() is not available for map ", x@objName)
        callNextMethod(x)
    }
)

setMethod("count.mappedRkeys", "Bimap",
    function(x) length(mappedRkeys(x)))

setMethod("count.mappedRkeys", "AnnDbBimap",
    function(x)
    {
        dbCountUniqueMappedKeys(dbconn(x), x@L2Rchain, x@Lkeys, x@Rkeys,
                                           -1, x@datacache)
    }
)

setMethod("count.mappedRkeys", "Go3AnnDbBimap",
    function(x)
    {
        countMappedNames <- function(ontology)
        {
            tablename <- Rtablename(x)[ontology]
            L2Rchain <- makeGo3L2Rchain(x@L2Rchain, tablename, ontology)
            dbCountUniqueMappedKeys(dbconn(x), L2Rchain, x@Lkeys, x@Rkeys,
                                               -1, x@datacache)
        }
        ## Because a given go_id can only belong to 1 of the 3 ontologies...
        countMappedNames("BP") + countMappedNames("CC") + countMappedNames("MF")
    }
)

setMethod("count.mappedRkeys", "AnnDbMap",
    function(x)
    {
        stop("count.mappedRkeys() is not supported for an \"", class(x), "\" object")
    }
)

setMethod("count.mappedkeys", "Bimap",
    function(x)
        switch(as.character(direction(x)),
                "1"=count.mappedLkeys(x),
               "-1"=count.mappedRkeys(x),
                    stop("count.mappedkeys() is undefined for an undirected bimap"))
)

setMethod("count.mappedkeys", "ANY", function(x) length(mappedkeys(x)))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" and "summary" methods.
###

.key.summary <- function(keys, nmapped)
{
    len0 <- length(keys)
    if (len0 == 0)
        return("")
    if (len0 > 2)
        keys <- keys[1:2]
    string <- paste(paste("\"", keys, "\"", sep=""), collapse=", ")
    if (len0 > 2)
        string <- paste(string, ", ...", sep="")
    paste(string, " (total=", len0, "/mapped=", nmapped, ")", sep="")
}

.Bimap.summary <- function(x)
{
    ## Left keys
    cat("| Lkeyname: ", Lkeyname(x), sep="")
    if (is(x, "AnnDbBimap"))
        cat(" (Ltablename: ", Ltablename(x), ")", sep="")
    cat("\n")
    tmp <- mappedLkeys(x) # just to put them in the cache
    cat("|    Lkeys: ", .key.summary(Lkeys(x), count.mappedLkeys(x)), "\n", sep="")
    cat("|\n")

    if (!is(x, "AnnDbMap")) {
        ## Right keys
        cat("| Rkeyname: ", Rkeyname(x), sep="")
        if (is(x, "AnnDbBimap"))
            cat(" (Rtablename: ", Rtablename(x), ")", sep="")
        cat("\n")
        tmp <- mappedRkeys(x) # just to put them in the cache
        cat("|    Rkeys: ", .key.summary(Rkeys(x), count.mappedRkeys(x)), "\n", sep="")
        cat("|\n")
    }

    ## Tag
    if (!is.na(tagname(x)))
        cat("| tagname: ", tagname(x), "\n|\n", sep="")

    ## direction
    direction <- names(.DIRECTION_STR2INT)[.DIRECTION_STR2INT == direction(x)]
    cat("| direction: ", direction, "\n", sep="")
}

.is.submap <- function(x)
{
    .inslot.Lkeys(x) || .inslot.Rkeys(x)
}

setMethod("show", "FlatBimap",
    function(object)
    {
        cat("\"", class(object), "\" object:\n", sep="")
        cat("|\n")
        .Bimap.summary(object)
        cat("\ndata:\n")
        if (nrow(object) <= 20) {
            show(object@data)
        } else {
            show(head(object, n=10))
            cat("...\n")
            cat("(", nrow(object), " rows)\n", sep="")
        }
    }
)

setMethod("show", "AnnDbBimap",
    function(object)
    {
        map <- "map"
        if (.is.submap(object))
            map <- "submap"
        cat(object@objName, " ", map, " for ", object@objTarget,
            " (object of class \"", class(object), "\")\n", sep="")
    }
)

setMethod("summary", "Bimap", function(object) show(object))

setMethod("summary", "AnnDbBimap",
    function(object)
    {
        show(object)
        cat("|\n")
        .Bimap.summary(object)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "toTable" methods.
###

setMethod("toTable", "FlatBimap",
    function(x)
    {
        x@data
    }
)

setMethod("toTable", "AnnDbBimap",
    function(x)
    {
        toTable(flatten(x, fromKeys.only=TRUE))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "head" and "tail" methods.
###
### TODO: Define these methods to AnnDbBimap objects.
###

setMethod("head", "FlatBimap",
    function(x, ...)
    {
        c <- colnames(x)
        y <- head(x@data, ...)
        if (!identical(colnames(y), c))
            colnames(y) <- c
        y
    }
)

setMethod("tail", "FlatBimap",
    function(x, ...)
    {
        c <- colnames(x)
        y <- tail(x@data, ...)
        if (!identical(colnames(y), c))
            colnames(y) <- c
        y
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "nrow" methods.
###

setMethod("nrow", "FlatBimap",
    function(x)
        nrow(x@data)
)

### CURRENTLY BROKEN!
setMethod("nrow", "AnnDbTable",
    function(x)
    {
        dbCountRawAnnDbMapRows(dbconn(x), Ltablename(x), Lkeyname(x), NULL, NULL, x@from)
    }
)

setMethod("nrow", "AnnDbBimap",
    function(x)
    {
        dbCountRowsFromL2Rchain(dbconn(x), x@L2Rchain, x@Lkeys, x@Rkeys)
    }
)

setMethod("nrow", "Go3AnnDbBimap",
    function(x)
    {
        countRows <- function(ontology)
        {
            tablename <- Rtablename(x)[ontology]
            L2Rchain <- makeGo3L2Rchain(x@L2Rchain, tablename, ontology)
            dbCountRowsFromL2Rchain(dbconn(x), L2Rchain, x@Lkeys, x@Rkeys)
        }
        countRows("BP") + countRows("CC") + countRows("MF")
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "links" methods.
###

setMethod("links", "FlatBimap",
    function(x)
    {
        Rattribnames(x) <- NULL
        x@data
    }
)

setMethod("links", "AnnDbBimap",
    function(x)
    {
        Rattribnames(x) <- NULL
        links(flatten(x, fromKeys.only=TRUE))
    }
)

setMethod("links", "Go3AnnDbBimap",
    function(x) links(flatten(x, fromKeys.only=TRUE)))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "count.links" methods.
###

setMethod("count.links", "Bimap",
    function(x)
    {
        Rattribnames(x) <- NULL
        nrow(x)
    }
)

setMethod("count.links", "Go3AnnDbBimap",
    function(x) nrow(x))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "nhit" methods.
###
### TODO: maybe it could be optimized for AnnDbBimap objects with a fast (and
### smart) SQL query (involving COUNT, HAVING or/and GROUP?) that retrieves
### only the strictly necessary stuff.
### Something that would be worth testing (e.g. with unit testing):
###    y <- nhit(x) # y is a named integer vector
###    names(y) should be identical to names(x)
###    sum(y) should be equal to count.links(x)
###

setMethod("nhit", "list",
    function(x)
    {
        sapply(x,
            function(xx)
                if (length(xx) == 1L && is.na(xx)) 0L else length(xx)
        )
    }
)

setMethod("nhit", "Bimap", function(x) nhit(as.list(x)))

setMethod("nhit", "environment",
    function(x)
    {
        #nhit(as.list(x, all.names=TRUE))
        ## Using eapply should be faster than the above (not tested, I'm just
        ## assuming).
        unlist(
            eapply(x,
                function(xx)
                    if (length(xx) == 1 && is.na(xx)) 0L else length(xx),
                all.names=TRUE
            ),
            recursive=FALSE
        )
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "ncol" and "dim" methods.
###

setMethod("ncol", "Bimap",
    function(x) length(colnames(x)))

setMethod("dim", "Bimap",
    function(x) c(nrow(x), ncol(x)))

