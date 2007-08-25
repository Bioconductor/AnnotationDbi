### =========================================================================
### The bimap concept and the Bimap interface
### -----------------------------------------


### -------------------------------------------------------------------------
### The bimap concept
### -----------------
###
### Example of a bimap M:
###
###   4 objects on the left (left keys): a, b, c, d
###   2 objects on the right (right keys): A, B, C
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
### A FlatBimap object is a bimap whose data (left keys, right keys and
### links) are stored in memory (in a data frame for the links).
### A AnnDbBimap object is a bimap whose data are stored in a data base.
### Conceptually, a FlatBimap and a AnnDbBimap object are the same (only
### their internal representation differ) so it's natural to try to define
### a set of methods that make sense for both (so they can be manipulated
### in a similar way). This common interface is the "Bimap" interface.
###
### Note that there is an important asymetry between these two classes:
###   A AnnDbBimap object can be converted into a FlatBimap object
###   but a FlatBimap object can't be converted into an AnnDbBimap
###   object (well, in theory maybe it could be, but for now the data bases
###   we use to store the data of the AnnDbBimap objects are treated as
###   read-only). This conversion from AnnDbBimap to FlatBimap is performed
###   by the "flatten" generic function (with methods for AnnDbBimap objects
###   only).
### The "flatten" generic (with methods for AnnDbBimap objects only) and the
### "subset" generic (with methods for AnnDbBimap and FlatBimap objects) play
### the following central roles:
###   1. flatten(x) converts AnnDbBimap object x into FlatBimap object y with
###      no loss of information.
###   2. If x is an AnnDbBimap object and f a Bimap generic, then f is
###      expected "to do the same thing" for AnnDbBimap and FlatBimap objects.
###      More precisely, this means that for any AnnDbBimap object x, we
###      expect f(x) to be identical to f(flatten(x)). We call this property
###      Property0.
### The checkProperty0() function (AnnDbPkg-checker.R file) checks that
### Property0 is satisfied on all the AnnDbBimap objects defined in a given
### package.
###
### Both AnnDbBimap and FlatBimap objects have a read-only semantic: the user
### can subset them but cannot change their data.
### More about "subset" soon...
###
### -------------------------------------------------------------------------


### KEEP THIS IN SYNC WITH THE STATE OF AFFAIRS! Only methods of the first and
### second group go here.
Bimap_methods <- c(
    ## GROUP 1: 8 methods that _must_ be defined for FlatBimap objects
    ## _and_ AnnDbBimap objects
    "collabels",
    "colnames",
    "direction",
    "direction<-",
    "left.keys", "right.keys",
    "left.keys<-", "right.keys<-",
    "subset",
    "left.mappedKeys", "right.mappedKeys",
    "nrow",
    "links",
    "left.toList", "right.toList",
    ## GROUP 2: Methods for which a default is provided (in this file) but
    ## some of them are redefined for AnnDbBimap objects to obtain better
    ## performance
    "left.length", "right.length",
    "count.left.mappedKeys", "count.right.mappedKeys",
    "count.links",
    "left.colname", "right.colname",
    ## GROUP 3: Directed methods (i.e. what they return depends on the
    ## direction of the map). All what they do is to dispatch on the
    ## corresponding undirected method according to the value of direction(x)
    "keys",
    "length",
    "mappedKeys",
    "count.mappedKeys",
    "toList"
)

### A virtual class with no slot (a kind of equivalent to a Java "interface")
setClass("Bimap", representation("VIRTUAL"))

setMethod("left.length", "Bimap",
    function(x) length(left.keys(x)))
setMethod("right.length", "Bimap",
    function(x) length(right.keys(x)))

setMethod("count.left.mappedKeys", "Bimap",
    function(x) length(left.mappedKeys(x)))
setMethod("count.right.mappedKeys", "Bimap",
    function(x) length(right.mappedKeys(x)))

setMethod("count.links", "Bimap",
    function(x) nrow(links(x)))

setMethod("ncol", "Bimap",
    function(x) length(colnames(x)))

### left-to-right: direction =  1
### right-to-left: direction = -1
setMethod("from.colpos", "Bimap",
    function(x, direction)
    {
        if (direction == 1) side = "left" else side = "right"
        match(side, collabels(x))
    }
)
setMethod("to.colpos", "Bimap",
    function(x, direction) from.colpos(x, - direction))

setMethod("left.colname", "Bimap",
    function(x) colnames(x)[from.colpos(x,  1)])
setMethod("right.colname", "Bimap",
    function(x) colnames(x)[from.colpos(x, -1)])

### FIXME
### For now, tags and attributes are all mixed together which is bad and will
### cause problems when reversing some maps or when plugging maps together.
### We need to make a clear distinction between tags and attributes but this
### will require to change the current L2Rbrick class and make things more
### complicated...
setMethod("tags.colpos", "Bimap",
    function(x) seq_len(ncol(x))[-c(from.colpos(x, 1), from.colpos(x, -1))])

setMethod("from.keys", "Bimap",
    function(x, direction) if (direction == 1) left.keys(x) else right.keys(x))
setMethod("to.keys", "Bimap",
    function(x, direction) from.keys(x, - direction))

setMethod("dim", "Bimap",
    function(x) c(nrow(x), ncol(x)))

### Directed methods

.DIRECTION_STR2INT <- c("left-to-right"=1L, "right-to-left"=-1L, "undirected"=0L)

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
                "1"=left.keys(x),
               "-1"=right.keys(x),
                    stop("keys() is undefined for an undirected bimap"))
)
setReplaceMethod("keys", "Bimap",
    function(x, value)
    {
        switch(as.character(direction(x)),
                "1"=`left.keys<-`(x, value),
               "-1"=`right.keys<-`(x, value),
                    stop("keys<- is undefined for an undirected bimap"))
    }
)
setMethod("length", "Bimap",
    function(x)
        switch(as.character(direction(x)),
                "1"=left.length(x),
               "-1"=right.length(x),
                    stop("length() is undefined for an undirected bimap"))
)
setMethod("mappedKeys", "Bimap",
    function(x)
        switch(as.character(direction(x)),
                "1"=left.mappedKeys(x),
               "-1"=right.mappedKeys(x),
                    stop("mappedKeys() is undefined for an undirected bimap"))
)
setMethod("count.mappedKeys", "Bimap",
    function(x)
        switch(as.character(direction(x)),
                "1"=count.left.mappedKeys(x),
               "-1"=count.right.mappedKeys(x),
                    stop("count.mappedKeys() is undefined for an undirected bimap"))
)
setMethod("toList", "Bimap",
    function(x, keys=NULL)
        switch(as.character(direction(x)),
                "1"=left.toList(x, keys),
               "-1"=right.toList(x, keys),
                    stop("toList() is undefined for an undirected bimap"))
)

