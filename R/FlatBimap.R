### =========================================================================
### FlatBimap objects
### -----------------
###

### Possible col metanames are: "Lkeyname", "Rkeyname" and "tagname"
### There must be exactly 1 "Lkeyname" and 1 "Rkeyname" col.
### There can be 0 or 1 "tagname" col.
setMethod("initialize", "FlatBimap",
    function(.Object, colmetanames, direction, data, Lkeys, Rkeys)
    {
        if (!is.character(colmetanames)
         || any(duplicated(colmetanames))
         || !all(colmetanames %in% c("Lkeyname", "Rkeyname", "tagname"))
         || !all(c("Lkeyname", "Rkeyname") %in% colmetanames))
            stop("invalid col metanames")
        if (ncol(data) < length(colmetanames))
            stop("FlatBimap object has not enough columns")
        .Object@colmetanames <- colmetanames
        if (!missing(direction))
            .Object@direction <- .normalize.direction(direction)
        .Object@data <- data
        .Object@Lkeys <- Lkeys
        .Object@Rkeys <- Rkeys
        if (any(duplicated(Rattribnames(.Object))))
           stop("duplicated Rattrib names")
        .Object
    }
)

