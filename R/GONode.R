### =========================================================================
### The GONode class
### ----------------
###
### The "GONode" class is the AnnotationDbi version of the "GOTerms" class
### defined in the annotate package. The reasons we define our own
### GOTerms-like class are (1) to get rid of the trailing "s" in "GOTerms"
### (using a plural form to name a class that can only represent 1 single
### GO node is confusing) and (2) because we can't import the GOTerms class
### from annotate (that's because annotate itself already imports
### AnnotationDbi).
### The global picture is the following:
###   - annotate Imports and Depends on AnnotationDbi.
###   - The definitions of the "GOID", "Term", "Ontology", "Synonym",
###     "Secondary" and "Definition" generics have been moved from annotate
###     to this file.
###   - The corresponding methods for GOTerms objects were kept in annotate.
###   - The new corresponding methods for GONode objects are defined in
###     in this file.
###   - GONode and GOTerms objects should remain semantically equivalent and
###     provide the same interface.
###   - Maybe at some point in the future, the "GOTerms" class should be
###     deprecated in favor of the "GONode" class...
###
### -------------------------------------------------------------------------


setClass("GONode",
    representation(
        GOID="character",       # a single string (mono-valued)
        Term="character",       # a single string (mono-valued)
        Ontology="character",   # a single string (mono-valued)
        Definition="character", # a single string (mono-valued)
        Synonym="character",    # any length including 0 (multi-valued)
        Secondary="character"   # any length including 0 (multi-valued)
    )
)

### The mono-valued slots are also the mandatory slots.
.GONODE_MONOVALUED_SLOTS <- c("GOID", "Term", "Ontology", "Definition")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Initialization.
###

setMethod("initialize", "GONode",
    function(.Object, ...)
    {
        args <- list(...)
        argnames <- names(args)
        if (is.null(argnames) || any(argnames == ""))
            stop("all arguments must be named")
        argnames <- match.arg(argnames, slotNames(.Object), several.ok=TRUE)
        if (!(all(.GONODE_MONOVALUED_SLOTS %in% argnames))) {
            s <- paste(.GONODE_MONOVALUED_SLOTS, collapse=", ")
            stop("arguments ", s, " are mandatory")
        }
        for (i in seq_len(length(args))) {
            argname <- argnames[i]
            value <- args[[i]]
            if ((argname %in% .GONODE_MONOVALUED_SLOTS)) {
                if (length(value) != 1)
                    stop("can't assign ", length(value),
                         " values to mono-valued slot ", argname)
            } else {
                value <- value[!(value %in% c(NA, ""))]
            }
            slot(.Object, argname) <- value
        }
        .Object
    }
)

GONode <- function(...) new("GONode", ...)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "GOID", "Term", "Ontology", "Definition", "Synonym" and "Secondary" 
### generics (accessor methods).
###

setGeneric("GOID", function(object) standardGeneric("GOID")) 
setGeneric("Term", function(object) standardGeneric("Term"))
setGeneric("Ontology", function(object) standardGeneric("Ontology"))
setGeneric("Definition", function(object) standardGeneric("Definition"))
setGeneric("Synonym", function(object) standardGeneric("Synonym"))
setGeneric("Secondary", function(object) standardGeneric("Secondary"))

setMethod("GOID", "GONode", function(object) object@GOID)

setMethod("Term", "GONode", function(object) object@Term)

setMethod("Ontology", "GONode", function(object) object@Ontology)

setMethod("Definition", "GONode", function(object) object@Definition)

setMethod("Synonym", "GONode", function(object) object@Synonym)

setMethod("Secondary", "GONode", function(object) object@Secondary)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" methods.
###

setMethod("show", "GONode",
    function(object)
    {
        s <- character(0)
        for (slotname in slotNames(object)) {
            x <- slot(object, slotname)
            if ((slotname %in% .GONODE_MONOVALUED_SLOTS) && length(x) != 1) {
                warning("mono-valued slot ", slotname,
                        " contains ", length(x), " values")
            } else {
                if (length(x) == 0)
                    next
            }
            s <- c(s, paste(slotname, ": ", x, sep=""))
        }
        cat(strwrap(s, exdent=4), sep="\n")
    }
)

