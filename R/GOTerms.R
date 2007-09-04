setClass("GOTerms",
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

setMethod("initialize", "GOTerms",
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

GOTerms <- function(GOId, term, ontology, synonym = "", secondary = "",
                    definition = ""){
    return(new("GOTerms", GOID = GOId, Term = term,
               Synonym = synonym, Secondary = secondary,
               Definition = definition, Ontology = ontology))
}


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

setMethod("GOID", "GOTerms", function(object) object@GOID)

setMethod("Term", "GOTerms", function(object) object@Term)

setMethod("Ontology", "GOTerms", function(object) object@Ontology)

setMethod("Definition", "GOTerms", function(object) object@Definition)

setMethod("Synonym", "GOTerms", function(object) object@Synonym)

setMethod("Secondary", "GOTerms", function(object) object@Secondary)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" methods.
###

setMethod("show", "GOTerms",
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

