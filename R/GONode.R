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
        GOID="character",
        Term="character",
        Ontology="character",
        Definition="character",
        Synonym="character",
        Secondary="character"
    ),
    prototype(
        #Definition=as.character(NA),
        Synonym=as.character(NA),
        Secondary=as.character(NA)
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor-like function.
###

GONode <- function(GOID, Term, Ontology, Definition,
                   Synonym=NA, Secondary=NA)
{
    if (!is.character(Synonym))
        Synonym <- as.character(Synonym)
    if (!is.character(Secondary))
        Secondary <- as.character(Secondary)
    new("GONode", GOID=GOID, Term=Term,
                  Ontology=Ontology, Definition=Definition,
                  Synonym=Synonym, Secondary=Secondary)
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
        if (!is.na(GOID(object)[1]))
            s <- c(s, paste("GOID:", GOID(object)))
        if (!is.na(Term(object)[1]))
            s <- c(s, paste("Term:", Term(object)))
        if (!is.na(Ontology(object)[1]))
            s <- c(s, paste("Ontology:", Ontology(object)))
        if (!is.na(Definition(object)[1]))
            s <- c(s, paste("Definition:", Definition(object)))
        if (!is.na(Synonym(object)[1]))
            s <- c(s, paste("Synonym:", Synonym(object)))
        if (!is.na(Secondary(object)[1]))
            s <- c(s, paste("Secondary:", Secondary(object)))
        cat(strwrap(s, exdent=4), sep="\n")
    }
)

