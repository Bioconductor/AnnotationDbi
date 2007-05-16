### =========================================================================
### The GOTerm class
### ----------------
###
### The "GOTerm" class is the AnnotationDbi version of the "GOTerms" class
### defined in the annotate package. The reasons we define our own
### GOTerms-like class are (1) to get rid of the trailing "s" in "GOTerms"
### (using a plural form to name a class that can only represent 1 single
### GO term is confusing) and (2) because we can't import the GOTerms class
### from annotate (that's because annotate itself already imports
### AnnotationDbi).
### The global picture is the following:
###   - annotate Imports and Depends on AnnotationDbi.
###   - The definitions of the "GOID", "Term", "Ontology", "Synonym",
###     "Secondary" and "Definition" generics have been moved from annotate
###     to this file.
###   - The corresponding methods for GOTerms objects were kept in annotate.
###   - The new corresponding methods for GOTerm objects are defined in
###     in this file.
###   - GOTerm and GOTerms objects should remain semantically equivalent and
###     provide the same interface.
###   - Maybe at some point in the future, the "GOTerms" class should be
###     deprecated in favor of the "GOTerm" class...
###
### -------------------------------------------------------------------------

setClass("GOTerm",
    representation(
        GOID="character",
        Term="character",
        Ontology="character",
        Synonym="character",
        Secondary="character",
        Definition="character"
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor-like function.
###

GOTerm <- function(GOId, term, ontology,
                   synonym="", secondary="", definition="")
{
    new("GOTerm", GOID=GOId, Term=term, Ontology=ontology,
                  Synonym=synonym, Secondary=secondary, Definition=definition)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "GOID", "Term", "Ontology", "Synonym", "Secondary" and "Definition"
### generics (accessor methods).
###

setGeneric("GOID", function(object) standardGeneric("GOID")) 
setGeneric("Term", function(object) standardGeneric("Term"))
setGeneric("Ontology", function(object) standardGeneric("Ontology"))
setGeneric("Synonym", function(object) standardGeneric("Synonym"))
setGeneric("Secondary", function(object) standardGeneric("Secondary"))
setGeneric("Definition", function(object) standardGeneric("Definition"))

setMethod("GOID", "GOTerm", function(object) object@GOID)

setMethod("Term", "GOTerm", function(object) object@Term)

setMethod("Ontology", "GOTerm", function(object) object@Ontology)
setMethod("Ontology", signature="ANY", 
    function(object) if (is.na(object)) NA else callNextMethod()
)

setMethod("Synonym", "GOTerm", function(object) object@Synonym)

setMethod("Secondary", "GOTerm", function(object) object@Secondary)

setMethod("Definition", "GOTerm", function(object) object@Definition)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" methods.
###

setMethod("show", "GOTerm",
    function(object)
    {
        s <- character(0)
        if (!is.na(GOID(object)))
            s <- c(s, paste("GOID =", GOID(object)), "")
        if (!is.na(Term(object)))
            s <- c(s, paste("Term =", Term(object)), "")
        if (!is.na(Ontology(object)))
            s <- c(s, paste("\nOntology =", Ontology(object)), "")
        if (!is.na(Synonym(object)[1]))
            s <- c(s, paste("\nSynonym =", Synonym(object)), "")
        if (!is.na(Secondary(object)[1]))
            s <- c(s, paste("\nSecondary =", Secondary(object)), "")
        if (!is.na(Definition(object)))
            s <- c(s, paste("\nDefinition =", Definition(object)), "")
        cat(strwrap(s, exdent=5), sep="\n")
    }
)

