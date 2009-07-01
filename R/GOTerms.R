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


##.GOid2go_termField() retrieves ids of type field from go_term
.GOid2go_termField <- function(ids, field){
    require("GO.db")
##     message(cat("Before SQL \n")) ##test
    sql <- sprintf("SELECT go_id, %s
                    FROM go_term
                    WHERE go_id IN ('%s')",
                   field,
                   paste(ids, collapse="','"))
    res <- dbGetQuery(GO_dbconn(), sql)
    if(dim(res)[1]==0 && dim(res)[2]==0){
        stop("None of your IDs match IDs from GO.  Are you sure you have valid IDs?")
    }else{
        ans <- res[[2]]
        names(ans) <- res[[1]]
        return(ans[ids]) ##This only works because each GO ID is unique (and therefore a decent index ID)
    }
}

setMethod("GOID", "GOTermsAnnDbBimap",function(object) .GOid2go_termField(keys(object),"go_id") )
setMethod("GOID", "character",function(object) .GOid2go_termField(object,"go_id") )

setMethod("Term", "GOTermsAnnDbBimap",function(object) .GOid2go_termField(keys(object),"term") )
setMethod("Term", "character",function(object) .GOid2go_termField(object,"term") )

setMethod("Ontology", "GOTermsAnnDbBimap",function(object) .GOid2go_termField(keys(object),"ontology") )
setMethod("Ontology", "character",function(object) .GOid2go_termField(object,"ontology") )

setMethod("Definition", "GOTermsAnnDbBimap",function(object) .GOid2go_termField(keys(object),"definition") )
setMethod("Definition", "character",function(object) .GOid2go_termField(object,"definition") )


##.GOid2go_synonymField() retrieves ids of type field from go_synonym
.GOid2go_synonymField <- function(ids, field){
    require("GO.db")
    sql <- paste("SELECT gt.go_id, gs.",field,"
                  FROM go_term AS gt, go_synonym AS gs
                  WHERE gt._id=gs._id AND go_id IN ('",paste(ids, collapse="','"),"')", sep="")
    res <- dbGetQuery(GO_dbconn(), sql)
    if(dim(res)[1]==0 && dim(res)[2]==0){
        stop("None of your IDs match IDs from GO.  Are you sure you have valid IDs?")
    }else{
        ans = split(res[,2],res[,1])
        return(ans[ids])##once again (this time in list context), we are indexing with unique IDs.
    }
}

setMethod("Synonym", "GOTermsAnnDbBimap",function(object) .GOid2go_synonymField(keys(object),"synonym") )
setMethod("Synonym", "character",function(object) .GOid2go_synonymField(object,"synonym") )

setMethod("Secondary", "GOTermsAnnDbBimap",function(object) .GOid2go_synonymField(keys(object),"secondary") )
setMethod("Secondary", "character",function(object) .GOid2go_synonymField(object,"secondary") )




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

