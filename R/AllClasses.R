### =========================================================================
### The Bimap interface and the FlatBimap class.
### -------------------------------------------------------------------------

### This is just an interface i.e. a virtual class with no slot (a kind of
### Java "interface").
setClass("Bimap", representation("VIRTUAL"))

setClass("FlatBimap",
    contains="Bimap",
    representation(
        colmetanames="character",   # must have a length <= ncol of data slot
        direction="integer",
        data="data.frame",
        Lkeys="character",
        Rkeys="character",
        ifnotfound="list"
    ),
    prototype(
        direction=1L,               # left-to-right by default
        Lkeys=as.character(NA),
        Rkeys=as.character(NA),
        ifnotfound=list()           # empty list => raise an error on first key not found
    )
)



### =========================================================================
### Containers for SQLite-based annotation data.
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "AnnObj" class.
###

setClass("AnnObj",
    representation(
        "VIRTUAL",
        objName="character",
        objTarget="character"      # "chip hgu95av2" or "YEAST" or...
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "AnnDbObj" class.
###

setClass("AnnDbObj",
    contains="AnnObj",
    representation(
        "VIRTUAL",
        ## Used to define objects like "dbconn" or "dbfile" that are shared
        ## by all the AnnDbObj objects defined on top of the same database
        ## (like all the AnnDbObj objects in a given ann db package).
        datacache="environment" 
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "AnnDbTable" class.
###
### WARNING: THIS CLASS IS CURRENTLY BROKEN!
###

setClass("AnnDbTable",
    contains="AnnDbObj",
    representation(
        leftTable="character",
        leftCol="character",
        from="character",
        showCols="character"    # cols to show in addition to the left col
    )
)



### =========================================================================
### Containers for SQLite-based annotation maps.
### -------------------------------------------------------------------------


setClass("L2Rlink",
    representation(
        tablename="character",              # length 1
        Lcolname="character",               # left col (length 1)
        tagname="character",                # tag col (length 1 + name)
        Rcolname="character",               # right col (length 1)
        Rattribnames="character",           # right attrib cols (length n + names)
        Rattrib_join="character",           # right attrib join (length 1, SQL string)
        filter="character",                 # filter (length 1, SQL string)
        altDB="character"                   # optional alternate DB (string of DB name)
    ),
    prototype(
        tagname=as.character(NA),
        Rattrib_join=as.character(NA),
        filter="1"
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "AnnDbBimap" class and subclasses.
###
### An AnnDbBimap object is a directed mapping between left values and
### right values. The direction of the mapping is "left-to-right" or
### "right-to-left".
### If the mapping is "left-to-right", the left values are the keys of the
### map and are retrieved with the "keys" or "ls" methods. The type, format
### and location in the DB of the right values depend on the particular
### subclass of the AnnDbBimap object.
###

setClass("AnnDbBimap",
    contains=c("Bimap", "AnnDbObj"),
    representation(
        L2Rchain="list",            # list of L2Rlink objects
        direction="integer",        # 1L for left-to-right, -1L for right-to-left
                                    # and 0L for undirected
        Lkeys="character",
        Rkeys="character",
        ifnotfound="list"
    ),
    prototype(
        direction=1L,               # left-to-right by default
        Lkeys=as.character(NA),
        Rkeys=as.character(NA),
        ifnotfound=list()           # empty list => raise an error on first key not found
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Inparanpoid Bimap
###
### This class is just needed so we can encapsulate some methods that are
### required for this more specialized set of packages.
setClass("InpAnnDbBimap", contains="AnnDbBimap")



### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### GO-related bimaps.
###

### For a GoAnnDbBimap object, the right values are named lists of GO nodes,
### each GO node being represented as a 3-element list of the form
###   list(GOID="GO:0006470" , Evidence="IEA" , Ontology="BP")
setClass("GoAnnDbBimap", contains="AnnDbBimap")

### Like "GoAnnDbBimap" but the right table is splitted in 3 parts.
setClass("Go3AnnDbBimap",
    contains="GoAnnDbBimap",
    representation(
        rightTables="character"
    )
)

### For a GOTermsAnnDbBimap object, the right values are GONode objects.
setClass("GOTermsAnnDbBimap", contains="AnnDbBimap")



### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Non reversible bimaps (hence we can just call them "maps").
###

setClass("AnnDbMap",
    contains="AnnDbBimap",
    representation(
        rightColType="character"
    )
)

setClass("IpiAnnDbMap", contains="AnnDbMap")

### We need 2 additional slots ('replace.single' and 'replace.multiple') to
### deal with silly maps ACCNUM/ENTREZID/MULTIHIT in the
### ARABIDOPSISCHIP_DB schema. These maps are complementary maps that both
### map probeset ids to AGI locus ids (note that the name of the maps doesn't
### help): in the ENTREZID map, probeset ids that have multiple matches are
### mapped to "multiple", and in the MULTIHIT map, probeset ids that have <= 1
### match are mapped to NAs. Sooooo:
### - for ENTREZID: don't set replace.single (default is character(0)),
###                 use replace.multiple="multiple",
### - for MULTIHIT: use replace.single=NA,
###                 don't set replace.multiple (default is character(0)),
setClass("AgiAnnDbMap",
    contains="AnnDbMap",
    representation(
        replace.single="character",
        replace.multiple="character"
    )
)



### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### probe Bimaps
###
### These classes is are needed so that we can more cleanly separate the probe
### mappings along with their more unusual Lkeys and Rkeys methods...
setClass("ProbeAnnDbBimap", contains="AnnDbBimap")
setClass("ProbeAnnDbMap", contains="AnnDbMap")
setClass("ProbeIpiAnnDbMap", contains="IpiAnnDbMap")
setClass("ProbeGo3AnnDbBimap", contains="Go3AnnDbBimap")



### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### GoFrames
###
### for people who cannot have bimaps but need a way to pass around their data
###

setClass("AnnotFrame", representation("VIRTUAL") )

## For standard GO mappings.
setClass("GOFrame", contains="AnnotFrame",
         representation(data="data.frame",
                        organism="character"),
         prototype=prototype(organism=""))

## For 'GO 2 ALL' style mappings
setClass("GOAllFrame", contains="GOFrame")                                    


## For standard KEGG mappings.
setClass("KEGGFrame", contains="AnnotFrame",
         representation(data="data.frame",
                        organism="character"),
         prototype=prototype(organism=""))









##############################################################################
## Annotations will be the base virtual class for all the annotation packages
## in the project.
.AnnotationDb <-
    setRefClass("AnnotationDb",
        fields=list(conn="SQLiteConnection", packageName="character"),
                )

.OrgDb <-
    setRefClass("OrgDb", contains="AnnotationDb")

.ChipDb <-
    setRefClass("ChipDb", contains="AnnotationDb")

.InparanoidDb <-
    setRefClass("InparanoidDb", contains="AnnotationDb")

.GODb <-
    setRefClass("GODb", contains="AnnotationDb")

.ReactomeDb <-
    setRefClass("ReactomeDb", contains="AnnotationDb")




##############################################################################
## We also need a class that will be "like" a Bimap (in that it can be passed
## to all the Bimap classes, but that in reality will just be an AnnotationDb
## object along with a single value for the cols argument.  The purpose of
## this is primarily to allow getAnnMap() from annotate to be able to return a
## value when no Bimaps are defined, (but when there is a select method
## defined) and to have things basically still work for the retro code.

setClass("AnnotationDbMap",
    representation(
        AnnotDb="AnnotationDb",
        cols="character",
        direction="integer"
    ),
    prototype(
        direction=1L
    )
)

## ## Methods that are mission critical (for the release)

## ## from R/AnnDbBimap-envirAPI.R
##     "ls",                        ## DONE
##     "mget",                      ## DONE
##     "eapply",                    ## 
##     "get",
##     "exists",
##     "[[",
##     "$",
##     "contents",
##     "sample",

## ## from R/BimapFormatting.R
##     "as.list",                    ## DONE
##     "as.character",               ## DONE


## ## from R/Bimap.R (the Bimap interface)
##     "direction", "direction<-",   ## in progress
##     "revmap",                     ## in progress
##     "head", "tail",
##     "subset",
##     "[",
##     "toTable","as.data.frame",


## ## Methods that I should get done soon as possible (but less urgent)

## ## from R/Bimap.R (the Bimap interface)
##     "colnames", "colmetanames",
##     "Lkeyname", "Rkeyname", "keyname",
##     "tagname",
##     "Rattribnames", "Rattribnames<-",
##     "Lkeys", "Rkeys", "keys",
##     "Lkeys<-", "Rkeys<-", "keys<-",
##     "Llength", "Rlength", "length",
##     "isNA",
##     "mappedLkeys", "mappedRkeys", "mappedkeys",
##     "count.mappedLkeys", "count.mappedRkeys", "count.mappedkeys",
##     "show",
##     "summary",
##     "nrow",
##     "nhit",
##     "ncol", "dim",
##     "getBimapFilters","setInpBimapFilter",

## ## from R/AnnDbObj-lowAPI.R
##     "dbconn",
##     "dbfile",
##     "dbmeta",
##     "dbschema",
##     "dbInfo",


## ## Methods that I fear may not ever make sense (or which may be unfeasible)

## ## from R/Bimap.R (the Bimap interface)
##     "toggleProbes",
##     "hasMultiProbes","hasSingleProbes",

##     "links",
##     "count.links",
