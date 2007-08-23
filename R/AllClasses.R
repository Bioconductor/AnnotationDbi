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
        objTarget="character"   # "chip hgu95av2" or "YEAST" or...
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "AnnDbObj" class.
###

setClass("AnnDbObj",
    contains="AnnObj",
    representation(
        "VIRTUAL",
        datacache="environment",
        conn="DBIConnection"
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "AnnDbMap" class.
###
### An "AnnDbMap" object is a mapping between left values and right values.
### For direct "AnnDbMap" objects, the mapping is "left-to-right". The left
### values are the keys of the map and are retrieved with the "keys" or "ls"
### methods. The type, format and location in the DB of the right values
### depend on the particular subclass of the "AnnDbMap" object.
### For reverse "AnnDbMap" objects, the mapping is "right-to-left".
###

setClass("L2Rbrick",
    representation(
        table="character",
        Lcolname="character",
        Rcolname="character",
        tagJoin="character",
        tagCols="character",
        filter="character"
    ),
    prototype(
        tagJoin=as.character(NA),
        tagCols=as.character(NA),
        filter="1"
    )
)

setClass("AnnDbMap",
    contains=c("AnnDbObj", "BimapAPI0"),
    representation(
        L2Rpath="list",             # list of L2Rbrick objects
        rightColType="character"
    )
)

setClass("RevAnnDbMap", representation("VIRTUAL"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "AtomicAnnDbMap" class.
###
### For a "AtomicAnnDbMap" object, the right values are unnamed atomic vectors
### (character or integer).
### The 2 additional slots ('replace.single' and 'replace.multiple') allow
### dealing with silly maps ENTREZID and MULTIHIT in ARABIDOPSISCHIP_DB schema:
### they are complementary maps that both map probeset ids to Entrez ids.
### In the ENTREZID map, probeset ids that have multiple matches are mapped
### to "multiple". In the MULTIHIT map, probeset ids that have <= 1 match are
### mapped to NAs. Sooooo:
###   - for ENTREZID: don't set replace.single (default is character(0)),
###                   use replace.multiple="multiple",
###   - for MULTIHIT: use replace.single=NA,
###                   don't set replace.multiple (default is character(0)),
###   - for any other map: don't set those fields (defaults will be just fine).
###

setClass("AtomicAnnDbMap",
    contains="AnnDbMap",
    representation(
        replace.single="character",
        replace.multiple="character"
    )
)

### DON'T ADD ANY SLOT HERE! A given AnnDbMap subclass and its corresponding
### "reverse" class should always have exactly the same slots.
setClass("RevAtomicAnnDbMap", contains=c("RevAnnDbMap", "AtomicAnnDbMap"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "IpiAnnDbMap" class.
###
### No "reverse" class for the IpiAnnDbMap class.
###

setClass("IpiAnnDbMap", contains="AnnDbMap")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "GoAnnDbMap" class.
###
### For a "GoAnnDbMap" object, the right values are named lists of GO nodes,
### each GO node being represented as a 3-element list of the form
###   list(GOID="GO:0006470" , Evidence="IEA" , Ontology="BP")
###

setClass("GoAnnDbMap", contains="AnnDbMap")

### Maps a GO term to a named character vector containing left values tagged
### with the Evidence code.
### DON'T ADD ANY SLOT HERE! (Why? See "RevAtomicAnnDbMap" def above.)
setClass("RevGoAnnDbMap", contains=c("RevAnnDbMap", "GoAnnDbMap"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "Go3AnnDbMap" class.
###
### Like "GoAnnDbMap" but the right table is splitted in 3 parts.
###

setClass("Go3AnnDbMap",
    contains="GoAnnDbMap",
    representation(
        rightTables="character"
    )
)

### DON'T ADD ANY SLOT HERE! (Why? See "RevAtomicAnnDbMap" def above.)
setClass("RevGo3AnnDbMap", contains=c("RevAnnDbMap", "Go3AnnDbMap"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "GONodeAnnDbMap" class.
###
### For a "GONodeAnnDbMap" object, the right values are GONode objects.
###

setClass("GONodeAnnDbMap", contains="AnnDbMap")

### DON'T ADD ANY SLOT HERE! (Why? See "RevAtomicAnnDbMap" def above.)
setClass("RevGONodeAnnDbMap", contains=c("RevAnnDbMap", "GONodeAnnDbMap"))

