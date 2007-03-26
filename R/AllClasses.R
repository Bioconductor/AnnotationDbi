### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Containers for SQLite-based annotation data.
###

setClass("AnnObj",
    representation(
        "VIRTUAL",
        objName="character",
        objTarget="character",  # "chip hgu95av2" or "YEAST" or...
        datacache="environment",
        conn="DBIConnection",
        leftTable="character",
        leftCol="character"
    )
)

setClass("AnnTable",
    contains="AnnObj",
    representation(
        from="character",
        showCols="character"    # cols to show in addition to the left col
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Containers for SQLite-based annotation maps.
###

### An "AnnMap" object is a mapping between left values and right values.
### The left values are the strings stored in the SQL col 'leftCol' of
### table 'leftTable'.
### For direct "AnnMap" objects, the mapping is "left-to-right". The left
### values are the names (or symbols, or keys) of the map and are retrieved
### with the "names" or "ls" methods. The type, format and location in the
### DB of the right values depend on the particular subclass of the "AnnMap"
### object. For reverse "AnnMap" objects, the mapping is "right-to-left".
setClass("AnnMap",
    contains="AnnObj",
    representation(
        rightTable="character",
        rightCol="character",
        rightColType="character",
        tagCols="character",
        join="character"
    )
)

setClass("ReverseAnnMap", representation("VIRTUAL"))

### For a "AtomicAnnMap" object, the right values are unnamed atomic vectors
### (character or integer).
### The 2 additional slots ('replace.single' and 'replace.multiple') allow dealing
### with silly maps ENTREZID and MULTIHIT in AG_DB schema: they are complementary
### maps that both map probeset ids to Entrez ids. In the ENTREZID map, probeset
### ids that have multiple matches are mapped to "multiple". In the MULTIHIT
### map, probeset ids that have <= 1 match are mapped to NAs. Sooooo:
###   - for ENTREZID: don't set replace.single (default is character(0)),
###                   use replace.multiple="multiple",
###   - for MULTIHIT: use replace.single=NA,
###                   don't set replace.multiple (default is character(0)),
###   - for any other map: don't set those fields (defaults will be just fine).
setClass("AtomicAnnMap",
    contains="AnnMap",
    representation(
        replace.single="character",
        replace.multiple="character"
    )
)

### DON'T ADD ANY SLOT HERE! A given AnnMap subclass and its corresponding
### "reverse" class should always have exactly the same slots.
setClass("ReverseAtomicAnnMap", contains=c("ReverseAnnMap", "AtomicAnnMap"))

### No "reverse" class for the IPIAnnMap class.
setClass("IPIAnnMap", contains="AnnMap")


### For a "GOAnnMap" object, the right values are named lists of GO nodes,
### each GO node being represented as a 3-element list of the form
###   list(GOID="GO:0006470" , Evidence="IEA" , Ontology="BP")
setClass("GOAnnMap", contains="AnnMap")

### Maps a GO term to a named character vector containing left values tagged
### with the Evidence code.
### DON'T ADD ANY SLOT HERE! (Why? See "ReverseAtomicAnnMap" def above.)
setClass("ReverseGOAnnMap", contains=c("ReverseAnnMap", "GOAnnMap"))

