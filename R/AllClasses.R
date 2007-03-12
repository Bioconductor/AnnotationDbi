### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Classes representing SQLite-based annotation data.
###

setClass("AnnObject",
    representation(
        "VIRTUAL",
        leftCol="character",
        leftTable="character",
        conn="DBIConnection",
        datacache="environment",
        objTarget="character", # "chip hgu95av2" or "YEAST" or...
        objName="character"
    )
)

setClass("AnnTable",
    contains="AnnObject",
    representation(
        rightTable="character",
        join="character"
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Classes representing SQLite-based annotation maps.
###

### An "AnnMap" object is a mapping between left values and right values.
### The left values are the strings stored in the SQL col 'leftCol' of
### table 'leftTable'.
### For direct "AnnMap" objects, the mapping is "left-to-right". The left
### values are the names (or symbols, or keys) of the map and are retrieved
### with the "names" or "ls" methods. The type, format and location in the
### DB of the right values depends on the particular subclass of the "AnnMap"
### object. For reverse "AnnMap" objects, the mapping is "right-to-left".
setClass("AnnMap",
    contains="AnnObject",
    representation(
        "VIRTUAL",
        join="character"
    )
)

setClass("ReverseAnnMap", representation("VIRTUAL"))

### For a "AtomicAnnMap" object, the right values are unnamed atomic vectors
### (character or integer).
### The last 2 slots ('replace.single' and 'replace.multiple') allow dealing
### with silly maps ENTREZID and MULTIHIT in AGDB schema: they are complementary
### maps that both map probeset ids to Entrez ids. In the ENTREZID map, probeset
### ids that have multiple matches are mapped to "multiple". In the MULTIHIT
### map, probeset ids that have <= 1 match are mapped to NAs. Sooo:
###   - for ENTREZID: don't set replace.single (default is character(0)),
###                   use replace.multiple="multiple",
###   - for MULTIHIT: use replace.single=NA,
###                   don't set replace.multiple (default is character(0)),
###   - for any other map: don't set those fields (defaults will be just fine).
setClass("AtomicAnnMap",
    contains="AnnMap",
    representation(
        rightTable="character",
        rightCol="character",
        ## set only if the right names are tagged
        tagCol="character", 
        ## set only if the right names need coercion after extraction
        rightColType="character",
        replace.single="character",
        replace.multiple="character"
    )
)

### DON'T ADD ANY SLOT HERE! A given AnnMap subclass and its corresponding
### "reverse" class should always have exactly the same slots.
setClass("ReverseAtomicAnnMap", contains=c("ReverseAnnMap", "AtomicAnnMap"))

### For a "GOAnnMap" object, the right values are named lists of GO nodes,
### each GO node being represented as a 3-element list of the form
###   list(GOID="GO:0006470" , Evidence="IEA" , Ontology="BP")
setClass("GOAnnMap",
    contains="AnnMap",
    representation(all="logical")
)

### Maps a GO term to a named character vector containing left values tagged
### with the Evidence code.
### DON'T ADD ANY SLOT HERE! (Why? See "ReverseAtomicAnnMap" def above.)
setClass("ReverseGOAnnMap", contains=c("ReverseAnnMap", "GOAnnMap"))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setClass(
    "AnnDataPkgSeed",
    representation(
        pkg.template="character",    # e.g. "HGU95AV2DB"
        dbSchema="character",        # e.g. "HGU95AV2DB"
        objNamePrefix="character",   # e.g. "hgu95av2"
        objTarget="character",       # e.g. "chip hgu95av2"
        organism="character",
        species="character",
        manufacturer="character",
        chipName="character",
        manufacturerUrl="character"
    )
)

