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
    contains=c("AnnDbObj", "Bimap"),
    representation(
        L2Rpath="list",             # list of L2Rbrick objects
        direction="integer",        # 1L for left-to-right, -1L for right-to-left
                                    # and 0L for undirected
        left.keys="character",
        right.keys="character",
        ifnotfound="list"
    ),
    prototype(
        direction=1L,               # left-to-right by default
        left.keys=as.character(NA),
        right.keys=as.character(NA),
        ifnotfound=list()           # empty list => raise an error on first key not found
    )
)

### The most common type of bimap.
### For an AtomicAnnDbBimap object, the right values are character vectors.
### FIXME: This subclass should not be needed, use AnnDbBimap instead!
setClass("AtomicAnnDbBimap", contains="AnnDbBimap")


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

### For a GONodeAnnDbBimap object, the right values are GONode objects.
setClass("GONodeAnnDbBimap", contains="AnnDbBimap")


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

