### Bimap interface
### (Please don't sort this section in alphabetical order)
setGeneric("direction", function(x) standardGeneric("direction"))
setGeneric("direction<-", signature="x", function(x, value) standardGeneric("direction<-"))
setGeneric("revmap", function(x, ...) standardGeneric("revmap"))
setGeneric("colmetanames", function(x) standardGeneric("colmetanames"))
setGeneric("Lkeyname", function(x) standardGeneric("Lkeyname"))
setGeneric("Rkeyname", function(x) standardGeneric("Rkeyname"))
setGeneric("keyname", function(x) standardGeneric("keyname"))
setGeneric("tagname", function(x) standardGeneric("tagname"))
setGeneric("Rattribnames", function(x) standardGeneric("Rattribnames"))
setGeneric("Rattribnames<-", signature="x", function(x, value) standardGeneric("Rattribnames<-"))
setGeneric("Lkeys", function(x) standardGeneric("Lkeys"))
setGeneric("Rkeys", function(x) standardGeneric("Rkeys"))
setGeneric("keys", signature="x",
           function(x, keytype, ...) standardGeneric("keys"))
setGeneric("Lkeys<-", signature="x", function(x, value) standardGeneric("Lkeys<-"))
setGeneric("Rkeys<-", signature="x", function(x, value) standardGeneric("Rkeys<-"))
setGeneric("keys<-", signature="x", function(x, value) standardGeneric("keys<-"))
setGeneric("Llength", function(x) standardGeneric("Llength"))
setGeneric("Rlength", function(x) standardGeneric("Rlength"))
setGeneric("isNA", function(x) standardGeneric("isNA"))
setGeneric("mappedLkeys", function(x) standardGeneric("mappedLkeys"))
setGeneric("mappedRkeys", function(x) standardGeneric("mappedRkeys"))
setGeneric("mappedkeys", function(x) standardGeneric("mappedkeys"))
setGeneric("count.mappedLkeys", function(x) standardGeneric("count.mappedLkeys"))
setGeneric("count.mappedRkeys", function(x) standardGeneric("count.mappedRkeys"))
setGeneric("count.mappedkeys", function(x) standardGeneric("count.mappedkeys"))
setGeneric("toTable", function(x) standardGeneric("toTable"))
setGeneric("links", function(x) standardGeneric("links"))
setGeneric("count.links", function(x) standardGeneric("count.links"))
setGeneric("nhit", function(x) standardGeneric("nhit"))
setGeneric("toggleProbes", signature="x", function(x, value) standardGeneric("toggleProbes"))
setGeneric("hasMultiProbes", function(x) standardGeneric("hasMultiProbes"))
setGeneric("hasSingleProbes", function(x) standardGeneric("hasSingleProbes"))
setGeneric("getBimapFilters", function(x) standardGeneric("getBimapFilters"))
setGeneric("setInpBimapFilter", signature="x", function(x, value) standardGeneric("setInpBimapFilter"))
setGeneric("toLList", function(x) standardGeneric("toLList"))
setGeneric("toRList", function(x) standardGeneric("toRList"))
setGeneric("toList", function(x) standardGeneric("toList"))
setGeneric("GOFrame", function(x, organism) standardGeneric("GOFrame"))
setGeneric("GOAllFrame", function(x) standardGeneric("GOAllFrame"))
setGeneric("getGOFrameData", function(x) standardGeneric("getGOFrameData"))
setGeneric("KEGGFrame", function(x, organism) standardGeneric("KEGGFrame"))
setGeneric("getKEGGFrameData", function(x) standardGeneric("getKEGGFrameData"))

setGeneric("GOID", function(object) standardGeneric("GOID")) 
setGeneric("Term", function(object) standardGeneric("Term"))
setGeneric("Definition", function(object) standardGeneric("Definition"))
setGeneric("Synonym", function(object) standardGeneric("Synonym"))
setGeneric("Secondary", function(object) standardGeneric("Secondary"))


### Others (do whatever you want with them ;-)
setGeneric("taxonomyId", function(x) standardGeneric("taxonomyId"))
setGeneric("dbmeta", signature="x", function(x, name) standardGeneric("dbmeta"))
setGeneric("dbschema", signature="x", function(x, file="", show.indices=FALSE) standardGeneric("dbschema"))
setGeneric("dbInfo", function(x) standardGeneric("dbInfo"))
setGeneric("Ltablename", function(x) standardGeneric("Ltablename"))
setGeneric("Rtablename", function(x) standardGeneric("Rtablename"))
setGeneric("Lfilter", function(x) standardGeneric("Lfilter"))
setGeneric("Rfilter", function(x) standardGeneric("Rfilter"))
setGeneric("flatten", function(x, ...) standardGeneric("flatten"))
setGeneric("orgPackageName", function(x, ...) standardGeneric("orgPackageName"))

## AnnotationDb
setGeneric("saveDb", signature="x",
           function(x, file) standardGeneric("saveDb"))

## Don't do this.  There is already a dbconn() method.  So use that one.
##setGeneric("dbConn", function(x) standardGeneric("dbConn"))

setGeneric("packageName", function(x) standardGeneric("packageName"))

setGeneric("columns", signature="x", function(x) {
    value <- standardGeneric("columns")
    sort(value)
})
           
setGeneric("keytypes", signature="x", function(x) {
    value <- standardGeneric("keytypes")
    sort(value)
})

setGeneric("select", signature="x",
           function(x, keys, columns, keytype, ...) standardGeneric("select"))

setGeneric("mapIds", signature="x",
           function(x, keys, column, keytype, ..., multiVals)
               standardGeneric("mapIds"))
