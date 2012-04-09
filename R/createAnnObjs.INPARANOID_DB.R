### =========================================================================
### An SQLite-based ann data package (AnnDbPkg) provides a set of pre-defined
### AnnObj objects that are created at load-time. This set depends only on
### the underlying db schema i.e. all the SQLite-based ann data packages that
### share the same underlying db schema will provide the same set of AnnObj
### objects.
###
### This file describes the set of AnnObj objects provided by any
### INPARANOID_DB-based package i.e. any SQLite-based ann data package
### based on the INPARANOID_DB schema.
### The createAnnObjs.INPARANOID_DB() function is the main entry point
### for this file: it is called by any INPARANOID_DB-based package at
### load-time.
### -------------------------------------------------------------------------


### Mandatory fields: objName, Class and L2Rchain


createAnnObjs.INPARANOID_DB <- function(prefix, objTarget, dbconn, datacache)
{
    # This list needs to figure out (from the source DB) which organism it is so that those mappings are not assigned.
    phyloName = dbmeta(datacache, 'ORGANISM') ##need to remove spaces
    phyloName = sub(" ", "_", phyloName)
    
    fields <- .makeFiveLetterMapping()
    ##Use this to figure out what our "species" value is
    species = fields[phyloName]
    
    ##Finally, we want to remove the one thing from fields that we don't need...
    ind = grep(phyloName,names(fields))
    fields = fields[-ind]
    
    INPARANOID_DB_AnnDbBimap_seeds <- makeSeedList(species, fields)

    checkDBSCHEMA(dbconn, "INPARANOID_DB")

    ## AnnDbBimap objects 
    seed0 <- list(
        objTarget=objTarget,
        datacache=datacache
    )
    ann_objs <- createAnnDbBimaps(INPARANOID_DB_AnnDbBimap_seeds, seed0)

    ann_objs$MAPCOUNTS <- createMAPCOUNTS(dbconn, prefix)

    prefixAnnObjNames(ann_objs, prefix)
}
