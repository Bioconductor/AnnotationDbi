### =========================================================================
### An SQLite-based ann data package (AnnDbPkg) provides a set of pre-defined
### AnnObj objects that are created at load-time. This set depends only on
### the underlying db schema i.e. all the SQLite-based ann data packages that
### share the same underlying db schema will provide the same set of AnnObj
### objects.
###
### This file describes the set of AnnObj objects provided by any
### GO_DB-based package i.e. any SQLite-based ann data package based
### on the GO_DB schema.
### The createAnnObjs.GO_DB() function is the main entry point for
### this file: it is called by any GO_DB-based package at load-time.
### -------------------------------------------------------------------------


createAnnObjs.GO_DB <- function(prefix, objTarget, conn, datacache)
{
    ann_objs <- new.env(hash=TRUE, parent=emptyenv())
    prefixAnnObjNames(ann_objs, prefix)
}

