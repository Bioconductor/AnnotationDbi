### =========================================================================
### An SQLite-based ann data package (AnnDbPkg) provides a set of pre-defined
### AnnObj objects that are created at load-time. This set depends only on
### the underlying db schema i.e. all the SQLite-based ann data packages that
### share the same underlying db schema will provide the same set of AnnObj
### objects.
###
### This file describes the set of AnnObj objects provided by any
### ARABIDOPSISCHIP_DB-based package i.e. any SQLite-based ann data package based
### on the ARABIDOPSISCHIP_DB schema.
### The createAnnObjs.ARABIDOPSISCHIP_DB() function is the main entry point for
### this file: it is called by any ARABIDOPSISCHIP_DB-based package at load-time.
### -------------------------------------------------------------------------
orgPkg = "org.At.tair"

ARABIDOPSISCHIP_DB_L2Rlink1 <- list(tablename="probes", Lcolname="probe_id", Rcolname="gene_id")
ARABIDOPSISCHIP_DB_L2Rlink2 <- list(tablename="genes", Lcolname="gene_id", Rcolname="_id", altDB=orgPkg)

### Mandatory fields: objName, Class and L2Rchain
ARABIDOPSISCHIP_DB_AnnDbBimap_seeds <- list(
    list(
        objName="ACCNUM",
        Class="AnnDbBimap",
        L2Rchain=list(
            ARABIDOPSISCHIP_DB_L2Rlink1,
            ARABIDOPSISCHIP_DB_L2Rlink2,
            list(
                tablename="genes",
                Lcolname="_id",
                Rcolname="gene_id"
            )
        )##,
##        replace.multiple="multiple"
    ),
    list(
        objName="ARACYC",
        Class="AnnDbBimap",
        L2Rchain=list(
            ARABIDOPSISCHIP_DB_L2Rlink1,
            ARABIDOPSISCHIP_DB_L2Rlink2,
            list(
                tablename="aracyc",
                Lcolname="_id",
                Rcolname="pathway_name",
                altDB=orgPkg

            )
        )
    ),
    list(
        objName="CHR",
        Class="AnnDbBimap",
        L2Rchain=list(
            ARABIDOPSISCHIP_DB_L2Rlink1,
            ARABIDOPSISCHIP_DB_L2Rlink2,
            list(
                tablename="gene_info",
                Lcolname="_id",
                Rcolname="chromosome",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="ENZYME",
        Class="AnnDbBimap",
        L2Rchain=list(
            ARABIDOPSISCHIP_DB_L2Rlink1,
            ARABIDOPSISCHIP_DB_L2Rlink2,
            list(
                tablename="ec",
                Lcolname="_id",
                Rcolname="ec_number",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="ARACYCENZYME",
        Class="AnnDbBimap",
        L2Rchain=list(
            ARABIDOPSISCHIP_DB_L2Rlink1,
            ARABIDOPSISCHIP_DB_L2Rlink2,
            list(
                tablename="enzyme",
                Lcolname="_id",
                Rcolname="ec_name",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="GENENAME",
        Class="AnnDbBimap",
        L2Rchain=list(
            ARABIDOPSISCHIP_DB_L2Rlink1,
            ARABIDOPSISCHIP_DB_L2Rlink2,
            list(
                tablename="gene_info",
                Lcolname="_id",
                Rcolname="gene_name",
                altDB=orgPkg
            )
        )
    ),
##     list(
##         objName="MULTIHIT",
##         Class="AgiAnnDbMap",
##         L2Rchain=list(
##             ARABIDOPSISCHIP_DB_L2Rlink1,
##             ARABIDOPSISCHIP_DB_L2Rlink2,
##             list(
##                 tablename="genes",
##                 Lcolname="_id",
##                 Rcolname="gene_id",
##                 altDB=orgPkg
##             )
##         ),
##         replace.single=as.character(NA)
##     ),
    list(
        objName="PATH",
        Class="AnnDbBimap",
        L2Rchain=list(
            ARABIDOPSISCHIP_DB_L2Rlink1,
            ARABIDOPSISCHIP_DB_L2Rlink2,
            list(
                tablename="kegg",
                Lcolname="_id",
                Rcolname="path_id",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="PMID",
        Class="AnnDbBimap",
        L2Rchain=list(
            ARABIDOPSISCHIP_DB_L2Rlink1,
            ARABIDOPSISCHIP_DB_L2Rlink2,
            list(
                tablename="pubmed",
                Lcolname="_id",
                Rcolname="pubmed_id",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="SYMBOL",
        Class="AnnDbBimap",
        L2Rchain=list(
            ARABIDOPSISCHIP_DB_L2Rlink1,
            ARABIDOPSISCHIP_DB_L2Rlink2,
            list(
                tablename="gene_info",
                Lcolname="_id",
                Rcolname="symbol",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="CHRLOC",
        Class="AnnDbMap",
        L2Rchain=list(
            ARABIDOPSISCHIP_DB_L2Rlink1,
            ARABIDOPSISCHIP_DB_L2Rlink2,
            list(
                tablename="chromosome_locations",
                Lcolname="_id",
                tagname=c(Chromosome="{seqname}"),
                Rcolname="start_location",
                altDB=orgPkg
            )
        ),
        rightColType="integer"
    ),
    list(
        objName="CHRLOCEND",
        Class="AnnDbMap",
        L2Rchain=list(
            ARABIDOPSISCHIP_DB_L2Rlink1,
            ARABIDOPSISCHIP_DB_L2Rlink2,
            list(
                tablename="chromosome_locations",
                Lcolname="_id",
                tagname=c(Chromosome="{seqname}"),
                Rcolname="end_location",
                altDB=orgPkg
            )
        ),
        rightColType="integer"
    ),                                            
    list(
        objName="GO",
        Class="Go3AnnDbBimap",
        L2Rchain=list(
            ARABIDOPSISCHIP_DB_L2Rlink1,
            ARABIDOPSISCHIP_DB_L2Rlink2,
            list(
                #tablename="go_term", # no rightmost table for a Go3AnnDbBimap
                Lcolname="_id",
                tagname=c(Evidence="{evidence}"),
                Rcolname="go_id",
                Rattribnames=c(Ontology="NULL"),
                altDB=orgPkg
            )
        ),
        rightTables=Go3tablenames()
    )
)

createAnnObjs.ARABIDOPSISCHIP_DB <- function(prefix, objTarget, dbconn, datacache)
{
    checkDBSCHEMA(dbconn, "ARABIDOPSISCHIP_DB")

    ## AnnDbBimap objects
    seed0 <- list(
        objTarget=objTarget,
        datacache=datacache
    )
    ann_objs <- createAnnDbBimaps(ARABIDOPSISCHIP_DB_AnnDbBimap_seeds, seed0)
    
    attachDBs(dbconn, ann_objs)

    
    ## Reverse maps
    ann_objs$ENZYME2PROBE <- revmap(ann_objs$ENZYME, objName="ENZYME2PROBE")
    ann_objs$PATH2PROBE <- revmap(ann_objs$PATH, objName="PATH2PROBE")
    ann_objs$PMID2PROBE <- revmap(ann_objs$PMID, objName="PMID2PROBE")
    ann_objs$GO2PROBE <- revmap(ann_objs$GO, objName="GO2PROBE")
    map <- ann_objs$GO2PROBE
    map@rightTables <- Go3tablenames(all=TRUE)
    map@objName <- "GO2ALLPROBES"
    ann_objs$GO2ALLPROBES <- map

    ## 2 special map that is not an AnnDbBimap object (just a named integer vector)
    ann_objs$CHRLENGTHS <- createCHRLENGTHS(dbconn, dbname="org.At.tair")
    ann_objs$MAPCOUNTS <- createMAPCOUNTS(dbconn, prefix)

    ## 1 special string to let us know who the supporting org package is.
    ann_objs$ORGPKG <- "org.At.tair"
    
    ## Some pre-caching
    Lkeys(ann_objs$GO)

    prefixAnnObjNames(ann_objs, prefix)
}

