### =========================================================================
### An SQLite-based ann data package (AnnDbPkg) provides a set of pre-defined
### AnnObj objects that are created at load-time. This set depends only on
### the underlying db schema i.e. all the SQLite-based ann data packages that
### share the same underlying db schema will provide the same set of AnnObj
### objects.
###
### This file describes the set of AnnObj objects provided by any
### ECOLICHIP_DB-based package i.e. any SQLite-based ann data package based
### on the ECOLICHIP_DB schema.
### The createAnnObjs.ECOLICHIP_DB() function is the main entry point for
### this file: it is called by any ECOLICHIP_DB-based package at load-time.
### -------------------------------------------------------------------------

orgPkg = "org.EcK12.eg"

ECOLICHIP_DB_L2Rlink1 <- list(tablename="probes", Lcolname="probe_id", Rcolname="gene_id", filter="{is_multiple}='0'")
ECOLICHIP_DB_L2Rlink2 <- list(tablename="genes", Lcolname="gene_id", Rcolname="_id", altDB=orgPkg)

### Mandatory fields: objName, Class and L2Rchain
ECOLICHIP_DB_AnnDbBimap_seeds <- list(
    list(
        objName="ACCNUM",
        Class="ProbeAnnDbBimap",
        L2Rchain=list(
            list(
                tablename="accessions",
                Lcolname="probe_id",
                Rcolname="accession"
            )
        )
    ),
    list(
        objName="ALIAS2PROBE",
        Class="ProbeAnnDbBimap",
        L2Rchain=list(
            ECOLICHIP_DB_L2Rlink1,
            ECOLICHIP_DB_L2Rlink2,
            list(
                tablename="alias",
                Lcolname="_id",
                Rcolname="alias_symbol",
                altDB=orgPkg
            )
        ),
        direction=-1L
    ),
    list(
        objName="ENTREZID",
        Class="ProbeAnnDbBimap",
        L2Rchain=list(
            ECOLICHIP_DB_L2Rlink1,
            ECOLICHIP_DB_L2Rlink2,
            list(
                tablename="genes",
                Lcolname="_id",
                Rcolname="gene_id",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="ENZYME",
        Class="ProbeAnnDbBimap",
        L2Rchain=list(
            ECOLICHIP_DB_L2Rlink1,
            ECOLICHIP_DB_L2Rlink2,
            list(
                tablename="ec",
                Lcolname="_id",
                Rcolname="ec_number",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="GENENAME",
        Class="ProbeAnnDbBimap",
        L2Rchain=list(
            ECOLICHIP_DB_L2Rlink1,
            ECOLICHIP_DB_L2Rlink2,
            list(
                tablename="gene_info",
                Lcolname="_id",
                Rcolname="gene_name",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="PATH",
        Class="ProbeAnnDbBimap",
        L2Rchain=list(
            ECOLICHIP_DB_L2Rlink1,
            ECOLICHIP_DB_L2Rlink2,
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
        Class="ProbeAnnDbBimap",
        L2Rchain=list(
            ECOLICHIP_DB_L2Rlink1,
            ECOLICHIP_DB_L2Rlink2,
            list(
                tablename="pubmed",
                Lcolname="_id",
                Rcolname="pubmed_id",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="REFSEQ",
        Class="ProbeAnnDbBimap",
        L2Rchain=list(
            ECOLICHIP_DB_L2Rlink1,
            ECOLICHIP_DB_L2Rlink2,
            list(
                tablename="refseq",
                Lcolname="_id",
                Rcolname="accession",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="SYMBOL",
        Class="ProbeAnnDbBimap",
        L2Rchain=list(
            ECOLICHIP_DB_L2Rlink1,
            ECOLICHIP_DB_L2Rlink2,
            list(
                tablename="gene_info",
                Lcolname="_id",
                Rcolname="symbol",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="GO",
        Class="ProbeGo3AnnDbBimap",
        L2Rchain=list(
            ECOLICHIP_DB_L2Rlink1,
            ECOLICHIP_DB_L2Rlink2,
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

createAnnObjs.ECOLICHIP_DB <- function(prefix, objTarget, dbconn, datacache)
{
    checkDBSCHEMA(dbconn, "ECOLICHIP_DB")

    ## AnnDbBimap objects
    seed0 <- list(
        objTarget=objTarget,
        datacache=datacache
    )
    ann_objs <- createAnnDbBimaps(ECOLICHIP_DB_AnnDbBimap_seeds, seed0)

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

    ## 2 special maps that are not AnnDbBimap objects (just named integer vectors)
    ann_objs$MAPCOUNTS <- createMAPCOUNTS(dbconn, prefix)
    ## 1 special string to let us know who the supporting org package is.
    ann_objs$ORGPKG <- "org.EcK12.eg"

    ## Some pre-caching
    Lkeys(ann_objs$GO)
    #mappedLkeys(ann_objs$GO)
    #Rkeys(ann_objs$GO2PROBE)
    #mappedRkeys(ann_objs$GO2PROBE)
    #Rkeys(ann_objs$GO2ALLPROBES)
    #mappedRkeys(ann_objs$GO2ALLPROBES)

    prefixAnnObjNames(ann_objs, prefix)
}

