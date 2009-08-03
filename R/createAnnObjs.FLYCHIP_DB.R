### =========================================================================
### An SQLite-based ann data package (AnnDbPkg) provides a set of pre-defined
### AnnObj objects that are created at load-time. This set depends only on
### the underlying db schema i.e. all the SQLite-based ann data packages that
### share the same underlying db schema will provide the same set of AnnObj
### objects.
###
### This file describes the set of AnnObj objects provided by any
### FLYCHIP_DB-based package i.e. any SQLite-based ann data package based
### on the FLYCHIP_DB schema.
### The createAnnObjs.FLYCHIP_DB() function is the main entry point for
### this file: it is called by any FLYCHIP_DB-based package at load-time.
### -------------------------------------------------------------------------

orgPkg = "org.Dm.eg"

FLYCHIP_DB_L2Rlink1 <- list(tablename="probes", Lcolname="probe_id", Rcolname="gene_id", filter="{is_multiple}='0'")
FLYCHIP_DB_L2Rlink2 <- list(tablename="genes", Lcolname="gene_id", Rcolname="_id", altDB=orgPkg)

### Mandatory fields: objName, Class and L2Rchain
FLYCHIP_DB_AnnDbBimap_seeds <- list(
    list(
        objName="ACCNUM",
        Class="AnnDbBimap",
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
        Class="AnnDbBimap",
        L2Rchain=list(
            FLYCHIP_DB_L2Rlink1,
            FLYCHIP_DB_L2Rlink2,
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
        objName="CHR",
        Class="AnnDbBimap",
        L2Rchain=list(
            FLYCHIP_DB_L2Rlink1,
            FLYCHIP_DB_L2Rlink2,
            list(
                tablename="chromosomes",
                Lcolname="_id",
                Rcolname="chromosome",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="ENTREZID",
        Class="AnnDbBimap",
        L2Rchain=list(
            FLYCHIP_DB_L2Rlink1,
            FLYCHIP_DB_L2Rlink2,
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
        Class="AnnDbBimap",
        L2Rchain=list(
            FLYCHIP_DB_L2Rlink1,
            FLYCHIP_DB_L2Rlink2,
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
        Class="AnnDbBimap",
        L2Rchain=list(
            FLYCHIP_DB_L2Rlink1,
            FLYCHIP_DB_L2Rlink2,
            list(
                tablename="gene_info",
                Lcolname="_id",
                Rcolname="gene_name",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="MAP",
        Class="AnnDbBimap",
        L2Rchain=list(
            FLYCHIP_DB_L2Rlink1,
            FLYCHIP_DB_L2Rlink2,
            list(
                tablename="cytogenetic_locations",
                Lcolname="_id",
                Rcolname="cytogenetic_location",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="PATH",
        Class="AnnDbBimap",
        L2Rchain=list(
            FLYCHIP_DB_L2Rlink1,
            FLYCHIP_DB_L2Rlink2,
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
            FLYCHIP_DB_L2Rlink1,
            FLYCHIP_DB_L2Rlink2,
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
        Class="AnnDbBimap",
        L2Rchain=list(
            FLYCHIP_DB_L2Rlink1,
            FLYCHIP_DB_L2Rlink2,
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
        Class="AnnDbBimap",
        L2Rchain=list(
            FLYCHIP_DB_L2Rlink1,
            FLYCHIP_DB_L2Rlink2,
            list(
                tablename="gene_info",
                Lcolname="_id",
                Rcolname="symbol",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="UNIGENE",
        Class="AnnDbBimap",
        L2Rchain=list(
            FLYCHIP_DB_L2Rlink1,
            FLYCHIP_DB_L2Rlink2,
            list(
                tablename="unigene",
                Lcolname="_id",
                Rcolname="unigene_id",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="CHRLOC",
        Class="AnnDbMap",
        L2Rchain=list(
            FLYCHIP_DB_L2Rlink1,
            FLYCHIP_DB_L2Rlink2,
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
            FLYCHIP_DB_L2Rlink1,
            FLYCHIP_DB_L2Rlink2,
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
        objName="FLYBASE",
        Class="AnnDbBimap",
        L2Rchain=list(
            FLYCHIP_DB_L2Rlink1,
            FLYCHIP_DB_L2Rlink2,
            list(
                tablename="flybase",
                Lcolname="_id",
                Rcolname="flybase_id",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="FLYBASECG",
        Class="AnnDbBimap",
        L2Rchain=list(
            FLYCHIP_DB_L2Rlink1,
            FLYCHIP_DB_L2Rlink2,
            list(
                tablename="flybase_cg",
                Lcolname="_id",
                Rcolname="flybase_cg_id",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="UNIPROT",
        Class="AnnDbBimap",
        L2Rchain=list(
            FLYCHIP_DB_L2Rlink1,
            FLYCHIP_DB_L2Rlink2,
            list(
                tablename="uniprot",
                Lcolname="_id",
                Rcolname="uniprot_id",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="GO",
        Class="Go3AnnDbBimap",
        L2Rchain=list(
            FLYCHIP_DB_L2Rlink1,
            FLYCHIP_DB_L2Rlink2,
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

createAnnObjs.FLYCHIP_DB <- function(prefix, objTarget, dbconn, datacache)
{
    checkDBSCHEMA(dbconn, "FLYCHIP_DB")

    ## AnnDbBimap objects
    seed0 <- list(
        objTarget=objTarget,
        datacache=datacache
    )
    ann_objs <- createAnnDbBimaps(FLYCHIP_DB_AnnDbBimap_seeds, seed0)

    attachDBs(dbconn, ann_objs)
    
    ## Reverse maps
    ann_objs$ENZYME2PROBE <- revmap(ann_objs$ENZYME, objName="ENZYME2PROBE")
    ann_objs$PATH2PROBE <- revmap(ann_objs$PATH, objName="PATH2PROBE")
    ann_objs$PMID2PROBE <- revmap(ann_objs$PMID, objName="PMID2PROBE")
    ann_objs$GO2PROBE <- revmap(ann_objs$GO, objName="GO2PROBE")
    ann_objs$FLYBASE2PROBE <- revmap(ann_objs$FLYBASE, objName="FLYBASE2PROBE")
    ann_objs$FLYBASECG2PROBE <- revmap(ann_objs$FLYBASECG, objName="FLYBASECG2PROBE")
    map <- ann_objs$GO2PROBE
    map@rightTables <- Go3tablenames(all=TRUE)
    map@objName <- "GO2ALLPROBES"
    ann_objs$GO2ALLPROBES <- map

    ## 2 special maps that are not AnnDbBimap objects (just named integer vectors)
    ann_objs$CHRLENGTHS <- createCHRLENGTHS(dbconn, dbname="org.Dm.eg")
    ann_objs$MAPCOUNTS <- createMAPCOUNTS(dbconn, prefix)
    ## 1 special string to let us know who the supporting org package is.
    ann_objs$ORGPKG <- "org.Dm.eg"

    ## Some pre-caching
    Lkeys(ann_objs$GO)
    #mappedLkeys(ann_objs$GO)
    #Rkeys(ann_objs$GO2PROBE)
    #mappedRkeys(ann_objs$GO2PROBE)
    #Rkeys(ann_objs$GO2ALLPROBES)
    #mappedRkeys(ann_objs$GO2ALLPROBES)

    prefixAnnObjNames(ann_objs, prefix)
}

