### =========================================================================
### An SQLite-based ann data package (AnnDbPkg) provides a set of pre-defined
### AnnObj objects that are created at load-time. This set depends only on
### the underlying db schema i.e. all the SQLite-based ann data packages that
### share the same underlying db schema will provide the same set of AnnObj
### objects.
###
### This file describes the set of AnnObj objects provided by any
### CANINECHIP_DB-based package i.e. any SQLite-based ann data package based
### on the CANINECHIP_DB schema.
### The createAnnObjs.CANINECHIP_DB() function is the main entry point for
### this file: it is called by any CANINECHIP_DB-based package at load-time.
### -------------------------------------------------------------------------


CANINECHIP_DB_L2Rlink1 <- list(tablename="probes", Lcolname="probe_id", Rcolname="_id")

### Mandatory fields: objName, Class and L2Rchain
CANINECHIP_DB_AnnDbBimap_seeds <- list(
    list(
        objName="ACCNUM",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="probes",
                Lcolname="probe_id",
                Rcolname="accession"
            )
        )
    ),
    list(
        objName="ALIAS2PROBE",
        Class="AnnDbBimap",
        L2Rchain=list(
            CANINECHIP_DB_L2Rlink1,
            list(
                tablename="alias",
                Lcolname="_id",
                Rcolname="alias_symbol"
            )
        ),
        direction=-1L
    ),
    list(
        objName="CHR",
        Class="AnnDbBimap",
        L2Rchain=list(
            CANINECHIP_DB_L2Rlink1,
            list(
                tablename="chromosomes",
                Lcolname="_id",
                Rcolname="chromosome"
            )
        )
    ),
    list(
        objName="ENTREZID",
        Class="AnnDbBimap",
        L2Rchain=list(
            CANINECHIP_DB_L2Rlink1,
            list(
                tablename="genes",
                Lcolname="_id",
                Rcolname="gene_id"
            )
        )
    ),
    list(
        objName="ENZYME",
        Class="AnnDbBimap",
        L2Rchain=list(
            CANINECHIP_DB_L2Rlink1,
            list(
                tablename="ec",
                Lcolname="_id",
                Rcolname="ec_number"
            )
        )
    ),
    list(
        objName="GENENAME",
        Class="AnnDbBimap",
        L2Rchain=list(
            CANINECHIP_DB_L2Rlink1,
            list(
                tablename="gene_info",
                Lcolname="_id",
                Rcolname="gene_name"
            )
        )
    ),
    list(
        objName="PATH",
        Class="AnnDbBimap",
        L2Rchain=list(
            CANINECHIP_DB_L2Rlink1,
            list(
                tablename="kegg",
                Lcolname="_id",
                Rcolname="path_id"
            )
        )
    ),
    list(
        objName="PMID",
        Class="AnnDbBimap",
        L2Rchain=list(
            CANINECHIP_DB_L2Rlink1,
            list(
                tablename="pubmed",
                Lcolname="_id",
                Rcolname="pubmed_id"
            )
        )
    ),
    list(
        objName="REFSEQ",
        Class="AnnDbBimap",
        L2Rchain=list(
            CANINECHIP_DB_L2Rlink1,
            list(
                tablename="refseq",
                Lcolname="_id",
                Rcolname="accession"
            )
        )
    ),
    list(
        objName="SYMBOL",
        Class="AnnDbBimap",
        L2Rchain=list(
            CANINECHIP_DB_L2Rlink1,
            list(
                tablename="gene_info",
                Lcolname="_id",
                Rcolname="symbol"
            )
        )
    ),
    list(
        objName="UNIGENE",
        Class="AnnDbBimap",
        L2Rchain=list(
            CANINECHIP_DB_L2Rlink1,
            list(
                tablename="unigene",
                Lcolname="_id",
                Rcolname="unigene_id"
            )
        )
    ),
    list(
        objName="UNIPROT",
        Class="AnnDbBimap",
        L2Rchain=list(
            CANINECHIP_DB_L2Rlink1,
            list(
                tablename="uniprot",
                Lcolname="_id",
                Rcolname="uniprot_id"
            )
        )
    ),
    list(
        objName="CHRLOC",
        Class="AnnDbMap",
        L2Rchain=list(
            CANINECHIP_DB_L2Rlink1,
            list(
                tablename="chromosome_locations",
                Lcolname="_id",
                tagname=c(Chromosome="{seqname}"),
                Rcolname="start_location"
            )
        ),
        rightColType="integer"
    ),
    list(
        objName="CHRLOCEND",
        Class="AnnDbMap",
        L2Rchain=list(
            CANINECHIP_DB_L2Rlink1,
            list(
                tablename="chromosome_locations",
                Lcolname="_id",
                tagname=c(Chromosome="{seqname}"),
                Rcolname="end_location"
            )
        ),
        rightColType="integer"
    ),
    list(
        objName="ENSEMBL",
        Class="AnnDbBimap",
        L2Rchain=list(
            CANINECHIP_DB_L2Rlink1,
            list(
                tablename="ensembl",
                Lcolname="_id",
                Rcolname="ensembl_id"
            )
        )
    ),
    list(
        objName="GO",
        Class="Go3AnnDbBimap",
        L2Rchain=list(
            CANINECHIP_DB_L2Rlink1,
            list(
                #tablename="go_term", # no rightmost table for a Go3AnnDbBimap
                Lcolname="_id",
                tagname=c(Evidence="{evidence}"),
                Rcolname="go_id",
                Rattribnames=c(Ontology="NULL")
            )
        ),
        rightTables=Go3tablenames()
    )
)

createAnnObjs.CANINECHIP_DB <- function(prefix, objTarget, dbconn, datacache)
{
    checkDBSCHEMA(dbconn, "CANINECHIP_DB")

    ## AnnDbBimap objects
    seed0 <- list(
        objTarget=objTarget,
        datacache=datacache
    )
    ann_objs <- createAnnDbBimaps(CANINECHIP_DB_AnnDbBimap_seeds, seed0)

    ## Reverse maps
    ann_objs$ENZYME2PROBE <- revmap(ann_objs$ENZYME, objName="ENZYME2PROBE")
    ann_objs$PATH2PROBE <- revmap(ann_objs$PATH, objName="PATH2PROBE")
    ann_objs$PMID2PROBE <- revmap(ann_objs$PMID, objName="PMID2PROBE")
    ann_objs$GO2PROBE <- revmap(ann_objs$GO, objName="GO2PROBE")
    ann_objs$ENSEMBL2PROBE <- revmap(ann_objs$ENSEMBL, objName="ENSEMBL2PROBE")
    map <- ann_objs$GO2PROBE
    map@rightTables <- Go3tablenames(all=TRUE)
    map@objName <- "GO2ALLPROBES"
    ann_objs$GO2ALLPROBES <- map

    ## 2 special maps that are not AnnDbBimap objects (just named integer vectors)
    ann_objs$CHRLENGTHS <- createCHRLENGTHS(dbconn)
    ann_objs$MAPCOUNTS <- createMAPCOUNTS(dbconn, prefix)

    ## Some pre-caching
    Lkeys(ann_objs$GO)
    #mappedLkeys(ann_objs$GO)
    #Rkeys(ann_objs$GO2PROBE)
    #mappedRkeys(ann_objs$GO2PROBE)
    #Rkeys(ann_objs$GO2ALLPROBES)
    #mappedRkeys(ann_objs$GO2ALLPROBES)

    prefixAnnObjNames(ann_objs, prefix)
}

