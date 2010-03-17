### =========================================================================
### An SQLite-based ann data package (AnnDbPkg) provides a set of pre-defined
### AnnObj objects that are created at load-time. This set depends only on
### the underlying db schema i.e. all the SQLite-based ann data packages that
### share the same underlying db schema will provide the same set of AnnObj
### objects.
###
### This file describes the set of AnnObj objects provided by any
### COELICOLOR_DB-based package i.e. any SQLite-based ann data package based
### on the COELICOLOR_DB schema.
### The createAnnObjs.COELICOLOR_DB() function is the main entry point for
### this file: it is called by any COELICOLOR_DB-based package at load-time.
### -------------------------------------------------------------------------


COELICOLOR_DB_L2Rlink1 <- list(tablename="genes", Lcolname="gene_id", Rcolname="_id")

### Mandatory fields: objName, Class and L2Rchain
COELICOLOR_DB_AnnDbBimap_seeds <- list(
    list(
        objName="ACCNUM",
        Class="AnnDbBimap",
        L2Rchain=list(
            COELICOLOR_DB_L2Rlink1,
            list(
                tablename="accessions",
                Lcolname="_id",
                Rcolname="accession"
            )
        )
    ),
    list(
        objName="ALIAS2EG",
        Class="AnnDbBimap",
        L2Rchain=list(
            COELICOLOR_DB_L2Rlink1,
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
            COELICOLOR_DB_L2Rlink1,
            list(
                tablename="chromosomes",
                Lcolname="_id",
                Rcolname="chromosome"
            )
        )
    ),
    list(
        objName="ENZYME",
        Class="AnnDbBimap",
        L2Rchain=list(
            COELICOLOR_DB_L2Rlink1,
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
            COELICOLOR_DB_L2Rlink1,
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
            COELICOLOR_DB_L2Rlink1,
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
            COELICOLOR_DB_L2Rlink1,
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
            COELICOLOR_DB_L2Rlink1,
            list(
                tablename="refseq",
                Lcolname="_id",
                Rcolname="accession"
            )
        )
    ),
    list(
        objName="LOCUSTAG",
        Class="AnnDbBimap",
        L2Rchain=list(
            COELICOLOR_DB_L2Rlink1,
            list(
                tablename="locus",
                Lcolname="_id",
                Rcolname="locus_tag"
            )
        )
    ),
    list(
        objName="PROTEINGI",
        Class="AnnDbBimap",
        L2Rchain=list(
            COELICOLOR_DB_L2Rlink1,
            list(
                tablename="protein",
                Lcolname="_id",
                Rcolname="protein_gi"
            )
        )
    ),
    list(
        objName="SYMBOL",
        Class="AnnDbBimap",
        L2Rchain=list(
            COELICOLOR_DB_L2Rlink1,
            list(
                tablename="symbol",
                Lcolname="_id",
                Rcolname="symbol"
            )
        )
    ),
    list(
        objName="CHRLOC",
        Class="AnnDbMap",
        L2Rchain=list(
            COELICOLOR_DB_L2Rlink1,
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
            COELICOLOR_DB_L2Rlink1,
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
        objName="GO",
        Class="Go3AnnDbBimap",
        L2Rchain=list(
            COELICOLOR_DB_L2Rlink1,
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

createAnnObjs.COELICOLOR_DB <- function(prefix, objTarget, dbconn, datacache)
{
    checkDBSCHEMA(dbconn, "COELICOLOR_DB")

    ## AnnDbBimap objects
    seed0 <- list(
        objTarget=objTarget,
        datacache=datacache
    )
    ann_objs <- createAnnDbBimaps(COELICOLOR_DB_AnnDbBimap_seeds, seed0)

    ## Reverse maps
    ann_objs$ACCNUM2EG <- revmap(ann_objs$ACCNUM, objName="ACCNUM2EG")
    ann_objs$ENZYME2EG <- revmap(ann_objs$ENZYME, objName="ENZYME2EG")
    ann_objs$PATH2EG <- revmap(ann_objs$PATH, objName="PATH2EG")
    ann_objs$PMID2EG <- revmap(ann_objs$PMID, objName="PMID2EG")
    ann_objs$REFSEQ2EG <- revmap(ann_objs$REFSEQ, objName="REFSEQ2EG")
    ann_objs$SYMBOL2EG <- revmap(ann_objs$SYMBOL, objName="SYMBOL2EG")
    ann_objs$GO2EG <- revmap(ann_objs$GO, objName="GO2EG")
    map <- ann_objs$GO2EG
    map@rightTables <- Go3tablenames(all=TRUE)
    map@objName <- "GO2ALLEGS"
    ann_objs$GO2ALLEGS <- map

    ## 2 special maps that are not AnnDbBimap objects (just named integer vectors)
    ann_objs$CHRLENGTHS <- createCHRLENGTHS(dbconn)
    ann_objs$MAPCOUNTS <- createMAPCOUNTS(dbconn, prefix)

    ## Some pre-caching
    Lkeys(ann_objs$GO)

    prefixAnnObjNames(ann_objs, prefix)
}

