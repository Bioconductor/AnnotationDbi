### =========================================================================
### An SQLite-based ann data package (AnnDbPkg) provides a set of pre-defined
### AnnObj objects that are created at load-time. This set depends only on
### the underlying db schema i.e. all the SQLite-based ann data packages that
### share the same underlying db schema will provide the same set of AnnObj
### objects.
###
### This file describes the set of AnnObj objects provided by any
### YEAST_DB-based package i.e. any SQLite-based ann data package based
### on the YEAST_DB schema.
### The createAnnObjs.YEAST_DB() function is the main entry point for
### this file: it is called by any YEAST_DB-based package at load-time.
### -------------------------------------------------------------------------


YEAST_DB_L2Rlink1 <- list(tablename="sgd", Lcolname="systematic_name", Rcolname="_id")

### Mandatory fields: objName, Class and L2Rchain
YEAST_DB_AnnDbBimap_seeds <- list(
    list(
        objName="ALIAS",
        Class="AnnDbBimap",
        L2Rchain=list(
            YEAST_DB_L2Rlink1,
            list(
                tablename="gene2alias",
                Lcolname="_id",
                Rcolname="alias"
            )
        )
    ),
    list(
        objName="CHR",
        Class="AnnDbBimap",
        L2Rchain=list(
            YEAST_DB_L2Rlink1,
            list(
                tablename="chromosome_features",
                Lcolname="_id",
                Rcolname="chromosome"
            )
        )
    ),
    list(
        objName="CHRLOC",
        Class="AnnDbMap",
        L2Rchain=list(
            YEAST_DB_L2Rlink1,
            list(
                tablename="chromosome_features",
                Lcolname="_id",
                tagname=c(Chromosome="{chromosome}"),
                Rcolname="start"
            )
        ),
        rightColType="integer"
    ),
    list(
        objName="CHRLOCEND",
        Class="AnnDbMap",
        L2Rchain=list(
            YEAST_DB_L2Rlink1,
            list(
                tablename="chromosome_features",
                Lcolname="_id",
                tagname=c(Chromosome="{chromosome}"),
                Rcolname="stop"
            )
        ),
        rightColType="integer"
    ),
    list(
        objName="DESCRIPTION",
        Class="AnnDbBimap",
        L2Rchain=list(
            YEAST_DB_L2Rlink1,
            list(
                tablename="chromosome_features",
                Lcolname="_id",
                Rcolname="feature_description"
            )
        )
    ),
    list(
        objName="ENZYME",
        Class="AnnDbBimap",
        L2Rchain=list(
            YEAST_DB_L2Rlink1,
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
            YEAST_DB_L2Rlink1,
            list(
                tablename="sgd",
                Lcolname="_id",
                Rcolname="gene_name"
            )
        )
    ),
    list(
        objName="PATH",
        Class="AnnDbBimap",
        L2Rchain=list(
            YEAST_DB_L2Rlink1,
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
            YEAST_DB_L2Rlink1,
            list(
                tablename="pubmed",
                Lcolname="_id",
                Rcolname="pubmed_id"
            )
        )
    ),
    list(
        objName="GO",
        Class="Go3AnnDbBimap",
        L2Rchain=list(
            YEAST_DB_L2Rlink1,
            list(
                #tablename="go_term", # no rightmost table for a Go3AnnDbBimap
                Lcolname="_id",
                tagname=c(Evidence="{evidence}"),
                Rcolname="go_id",
                Rattribnames=c(Ontology="NULL")
            )
        ),
        rightTables=Go3tablenames()
    ),
    list(
        objName="COMMON2ORF",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="gene2systematic",
                Lcolname="gene_name",
                Rcolname="systematic_name"
            )
        )
    ),
    list(
        objName="INTERPRO",
        Class="AnnDbBimap",
        L2Rchain=list(
            YEAST_DB_L2Rlink1,
            list(
                tablename="interpro",
                Lcolname="_id",
                Rcolname="interpro_id"
            )
        )
    ),
    list(
        objName="PFAM",
        Class="AnnDbBimap",
        L2Rchain=list(
            YEAST_DB_L2Rlink1,
            list(
                tablename="pfam",
                Lcolname="_id",
                Rcolname="pfam_id"
            )
        )
    ),
    list(
        objName="SMART",
        Class="AnnDbBimap",
        L2Rchain=list(
            YEAST_DB_L2Rlink1,
            list(
                tablename="smart",
                Lcolname="_id",
                Rcolname="smart_id"
            )
        )
    ),
    list(
        objName="SGD",
        Class="AnnDbBimap",
        L2Rchain=list(
            YEAST_DB_L2Rlink1,
            list(
                tablename="sgd",
                Lcolname="_id",
                Rcolname="sgd_id"
            )
        )
    ),
    list(
        objName="ENTREZID",
        Class="AnnDbBimap",
        L2Rchain=list(
            YEAST_DB_L2Rlink1,
            list(
                tablename="genes",
                Lcolname="_id",
                Rcolname="gene_id"
            )
        )
    ),
    list(
        objName="REFSEQ",
        Class="AnnDbBimap",
        L2Rchain=list(
            YEAST_DB_L2Rlink1,
            list(
                tablename="refseq",
                Lcolname="_id",
                Rcolname="accession"
            )
        )
    ),
    list(
        objName="ENSEMBL",
        Class="AnnDbBimap",
        L2Rchain=list(
            YEAST_DB_L2Rlink1,
            list(
                tablename="ensembl",
                Lcolname="_id",
                Rcolname="ensembl_id"
            )
        )
    ),
    list(
        objName="ENSEMBLPROT",
        Class="AnnDbBimap",
        L2Rchain=list(
            YEAST_DB_L2Rlink1,
            list(
                tablename="ensembl_prot",
                Lcolname="_id",
                Rcolname="prot_id"
            )
        )
    ),
    list(
        objName="ENSEMBLTRANS",
        Class="AnnDbBimap",
        L2Rchain=list(
            YEAST_DB_L2Rlink1,
            list(
                tablename="ensembl_trans",
                Lcolname="_id",
                Rcolname="trans_id"
            )
        )
    ),
    list(
        objName="UNIPROT",
        Class="AnnDbBimap",
        L2Rchain=list(
            YEAST_DB_L2Rlink1,
            list(
                tablename="uniprot",
                Lcolname="_id",
                Rcolname="uniprot_id"
            )
        )
    )
)

createAnnObjs.YEAST_DB <- function(prefix, objTarget, dbconn, datacache)
{
    checkDBSCHEMA(dbconn, "YEAST_DB")

    ## AnnDbBimap objects
    seed0 <- list(
        objTarget=objTarget,
        datacache=datacache
    )
    ann_objs <- createAnnDbBimaps(YEAST_DB_AnnDbBimap_seeds, seed0)

    ## Reverse maps
    ann_objs$ENZYME2ORF <- revmap(ann_objs$ENZYME, objName="ENZYME2ORF")
    ann_objs$PATH2ORF <- revmap(ann_objs$PATH, objName="PATH2ORF")
    ann_objs$PMID2ORF <- revmap(ann_objs$PMID, objName="PMID2ORF")
    ann_objs$ALIAS2ORF <- revmap(ann_objs$ALIAS, objName="ALIAS2ORF")
    ann_objs$ENSEMBL2ORF <- revmap(ann_objs$ENSEMBL, objName="ENSEMBL2ORF")
    ann_objs$ENSEMBLPROT2ORF <- revmap(ann_objs$ENSEMBLPROT, objName="ENSEMBLPROT2ORF")
    ann_objs$ENSEMBLTRANS2ORF <- revmap(ann_objs$ENSEMBLTRANS, objName="ENSEMBLTRANS2ORF")
    ann_objs$GO2ORF <- revmap(ann_objs$GO, objName="GO2ORF")
    map <- ann_objs$GO2ORF
    map@rightTables <- Go3tablenames(all=TRUE)
    map@objName <- "GO2ALLORFS"
    ann_objs$GO2ALLORFS <- map

    ## 3 special maps that are not AnnDbBimap objects (just named vectors)
    ann_objs$CHRLENGTHS <- createCHRLENGTHS(dbconn)
    ann_objs$REJECTORF <- createREJECTORF(dbconn)
    ann_objs$MAPCOUNTS <- createMAPCOUNTS(dbconn, prefix)

    ## Some pre-caching
    Lkeys(ann_objs$GO)

    prefixAnnObjNames(ann_objs, prefix)
}

