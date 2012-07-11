### =========================================================================
### An SQLite-based ann data package (AnnDbPkg) provides a set of pre-defined
### AnnObj objects that are created at load-time. This set depends only on
### the underlying db schema i.e. all the SQLite-based ann data packages that
### share the same underlying db schema will provide the same set of AnnObj
### objects.
###
### This file describes the set of AnnObj objects provided by any
### ARABIDOPSIS_DB-based package i.e. any SQLite-based ann data package based
### on the ARABIDOPSIS_DB schema.
### The createAnnObjs.ARABIDOPSIS_DB() function is the main entry point for
### this file: it is called by any ARABIDOPSIS_DB-based package at load-time.
### -------------------------------------------------------------------------


ARABIDOPSIS_DB_L2Rlink1 <- list(tablename="genes", Lcolname="gene_id", Rcolname="_id")

### Mandatory fields: objName, Class and L2Rchain
ARABIDOPSIS_DB_AnnDbBimap_seeds <- list(
    list(
        objName="ARACYC",
        Class="AnnDbBimap",
        L2Rchain=list(
            ARABIDOPSIS_DB_L2Rlink1,
            list(
                tablename="aracyc",
                Lcolname="_id",
                Rcolname="pathway_name"
            )
        ),
        direction=-1L
    ),
    list(
        objName="CHR",
        Class="AnnDbBimap",
        L2Rchain=list(
            ARABIDOPSIS_DB_L2Rlink1,
            list(
                tablename="gene_info",
                Lcolname="_id",
                Rcolname="chromosome"
            )
        )
    ),
    list(
        objName="ENZYME",
        Class="AnnDbBimap",
        L2Rchain=list(
            ARABIDOPSIS_DB_L2Rlink1,
            list(
                tablename="ec",
                Lcolname="_id",
                Rcolname="ec_number"
            )
        )
    ),
    list(
        objName="ARACYCENZYME",
        Class="AnnDbBimap",
        L2Rchain=list(
            ARABIDOPSIS_DB_L2Rlink1,
            list(
                tablename="enzyme",
                Lcolname="_id",
                Rcolname="ec_name"
            )
        )
    ),
    list(
        objName="GENENAME",
        Class="AnnDbBimap",
        L2Rchain=list(
            ARABIDOPSIS_DB_L2Rlink1,
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
            ARABIDOPSIS_DB_L2Rlink1,
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
            ARABIDOPSIS_DB_L2Rlink1,
            list(
                tablename="pubmed",
                Lcolname="_id",
                Rcolname="pubmed_id"
            )
        )
    ),
    list(
        objName="SYMBOL",
        Class="AnnDbBimap",
        L2Rchain=list(
            ARABIDOPSIS_DB_L2Rlink1,
            list(
                tablename="gene_info",
                Lcolname="_id",
                Rcolname="symbol"
            )
        )
    ),
    list(
        objName="CHRLOC",
        Class="AnnDbMap",
        L2Rchain=list(
            ARABIDOPSIS_DB_L2Rlink1,
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
            ARABIDOPSIS_DB_L2Rlink1,
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
        objName="REFSEQ",
        Class="AnnDbBimap",
        L2Rchain=list(
            ARABIDOPSIS_DB_L2Rlink1,
            list(
                tablename="refseq",
                Lcolname="_id",
                Rcolname="accession"
            )
        )
    ),
    list(
        objName="ENTREZID",
        Class="AnnDbBimap",
        L2Rchain=list(
            ARABIDOPSIS_DB_L2Rlink1,
            list(
                tablename="entrez_genes",
                Lcolname="_id",
                Rcolname="gene_id"
            )
        )
    ),
##     list(
##         objName="UNIPROT",
##         Class="AnnDbBimap",
##         L2Rchain=list(
##             ARABIDOPSIS_DB_L2Rlink1,
##             list(
##                 tablename="uniprot",
##                 Lcolname="_id",
##                 Rcolname="uniprot_id"
##             )
##         )
##     ),
    list(
        objName="GO",
        Class="Go3AnnDbBimap",
        L2Rchain=list(
            ARABIDOPSIS_DB_L2Rlink1,
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

createAnnObjs.ARABIDOPSIS_DB <- function(prefix, objTarget, dbconn, datacache)
{
    checkDBSCHEMA(dbconn, "ARABIDOPSIS_DB")

    ## AnnDbBimap objects
    seed0 <- list(
        objTarget=objTarget,
        datacache=datacache
    )
    ann_objs <- createAnnDbBimaps(ARABIDOPSIS_DB_AnnDbBimap_seeds, seed0)

    ## Reverse maps
    ann_objs$ENZYME2TAIR <- revmap(ann_objs$ENZYME, objName="ENZYME2TAIR")
    ann_objs$PATH2TAIR <- revmap(ann_objs$PATH, objName="PATH2TAIR")
    ann_objs$PMID2TAIR <- revmap(ann_objs$PMID, objName="PMID2TAIR")
    ann_objs$GO2TAIR <- revmap(ann_objs$GO, objName="GO2TAIR")
    ann_objs$REFSEQ2TAIR <- revmap(ann_objs$REFSEQ, objName="REFSEQ2TAIR")
    map <- ann_objs$GO2TAIR
    map@rightTables <- Go3tablenames(all=TRUE)
    map@objName <- "GO2ALLTAIRS"
    ann_objs$GO2ALLTAIRS <- map

    ## 2 special maps that are not AnnDbBimap objects (just named integer vectors)
    ann_objs$CHRLENGTHS <- createCHRLENGTHS(dbconn)
    ann_objs$MAPCOUNTS <- createMAPCOUNTS(dbconn, prefix)

    ## Some pre-caching
    Lkeys(ann_objs$GO)
    #mappedLkeys(ann_objs$GO)
    #Rkeys(ann_objs$GO2TAIR)
    #mappedRkeys(ann_objs$GO2TAIR)

    prefixAnnObjNames(ann_objs, prefix)
}

