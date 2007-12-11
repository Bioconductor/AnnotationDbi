### =========================================================================
### An SQLite-based ann data package (AnnDbPkg) provides a set of pre-defined
### AnnObj objects that are created at load-time. This set depends only on
### the underlying db schema i.e. all the SQLite-based ann data packages that
### share the same underlying db schema will provide the same set of AnnObj
### objects.
###
### This file describes the set of AnnObj objects provided by any
### MOUSE_DB-based package i.e. any SQLite-based ann data package based
### on the MOUSE_DB schema.
### The createAnnObjs.MOUSE_DB() function is the main entry point for
### this file: it is called by any MOUSE_DB-based package at load-time.
### -------------------------------------------------------------------------


MOUSE_DB_L2Rlink1 <- list(tablename="genes", Lcolname="gene_id", Rcolname="_id")

### Mandatory fields: objName, Class and L2Rchain
MOUSE_DB_AnnDbBimap_seeds <- list(
    list(
        objName="ACCNUM",
        Class="AnnDbBimap",
        L2Rchain=list(
            MOUSE_DB_L2Rlink1,
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
            MOUSE_DB_L2Rlink1,
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
            MOUSE_DB_L2Rlink1,
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
            MOUSE_DB_L2Rlink1,
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
            MOUSE_DB_L2Rlink1,
            list(
                tablename="gene_info",
                Lcolname="_id",
                Rcolname="gene_name"
            )
        )
    ),
    list(
        objName="MAP",
        Class="AnnDbBimap",
        L2Rchain=list(
            MOUSE_DB_L2Rlink1,
            list(
                tablename="cytogenetic_locations",
                Lcolname="_id",
                Rcolname="cytogenetic_location"
            )
        )
    ),
    list(
        objName="PATH",
        Class="AnnDbBimap",
        L2Rchain=list(
            MOUSE_DB_L2Rlink1,
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
            MOUSE_DB_L2Rlink1,
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
            MOUSE_DB_L2Rlink1,
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
            MOUSE_DB_L2Rlink1,
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
            MOUSE_DB_L2Rlink1,
            list(
                tablename="unigene",
                Lcolname="_id",
                Rcolname="unigene_id"
            )
        )
    ),
    list(
        objName="CHRLOC",
        Class="AnnDbMap",
        L2Rchain=list(
            MOUSE_DB_L2Rlink1,
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
        objName="PFAM",
        Class="IpiAnnDbMap",
        L2Rchain=list(
            MOUSE_DB_L2Rlink1,
            list(
                tablename="pfam",
                Lcolname="_id",
                Rcolname="ipi_id",
                Rattribnames=c(PfamId="{pfam_id}")
            )
        )
    ),
    list(
        objName="PROSITE",
        Class="IpiAnnDbMap",
        L2Rchain=list(
            MOUSE_DB_L2Rlink1,
            list(
                tablename="prosite",
                Lcolname="_id",
                Rcolname="ipi_id",
                Rattribnames=c(PrositeId="{prosite_id}")
            )
        )
    ),
    list(
        objName="ENSEMBL",
        Class="AnnDbBimap",
        L2Rchain=list(
            MOUSE_DB_L2Rlink1,
            list(
                tablename="ensembl",
                Lcolname="_id",
                Rcolname="ensembl_id"
            )
        )
    ),
    list(
        objName="MGI",
        Class="AnnDbBimap",
        L2Rchain=list(
            MOUSE_DB_L2Rlink1,
            list(
                tablename="mgi",
                Lcolname="_id",
                Rcolname="mgi_id"
            )
        )
    ),
    list(
        objName="GO",
        Class="Go3AnnDbBimap",
        L2Rchain=list(
            MOUSE_DB_L2Rlink1,
            list(
                #tablename="go_term", # no rightmost table for a Go3AnnDbBimap
                Lcolname="_id",
                tagname=c(Evidence="{relationship_type}"),
                Rcolname="go_id",
                Rattribnames=c(Ontology="NULL")
            )
        ),
        rightTables=Go3tablenames()
    )
)

createAnnObjs.MOUSE_DB <- function(prefix, objTarget, dbconn, datacache)
{
    checkDBSCHEMA(dbconn, "MOUSE_DB")

    ## AnnDbBimap objects
    seed0 <- list(
        objTarget=objTarget,
        datacache=datacache
    )
    ann_objs <- createAnnDbBimaps(MOUSE_DB_AnnDbBimap_seeds, seed0)

    ## Reverse maps
    ann_objs$ACCNUM2EG <- revmap(ann_objs$ACCNUM, objName="ACCNUM2EG")
    ann_objs$ENZYME2EG <- revmap(ann_objs$ENZYME, objName="ENZYME2EG")
    ann_objs$MAP2EG <- revmap(ann_objs$MAP, objName="MAP2EG")
    ann_objs$PATH2EG <- revmap(ann_objs$PATH, objName="PATH2EG")
    ann_objs$PMID2EG <- revmap(ann_objs$PMID, objName="PMID2EG")
    ann_objs$REFSEQ2EG <- revmap(ann_objs$REFSEQ, objName="REFSEQ2EG")
    ann_objs$SYMBOL2EG <- revmap(ann_objs$SYMBOL, objName="SYMBOL2EG")
    ann_objs$UNIGENE2EG <- revmap(ann_objs$UNIGENE, objName="UNIGENE2EG")
    #ann_objs$PFAM2EG <- revmap(ann_objs$PFAM, objName="PFAM2EG")
    #ann_objs$PROSITE2EG <- revmap(ann_objs$PROSITE, objName="PROSITE2EG")
    ann_objs$ENSEMBL2EG <- revmap(ann_objs$ENSEMBL, objName="ENSEMBL2EG")
    ann_objs$MGI2EG <- revmap(ann_objs$MGI, objName="MGI2EG")
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
    #mappedLkeys(ann_objs$GO)
    #Rkeys(ann_objs$GO2EG)
    #mappedRkeys(ann_objs$GO2EG)

    prefixAnnObjNames(ann_objs, prefix)
}

