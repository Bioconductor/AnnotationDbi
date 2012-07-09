## Generic code to createAnnObjs for bimaps of NCBIORG packages

## This file will define what it means to be MOUSE_DB or HUMAN_DB etc. as a
## series of functions named like createAnnObjs.MOUSE_DB() etc.


## Start with a huge named list of all the possible mappings.


NCBI_DB_L2Rlink1 <- list(tablename="genes", Lcolname="gene_id", Rcolname="_id")

NCBI_DB_AnnDbBimap_seeds <- list(
    list(
        objName="ACCNUM",
        Class="AnnDbBimap",
        L2Rchain=list(
            NCBI_DB_L2Rlink1,
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
            NCBI_DB_L2Rlink1,
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
            NCBI_DB_L2Rlink1,
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
            NCBI_DB_L2Rlink1,
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
            NCBI_DB_L2Rlink1,
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
            NCBI_DB_L2Rlink1,
            list(
                tablename="cytogenetic_locations",
                Lcolname="_id",
                Rcolname="cytogenetic_location"
            )
        )
    ),
    list(
        objName="OMIM",
        Class="AnnDbBimap",
        L2Rchain=list(
            NCBI_DB_L2Rlink1,
            list(
                tablename="omim",
                Lcolname="_id",
                Rcolname="omim_id"
            )
        )
    ),
    list(
        objName="PATH",
        Class="AnnDbBimap",
        L2Rchain=list(
            NCBI_DB_L2Rlink1,
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
            NCBI_DB_L2Rlink1,
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
            NCBI_DB_L2Rlink1,
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
            NCBI_DB_L2Rlink1,
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
            NCBI_DB_L2Rlink1,
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
            NCBI_DB_L2Rlink1,
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
            NCBI_DB_L2Rlink1,
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
        objName="PFAM",
        Class="IpiAnnDbMap",
        L2Rchain=list(
            NCBI_DB_L2Rlink1,
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
            NCBI_DB_L2Rlink1,
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
            NCBI_DB_L2Rlink1,
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
            NCBI_DB_L2Rlink1,
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
            NCBI_DB_L2Rlink1,
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
            NCBI_DB_L2Rlink1,
            list(
                tablename="uniprot",
                Lcolname="_id",
                Rcolname="uniprot_id"
            )
        )
    ),
    list(
        objName="UCSCKG",
        Class="AnnDbBimap",
        L2Rchain=list(
            NCBI_DB_L2Rlink1,
            list(
                tablename="ucsc",
                Lcolname="_id",
                Rcolname="ucsc_id"
            )
        )
    ),
    list(
        objName="FLYBASE",
        Class="AnnDbBimap",
        L2Rchain=list(
            NCBI_DB_L2Rlink1,
            list(
                tablename="flybase",
                Lcolname="_id",
                Rcolname="flybase_id"
            )
        )
    ),
    list(
        objName="FLYBASECG",
        Class="AnnDbBimap",
        L2Rchain=list(
            NCBI_DB_L2Rlink1,
            list(
                tablename="flybase_cg",
                Lcolname="_id",
                Rcolname="flybase_cg_id"
            )
        )
    ),
    list(
        objName="FLYBASEPROT",
        Class="AnnDbBimap",
        L2Rchain=list(
            NCBI_DB_L2Rlink1,
            list(
                tablename="flybase_prot",
                Lcolname="_id",
                Rcolname="prot_id"
            )
        )
    ),                                 
    list(
        objName="MGI",
        Class="AnnDbBimap",
        L2Rchain=list(
            NCBI_DB_L2Rlink1,
            list(
                tablename="mgi",
                Lcolname="_id",
                Rcolname="mgi_id"
            )
        )
    ),
    list(
        objName="WORMBASE",
        Class="AnnDbBimap",
        L2Rchain=list(
            NCBI_DB_L2Rlink1,
            list(
                tablename="wormbase",
                Lcolname="_id",
                Rcolname="wormbase_id"
            )
        )
    ),
    list(
        objName="ZFIN",
        Class="AnnDbBimap",
        L2Rchain=list(
            NCBI_DB_L2Rlink1,
            list(
                tablename="zfin",
                Lcolname="_id",
                Rcolname="zfin_id"
            )
        )
    ),
    list(
        objName="GO",
        Class="Go3AnnDbBimap",
        L2Rchain=list(
            NCBI_DB_L2Rlink1,
            list(
                #tablename="go_term", # no rightmost table for a Go3AnnDbBimap
                Lcolname="_id",
                tagname=c(Evidence="{evidence}"),
                Rcolname="go_id",
                Rattribnames=c(Ontology="NULL")
            )
        ),
        rightTables=AnnotationDbi:::Go3tablenames()
    )
)


## Add names for easier subsetting.
names(NCBI_DB_AnnDbBimap_seeds) <- sapply(NCBI_DB_AnnDbBimap_seeds,
                                          function(x){x$objName})






## Then some helper functions to take a list of arguments and convert them into the ann_objs object that is returned by each function

## helper to make MOUSE_DB_AnnDbBimap_seeds from the big list and a list of mappings that are needed

## helper to make revmaps (based on that same list and the maps that are sometimes reversed)

## another helper to call both those helpers and any other stuff so that the guts of each createAnnObjs.XXX_DB function are as short as possible.



## Then define each function briefly like:


## createAnnObjs.MOUSE_DB <- function(prefix, objTarget, dbconn, datacache){
##   ## helper function goes here.
## }
