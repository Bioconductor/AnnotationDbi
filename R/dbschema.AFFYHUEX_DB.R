### =========================================================================
### Create all data objects for an annotation data package
### with db schema AFFYHUEX_DB
### -------------------------------------------------------------------------

### TODO:

AFFYHUEX_DB_default_leftTable <- "probeset"
AFFYHUEX_DB_default_leftCol <- "probeset_id"
AFFYHUEX_DB_default_join <- "INNER JOIN probeset USING (probeset_id)"
AFFYHUEX_DB_default_rightColType <- character(0)

### Mandatory fields: objName, rightTable and rightCol
AFFYHUEX_DB_SimpleAnnMap_seeds <- list(
        list(
                objName="FEATURESET"
        )
)

AFFYHUEX_DB_WideAnnMap_seeds <- list(
        list(
                objName="GENEASSIGN",
                rightTable="gene_assignment",
                rightCol="accession"
        ),
        list(
                objName="MRNAASSIGN",
                rightTable="mrna_assignment",
                rightCol="accession"
        ),
        list(
                objName="SWISSPROT",
                rightTable="swissprot",
                rightCol="swissprot_accession"
        ),
        list(
                objName="UNIGENE",
                rightTable="unigene",
                rightCol="unigene_id"
        ),
        list(
                objName="GOBP",
                rightTable="GO_biological_process",
                rightCol="GO_id"
        ),
        list(
                objName="GOCC",
                rightTable="GO_cellular_component",
                rightCol="GO_id"
        ),
        list(
                objName="GOMF",
                rightTable="GO_molecular_function",
                rightCol="GO_id"
        ),
        list(
                objName="PATHWAY",
                rightTable="pathway",
                
        ),
        list(
                objName="PROTDOMAINS",
                rightTable="protein_domains"
        ),
        list(
                objName="PROTFAMILIES",
                rightTable="protein_families_desc"
        )
)

createAnnObjects.AFFYHUEX_DB <- function(prefix, objTarget, conn, datacache)
{
    ## WideAnnMap objects
    seed0 <- list(
        objTarget=objTarget,
        conn=conn,
        datacache=datacache,
        leftTable=AFFYHUEX_DB_default_leftTable,
        leftCol=AFFYHUEX_DB_default_leftCol,
        join=AFFYHUEX_DB_default_join,
        rightColType=AFFYHUEX_DB_default_rightColType
    )
    maps <- createWideAnnMapObjects(AFFYHUEX_DB_WideAnnMap_seeds, seed0)

    ## Some pre-caching
    left.names(maps$FEATURESET)

    names(maps) <- paste(prefix, names(maps), sep="")
    maps
}

