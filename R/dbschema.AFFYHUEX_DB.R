### =========================================================================
### Create all data objects for an annotation data package
### with db schema AFFYHUEX_DB
### -------------------------------------------------------------------------

### TODO:

AFFYHUEX_DB_default_leftTable <- "transcript_cluster"
AFFYHUEX_DB_default_leftCol <- "transcript_cluster_id"

### Mandatory fields: objName, from
AFFYHUEX_DB_AnnTable_seeds <- list(
        list(
                objName="TRANSCRIPT",
                from="transcript_cluster"
        ),
        list(
                objName="TR2MRNA",
                from="TR2mrna INNER JOIN mrna USING(_mrna_id) INNER JOIN TR2mrna_details ON(TR2mrna._TR2mrna_id=TR2mrna_details._TR2mrna_id)"
        ),
        list(
                objName="MRNA2GENE",
                from="mrna LEFT JOIN gene USING(entrez_gene_id)",
                leftTable="mrna",
                leftCol="accession"
        )
)

createAnnObjects.AFFYHUEX_DB <- function(prefix, objTarget, conn, datacache)
{
    ## AnnTable objects
    seed0 <- list(
        objTarget=objTarget,
        conn=conn,
        datacache=datacache,
        leftTable=AFFYHUEX_DB_default_leftTable,
        leftCol=AFFYHUEX_DB_default_leftCol
    )
    maps <- createAnnTableObjects(AFFYHUEX_DB_AnnTable_seeds, seed0)

    ## Some pre-caching
    left.names(maps$TRANSCRIPT)

    names(maps) <- paste(prefix, names(maps), sep="")
    maps
}

