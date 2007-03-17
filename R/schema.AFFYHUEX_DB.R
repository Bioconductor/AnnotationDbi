### =========================================================================
### Create all data objects for an annotation data package
### with db schema AFFYHUEX_DB
### -------------------------------------------------------------------------

### Mandatory fields: objName, from
AFFYHUEX_DB_AnnTable_seeds <- list(
    list(
        objName="TRANSCRIPT",
        leftTable="transcript_cluster",
        leftCol="transcript_cluster_ID",
        from="transcript_cluster",
        showCols="*"
    ),
    list(
        objName="PROBESET",
        leftTable="probeset",
        leftCol="probeset_ID",
        from="probeset",
        showCols="*"
    )
)

AFFYHUEX_DB_RawAnnMap_seeds <- list(
    list(
        objName="MRNA2GENE",
        leftTable="mrna",
        leftCol="accession",
        from="mrna INNER JOIN gene USING(entrez_gene_id)",
        showCols="accession,entrez_gene_id,gene_symbol,gene_title,cytoband",
        rightTable="gene",
        rightCol="entrez_gene_id"
    ),
    list(
        objName="TR2MRNA",
        leftTable="transcript_cluster",
        leftCol="transcript_cluster_ID",
        from=paste("TR2mrna INNER JOIN mrna",
                     "USING(_mrna_id)",
                   "INNER JOIN TR2mrna_details",
                     "ON(TR2mrna._TR2mrna_id=TR2mrna_details._TR2mrna_id)"),
        showCols="transcript_cluster_ID,accession,source_name,description,assignment_seqname,assignment_score,assignment_coverage,direct_probes,possible_probes,xhyb",
        rightTable="mrna",
        rightCol="accession"
    ),
    list(
        objName="PBS2MRNA",
        leftTable="probeset",
        leftCol="probeset_ID",
        from="PBS2mrna INNER JOIN mrna USING(_mrna_id)",
        showCols="probeset_ID,accession,assignment_seqname,assignment_score,direct_probes,possible_probes,xhyb",
        rightTable="mrna",
        rightCol="accession"
    )
)

createAnnObjs.AFFYHUEX_DB <- function(prefix, objTarget, conn, datacache)
{
    ## AnnTable objects
    seed0 <- list(
        objTarget=objTarget,
        conn=conn,
        datacache=datacache
    )
    annobjs <- createAnnObjs("AnnTable", AFFYHUEX_DB_AnnTable_seeds, seed0)

    ## RawAnnMap objects
    seed0 <- list(
        objTarget=objTarget,
        conn=conn,
        datacache=datacache
    )
    createAnnObjs("RawAnnMap", AFFYHUEX_DB_RawAnnMap_seeds, seed0, annobjs)

    ## Some pre-caching
    left.names(annobjs$TRANSCRIPT)
    left.names(annobjs$PROBESET)

    prefixAnnObjNames(annobjs, prefix)
}

