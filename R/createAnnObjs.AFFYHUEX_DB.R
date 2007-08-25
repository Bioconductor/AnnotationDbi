### =========================================================================
### Create all data objects for an annotation data package
### with db schema AFFYHUEX_DB
### -------------------------------------------------------------------------

### Mandatory fields: objName, from
AFFYHUEX_DB_AnnDbTable_seeds <- list(
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

AFFYHUEX_DB_RawAnnDbMap_seeds <- list(
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
    ## AnnDbTable and RawAnnDbMap objects
    seed0 <- list(
        objTarget=objTarget,
        conn=conn,
        datacache=datacache
    )
    ann_objs <- createAnnObjs("AnnDbTable", AFFYHUEX_DB_AnnDbTable_seeds, seed0)
    createAnnObjs("RawAnnDbMap", AFFYHUEX_DB_RawAnnDbMap_seeds, seed0, ann_objs)

    ## Some pre-caching
    Lkeys(ann_objs$TRANSCRIPT)
    Lkeys(ann_objs$PROBESET)

    prefixAnnObjNames(ann_objs, prefix)
}

