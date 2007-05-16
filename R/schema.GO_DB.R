### =========================================================================
### An SQLite-based ann data package (AnnDbPkg) provides a set of pre-defined
### AnnObj objects that are created at load-time. This set depends only on
### the underlying db schema i.e. all the SQLite-based ann data packages that
### share the same underlying db schema will provide the same set of AnnObj
### objects.
###
### This file describes the set of AnnObj objects provided by any
### GO_DB-based package i.e. any SQLite-based ann data package based
### on the GO_DB schema.
### The createAnnObjs.GO_DB() function is the main entry point for
### this file: it is called by any GO_DB-based package at load-time.
### -------------------------------------------------------------------------


### Still missing:
###   TERM, OBSOLETE, SYNONYM: right values are GONode objects

### Mandatory fields: objName and L2Rpath
GO_DB_AtomicAnnDbMap_seeds <- list(
    list(
        objName="ENTREZID",
        L2Rpath=list(go_term=c("go_id","term_id"),
                     go_gene=c("term_id","gene_id")),
        tagCols="evidence"
    ),
    list(
        objName="ALLENTREZID",
        L2Rpath=list(go_term=c("go_id","term_id"),
                     go_all_gene=c("term_id","gene_id")),
        tagCols="evidence"
    ),
    list(
        objName="BPPARENTS",
        L2Rpath=list(go_term=c("go_id","term_id"),
                     go_bp_parents=c("term_id","parent_id"),
                     go_term=c("term_id","go_id")),
        Lfilter="{{ontology}}='BP'",
        Rfilter="{{ontology}}='BP'",
        tagCols="evidence"
    ),
    list(
        objName="CCPARENTS",
        L2Rpath=list(go_term=c("go_id","term_id"),
                     go_cc_parents=c("term_id","parent_id"),
                     go_term=c("term_id","go_id")),
        Lfilter="{{ontology}}='CC'",
        Rfilter="{{ontology}}='CC'",
        tagCols="evidence"
    ),
    list(
        objName="MFPARENTS",
        L2Rpath=list(go_term=c("go_id","term_id"),
                     go_mf_parents=c("term_id","parent_id"),
                     go_term=c("term_id","go_id")),
        Lfilter="{{ontology}}='MF'",
        Rfilter="{{ontology}}='MF'",
        tagCols="evidence"
    ),
    list(
        objName="BPANCESTOR",
        L2Rpath=list(go_term=c("go_id","term_id"),
                     go_bp_offspring=c("offspring_id","term_id"),
                     go_term=c("term_id","go_id")),
        Lfilter="{{ontology}}='BP'",
        Rfilter="{{ontology}}='BP'"
    ),
    list(
        objName="CCANCESTOR",
        L2Rpath=list(go_term=c("go_id","term_id"),
                     go_cc_offspring=c("offspring_id","term_id"),
                     go_term=c("term_id","go_id")),
        Lfilter="{{ontology}}='CC'",
        Rfilter="{{ontology}}='CC'"
    ),
    list(
        objName="MFANCESTOR",
        L2Rpath=list(go_term=c("go_id","term_id"),
                     go_mf_offspring=c("offspring_id","term_id"),
                     go_term=c("term_id","go_id")),
        Lfilter="{{ontology}}='MF'",
        Rfilter="{{ontology}}='MF'"
    )
)

createAnnObjs.GO_DB <- function(prefix, objTarget, conn, datacache)
{
    ## AtomicAnnDbMap objects
    seed0 <- list(
        objTarget=objTarget,
        datacache=datacache,
        conn=conn
    )
    ann_objs <- createAnnObjs("AtomicAnnDbMap", GO_DB_AtomicAnnDbMap_seeds, seed0)

    ## RevAtomicAnnDbMap objects
    ## Nianhua uses the following query for the GOBPCHILDREN map:
    ##   SELECT DISTINCT t1.parent_go, t1.evidence, t2.go_id
    ##     FROM (SELECT t.go_id AS parent_go, r.term_id AS child_id, r.evidence AS evidence
    ##             FROM go_term AS t LEFT OUTER JOIN go_bp_parents AS r ON r.parent_id=t.term_id
    ##               WHERE t.ontology='BP') AS t1
    ##          LEFT OUTER JOIN go_term AS t2 ON t1.child_id=t2.term_id
    ##     ORDER BY t1.parent_go
    ## and the following query for the GOBPOFFSPRING map:
    ##   SELECT DISTINCT t1.ancestor_go, t2.go_id
    ##     FROM (SELECT t.go_id AS ancestor_go, r.offspring_id AS offspring_id
    ##             FROM go_term AS t LEFT OUTER JOIN go_bp_offspring AS r ON r.term_id=t.term_id
    ##               WHERE t.ontology='BP') AS t1
    ##          LEFT OUTER JOIN go_term AS t2 ON t1.offspring_id=t2.term_id
    ##     ORDER BY t1.ancestor_go
    ## What we do below (reverse the GOBPPARENTS and GOBPANCESTOR maps) results
    ## in much simpler queries (not nested SELECTs).
    ## TODO: Make sure that our simplification below is equivalent.
    ann_objs$BPCHILDREN <- revmap(ann_objs$BPPARENTS, objName="BPCHILDREN")
    ann_objs$CCCHILDREN <- revmap(ann_objs$CCPARENTS, objName="CCCHILDREN")
    ann_objs$MFCHILDREN <- revmap(ann_objs$MFPARENTS, objName="MFCHILDREN")
    ann_objs$BPOFFSPRING <- revmap(ann_objs$BPANCESTOR, objName="BPOFFSPRING")
    ann_objs$CCOFFSPRING <- revmap(ann_objs$CCANCESTOR, objName="CCOFFSPRING")
    ann_objs$MFOFFSPRING <- revmap(ann_objs$MFANCESTOR, objName="MFOFFSPRING")

    ## GoAnnDbMap object
    ann_objs$ENTREZID2GO <- new("GoAnnDbMap",
        objName="ENTREZID2GO",
        objTarget=objTarget,
        datacache=datacache,
        conn=conn,
        L2Rpath=list(go_gene=c("gene_id","term_id"),
                     go_term=c("term_id", "go_id")),
        tagCols=c("evidence", "ontology")
    )

    ## 2 special maps that are not AnnDbMap objects (just named integer vectors)
    ann_objs$MAPCOUNTS <- createMAPCOUNTS(conn, prefix)

    prefixAnnObjNames(ann_objs, prefix)
}

