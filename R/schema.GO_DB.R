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

### A GO_DB-based package provides the following AnnObj objects:
###   objName      class
###   -----------  ----------------
###   ALLENTREZID  AtomicAnnDbMap
###   BPANCESTOR 
###   BPCHILDREN 
###   BPOFFSPRING
###   BPPARENTS
###   CCANCESTOR
###   CCCHILDREN
###   CCOFFSPRING
###   CCPARENTS
###   ENTREZID2GO
###   ENTREZID 
###   MFANCESTOR
###   MFCHILDREN
###   MFOFFSPRING
###   MFPARENTS
###   OBSOLETE
###   SYNONYM
###   TERM

# Pbs found in /home/nli/proj/ann/nli/annosrc/db/GO.sqlite (report to Nianhua):
#   - qcdata table: remove GO prefix from map names (map_name col) like for the
#     other qcdata in other schemas (consistency)
#   - good SQL practice would be to make map_name a primary key (unless there
#     is a good reason to use UNIQUE, the standard practice is to use PRIMARY)

### Mandatory fields: objName, rightTable and rightCol
GO_DB_AtomicAnnDbMap_seeds <- list(
    list(
        objName="ALLENTREZID",
        leftTable="go_term",
        leftCol="go_id",
        rightTable="go_all_gene",
        rightCol="gene_id",
        tagCols="evidence",
        join="INNER JOIN go_term USING (term_id)"
    ),
    list(
        objName="BPPARENTS",
        L2Rpath=list(go_term=c("go_id","term_id"),
                     go_bp_parents=c("term_id","parent_id"),
                     go_term=c("term_id","go_id")),
        tagCols="evidence"
    )
)

# myGOBPANCESTOR <- makeVector(db,
# "SELECT DISTINCT t2.go_id, t1.go_id FROM go_term AS t1, go_bp_offspring as r, go_term AS t2 WHERE t1.term_id=r.term_id AND t2.term_id=r.offspring_id")
# myGOBPCHILDREN <- makeNamedVector(db,
# "SELECT DISTINCT t1.parent_go, t1.evidence, t2.go_id FROM (SELECT t.go_id AS parent_go, r.term_id AS child_id, r.evidence AS evidence FROM go_term AS t LEFT OUTER JOIN go_bp_parents AS r ON r.parent_id=t.term_id WHERE t.ontology='BP') AS t1 LEFT OUTER JOIN go_term AS t2 ON t1.child_id=t2.term_id ORDER BY t1.parent_go;")
# myGOBPOFFSPRING <- makeVector(db,
# "SELECT DISTINCT t1.ancestor_go, t2.go_id FROM (SELECT t.go_id AS ancestor_go, r.offspring_id AS offspring_id FROM go_term AS t LEFT OUTER JOIN go_bp_offspring AS r ON r.term_id=t.term_id WHERE t.ontology='BP') AS t1 LEFT OUTER JOIN go_term AS t2 ON t1.offspring_id=t2.term_id ORDER BY t1.ancestor_go;")

createAnnObjs.GO_DB <- function(prefix, objTarget, conn, datacache)
{
    ## AtomicAnnDbMap and IpiAnnDbMap objects
    seed0 <- list(
        objTarget=objTarget,
        datacache=datacache,
        conn=conn
    )
    ann_objs <- createAnnObjs("AtomicAnnDbMap", GO_DB_AtomicAnnDbMap_seeds, seed0)

    ## 2 special maps that are not AnnDbMap objects (just named integer vectors)
    ann_objs$MAPCOUNTS <- createMAPCOUNTS(conn, prefix)

    prefixAnnObjNames(ann_objs, prefix)
}

