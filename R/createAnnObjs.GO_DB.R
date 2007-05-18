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


### Mandatory fields: objName, Class and L2Rpath
GO_DB_AnnDbMap_seeds <- list(
    list(
        objName="ENTREZID2GO",
        Class="GoAnnDbMap",
        L2Rpath=list(
            list(
                table="go_gene",
                Lcolname="gene_id",
                Rcolname="term_id",
                tagCols=c(Evidence="{evidence}")
            ),
            list(
                table="go_term",
                Lcolname="term_id",
                Rcolname="go_id",
                tagCols=c(Ontology="{ontology}")
            )
        )
    ),
    ## TODO: Try to define the ENTREZID map as the reverse of ENTREZID2GO
    list(
        objName="ENTREZID",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            list(
                table="go_term",
                Lcolname="go_id",
                Rcolname="term_id"
            ),
            list(
                table="go_gene",
                Lcolname="term_id",
                Rcolname="gene_id",
                tagCols=c(Evidence="{evidence}")
            )
        )
    ),
    list(
        objName="ALLENTREZID",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            list(
                table="go_term",
                Lcolname="go_id",
                Rcolname="term_id"
            ),
            list(
                table="go_all_gene",
                Lcolname="term_id",
                Rcolname="gene_id",
                tagCols=c(Evidence="{evidence}")
            )
        )
    ),
    list(
        objName="BPPARENTS",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            list(
                table="go_term",
                Lcolname="go_id",
                Rcolname="term_id",
                filter="{ontology}='BP'"
            ),
            list(
                table="go_bp_parents",
                Lcolname="term_id",
                Rcolname="parent_id",
                tagCols=c(Evidence="{evidence}")
            ),
            list(
                table="go_term",
                Lcolname="term_id",
                Rcolname="go_id"
            )
        )
    ),
    list(
        objName="CCPARENTS",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            list(
                table="go_term",
                Lcolname="go_id",
                Rcolname="term_id",
                filter="{ontology}='CC'"
            ),
            list(
                table="go_cc_parents",
                Lcolname="term_id",
                Rcolname="parent_id",
                tagCols=c(Evidence="{evidence}")
            ),
            list(
                table="go_term",
                Lcolname="term_id",
                Rcolname="go_id"
            )
        )
    ),
    list(
        objName="MFPARENTS",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            list(
                table="go_term",
                Lcolname="go_id",
                Rcolname="term_id",
                filter="{ontology}='MF'"
            ),
            list(
                table="go_mf_parents",
                Lcolname="term_id",
                Rcolname="parent_id",
                tagCols=c(Evidence="{evidence}")
            ),
            list(
                table="go_term",
                Lcolname="term_id",
                Rcolname="go_id"
            )
        )
    ),
    list(
        objName="BPANCESTOR",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            list(
                table="go_term",
                Lcolname="go_id",
                Rcolname="term_id",
                filter="{ontology}='BP'"
            ),
            list(
                table="go_bp_offspring",
                Lcolname="offspring_id",
                Rcolname="term_id"
            ),
            list(
                table="go_term",
                Lcolname="term_id",
                Rcolname="go_id"
            )
        )
    ),
    list(
        objName="CCANCESTOR",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            list(
                table="go_term",
                Lcolname="go_id",
                Rcolname="term_id",
                filter="{ontology}='CC'"
            ),
            list(
                table="go_cc_offspring",
                Lcolname="offspring_id",
                Rcolname="term_id"
            ),
            list(
                table="go_term",
                Lcolname="term_id",
                Rcolname="go_id"
            )
        )
    ),
    list(
        objName="MFANCESTOR",
        Class="AtomicAnnDbMap",
        L2Rpath=list(
            list(
                table="go_term",
                Lcolname="go_id",
                Rcolname="term_id",
                filter="{ontology}='MF'"
            ),
            list(
                table="go_mf_offspring",
                Lcolname="offspring_id",
                Rcolname="term_id"
            ),
            list(
                table="go_term",
                Lcolname="term_id",
                Rcolname="go_id"
            )
        )
    ),
    list(
        objName="TERM",
        Class="GONodeAnnDbMap",
        L2Rpath=list(
            list(
                table="go_term",
                Lcolname="go_id",
                Rcolname="go_id",
                tagJoin="LEFT JOIN go_synonym ON {term_id}=go_synonym.term_id",
                tagCols=c(
                    Term="{term}",
                    Ontology="{ontology}",
                    Definition="{definition}",
                    Synonym="go_synonym.synonym",
                    Secondary="go_synonym.secondary"
                )
            )
        )
    ),
    list(
        objName="OBSOLETE",
        Class="GONodeAnnDbMap",
        L2Rpath=list(
            list(
                table="go_obsolete",
                Lcolname="go_id",
                Rcolname="go_id",
                tagCols=c(
                    Term="{term}",
                    Ontology="{ontology}",
                    Definition="{definition}",
                    Synonym="NULL",
                    Secondary="NULL"
                )
            )
        )
    ),
    list(
        objName="SYNONYM",
        Class="GONodeAnnDbMap",
        L2Rpath=list(
            list(
                table="go_synonym",
                Lcolname="synonym",
                Rcolname="term_id",
                filter="{like_go_id}=1"
            ),
            list(
                table="go_term",
                Lcolname="term_id",
                Rcolname="go_id",
                tagJoin="LEFT JOIN go_synonym ON {term_id}=go_synonym.term_id",
                tagCols=c(
                    Term="{term}",
                    Ontology="{ontology}",
                    Definition="{definition}",
                    Synonym="go_synonym.synonym",
                    Secondary="go_synonym.secondary"
                )
            )
        )
    )
)

createAnnObjs.GO_DB <- function(prefix, objTarget, conn, datacache)
{
    ## AnnDbMap objects
    seed0 <- list(
        objTarget=objTarget,
        datacache=datacache,
        conn=conn
    )
    ann_objs <- createAnnDbMaps(GO_DB_AnnDbMap_seeds, seed0)

    ## RevAnnDbMap objects
    revmap2 <- function(from, to)
    {
        map <- revmap(ann_objs[[from]], objName=to)
        L2Rpath <- map@L2Rpath
        tmp <- L2Rpath[[1]]@filter
        L2Rpath[[1]]@filter <- L2Rpath[[length(L2Rpath)]]@filter
        L2Rpath[[length(L2Rpath)]]@filter <- tmp
        map@L2Rpath <- L2Rpath
        map
    }
    ann_objs$BPCHILDREN <- revmap2("BPPARENTS", "BPCHILDREN")
    ann_objs$CCCHILDREN <- revmap2("CCPARENTS", "CCCHILDREN")
    ann_objs$MFCHILDREN <- revmap2("MFPARENTS", "MFCHILDREN")
    ann_objs$BPOFFSPRING <- revmap2("BPANCESTOR", "BPOFFSPRING")
    ann_objs$CCOFFSPRING <- revmap2("CCANCESTOR", "CCOFFSPRING")
    ann_objs$MFOFFSPRING <- revmap2("MFANCESTOR", "MFOFFSPRING")

    ## 1 special map that is not an AnnDbMap object (just a named integer vector)
    ann_objs$MAPCOUNTS <- createMAPCOUNTS(conn, prefix)

    prefixAnnObjNames(ann_objs, prefix)
}

