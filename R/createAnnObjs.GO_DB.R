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


### Mandatory fields: objName, Class and L2Rchain
GO_DB_AnnDbBimap_seeds <- list(
#    list(
#        objName="ENTREZID2GO",
#        Class="GoAnnDbBimap",
#        L2Rchain=list(
#            list(
#                tablename="go_gene",
#                Lcolname="gene_id",
#                tagname=c(Evidence="{evidence}"),
#                Rcolname="term_id"
#            ),
#            list(
#                tablename="go_term",
#                Lcolname="term_id",
#                Rcolname="go_id",
#                Rattribnames=c(Ontology="{ontology}")
#            )
#        )
#    ),
#    ## TODO: Try to define the ENTREZID map as the reverse of ENTREZID2GO
#    list(
#        objName="ENTREZID",
#        Class="AnnDbBimap",
#        L2Rchain=list(
#            list(
#                tablename="go_term",
#                Lcolname="go_id",
#                Rcolname="term_id"
#            ),
#            list(
#                tablename="go_gene",
#                Lcolname="term_id",
#                tagname=c(Evidence="{evidence}"),
#                Rcolname="gene_id"
#            )
#        )
#    ),
#    list(
#        objName="ALLENTREZID",
#        Class="AnnDbBimap",
#        L2Rchain=list(
#            list(
#                tablename="go_term",
#                Lcolname="go_id",
#                Rcolname="term_id"
#            ),
#            list(
#                tablename="go_all_gene",
#                Lcolname="term_id",
#                tagname=c(Evidence="{evidence}"),
#                Rcolname="gene_id"
#            )
#        )
#    ),
    list(
        objName="BPPARENTS",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="go_term",
                Lcolname="go_id",
                Rcolname="term_id",
                filter="{ontology}='BP'"
            ),
            list(
                tablename="go_bp_parents",
                Lcolname="term_id",
                tagname=c(RelationshipType="{evidence}"),
                Rcolname="parent_id"
            ),
            list(
                tablename="go_term",
                Lcolname="term_id",
                Rcolname="go_id"
            )
        )
    ),
    list(
        objName="CCPARENTS",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="go_term",
                Lcolname="go_id",
                Rcolname="term_id",
                filter="{ontology}='CC'"
            ),
            list(
                tablename="go_cc_parents",
                Lcolname="term_id",
                tagname=c(RelationshipType="{evidence}"),
                Rcolname="parent_id"
            ),
            list(
                tablename="go_term",
                Lcolname="term_id",
                Rcolname="go_id"
            )
        )
    ),
    list(
        objName="MFPARENTS",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="go_term",
                Lcolname="go_id",
                Rcolname="term_id",
                filter="{ontology}='MF'"
            ),
            list(
                tablename="go_mf_parents",
                Lcolname="term_id",
                tagname=c(RelationshipType="{evidence}"),
                Rcolname="parent_id"
            ),
            list(
                tablename="go_term",
                Lcolname="term_id",
                Rcolname="go_id"
            )
        )
    ),
    list(
        objName="BPANCESTOR",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="go_term",
                Lcolname="go_id",
                Rcolname="term_id",
                filter="{ontology}='BP'"
            ),
            list(
                tablename="go_bp_offspring",
                Lcolname="offspring_id",
                Rcolname="term_id"
            ),
            list(
                tablename="go_term",
                Lcolname="term_id",
                Rcolname="go_id"
            )
        )
    ),
    list(
        objName="CCANCESTOR",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="go_term",
                Lcolname="go_id",
                Rcolname="term_id",
                filter="{ontology}='CC'"
            ),
            list(
                tablename="go_cc_offspring",
                Lcolname="offspring_id",
                Rcolname="term_id"
            ),
            list(
                tablename="go_term",
                Lcolname="term_id",
                Rcolname="go_id"
            )
        )
    ),
    list(
        objName="MFANCESTOR",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="go_term",
                Lcolname="go_id",
                Rcolname="term_id",
                filter="{ontology}='MF'"
            ),
            list(
                tablename="go_mf_offspring",
                Lcolname="offspring_id",
                Rcolname="term_id"
            ),
            list(
                tablename="go_term",
                Lcolname="term_id",
                Rcolname="go_id"
            )
        )
    ),
    list(
        objName="TERM",
        Class="GOTermsAnnDbBimap",
        L2Rchain=list(
            list(
                tablename="go_term",
                Lcolname="go_id",
                Rcolname="go_id",
                Rattribnames=c(
                    Term="{term}",
                    Ontology="{ontology}",
                    Definition="{definition}",
                    Synonym="go_synonym.synonym",
                    Secondary="go_synonym.secondary"
                ),
                Rattrib_join="LEFT JOIN go_synonym ON {term_id}=go_synonym.term_id"
            )
        )
    ),
    list(
        objName="OBSOLETE",
        Class="GOTermsAnnDbBimap",
        L2Rchain=list(
            list(
                tablename="go_obsolete",
                Lcolname="go_id",
                Rcolname="go_id",
                Rattribnames=c(
                    Term="{term}",
                    Ontology="{ontology}",
                    Definition="{definition}",
                    ## The RSQLite driver crashes on queries like
                    ##   SELECT NULL, ... FROM ...
                    ## so a temporary workaround is to use
                    ##   SELECT '', ... FROM ...
                    #Synonym="NULL",
                    #Secondary="NULL"
                    Synonym="''",
                    Secondary="''"
                )
            )
        )
    ),
    list(
        objName="SYNONYM",
        Class="GOTermsAnnDbBimap",
        L2Rchain=list(
            list(
                tablename="go_synonym",
                Lcolname="synonym",
                Rcolname="term_id",
                filter="{like_go_id}=1"
            ),
            list(
                tablename="go_term",
                Lcolname="term_id",
                Rcolname="go_id",
                Rattribnames=c(
                    Term="{term}",
                    Ontology="{ontology}",
                    Definition="{definition}",
                    Synonym="go_synonym.synonym",
                    Secondary="go_synonym.secondary"
                ),
                Rattrib_join="LEFT JOIN go_synonym ON {term_id}=go_synonym.term_id"
            )
        )
    )
)

createAnnObjs.GO_DB <- function(prefix, objTarget, dbconn, datacache)
{
    checkDBSCHEMA(dbconn, "GO_DB")

    ## AnnDbBimap objects
    seed0 <- list(
        objTarget=objTarget,
        datacache=datacache
    )
    ann_objs <- createAnnDbBimaps(GO_DB_AnnDbBimap_seeds, seed0)

    ## Reverse maps
    revmap2 <- function(from, to)
    {
        map <- revmap(ann_objs[[from]], objName=to)
        L2Rchain <- map@L2Rchain
        tmp <- L2Rchain[[1]]@filter
        L2Rchain[[1]]@filter <- L2Rchain[[length(L2Rchain)]]@filter
        L2Rchain[[length(L2Rchain)]]@filter <- tmp
        map@L2Rchain <- L2Rchain
        map
    }
    ann_objs$BPCHILDREN <- revmap2("BPPARENTS", "BPCHILDREN")
    ann_objs$CCCHILDREN <- revmap2("CCPARENTS", "CCCHILDREN")
    ann_objs$MFCHILDREN <- revmap2("MFPARENTS", "MFCHILDREN")
    ann_objs$BPOFFSPRING <- revmap2("BPANCESTOR", "BPOFFSPRING")
    ann_objs$CCOFFSPRING <- revmap2("CCANCESTOR", "CCOFFSPRING")
    ann_objs$MFOFFSPRING <- revmap2("MFANCESTOR", "MFOFFSPRING")

    ## 1 special map that is not an AnnDbBimap object (just a named integer vector)
    ann_objs$MAPCOUNTS <- createMAPCOUNTS(dbconn, prefix)

    prefixAnnObjNames(ann_objs, prefix)
}

