### =========================================================================
### An SQLite-based ann data package (AnnDbPkg) provides a set of pre-defined
### AnnObj objects that are created at load-time. This set depends only on
### the underlying db schema i.e. all the SQLite-based ann data packages that
### share the same underlying db schema will provide the same set of AnnObj
### objects.
###
### This file describes the set of AnnObj objects provided by any
### PFAM_DB-based package i.e. any SQLite-based ann data package based
### on the PFAM_DB schema.
### The createAnnObjs.PFAM_DB() function is the main entry point for
### this file: it is called by any PFAM_DB-based package at load-time.
### -------------------------------------------------------------------------


PFAM_DB_L2Rlink1 <- list(tablename="probes", Lcolname="probe_id", Rcolname="_id")

### Mandatory fields: objName, Class and L2Rchain
PFAM_DB_AnnDbBimap_seeds <- list(
    list(
        objName="CAZY",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="cazy",
                Lcolname="ac",
                Rcolname="cazy"
            )
        )
    ),
    list(
        objName="DE",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="de",
                Lcolname="ac",
                Rcolname="de"
            )
        )
    ),
    list(
        objName="HOMSTRAD",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="homstrad",
                Lcolname="ac",
                Rcolname="homstrad"
            )
        )
    ),
    list(
        objName="ID",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="id",
                Lcolname="ac",
                Rcolname="id"
            )
        )
    ),
    list(
        objName="INTERPRO",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="interpro",
                Lcolname="ac",
                Rcolname="interpro"
            )
        )
    ),
    list(
        objName="LOAD",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="load",
                Lcolname="ac",
                Rcolname="load"
            )
        )
    ),
    list(
        objName="MEROPS",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="merops",
                Lcolname="ac",
                Rcolname="merops"
            )
        )
    ),
    list(
        objName="MIM",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="mim",
                Lcolname="ac",
                Rcolname="mim"
            )
        )
    ),
    ###THIS ONE HAS MORE STUFF to display
    list(
        objName="PDB",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="pdb",
                Lcolname="ac",
                Rcolname="pdb",
                Rattribnames=c(StartPoint="{start_point}",EndPoint="{end_point}" )     
            )
        )
    ),
##     list(
##         objName="PFAMB",
##         Class="AnnDbBimap",
##         L2Rchain=list(
##             list(
##                 tablename="pfamb",
##                 Lcolname="ac",
##                 Rcolname="pfamb"
##             )
##         )
##     ),
    list(
        objName="PRINTS",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="prints",
                Lcolname="ac",
                Rcolname="prints"
            )
        )
    ),
    list(
        objName="PROSITE",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="prosite",
                Lcolname="ac",
                Rcolname="prosite"
            )
        )
    ),
    list(
        objName="PROSITEPROFILE",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="prosite_profile",
                Lcolname="ac",
                Rcolname="prosite_profile"
            )
        )
    ),
    list(
        objName="RM",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="rm",
                Lcolname="ac",
                Rcolname="rm"
            )
        )
    ),
    ### Use an R attrib for this one.
    list(
        objName="SCOP",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="scop",
                Lcolname="ac",
                Rcolname="scop",
                Rattribnames=c(Placement="{placement}")                  
            )
        )
    ),                                 
    list(
        objName="SMART",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="smart",
                Lcolname="ac",
                Rcolname="smart"
            )
        )
    ),
    list(
        objName="TC",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="tc",
                Lcolname="ac",
                Rcolname="tc"
            )
        )
    ),
    list(
        objName="TP",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="tp",
                Lcolname="ac",
                Rcolname="tp"
            )
        )
    ),                                                                 
    list(
        objName="URL",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="url",
                Lcolname="ac",
                Rcolname="url"
            )
        )
    )


                                 
)

createAnnObjs.PFAM_DB <- function(prefix, objTarget, dbconn, datacache)
{
    checkDBSCHEMA(dbconn, "PFAM_DB")

    ## AnnDbBimap objects
    seed0 <- list(
        objTarget=objTarget,
        datacache=datacache
    )
    ann_objs <- createAnnDbBimaps(PFAM_DB_AnnDbBimap_seeds, seed0)

    ## Reverse maps
##     ann_objs$ENZYME2PROBE <- revmap(ann_objs$ENZYME, objName="ENZYME2PROBE")
##     ann_objs$PATH2PROBE <- revmap(ann_objs$PATH, objName="PATH2PROBE")
##     ann_objs$PMID2PROBE <- revmap(ann_objs$PMID, objName="PMID2PROBE")
##     ann_objs$GO2PROBE <- revmap(ann_objs$GO, objName="GO2PROBE")
##     ann_objs$ENSEMBL2PROBE <- revmap(ann_objs$ENSEMBL, objName="ENSEMBL2PROBE")
##     map <- ann_objs$GO2PROBE
##     map@rightTables <- Go3tablenames(all=TRUE)
##     map@objName <- "GO2ALLPROBES"
##     ann_objs$GO2ALLPROBES <- map

    
    ann_objs$CAZY2AC <- revmap(ann_objs$CAZY, objName="CAZY2AC")
    ann_objs$DE2AC <- revmap(ann_objs$DE, objName="DE2AC")
    ann_objs$HOMSTRAD2AC <- revmap(ann_objs$HOMSTRAD, objName="HOMSTRAD2AC")
    ann_objs$ID2AC <- revmap(ann_objs$ID, objName="ID2AC")
    ann_objs$INTERPRO2AC <- revmap(ann_objs$INTERPRO, objName="INTERPRO2AC")
    ann_objs$LOAD2AC <- revmap(ann_objs$LOAD, objName="LOAD2AC")
    ann_objs$MEROPS2AC <- revmap(ann_objs$MEROPS, objName="MEROPS2AC")
    ann_objs$MIM2AC <- revmap(ann_objs$MIM, objName="MIM2AC")
    ann_objs$PDB2AC <- revmap(ann_objs$PDB, objName="PDB2AC")
##     ann_objs$PFAMB2AC <- revmap(ann_objs$PFAMB, objName="PFAMB2AC")
    ann_objs$PRINTS2AC <- revmap(ann_objs$PRINTS, objName="PRINTS2AC")
    ann_objs$PROSITE2AC <- revmap(ann_objs$PROSITE, objName="PROSITE2AC")
    ann_objs$PROSITEPROFILE2AC <- revmap(ann_objs$PROSITEPROFILE, objName="PROSITEPROFILE2AC")
    ann_objs$RM2AC <- revmap(ann_objs$RM, objName="RM2AC")
    ann_objs$SCOP2AC <- revmap(ann_objs$SCOP, objName="SCOP2AC")
    ann_objs$SMART2AC <- revmap(ann_objs$SMART, objName="SMART2AC") #
    ann_objs$TC2AC <- revmap(ann_objs$TC, objName="TC2AC")
    ann_objs$TP2AC <- revmap(ann_objs$TP, objName="TP2AC")
    ann_objs$URL2AC <- revmap(ann_objs$URL, objName="URL2AC")
    

    
    
    ## 2 special maps that are not AnnDbBimap objects (just named integer vectors)
##     ann_objs$CHRLENGTHS <- createCHRLENGTHS(dbconn)
    ann_objs$MAPCOUNTS <- createMAPCOUNTS(dbconn, prefix)

    ## Some pre-caching
#    Lkeys(ann_objs$GO)
    #mappedLkeys(ann_objs$GO)
    #Rkeys(ann_objs$GO2PROBE)
    #mappedRkeys(ann_objs$GO2PROBE)
    #Rkeys(ann_objs$GO2ALLPROBES)
    #mappedRkeys(ann_objs$GO2ALLPROBES)

    prefixAnnObjNames(ann_objs, prefix)
}

