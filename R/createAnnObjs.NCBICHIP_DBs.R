## Generic code to createAnnObjs for bimaps of NCBICHIP packages

## 1st the big list:

NCBICHIP_DB_SeedGenerator <- function(orgPkg){

#orgPkg = "org.Hs.eg"  ## Oh crap

NCBICHIP_DB_L2Rlink1 <- list(tablename="probes", Lcolname="probe_id", Rcolname="gene_id", filter="{is_multiple}='0'")
NCBICHIP_DB_L2Rlink2 <- list(tablename="genes", Lcolname="gene_id", Rcolname="_id", altDB=orgPkg)



### Mandatory fields: objName, Class and L2Rchain
NCBICHIP_DB_AnnDbBimap_seeds <- list(
    list(
        objName="ACCNUM",
        Class="ProbeAnnDbBimap",
        L2Rchain=list(
            list(
                tablename="accessions",
                Lcolname="probe_id",
                Rcolname="accession"
            )
        )
    ),
    list(
        objName="ALIAS2PROBE",
        Class="ProbeAnnDbBimap",
        L2Rchain=list(
            NCBICHIP_DB_L2Rlink1,
            NCBICHIP_DB_L2Rlink2,
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
        Class="ProbeAnnDbBimap",
        L2Rchain=list(
            NCBICHIP_DB_L2Rlink1,
            NCBICHIP_DB_L2Rlink2,
            list(
                tablename="chromosomes",
                Lcolname="_id",
                Rcolname="chromosome",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="ENTREZID",
        Class="ProbeAnnDbBimap",
        L2Rchain=list(
            NCBICHIP_DB_L2Rlink1,
            NCBICHIP_DB_L2Rlink2,
            list(
                tablename="genes",
                Lcolname="_id",
                Rcolname="gene_id",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="ENZYME",
        Class="ProbeAnnDbBimap",
        L2Rchain=list(
            NCBICHIP_DB_L2Rlink1,
            NCBICHIP_DB_L2Rlink2,
            list(
                tablename="ec",
                Lcolname="_id",
                Rcolname="ec_number",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="GENENAME",
        Class="ProbeAnnDbBimap",
        L2Rchain=list(
            NCBICHIP_DB_L2Rlink1,
            NCBICHIP_DB_L2Rlink2,
            list(
                tablename="gene_info",
                Lcolname="_id",
                Rcolname="gene_name",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="MAP",
        Class="ProbeAnnDbBimap",
        L2Rchain=list(
            NCBICHIP_DB_L2Rlink1,
            NCBICHIP_DB_L2Rlink2,
            list(
                tablename="cytogenetic_locations",
                Lcolname="_id",
                Rcolname="cytogenetic_location",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="OMIM",
        Class="ProbeAnnDbBimap",
        L2Rchain=list(
            NCBICHIP_DB_L2Rlink1,
            NCBICHIP_DB_L2Rlink2,
            list(
                tablename="omim",
                Lcolname="_id",
                Rcolname="omim_id",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="PATH",
        Class="ProbeAnnDbBimap",
        L2Rchain=list(
            NCBICHIP_DB_L2Rlink1,
            NCBICHIP_DB_L2Rlink2,
            list(
                tablename="kegg",
                Lcolname="_id",
                Rcolname="path_id",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="PMID",
        Class="ProbeAnnDbBimap",
        L2Rchain=list(
            NCBICHIP_DB_L2Rlink1,
            NCBICHIP_DB_L2Rlink2,
            list(
                tablename="pubmed",
                Lcolname="_id",
                Rcolname="pubmed_id",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="REFSEQ",
        Class="ProbeAnnDbBimap",
        L2Rchain=list(
            NCBICHIP_DB_L2Rlink1,
            NCBICHIP_DB_L2Rlink2,
            list(
                tablename="refseq",
                Lcolname="_id",
                Rcolname="accession",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="SYMBOL",
        Class="ProbeAnnDbBimap",
        L2Rchain=list(
            NCBICHIP_DB_L2Rlink1,
            NCBICHIP_DB_L2Rlink2,
            list(
                tablename="gene_info",
                Lcolname="_id",
                Rcolname="symbol",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="UNIGENE",
        Class="ProbeAnnDbBimap",
        L2Rchain=list(
            NCBICHIP_DB_L2Rlink1,
            NCBICHIP_DB_L2Rlink2,
            list(
                tablename="unigene",
                Lcolname="_id",
                Rcolname="unigene_id",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="CHRLOC",
        Class="ProbeAnnDbMap",
        L2Rchain=list(
            NCBICHIP_DB_L2Rlink1,
            NCBICHIP_DB_L2Rlink2,
            list(
                tablename="chromosome_locations",
                Lcolname="_id",
                tagname=c(Chromosome="{seqname}"),
                Rcolname="start_location",
                altDB=orgPkg
            )
        ),
        rightColType="integer"
    ),
    list(
        objName="CHRLOCEND",
        Class="ProbeAnnDbMap",
        L2Rchain=list(
            NCBICHIP_DB_L2Rlink1,
            NCBICHIP_DB_L2Rlink2,
            list(
                tablename="chromosome_locations",
                Lcolname="_id",
                tagname=c(Chromosome="{seqname}"),
                Rcolname="end_location",
                altDB=orgPkg
            )
        ),
        rightColType="integer"
    ),
    list(
        objName="PFAM",
        Class="ProbeIpiAnnDbMap",
        L2Rchain=list(
            NCBICHIP_DB_L2Rlink1,
            NCBICHIP_DB_L2Rlink2,
            list(
                tablename="pfam",
                Lcolname="_id",
                Rcolname="ipi_id",
                Rattribnames=c(PfamId="{pfam_id}"),
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="PROSITE",
        Class="ProbeIpiAnnDbMap",
        L2Rchain=list(
            NCBICHIP_DB_L2Rlink1,
            NCBICHIP_DB_L2Rlink2,
            list(
                tablename="prosite",
                Lcolname="_id",
                Rcolname="ipi_id",
                Rattribnames=c(PrositeId="{prosite_id}"),
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="ENSEMBL",
        Class="ProbeAnnDbBimap",
        L2Rchain=list(
            NCBICHIP_DB_L2Rlink1,
            NCBICHIP_DB_L2Rlink2,
            list(
                tablename="ensembl",
                Lcolname="_id",
                Rcolname="ensembl_id",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="UNIPROT",
        Class="ProbeAnnDbBimap",
        L2Rchain=list(
            NCBICHIP_DB_L2Rlink1,
            NCBICHIP_DB_L2Rlink2,
            list(
                tablename="uniprot",
                Lcolname="_id",
                Rcolname="uniprot_id",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="FLYBASE",
        Class="ProbeAnnDbBimap",
        L2Rchain=list(
            NCBICHIP_DB_L2Rlink1,
            NCBICHIP_DB_L2Rlink2,
            list(
                tablename="flybase",
                Lcolname="_id",
                Rcolname="flybase_id",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="FLYBASECG",
        Class="ProbeAnnDbBimap",
        L2Rchain=list(
            NCBICHIP_DB_L2Rlink1,
            NCBICHIP_DB_L2Rlink2,
            list(
                tablename="flybase_cg",
                Lcolname="_id",
                Rcolname="flybase_cg_id",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="MGI",
        Class="ProbeAnnDbBimap",
        L2Rchain=list(
            NCBICHIP_DB_L2Rlink1,
            NCBICHIP_DB_L2Rlink2,
            list(
                tablename="mgi",
                Lcolname="_id",
                Rcolname="mgi_id",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="WORMBASE",
        Class="ProbeAnnDbBimap",
        L2Rchain=list(
            NCBICHIP_DB_L2Rlink1,
            NCBICHIP_DB_L2Rlink2,
            list(
                tablename="wormbase",
                Lcolname="_id",
                Rcolname="wormbase_id",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="ZFIN",
        Class="ProbeAnnDbBimap",
        L2Rchain=list(
            NCBICHIP_DB_L2Rlink1,
            NCBICHIP_DB_L2Rlink2,
            list(
                tablename="zfin",
                Lcolname="_id",
                Rcolname="zfin_id",
                altDB=orgPkg
            )
        )
    ),
    list(
        objName="GO",
        Class="ProbeGo3AnnDbBimap",
        L2Rchain=list(
            NCBICHIP_DB_L2Rlink1,
            NCBICHIP_DB_L2Rlink2,
            list(
                Lcolname="_id",
                tagname=c(Evidence="{evidence}"),
                Rcolname="go_id",
                Rattribnames=c(Ontology="NULL"),
                altDB=orgPkg
            )
        ),
        rightTables=Go3tablenames()
    )
)
## return this
NCBICHIP_DB_AnnDbBimap_seeds
}



## Then we need to define our specific functions.

createAnnObjs.BOVINECHIP_DB <- function(prefix,
                                        objTarget,
                                        dbconn,
                                        datacache,
                                        schema="BOVINECHIP_DB",
                                        class="ChipDb",
                                        dbname="org.Bt.eg"){
  createAnnObjs.NCBI_DB(prefix,objTarget,dbconn,datacache,schema,class,
                        dbname=dbname)
}


createAnnObjs.CANINECHIP_DB <- function(prefix,
                                        objTarget,
                                        dbconn,
                                        datacache,
                                        schema="CANINECHIP_DB",
                                        class="ChipDb",
                                        dbname="org.Cf.eg"){
  createAnnObjs.NCBI_DB(prefix,objTarget,dbconn,datacache,schema,class,
                        dbname=dbname)
}


createAnnObjs.CHICKENCHIP_DB <- function(prefix,
                                         objTarget,
                                         dbconn,
                                         datacache,
                                         schema="CHICKENCHIP_DB",
                                         class="ChipDb",
                                         dbname="org.Gg.eg"){
  createAnnObjs.NCBI_DB(prefix,objTarget,dbconn,datacache,schema,class,
                        dbname=dbname)
}


createAnnObjs.ECOLICHIP_DB <- function(prefix,
                                       objTarget,
                                       dbconn,
                                       datacache,
                                       schema="ECOLICHIP_DB",
                                       class="ChipDb",
                                       dbname="org.EcK12.eg"){
  createAnnObjs.NCBI_DB(prefix,objTarget,dbconn,datacache,schema,class,
                        dbname=dbname)
}


createAnnObjs.FLYCHIP_DB <- function(prefix,
                                     objTarget,
                                     dbconn,
                                     datacache,
                                     schema="FLYCHIP_DB",
                                     class="ChipDb",
                                     dbname="org.Dm.eg"){
  createAnnObjs.NCBI_DB(prefix,objTarget,dbconn,datacache,schema,class,
                        dbname=dbname)
}


createAnnObjs.HUMANCHIP_DB <- function(prefix,
                                       objTarget,
                                       dbconn,
                                       datacache,
                                       schema="HUMANCHIP_DB",
                                       class="ChipDb",
                                       dbname="org.Hs.eg"){
  createAnnObjs.NCBI_DB(prefix,objTarget,dbconn,datacache,schema,class,
                        dbname=dbname)
}


createAnnObjs.MOUSECHIP_DB <- function(prefix,
                                       objTarget,
                                       dbconn,
                                       datacache,
                                       schema="MOUSECHIP_DB",
                                       class="ChipDb",
                                       dbname="org.Mm.eg"){
  createAnnObjs.NCBI_DB(prefix,objTarget,dbconn,datacache,schema,class,
                        dbname=dbname)
}


createAnnObjs.PIGCHIP_DB <- function(prefix,
                                     objTarget,
                                     dbconn,
                                     datacache,
                                     schema="PIGCHIP_DB",
                                     class="ChipDb",
                                     dbname="org.Ss.eg"){
  createAnnObjs.NCBI_DB(prefix,objTarget,dbconn,datacache,schema,class,
                        dbname=dbname)
}


createAnnObjs.RATCHIP_DB <- function(prefix,
                                     objTarget,
                                     dbconn,
                                     datacache,
                                     schema="RATCHIP_DB",
                                     class="ChipDb",
                                     dbname="org.Rn.eg"){
  createAnnObjs.NCBI_DB(prefix,objTarget,dbconn,datacache,schema,class,
                        dbname=dbname)
}


createAnnObjs.RHESUSCHIP_DB <- function(prefix,
                                        objTarget,
                                        dbconn,
                                        datacache,
                                        schema="RHESUSCHIP_DB",
                                        class="ChipDb",
                                        dbname="org.Mmu.eg"){
  createAnnObjs.NCBI_DB(prefix,objTarget,dbconn,datacache,schema,class,
                        dbname=dbname)
}


createAnnObjs.WORMCHIP_DB <- function(prefix,
                                      objTarget,
                                      dbconn,
                                      datacache,
                                      schema="WORMCHIP_DB",
                                      class="ChipDb",
                                      dbname="org.Ce.eg"){
  createAnnObjs.NCBI_DB(prefix,objTarget,dbconn,datacache,schema,class,
                        dbname=dbname)
}


createAnnObjs.XENOPUSCHIP_DB <- function(prefix,
                                         objTarget,
                                         dbconn,
                                         datacache,
                                         schema="XENOPUSCHIP_DB",
                                         class="ChipDb",
                                         dbname="org.Xl.eg"){
  createAnnObjs.NCBI_DB(prefix,objTarget,dbconn,datacache,schema,class,
                        dbname=dbname)
}


createAnnObjs.ZEBRAFISHCHIP_DB<- function(prefix,
                                          objTarget,
                                          dbconn,
                                          datacache,
                                          schema="ZEBRAFISHCHIP_DB",
                                          class="ChipDb",
                                          dbname="org.Dr.eg"){
  createAnnObjs.NCBI_DB(prefix,objTarget,dbconn,datacache,schema,class,
                        dbname=dbname)
}

