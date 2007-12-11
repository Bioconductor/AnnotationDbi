### =========================================================================
### An SQLite-based ann data package (AnnDbPkg) provides a set of pre-defined
### AnnObj objects that are created at load-time. This set depends only on
### the underlying db schema i.e. all the SQLite-based ann data packages that
### share the same underlying db schema will provide the same set of AnnObj
### objects.
###
### This file describes the set of AnnObj objects provided by any
### INPARANOIDSACCE_DB-based package i.e. any SQLite-based ann data package
### based on the INPARANOIDSACCE_DB schema.
### The createAnnObjs.INPARANOIDSACCE_DB() function is the main entry point
### for this file: it is called by any INPARANOIDSACCE_DB-based package at
### load-time.
### -------------------------------------------------------------------------


### Mandatory fields: objName, Class and L2Rchain




createAnnObjs.INPARANOIDSACCE_DB <- function(prefix, objTarget, dbconn, datacache)
{
    # This list needs to be kept aligned with the aliases for the man pages
    species = "SACCE"
    fields = c(
          "aedes_aegypti" = "AEDAE",          
          "anopheles_gambiae" = "ANOGA",
          "apis_mellifera" = "APIME",
          "arabidopsis_thaliana" = "ARATH",
          "bos_taurus" = "BOSTA",
          "caenorhabditis_briggsae" = "CAEBR", 
          "caenorhabditis_elegans" = "CAEEL",
          "caenorhabditis_remanei" = "CAERE",
          "candida_glabrata" = "CANGL",
          "canis_familiaris" = "CANFA",
          "ciona_intestinalis" = "CIOIN",
          "cryptococcus_neoformans" = "CRYNE",
          "danio_rerio" = "DANRE",
          "debaryomyces_hanseneii" = "DEBHA",
          "dictyostelium_discoideum" = "DICDI",
          "drosophila_melanogaster" = "DROME",
          "drosophila_pseudoobscura" = "DROPS",
          "entamoeba_histolytica" = "ENTHI",
          "escherichia_coliK12" = "ESCCO",
          "gallus_gallus" = "GALGA",
          "gasterosteus_aculeatus" = "GASAC",
          "homo_sapiens" = "HOMSA",
          "kluyveromyces_lactis" = "KLULA",
          "macaca_mulatta" = "MACMU",
          "monodelphis_domestica" = "MONDO",
          "mus_musculus" = "MUSMU",
          "oryza_sativa" = "ORYSA",
          "pan_troglodytes" = "PANTR",         
          "rattus_norvegicus" = "RATNO",
         #"saccharomyces_cerevisiae" = "SACCE",    
          "schizosaccharomyces_pombe" = "SCHPO",
          "takifugu_rubripes" = "FUGRU",
          "tetraodon_nigroviridis" = "TETNI",  
          "xenopus_tropicalis" = "XENTR",
          "yarrowia_lipolytica" = "YARLI"
     )

    INPARANOIDSACCE_DB_AnnDbBimap_seeds <- makeSeedList(species, fields)
    
    checkDBSCHEMA(dbconn, "INPARANOIDSACCE_DB")

    ## AnnDbBimap objects 
    seed0 <- list(
        objTarget=objTarget,
        datacache=datacache
    )
    ann_objs <- createAnnDbBimaps(INPARANOIDSACCE_DB_AnnDbBimap_seeds, seed0)

    ann_objs$MAPCOUNTS <- createMAPCOUNTS(dbconn, prefix)

    prefixAnnObjNames(ann_objs, prefix)
}

