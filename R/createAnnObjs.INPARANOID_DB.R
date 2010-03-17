### =========================================================================
### An SQLite-based ann data package (AnnDbPkg) provides a set of pre-defined
### AnnObj objects that are created at load-time. This set depends only on
### the underlying db schema i.e. all the SQLite-based ann data packages that
### share the same underlying db schema will provide the same set of AnnObj
### objects.
###
### This file describes the set of AnnObj objects provided by any
### INPARANOID_DB-based package i.e. any SQLite-based ann data package
### based on the INPARANOID_DB schema.
### The createAnnObjs.INPARANOID_DB() function is the main entry point
### for this file: it is called by any INPARANOID_DB-based package at
### load-time.
### -------------------------------------------------------------------------


### Mandatory fields: objName, Class and L2Rchain


createAnnObjs.INPARANOID_DB <- function(prefix, objTarget, dbconn, datacache)
{
    # This list needs to figure out (from the source DB) which organism it is so that those mappings are not assigned.
    phyloName = dbmeta(datacache, 'ORGANISM') ##need to remove spaces
    phyloName = sub(" ", "_", phyloName)
    
    fields = c(
      "Acyrthosiphon_pisum" = "ACYPI",
      "Aedes_aegypti" = "AEDAE",
      "Anopheles_gambiae" = "ANOGA",
      "Apis_mellifera" = "APIME",
      "Arabidopsis_thaliana" = "ARATH",
      "Aspergillus_fumigatus" = "ASPFU",
      "Batrachochytrium_dendrobatidis" = "BATDE",
      "Bombyx_mori" = "BOMMO",
      "Bos_taurus" = "BOSTA",
      "Branchiostoma_floridae" = "BRAFL",
      "Brugia_malayi" = "BRUMA",
      "Caenorhabditis_brenneri" = "CAEBRE",
      "Caenorhabditis_briggsae" = "CAEBR",
      "Caenorhabditis_elegans" = "CAEEL",
      "Caenorhabditis_japonica" = "CAEJA",
      "Caenorhabditis_remanei" = "CAERE",
      "Candida_albicans" = "CANAL",
      "Candida_glabrata" = "CANGL",
      "Canis_familiaris" = "CANFA",
      "Capitella_spI" = "CAPSP",
      "Cavia_porcellus" = "CAVPO",
      "Chlamydomonas_reinhardtii" = "CHLRE",
      "Ciona_intestinalis" = "CIOIN",
      "Ciona_savignyi" = "CIOSA",
      "Coccidioides_immitis" = "COCIM",
      "Coprinopsis_cinereus" = "COPCI",
      "Cryptococcus_neoformans" = "CRYNE",
      "Cryptosporidium_hominis" = "CRYHO",
      "Cryptosporidium_parvum" = "CRYPA",
      "Culex_pipiens" = "CULPI",
      "Cyanidioschyzon_merolae" = "CYAME",
      "Danio_rerio" = "DANRE",
      "Daphnia_pulex" = "DAPPU",
      "Debaryomyces_hansenii" = "DEBHA",
      "Dictyostelium_discoideum" = "DICDI",
      "Drosophila_ananassae" = "DROAN",
      "Drosophila_grimshawi" = "DROGR",
      "Drosophila_melanogaster" = "DROME",
      "Drosophila_mojavensis" = "DROMO",
      "Drosophila_pseudoobscura" = "DROPS",
      "Drosophila_virilis" = "DROVI",
      "Drosophila_willistoni" = "DROWI",
      "Entamoeba_histolytica" = "ENTHI",
      "Equus_caballus" = "EQUCA",
      "Escherichia_coliK12" = "ESCCO",
      "Fusarium_graminearum" = "FUSGR",
      "Gallus_gallus" = "GALGA",
      "Gasterosteus_aculeatus" = "GASAC",
      "Giardia_lamblia" = "GIALA",
      "Helobdella_robusta" = "HELRO",
      "Homo_sapiens" = "HOMSA",
      "Ixodes_scapularis" = "IXOSC",
      "Kluyveromyces_lactis" = "KLULA",
      "Leishmania_major" = "LEIMA",
      "Lottia_gigantea" = "LOTGI",
      "Macaca_mulatta" = "MACMU",
      "Magnaporthe_grisea" = "MAGGR",
      "Monodelphis_domestica" = "MONDO",
      "Monosiga_brevicollis" = "MONBR",
      "Mus_musculus" = "MUSMU",
      "Nasonia_vitripennis" = "NASVI",
      "Nematostella_vectensis" = "NEMVE",
      "Neurospora_crassa" = "NEUCR",
      "Ornithorhynchus_anatinus" = "ORNAN",
      "Oryza_sativa" = "ORYSA",
      "Oryzias_latipes" = "ORYLA",
      "Ostreococcus_tauri" = "OSTTA",
      "Pan_troglodytes" = "PANTR",
      "Pediculus_humanus" = "PEDPA",
      "Physcomitrella_patens" = "PHYPA",
      "Phytophthora_ramorum" = "PHYRA",
      "Phytophthora_sojae" = "PHYSO",
      "Plasmodium_falciparum" = "PLAFA",
      "Plasmodium_vivax" = "PLAVI",
      "Pongo_pygmaeus" = "PONPY",
      "Populus_trichocarpa" = "POPTR",
      "Pristionchus_pacificus" = "PRIPA",
      "Puccinia_graminis" = "PUCGR",
      "Rattus_norvegicus" = "RATNO",
      "Rhizopus_oryzae" = "RHIOR",
      "Saccharomyces_cerevisiae" = "SACCE",
      "Schistosoma_mansoni" = "SCHMA",
      "Schizosaccharomyces_pombe" = "SCHPO",
      "Sclerotinia_sclerotiorum" = "SCLSC",
      "Sorghum_bicolor" = "SORBI",
      "Stagonospora_nodorum" = "STANO",
      "Strongylocentrotus_purpuratus" = "STRPU",
      "Takifugu_rubripes" = "TAKRU",
      "Tetrahymena_thermophila" = "TETTH",
      "Tetraodon_nigroviridis" = "TETNI",
      "Thalassiosira_pseudonana" = "THAPS",
      "Theileria_annulata" = "THEAN",
      "Theileria_parva" = "THEPA",
      "Tribolium_castaneum" = "TRICA",
      "Trichomonas_vaginalis" = "TRIVA",
      "Trichoplax_adhaerens" = "TRIAD",
      "Trypanosoma_cruzi" = "TRYCR",
      "Ustilago_maydis" = "USTMA",
      "Xenopus_tropicalis" = "XENTR",
      "Yarrowia_lipolytica" = "YARLI"
    )
    
    ##Use this to figure out what our "species" value is
    species = fields[phyloName]
    
    ##Finally, we want to remove the one thing from fields that we don't need...
    ind = grep(phyloName,names(fields))
    fields = fields[-ind]
    
    INPARANOID_DB_AnnDbBimap_seeds <- makeSeedList(species, fields)

    checkDBSCHEMA(dbconn, "INPARANOID_DB")

    ## AnnDbBimap objects 
    seed0 <- list(
        objTarget=objTarget,
        datacache=datacache
    )
    ann_objs <- createAnnDbBimaps(INPARANOID_DB_AnnDbBimap_seeds, seed0)

    ann_objs$MAPCOUNTS <- createMAPCOUNTS(dbconn, prefix)

    prefixAnnObjNames(ann_objs, prefix)
}
