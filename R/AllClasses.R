setClass(
    "AnnDataPkgSeed",
    representation(
        pkg.template="character",    # e.g. "HGU95AV2DB"
        dbSchema="character",        # e.g. "HGU95AV2DB"
        mapPrefix="character",       # e.g. "hgu95av2"
        mapTarget="character",       # e.g. "chip hgu95av2"
        organism="character",
        species="character",
        manufacturer="character",
        chipName="character",
        manufacturerUrl="character"
    )
)

