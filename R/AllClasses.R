setClassUnion("functionORNULL",c("function", "NULL"))

setClass("AnnotDbTable", representation(tableName="character",
                                        fieldNames="character",
                                        nrow="integer",
                                        firstRow="data.frame",
                                        dbRefGetter="function",
					                    rsProcessor="functionORNULL"))

setClass("AnnotDbEnv", contains="AnnotDbTable")

setClass("AnnotDbTableTwoWayMap", contains="AnnotDbTable",
     representation(LHS="character", RHS="character"))

setClass("AnnotTwoColTable", contains=c("AnnotDbEnv", "AnnotDbTableTwoWayMap"))

setClass("AnnotMultiColTable", contains=c("AnnotDbEnv", "AnnotDbTable"), 
	representation(keyCol="character"))

setClass("AnnotMultiColTwoKeyTable", contains=c("AnnotMultiColTable"),
    representation(secKey="character"))

setClass("AnnotThreeColTable", contains="AnnotMultiColTable", 
	representation(nameCol="character", valCol="character"))

setClass("AnnotGOTermsTable", contains="AnnotMultiColTable")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setClass(
    "AnnDataPkgSeed",
    representation(
        pkg.template="character",    # e.g. "HGU95AV2DB"
        dbSchema="character",        # e.g. "HGU95AV2DB"
        mapPrefix="character",       # e.g. "hgu95av2"
        mapTarget="character",       # e.g. "chip hgu95av2"
        chipShortName="character",   # e.g. "hgu95av2"
        organism="character",
        species="character",
        manufacturer="character",
        chipName="character",
        manufacturerUrl="character"
    )
)

