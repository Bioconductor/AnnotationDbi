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

