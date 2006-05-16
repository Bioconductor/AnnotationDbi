setClassUnion("functionORNULL",c("function", "NULL"))

setClass("AnnotDbTable", representation(tableName="character",
                                        fieldNames="character",
                                        nrow="integer",
                                        firstRow="data.frame",
                                        dbRefGetter="function",
					rsProcessor="functionORNULL"))

setClass("AnnotDbTableTwoWayMap", contains="AnnotDbTable",
         representation(LHS="character", RHS="character"))

setClass("AnnotMultiColTable", contains="AnnotDbTable", 
	representation(keyCol="character"))

setClass("AnnotThreeColTable", contains="AnnotMultiColTable", 
	representation(nameCol="character", valCol="character"))

setClass("AnnotGOTermsTable", contains="AnnotMultiColTable")

