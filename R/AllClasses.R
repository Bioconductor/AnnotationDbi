setClass("AnnotDbTable", representation(tableName="character",
                                        fieldNames="character",
                                        nrow="integer",
                                        firstRow="data.frame",
                                        dbRefGetter="function"))

setClass("AnnotDbTableTwoWayMap", contains="AnnotDbTable",
         representation(LHS="character", RHS="character"))

