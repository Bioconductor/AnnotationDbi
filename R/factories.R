generateTableObjects <- function(package) {
    ## Creates the TABLE_table objects.
    ## FIXME: For both of the generateXXX functions we need access to
    ## the DB and we need to create AnnotDbTable instances and give them
    ## a dbRefGetter, a function that allows them to retrieve the
    ## package-level DB connection.  Perhaps the solution is to use
    ## 'package' and pull the dbRefGetter out of that env.
    if (!missing(package))
      ns <- asNamespace(package)
    else
      ns <- .GlobalEnv
    getDb <- get("getDb", ns)
    tables <- dbListTables(getDb())
    for (table in tables) {
        var <- paste(table, "_table", sep="")
        assign(var,
               new("AnnotDbTable", tableName=table, dbRefGetter=getDb),
               envir=ns)
    }
}


twoWayTableMapFactory <- function(from, to, table, getDb) {
    new("AnnotDbTableTwoWayMap", LHS=from, RHS=to, tableName=table,
        dbRefGetter=getDb)
}


generateTwoWayMappings <- function(table, package) {
    ## Creates the TABLE_colA_to_colB objects.
    ## Creates all pairs of mappers for a given table.
    ## Creates the objects in the namespace of 'package'.
    if (!missing(package))
      ns <- asNamespace(package)
    else
      ns <- .GlobalEnv
    getDb <- get("getDb", ns)
    makeTwoWayMap <- function(from, to) {
        if (from == to)
          return(FALSE)
        var <- paste(table, "_", from, "_to_", to, sep="")
        val <- twoWayTableMapFactory(from, to, table, getDb)
        assign(var, val, envir=ns)
        TRUE
    }
    
    fields <- dbListFields(getDb(), table)
    for (from in fields) {
        for (to in fields) {
            if (from == to)
              next
            makeTwoWayMap(from, to)
        }
    }
}


