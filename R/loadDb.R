###############################################################################
## loadDb is for loading a database from a string (path) to that .sqlite file.

## Helper makes queries more compact and allows debug.
dbEasyQuery <- function(conn, SQL, j0=NA)
{
   ## if (get("debugSQL", envir=RTobjs)) {
   ##      if (!is.character(SQL) || length(SQL) != 1L || is.na(SQL))
   ##          stop("[debugSQL] 'SQL' must be a single string")
   ##      cat("[debugSQL] SQL query: ", SQL, "\n", sep="")
   ##      st <- system.time(data0 <- dbGetQuery(conn, SQL))
   ##      cat("[debugSQL]      time: ", st["user.self"], " seconds\n", sep="")
   ##  } else {
        data0 <- dbGetQuery(conn, SQL)
    ## }
    if (is.na(j0))
        return(data0)
    ## Needed to deal properly with data frame with 0 column ("NULL data
    ## frames with 0 rows") returned by RSQLite when the result of a SELECT
    ## query has 0 row
    if (nrow(data0) == 0L)
        character(0)
    else
        data0[[j0]]
}

## gets field from metadata
.getMetaValue <- function(conn, name) {
    colnames <- c("name", "value")
    msg <- .valid.table.colnames(conn, "metadata", colnames)
    if (!is.null(msg))
        stop(msg)
    sql <-  paste0("SELECT * FROM metadata",
                   " WHERE name = '", name, "'")
    data <- dbEasyQuery(conn, sql)
    if (nrow(data) != 1L) {
        msg <- paste0("The metadata table in the DB has 0 or more ",
                      "than 1 '", name, "' entries")
        stop(msg)
    }
    data$value
}

.valid.colnames <- function(conn, tablename, colnames)
{
    sql0 <- paste0("SELECT * FROM ", tablename, " LIMIT 0")
    data0 <- dbEasyQuery(conn, sql0)
    colnames0 <- colnames(data0)
    if (!all(colnames %in% colnames0)) {
        msg <- paste0("the ", tablename, " table in the DB doesn't have ",
                      "all the expected columns (",
                      paste0("\"", colnames, "\"", collapse=", "),
                      ")")
        return(msg)
    }
    NULL
}

.valid.table.colnames <- function(conn, tablename, colnames)
{
    tmp <- try(dbExistsTable(conn, tablename), silent=TRUE)
    if (is(tmp, "try-error"))
        return("invalid DB file")
    if (!tmp)
        return(paste0("the DB has no ", tablename, " table"))
    .valid.colnames(conn, tablename, colnames)
}

.valid.metadata.table <- function(conn, name, value)
{
    colnames <- c("name", "value")
    msg <- .valid.table.colnames(conn, "metadata", colnames)
    if (!is.null(msg))
        return(msg)
    db_value <- try(.getMetaValue(conn, name), silent = TRUE)
    if(is(db_value, "try-error"))
        return(db_value[1])
    if (is.na(db_value) || db_value != value) {
        return(paste0("'", name, "' is not '", value, "'"))
    }
    NULL
}


## Two methods allow me to conceal the extra arguments and break the
## chicken-egg problem

## This is the hidden method (the one we won't usually use)
## it only exists so that we can have a loadDb method for each sub-class.
## we don't really intent do use the "character, character, character" method
## in practice (though I guess you could if you really wanted to do extra
## typing)
setMethod(loadDb, c("character", "character", "character"),
    function(file, dbType, dbPackage, ...)
{
    require(dbPackage, character.only=TRUE)
    db <- getClassDef(dbType, where=getNamespace(dbPackage))
    new(db, conn = dbConnect(SQLite(), file), ...)
})


## This is the method people will use
setMethod(loadDb, c("character", "missing", "missing"),
    function(file, dbType, dbPackage, ...)
{
    ## conn <- dbConnect(SQLite(), file)
    ## sql <- 'SELECT value FROM metadata WHERE name="Db type"'
    ## dbType <- dbGetQuery(conn, sql)[[1]]
    conn <- dbConnect(SQLite(), file)
    if(dbExistsTable(conn, "metadata")) {
        dbType <- tryCatch({
            .getMetaValue(conn, "Db type")
        }, error=function(err) {
            stop("the database is missing 'Db type' metadata\n  error: ",
                 conditionMessage(err))
        })
        ## TEMP: On 07/25/2014 TranscriptDb was renamed TxDb so we need to
        ## replace with TxDb until there are no more SQLite db files around
        ## that have 'Db type' set to 'TranscriptDb' (will take a couple of
        ## years).
        if (dbType == "TranscriptDb")
            dbType <- "TxDb"
        ## H.P.: 'Supporting package' is the new name for the 'package' entry
        ## (as of Feb 10, 2012). For now we keep backward compatibility with
        ## 'package' but at some point (in 1 year? 2 years?), this won't be
        ## needed anymore.
        dbPackage <- tryCatch({
            .getMetaValue(conn, "Supporting package")
        }, error=function(err1) {
            tryCatch({
                .getMetaValue(conn, "package")
            }, error=function(err2) {
                ## TEMP: if it's a TxDb or FeatureDb, lets give it a pass.
                if(dbType == "TxDb" || dbType == "FeatureDb")
                  return("GenomicFeatures")
                stop("no 'Supporting package' entry found ",
                     "in 'metadata' table of database:\n  ", file)
            })
        })
    }
    dbDisconnect(conn)
    loadDb(file, dbType, dbPackage, ...)
})


