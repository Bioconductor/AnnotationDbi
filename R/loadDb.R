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

.attach_supporting_pkg <- function(conn, db_type)
{
    supporting_pkg <- try(.getMetaValue(conn, "Supporting package"),
                          silent=TRUE)
    if (is(supporting_pkg, "try-error")) {
        if (is.null(getClassDef(db_type)))
            stop("this db is of type ", db_type, " but this is not ",
                 "a defined class")
    } else {
        if (!suppressWarnings(require(supporting_pkg, character.only=TRUE)))
            stop("this db is of type ", db_type, " and requires the ",
                 supporting_pkg, " package before it can be loaded")
    }
}

### We distinguish 2 uses of loadDb():
###   1. By the end-user for loading a standalone .sqlite file. In that case
###      'packageName' should NOT be specified.
###   2. By the .onLoad() hook of an annotation package. In that case
###      'packageName' must be specified and indicate the name of the
###      annotation package.
loadDb <- function(file, packageName=NA)
{
    conn <- dbFileConnect(file)
    stopifnot(dbExistsTable(conn, "metadata"))
    db_type <- .getMetaValue(conn, "Db type")
    ## TEMP: On 07/25/2014 TranscriptDb was renamed TxDb so we need to
    ## replace with TxDb until there are no more SQLite db files around
    ## that have 'Db type' set to 'TranscriptDb' (will take a couple of
    ## years).
    if (db_type == "TranscriptDb")
        db_type <- "TxDb"
    if (identical(packageName, NA)) {
        ## loadDb() was called by the end-user.
        ## The user doesn't necessarily have the supporting package in his/her
        ## search path so we try to attach it for him/her in order to get
        ## access to the definition of the class specified in 'db_type'.
        .attach_supporting_pkg(conn, db_type)
        packageName <- character(0)
    } else {
        ## loadDb() was called by the .onLoad() hook of package 'packageName'.
        ## This package *should* import the supporting package so we should
        ## already have access to the definition of the class specified in
        ## 'db_type'. There is no need to import the namespace of the
        ## supporting package.
        if (!(isSingleString(packageName) && packageName != ""))
            stop("'packageName' must be NA or a single non-empty string")
    }
    new(db_type, conn=conn, packageName=packageName)
}

