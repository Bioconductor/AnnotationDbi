setMethod(loadDb, c("character", "character"),
    function(x, dbType, ...)
{
    getRefClass(dbType)$new(sqliteFile=x)
})

setMethod(loadDb, c("character", "missing"),
    function(x, dbType, ...)
{
    conn <- dbConnect(SQLite(), x)
    sql <- 'SELECT value FROM metadata WHERE name="Db type"'
    dbType <- dbGetQuery(conn, sql)[[1]]
    dbDisconnect(conn)
    loadDb(x, dbType, ...)
})
