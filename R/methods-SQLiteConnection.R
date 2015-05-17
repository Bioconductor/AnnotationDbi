setMethod("dbconn", "SQLiteConnection", function(x) x)

setMethod("dbfile", "SQLiteConnection", function(x) x@dbname)
