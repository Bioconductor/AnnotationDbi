### =========================================================================
### Miscellaneous low-level utils
### -------------------------------------------------------------------------
###

### Used at load time (in .onLoad) by all SQLite-based ann data packages.
dbFileConnect <- function(dbfile)
{
    ## This is a protection against dbConnect() working even with non-existing
    ## files (for our use case, the .sqlite file _must_ exist):
    if (!file.exists(dbfile))
        stop("DB file '", dbfile, "' not found")

    ## Use of 'vsf="unix-none"' added on 01/11/2017 to avoid the infamous "da-
    ## tabase is locked" error that some users have reported on a few occasions
    ## in the past 10 years (and that we were never able to reproduce). Last
    ## time it was reported was on 01/11/2017 by Rob Bradley at Fred Hutch.
    ## Rob was trying to access TxDb.Hsapiens.UCSC.hg19.knownGene from multiple
    ## nodes on the Hutch cluster where TxDb.Hsapiens.UCSC.hg19.knownGene is
    ## installed on an NFS file system. Many thanks to Ben McGough, System
    ## Administrator at the Hutch, for investigating this and suggesting the
    ## 'vsf="unix-none"' solution.
    ## See ?`dbConnect,SQLiteDriver-method` and http://www.sqlite.org/vfs.html
    ## for more information about the the SQLite OS Interface or "VFS".
    if (.Platform$OS.type == "unix") {
        dbConnect(SQLite(), dbname=dbfile, cache_size=64000L, 
                  synchronous="off", flags=SQLITE_RO, vfs="unix-none")
    } else {
        ## Use default 'vfs' on Windows.
        dbConnect(SQLite(), dbname=dbfile, cache_size=64000L, 
                  synchronous="off", flags=SQLITE_RO)
    }
}

### Used at unload time (in .onUnload) by all SQLite-based ann data packages.
dbFileDisconnect <- function(dbconn)
{
    dbDisconnect(dbconn)
}

