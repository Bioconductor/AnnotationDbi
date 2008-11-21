
#Just a utility to prevent empty IDs from ever causing any more mayhem
cleanSrcMap <- function(file) {
    insVals <- read.delim(file=file, header=FALSE, sep="\t", quote="", colClasses = "character")
    insVals <- insVals[insVals[,1]!='',]

    #later we might want to cycle through all the IDs that people put and try each one, but right now lets try the 1st one
    #that they gave us instead of just failing silently. (which is definitely a BAD thing)
#    for(i in 1:length(insVals[,1])){
        insVals[,2] = sub(';.+', '', insVals[,2], perl=TRUE)
#    }
        
    #need to watch for doubles which get cast by sqlite in BAD ways...
##     if(typeof(insVals[1,1])=="double" || typeof(insVals[1,2])=="double"){
##         message(cat("baseMap contained doubles"))
##         col1 <- unlist(lapply(unlist(insVals[,1]),function(x) as.character(x)))
##         col2 <- unlist(lapply(unlist(insVals[,2]),function(x) as.character(x)))
##         insVals <- as.data.frame(cbind(col1,col2))
##     }
    insVals
}

cleanRefSeqs <- function(baseMap){
    baseMap = as.matrix(baseMap)
#    for(i in 1:length(baseMap[,1])){
        baseMap[,2] = sub("\\.\\d+?$", "", baseMap[,2], perl=TRUE)
#    }
    baseMap = cbind(baseMap[,1],baseMap[,2])
    baseMap = as.data.frame(baseMap)
    baseMap
}

printOutBaseMaps <- function(baseMap, pkgName, otherSrcObjs){
    write.table(baseMap, paste(pkgName,"_baseMap.txt", sep=""), quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)
    for(i in 1:length(otherSrcObjs)){
        write.table(otherSrcObjs[i], paste(pkgName,"_otherSrcMap_",i,".txt", sep=""), quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)
    }
}

makeBaseMaps <- function(csvFileName,
                          GenBankIDName="Representative.Public.ID",
                          EntrezGeneIDName="Entrez.Gene",
                          targetDir="."
                          ) {
    csvFile <- read.csv(csvFileName, as.is=TRUE, na.strings="---", 
		colClasses="character", skip = 12)
    probe <- csvFile[,1]
    gb <- csvFile[, GenBankIDName]
    eg <- csvFile[, EntrezGeneIDName]
    rm(csvFile)
    gb <- lapply(unlist(gb), function(x) toupper(strsplit(x,"\\.")[[1]][1]))
    gb <- unlist(gb)
    eg <- strsplit(eg, split=" /// ", fix=T)
    eg_count <- sapply(eg, length)
    eg_probe <- rep(probe, eg_count)
    eg <- unlist(eg)
    
    baseFiles = list()
    
    baseFiles[[1]] = cbind(probe, gb)
    baseFiles[[2]] = cbind(eg_probe, eg)
    baseFiles
}


probe2gene <- function(baseMap, otherSrc,
			baseMapType=c("gb", "ug", "eg", "refseq", "gbNRef", "image"), 
			chipMapSrc, pkgName, outputDir=".", allowHomologyGene=FALSE) {
	baseMapType <- match.arg(baseMapType)
        require("RSQLite")
	drv <- dbDriver("SQLite")
	outputFile <- file.path(outputDir, paste(pkgName, "sqlite", sep="."))	
	db <- dbConnect(drv, outputFile)
	sqliteQuickSQL(db, 
		"CREATE TABLE metadata (name VARCHAR(80) PRIMARY KEY, value VARCHAR(255) );")
	metadata_sql <- paste("INSERT INTO metadata VALUES ('PKGNAME', '",
				pkgName, "');", sep="", collapse="")
	sqliteQuickSQL(db, metadata_sql)
	sqliteQuickSQL(db, "CREATE TABLE curr_map (probe_id TEXT, gene_id TEXT);")  #curr_map is the baseMap file
	sqliteQuickSQL(db, "CREATE TABLE probes_ori (probe_id TEXT);")             #all the probes from curr_map
	sqliteQuickSQL(db, "CREATE TABLE other (probe_id TEXT, gene_id TEXT);")     #This holds the otherSrc data
	sqliteQuickSQL(db, "CREATE TABLE other_rank (probe_id TEXT, gene_id TEXT, vote INTEGER,  row_id INTEGER);") 
	sqliteQuickSQL(db, "CREATE TABLE probe2gene (probe_id TEXT, gene_id TEXT);") 
	sqliteQuickSQL(db, "CREATE TABLE probe2acc (probe_id TEXT, gene_id TEXT);") 
	sqliteQuickSQL(db, "CREATE TABLE probe_map (probe_id TEXT, gene_id TEXT, accession TEXT);")

        #If there might be refseq IDs, then they might have unwanted .<digit> extensions which must be removed.
        #This (unfortunately cannot be done for the otherSrc files since I won't know what I am getting in that case).
        if(baseMapType=='refseq' || baseMapType=='gbNRef'){  #need to verify that this function is not destructive to genbank IDs
        #if(baseMapType=='refseq'){
            baseMap=cleanRefSeqs(baseMap)
        }
        
        #populate the contents of baseMap int curr_map
        clnVals <-baseMap
        sqlIns <- "INSERT INTO curr_map (probe_id,gene_id) VALUES (?,?);"
        dbBeginTransaction(db)
        rset <- dbSendPreparedQuery(db, sqlIns, clnVals)
        dbClearResult(rset)
        dbCommit(db)

        sqliteQuickSQL(db, "INSERT INTO probes_ori SELECT DISTINCT probe_id FROM curr_map;")
	sqliteQuickSQL(db,
		"DELETE FROM curr_map WHERE gene_id='NA' OR gene_id='';")
	attach_sql <- paste("ATTACH DATABASE '",chipMapSrc,"' AS src;", sep="", collapse="")
	sqliteQuickSQL(db, attach_sql)
	if(baseMapType=='eg') {
                message(cat("baseMapType is eg"))
		sqliteQuickSQL(db, "INSERT INTO probe2acc SELECT probe_id, NULL FROM probes_ori;")
		sql <- paste("INSERT INTO probe2gene",
			   "SELECT DISTINCT probe_id, gene_id",
			    "FROM curr_map GROUP BY probe_id;", sep=" ", collapse="")
		sqliteQuickSQL(db, sql)
	} else if (baseMapType=='image') {
                message(cat("baseMapType is image"))
		sqliteQuickSQL(db, "INSERT INTO probe2acc SELECT probe_id, NULL FROM probes_ori;")
		sql <- paste("INSERT INTO probe2gene",
			    "SELECT DISTINCT c.probe_id, i.gene_id",
			    "FROM curr_map as c, src.image_acc_from_uni as i",
			    "WHERE c.gene_id=i.accession GROUP BY c.probe_id;", sep=" ", collapse="")
		sqliteQuickSQL(db, sql)
	} else if (baseMapType=='ug') {
                message(cat("baseMapType is ug"))
		sqliteQuickSQL(db, "INSERT INTO probe2acc SELECT probe_id, NULL FROM probes_ori;")
		sql <- paste("INSERT INTO probe2gene",
			    "SELECT DISTINCT c.probe_id, u.gene_id",
			    "FROM curr_map as c, src.unigene as u",
			    "WHERE c.gene_id=u.unigene_id GROUP BY c.probe_id;", sep=" ", collapse="")
		sqliteQuickSQL(db, sql)
	} else if (baseMapType=='refseq') {
                message(cat("baseMapType is refseq"))
		sqliteQuickSQL(db, "CREATE INDEX cm1 ON curr_map(probe_id);")
		sqliteQuickSQL(db, "INSERT INTO probe2acc SELECT p.probe_id, c.gene_id FROM probes_ori AS p LEFT OUTER JOIN curr_map AS c ON p.probe_id=c.probe_id GROUP BY p.probe_id;")
		sql <- paste("INSERT INTO probe2gene", 
			    "SELECT DISTINCT c.probe_id, a.gene_id",
			    "FROM curr_map as c, src.refseq as a",
			    "WHERE c.gene_id=a.accession GROUP BY c.probe_id;", sep=" ", collapse="") 
		sqliteQuickSQL(db, sql)
	} else 	{ ### type='gb' or 'gbNRef'
                message(cat("baseMapType is gb or gbNRef"))
		sqliteQuickSQL(db, "CREATE INDEX cm1 ON curr_map(probe_id);")
		sqliteQuickSQL(db, "INSERT INTO probe2acc SELECT p.probe_id, c.gene_id FROM probes_ori AS p LEFT OUTER JOIN curr_map AS c ON p.probe_id=c.probe_id GROUP BY p.probe_id;")
		sql <- paste("INSERT INTO probe2gene", 
			    "SELECT DISTINCT c.probe_id, a.gene_id",
			    "FROM curr_map as c, src.accession as a",
			    "WHERE c.gene_id=a.accession GROUP BY c.probe_id;", sep=" ", collapse="") 
		sqliteQuickSQL(db, sql)
		sql <- paste("DELETE FROM curr_map WHERE probe_id IN",
			"(SELECT probe_id FROM probe2gene);", sep=" ", collapse="")
 		sqliteQuickSQL(db, sql)
		sql <- paste("INSERT INTO probe2gene ", 
			    "SELECT DISTINCT c.probe_id, a.gene_id ",
			    "FROM curr_map as c, src.accession_unigene as a",  #The HORRIBLY MISNAMED accession_unigene table contains refseq IDs as well as GenBank IDs but no unigene IDs!
                            "WHERE c.gene_id=a.accession GROUP BY c.probe_id;", sep=" ", collapse="") #This name is not my fault!  NOT MY FAULT! - Marc
		sqliteQuickSQL(db, sql)
	}
	#sqliteQuickSQL(db, "DETACH DATABASE src;")
        #At the present time allowHomologyGene is deactivated and has been for
        #as long as I have been using this code. (-Marc) The use of this seems
        #a little risky (since the src tables are not filtered by organism
        #first) and we would also be increasing the size of the intermediate
        #DBs by about 50% since we would be adding two ENORMOUS tables to all
        #of these packages (that contain all the refseqs and genbanks ever
        #matched to an entrez ID)
	if (allowHomologyGene && baseMapType %in% c('refseq', 'gb', 'gbNRef')) {
                message(cat("Searching for other useful ID's from the \"all\" indices \n"))
		#sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipmapsrc,"' AS src;",sep=""))
		sqliteQuickSQL(db,
			"DELETE FROM curr_map WHERE probe_id IN (SELECT probe_id FROM probe2gene);")
		if (baseMapType=='refseq') { 
			sql <- paste("INSERT INTO probe2gene",
                            "SELECT DISTINCT c.probe_id, a.gene_id",
                            "FROM curr_map as c, src.refseq_all as a",
                            "WHERE c.gene_id=a.accession GROUP BY c.probe_id;", 
				sep=" ", collapse="")
		} else { ## type='gb' or 'gbNRef'
			sql <- paste("INSERT INTO probe2gene",
                            "SELECT DISTINCT c.probe_id, a.gene_id",
                            "FROM curr_map as c, src.accession_all as a",
                            "WHERE c.gene_id=a.accession GROUP BY c.probe_id;", 
				sep=" ", collapse="")
		}
		sqliteQuickSQL(db, sql)
		sqliteQuickSQL(db, "DETACH DATABASE src;")
	}else{
            sqliteQuickSQL(db, "DETACH DATABASE src;")
        }
	sqliteQuickSQL(db, "DROP TABLE curr_map;")
	lapply(otherSrc, function(thisOtherName) {
            clnVals <- thisOtherName
            sqlIns <- "INSERT INTO other (probe_id,gene_id) VALUES(?,?);"
            dbBeginTransaction(db)
            rset <- dbSendPreparedQuery(db, sqlIns, clnVals)
            dbClearResult(rset)
            dbCommit(db)
	})
	sqliteQuickSQL(db, "DELETE FROM other WHERE gene_id IN ('NA', '');")
	sqliteQuickSQL(db, "DELETE FROM other WHERE probe_id IN (SELECT probe_id FROM probe2gene);")
	sqliteQuickSQL(db,
		"CREATE TEMP TABLE other_stat AS SELECT probe_id, gene_id, count(*) as vote FROM other GROUP BY probe_id, gene_id;")
	sqliteQuickSQL(db,
		"INSERT INTO other_rank (probe_id, gene_id, vote, row_id) SELECT DISTINCT o.probe_id AS Probe, o.gene_id AS Gene, s.vote AS Vote, o.rowid AS Row FROM other AS o, other_stat AS s WHERE o.probe_id=s.probe_id AND o.gene_id=s.gene_id ORDER BY Probe asc, Vote desc, Row asc;")

        ##The following worked great when it was always affy data (and the extra data in the "other_rank" was always an EG)
        ##but now we have stuff like unigene IDs that can be in these fields, so we need to search the DB, and get out any EGs that match from the chipmapsrc and
        ##put THOSE into the probe2-gene mapping.

        ##NOW I want to use the stuff inside of "other_rank" directly if it is already *IN* the src.EGList, but if it is not, then we want to do another search of the 3 fields: accession_unigene, refseq and accession

        ##We will be needing the chipMapSrc file once again...
 	attach_sql <- paste("ATTACH DATABASE '",chipMapSrc,"' AS src;", sep="", collapse="")
 	sqliteQuickSQL(db, attach_sql)

        ##The following will insert any entrez gene IDs into the database.
        sql <- "INSERT INTO probe2gene SELECT DISTINCT probe_id, gene_id FROM other_rank WHERE rowid IN (SELECT min(rowid) FROM other_rank GROUP BY probe_id)
                 AND gene_id IN src.EGList;"
 	sqliteQuickSQL(db, sql)

        ##TODO?  Someone might want it set up so that if their unigene etc. is not found, it is still jammed in there as a unigene.  Right now we ignore it if we don't have an EG...

        ##Now to proceed I need another tabled to hold only the minimal row number information from the other_rank table
        sqliteQuickSQL(db, "CREATE TABLE min_other_rank (probe_id TEXT, gene_id TEXT, vote INTEGER,  row_id INTEGER);")
        sqliteQuickSQL(db, "INSERT INTO min_other_rank SELECT * FROM other_rank WHERE rowid IN (SELECT min(rowid) FROM other_rank GROUP BY probe_id);")
        
        ##The following will insert any unigene IDs into the database AS entrez gene IDs, (by joining with the prefiltered min_other_rank table)
        sql <- "INSERT INTO probe2gene SELECT DISTINCT m.probe_id, u.gene_id FROM min_other_rank as m INNER JOIN src.unigene as u WHERE m.gene_id=u.unigene_id;"
 	sqliteQuickSQL(db, sql)

        ##The following will insert any missing refseq IDs into the database AS entrez gene IDs, (by joining with the prefiltered min_other_rank table)
        sql <- "INSERT INTO probe2gene SELECT DISTINCT m.probe_id, r.gene_id FROM min_other_rank as m INNER JOIN src.refseq as r WHERE m.gene_id=r.accession;"
 	sqliteQuickSQL(db, sql)
        
        ##The following will insert any missing GenBank IDs into the database AS entrez gene IDs, (by joining with the prefiltered min_other_rank table)
        sql <- "INSERT INTO probe2gene SELECT DISTINCT m.probe_id, gb.gene_id FROM min_other_rank as m INNER JOIN src.accession as gb WHERE m.gene_id=gb.accession;"
 	sqliteQuickSQL(db, sql)
        
	sqliteQuickSQL(db, "INSERT INTO probe2gene SELECT probe_id, NULL FROM probes_ori WHERE probe_id NOT IN (SELECT probe_id FROM probe2gene);")
	sqliteQuickSQL(db, "CREATE INDEX p1 ON probe2gene(probe_id);")
	sqliteQuickSQL(db, "INSERT INTO probe_map SELECT DISTINCT p.probe_id, p.gene_id, a.gene_id FROM probe2acc as a LEFT OUTER JOIN probe2gene as p ON a.probe_id=p.probe_id;")
	sqliteQuickSQL(db, "DROP TABLE other_rank;")
        sqliteQuickSQL(db, "DROP TABLE other;")
        sqliteQuickSQL(db, "DROP TABLE probe2gene;")
        sqliteQuickSQL(db, "DROP TABLE probe2acc;")
        sqliteQuickSQL(db, "DROP TABLE probes_ori;")
        sqliteQuickSQL(db, "DROP TABLE min_other_rank;")
        dbDisconnect(db)
	pkgName
}

getMapForBiocChipPkg <- function(csvFileName, pkgName, chipMapSrc, 
				otherSrc=character(0),
				baseMapType="gbNRef", 
				outputDir=".",
				allowHomologyGene=FALSE) {
    baseMaps = makeBaseMaps(csvFileName=csvFileName, targetDir=outputDir)
    
    #pre-clean the otherSrc's and pass them along as lists of lists:  #TODO: this fails if there are not any otherSrc's
    otherSrcObjs = list()
    if(length(otherSrc)==0){
        otherSrc = otherSrcObjs
    }else{
        for(i in 1:length(otherSrc)){
            otherSrcObjs[[i]] = cleanSrcMap(otherSrc[[i]])
        }
    }
    #The 1st item in baseMaps is always the genbank ID
    if (baseMapType == "eg"){
        ##baseMap <- cleanSrcMap(egMapFile)
        baseMap <- as.data.frame(baseMaps[[2]])
    } else {
        ##baseMap <- cleanSrcMap(gbMapFile)
        baseMap <- as.data.frame(baseMaps[[1]])
        ##otherSrc <- c(egMapFile, otherSrc)
        ##this part should be ok, but an issue is creeping in from above...
        otherSrcObjs[[length(otherSrcObjs) + 1]] <- as.data.frame(baseMaps[[2]])
    }
    
    ##just for debugging (disable the rest of the time)
    if(FALSE){
        printOutBaseMaps(baseMap, pkgName, otherSrcObjs)
    }
    
    probe2gene(baseMap=baseMap, 
               baseMapType=baseMapType, 
               otherSrc=otherSrcObjs,
               chipMapSrc=chipMapSrc,
               pkgName=pkgName,
               outputDir=outputDir,
               allowHomologyGene=allowHomologyGene)
}

getMapForOtherChipPkg <- function(filePath,
                                  pkgName,
                                  chipMapSrc,
                                  otherSrc=character(0),
                                  baseMapType="gbNRef",
                                  outputDir=".",
                                  allowHomologyGene=FALSE) {
    #pre-clean the otherSrc's and pass them along as lists of lists:  TODO: fix no otherSrc bug here too
    otherSrcObjs = list()
    if(length(otherSrc)==0){
        otherSrc = otherSrcObjs
    }else{
        for(i in 1:length(otherSrc)){
            otherSrcObjs[[i]] = cleanSrcMap(otherSrc[[i]])
        }
    }
    baseMap = cleanSrcMap(filePath)

    ##just for debugging (disable the rest of the time)
    if(FALSE){
        printOutBaseMaps(baseMap, pkgName, otherSrcObjs)
    }

    probe2gene(baseMap=baseMap,
               baseMapType=baseMapType,
               otherSrc=otherSrcObjs,
               chipMapSrc=chipMapSrc,
               pkgName=pkgName,
               outputDir=outputDir,
               allowHomologyGene=allowHomologyGene)
}


getMapForYeastChipPkg <- function(affy, fileName, pkgName, outputDir=".") {

    baseMaps = data.frame()
    if(affy==TRUE){
        baseMaps = makeBaseMaps (csvFileName=fileName,
                        targetDir=outputDir) ##,
                        ##outputPrefix=pkgName)
    }
        require("RSQLite")
	drv <- dbDriver("SQLite")
	outputFile <- file.path(outputDir, paste(pkgName, "sqlite", sep="."))	
	db <- dbConnect(drv, outputFile)
	sqliteQuickSQL(db,
                "CREATE TABLE metadata (name VARCHAR(80) PRIMARY KEY, value VARCHAR(255) );")
        metadata_sql <- paste("INSERT INTO metadata VALUES ('PKGNAME', '",
                                pkgName, "');", sep="", collapse="")
	sqliteQuickSQL(db, metadata_sql)
	sqliteQuickSQL(db, 
		"CREATE TABLE probe_map (probe_id TEXT NOT NULL, systematic_name TEXT);");
    if(affy==TRUE){
        clnVals <- as.data.frame(baseMaps[[1]])
        sqlIns <- "INSERT INTO probe_map VALUES(?,?);"
        dbBeginTransaction(db)
        rset <- dbSendPreparedQuery(db, sqlIns, clnVals)
        dbClearResult(rset)
        dbCommit(db)        
    }else
    {
        clnVals <- cleanSrcMap(fileName)
        sqlIns <- "INSERT INTO probe_map VALUES(?,?);"
        dbBeginTransaction(db)
        rset <- dbSendPreparedQuery(db, sqlIns, clnVals)
        dbClearResult(rset)
        dbCommit(db)
    }
	sqliteQuickSQL(db,
		"UPDATE probe_map SET systematic_name=NULL WHERE systematic_name='NA';")
	dbDisconnect(db)
    
	pkgName
}


#Need to add fileName and affy param here
getMapForArabidopsisChipPkg <- function(affy, fileName, pkgName, chipMapSrc, outputDir=".") {

    #Note: this function and the associated chipmapsrc database for Arabidopsis both assume that affy will only
    #ever make two arrays for arabidposis.
    #if this changes, then the database and this function will need to be updated (this is unlikely however)
    #for non-affy functions, the map will be read in "as is" and used to make a package.
        require("RSQLite")
	drv <- dbDriver("SQLite")
	outputFile <- file.path(outputDir, paste(pkgName, "sqlite", sep="."))	
	db <- dbConnect(drv, outputFile)
	sqliteQuickSQL(db,
                "CREATE TABLE metadata (name VARCHAR(80) PRIMARY KEY, value VARCHAR(255) );")
        metadata_sql <- paste("INSERT INTO metadata VALUES ('PKGNAME', '",
                                pkgName, "');", sep="", collapse="")
	sqliteQuickSQL(db, metadata_sql)
	sqliteQuickSQL(db, 
		"CREATE TABLE probe_map (probe_id TEXT NOT NULL, gene_id TEXT);");
        sqliteQuickSQL(db, paste("ATTACH DATABASE '",chipMapSrc,"' AS src;",sep=""))

    if(affy==TRUE){#Warning: This will only work if the arabidopsis package is one of "the" TWO affy packages.

	if (pkgName == "ag"){url_name="TAIRAGURL"}
	else if (pkgName == "ath1121501"){url_name="TAIRATHURL"}

        insert_sql <- paste("INSERT INTO metadata SELECT 'TAIRCHIPMAPURL', value FROM src.metadata WHERE name='", url_name, "';", sep="")
	sqliteQuickSQL(db, insert_sql);
                
      	insert_sql <- paste("INSERT INTO probe_map SELECT * FROM src.", pkgName, ";", sep="")
       	sqliteQuickSQL(db, insert_sql);
    }else
    {        
        clnVals <- cleanSrcMap(fileName)
        sqlIns <- "INSERT INTO probe_map VALUES(?,?);"
        dbBeginTransaction(db)
        rset <- dbSendPreparedQuery(db, sqlIns, clnVals)
        dbClearResult(rset)
        dbCommit(db)
        
    }
	sqliteQuickSQL(db, "DETACH src;");			
	dbDisconnect(db)
	pkgName
}


makeUniversalMapping <- function(pkgName,
                                chipSrc,
                                baseMapType="eg",
                                outputDir=".") {

	## The rest of this will just make a map of ALL the EGs
        require("RSQLite")
        drv <- dbDriver("SQLite")
        outputFile <- file.path(outputDir, paste(pkgName, "sqlite", sep="."))
        db <- dbConnect(drv, outputFile)
        sqliteQuickSQL(db,
                "CREATE TABLE metadata (name VARCHAR(80) PRIMARY KEY, value VARCHAR(255) );")
        metadata_sql <- paste("INSERT INTO metadata VALUES ('PKGNAME', '",
                                pkgName, "');", sep="", collapse="")
        sqliteQuickSQL(db, metadata_sql)

	#no source file so we have to grab this from the chipmapsrc
        create_sql <- "CREATE TABLE probe_map (gene_id TEXT NOT NULL);"
        sqliteQuickSQL(db, create_sql)

        attach_sql <- paste("ATTACH DATABASE '",chipSrc,"' AS src;", sep="", collapse="")
        sqliteQuickSQL(db, attach_sql)

        #just grab ALL Entrez genes (without any constraint other than that they be UNIQUE) from chipsrc_XXX
        insert_sql <- "INSERT INTO probe_map SELECT DISTINCT gene_id from src.genes ;"
        sqliteQuickSQL(db, insert_sql)
        
        dbDisconnect(db)
        
        pkgName

}
