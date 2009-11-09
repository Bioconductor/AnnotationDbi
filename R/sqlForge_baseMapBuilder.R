
## Just a utility to prevent empty IDs from ever causing any more mayhem
cleanSrcMap <- function(file) {
    fileVals <- read.delim(file=file, header=FALSE, sep="\t", quote="", colClasses = "character")
    fileVals <- fileVals[fileVals[,1]!='',]

    ##For the case where someone puts no value in, we need things to be an NA
    fileVals[!is.na(fileVals[,2]) & fileVals[,2]=='',2] <- NA
    
    ##Expand IDs to match all the ones available
    probe <- fileVals[,1]
    id <- fileVals[,2]
    id <- gsub(" ","", id)
    id <- strsplit(id, split=";", fix=T)
    ##Otherwise, the following may remove probes that map to nothing
    id_count <- sapply(id, length)  
    id_probe <- rep(probe, id_count)
    id <- unlist(id)

    insVals <- cbind(id_probe, id)
    insVals <- as.data.frame(insVals)
    insVals
}



cleanRefSeqs <- function(baseMap){
    baseMap = as.matrix(baseMap)
    baseMap[,2] = sub("\\.\\d+?$", "", baseMap[,2], perl=TRUE)
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
    ##in the case where we have read in an affy file, we want to 1st check to see if that file has or has not already had the 1st 12 lines removed.
    ##1st read it in.
    csvFile <- read.csv(csvFileName, as.is=TRUE, na.strings="---", colClasses="character")
    #Then look at the 1st line and if not ok, then TRY AGAIN and skip the 1st 12 lines...
    if(length(grep("Probe.+?Set.+?ID",colnames(csvFile)[1], perl=TRUE))==0){
        csvFile <- read.csv(csvFileName, as.is=TRUE, na.strings="---", 
                            colClasses="character", skip = 12)        
    }
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


##For labelDuplicatedProbes() The 1st col HAS to be probes, and the 2nd col
##MUST be the central ID for the platform!
labelDuplicatedProbes <- function(frame){
    ##I need to test 1st if the probe matches more than one thing in the 1st
    ##col (numProbes) Then I need to test if among those matches there is more
    ##than one entrez gene ID.  If more than one, then I need to put a 1
    ##There is a corner case however, if I have 1 unmapped and one
    multipleAssign = function(x){
        probeInds = grep(paste('^',x[1],'$',sep=""),frame[,1],perl=TRUE)
        numProbes = length(probeInds)
        EGs = frame[probeInds,2]
        uniqEGs = unique(EGs[!is.na(EGs)]) ##must filter out "" since it does not count as an EG
        numUniqEGs = length(uniqEGs)
        if( numProbes>1  &&  numUniqEGs>1 ){
            return(1)
        }else{
            return(0)
        }
    }
    is_multiple = unlist(apply(frame,1,multipleAssign))
    ans = cbind(frame, is_multiple)
    ans
}


probe2gene <- function(baseMap, otherSrc,
			baseMapType=c("gb", "ug", "eg", "refseq", "gbNRef", "image"), 
			chipMapSrc, chipSrc, pkgName, outputDir=".", allowHomologyGene=FALSE) {
        ## message(cat(paste("Using '",chipSrc,"' for chipSrc.", sep="")))
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
	sqliteQuickSQL(db, "CREATE TABLE probe2gene (probe_id TEXT, gene_id TEXT);") 
	sqliteQuickSQL(db, "CREATE TABLE probe2acc (probe_id TEXT, gene_id TEXT);") 
	sqliteQuickSQL(db, "CREATE TABLE temp_probe_map (probe_id TEXT, gene_id TEXT, accession TEXT);")
	sqliteQuickSQL(db, "CREATE TABLE probe_map (probe_id TEXT, gene_id TEXT, accession TEXT, is_multiple SMALLINT NOT NULL);")

        #If there might be refseq IDs, then they might have unwanted .<digit> extensions which must be removed.
        #This (unfortunately cannot be done for the otherSrc files since I won't know what I am getting in that case).
        if(baseMapType=='refseq' || baseMapType=='gbNRef'){  #need to verify that this function is not destructive to genbank IDs
            baseMap=cleanRefSeqs(baseMap)
        }
        
        #populate the contents of baseMap into curr_map
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
                             "SELECT probe_id, gene_id",  ##To make things map to multiples, I will have to remove the DISTINCT and GROUP BY clauses when I insert into probe2gene
                             "FROM curr_map;", sep=" ", collapse="")
		sqliteQuickSQL(db, sql)
	} else if (baseMapType=='image') {
                message(cat("baseMapType is image"))
		sqliteQuickSQL(db, "INSERT INTO probe2acc SELECT probe_id, NULL FROM probes_ori;")
		sql <- paste("INSERT INTO probe2gene",
			    "SELECT c.probe_id, i.gene_id",
			    "FROM curr_map as c, src.image_acc_from_uni as i",
			    "WHERE c.gene_id=i.accession;", sep=" ", collapse="")
		sqliteQuickSQL(db, sql)
	} else if (baseMapType=='ug') {
                message(cat("baseMapType is ug"))
		sqliteQuickSQL(db, "INSERT INTO probe2acc SELECT probe_id, NULL FROM probes_ori;")
		sql <- paste("INSERT INTO probe2gene",
			    "SELECT c.probe_id, u.gene_id",
			    "FROM curr_map as c, src.unigene as u",
			    "WHERE c.gene_id=u.unigene_id;", sep=" ", collapse="")
		sqliteQuickSQL(db, sql)
	} else if (baseMapType=='refseq') {
                message(cat("baseMapType is refseq"))
		sqliteQuickSQL(db, "CREATE INDEX cm1 ON curr_map(probe_id);")
                sqliteQuickSQL(db, "CREATE INDEX cm2 ON curr_map(gene_id);")
                ##just keep ALL of the accessions that went in from the base map (whether or NOT they successfully map over)
		sqliteQuickSQL(db, "INSERT INTO probe2acc SELECT DISTINCT probe_id, gene_id FROM curr_map;")
		sql <- paste("INSERT INTO probe2gene", 
			    "SELECT c.probe_id, a.gene_id",
			    "FROM curr_map as c, src.refseq as a",
			    "WHERE c.gene_id=a.accession;", sep=" ", collapse="") 
		sqliteQuickSQL(db, sql)
	} else 	{ ### type='gb' or 'gbNRef'
                message(cat("baseMapType is gb or gbNRef"))
		sqliteQuickSQL(db, "CREATE INDEX cm1 ON curr_map(probe_id);")
		sqliteQuickSQL(db, "CREATE INDEX cm2 ON curr_map(gene_id);")
                ##just keep ALL of the accessions that went in from the base map (whether or NOT they successfully map over)
		sqliteQuickSQL(db, "INSERT INTO probe2acc SELECT DISTINCT probe_id, gene_id FROM curr_map;")
		sql <- paste("INSERT INTO probe2gene", 
			    "SELECT c.probe_id, a.gene_id",
			    "FROM curr_map as c, src.accession as a",
			    "WHERE c.gene_id=a.accession;", sep=" ", collapse="") 
		sqliteQuickSQL(db, sql)
		sql <- paste("DELETE FROM curr_map WHERE probe_id IN",
			"(SELECT probe_id FROM probe2gene);", sep=" ", collapse="")
 		sqliteQuickSQL(db, sql)
		sql <- paste("INSERT INTO probe2gene ", 
			    "SELECT c.probe_id, a.gene_id ",
			    "FROM curr_map as c, src.accession_unigene as a",  #The HORRIBLY MISNAMED accession_unigene table contains refseq IDs as well as GenBank IDs but no unigene IDs!
                            "WHERE c.gene_id=a.accession;", sep=" ", collapse="") #This name is not my fault!  NOT MY FAULT! - Marc
		sqliteQuickSQL(db, sql)
	}
        
        ##Code for dealing with other sources of IDs (try to find other EG matches and if so, stick them in!)        
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

        ##I need to know if there is refseq, unigene or GenBank data for the organism in question.
        tables = sqliteQuickSQL(db, "SELECT name FROM src.sqlite_master;")       
        
        ##The following will insert any unigene IDs into the database AS entrez gene IDs, (by joining with the other table)
        if(length(grep("unigene",tables))>0){
            sql <- "INSERT INTO probe2gene SELECT DISTINCT m.probe_id, u.gene_id FROM other as m INNER JOIN src.unigene as u WHERE m.gene_id=u.unigene_id;"
            sqliteQuickSQL(db, sql)
        }
        ##The following will insert any missing refseq IDs into the database AS entrez gene IDs, (by joining with the other table)
        if(length(grep("refseq",tables))>0){
            sql <- "INSERT INTO probe2gene SELECT DISTINCT m.probe_id, r.gene_id FROM other as m INNER JOIN src.refseq as r WHERE m.gene_id=r.accession;"
            sqliteQuickSQL(db, sql)
        }        
        ##The following will insert any missing GenBank IDs into the database AS entrez gene IDs, (by joining with the other table)
        if(length(grep("accession",tables))>0){
            sql <- "INSERT INTO probe2gene SELECT DISTINCT m.probe_id, gb.gene_id FROM other as m INNER JOIN src.accession as gb WHERE m.gene_id=gb.accession;"
            sqliteQuickSQL(db, sql)
        }
        
        ##The following will insert any missing Entrez Gene IDs into the database AS themselves, 
        if(length(grep("EGList",tables))>0){
            sql <- "INSERT INTO probe2gene SELECT DISTINCT probe_id, gene_id FROM other WHERE gene_id IN (SELECT gene_id FROM src.EGList);"
            sqliteQuickSQL(db, sql)
        }

        sqliteQuickSQL(db, "INSERT INTO probe2gene SELECT probe_id, NULL FROM probes_ori WHERE probe_id NOT IN (SELECT probe_id FROM probe2gene);")
	sqliteQuickSQL(db, "CREATE INDEX p1 ON probe2gene(probe_id);")
	sqliteQuickSQL(db, "INSERT INTO temp_probe_map SELECT DISTINCT p.probe_id, p.gene_id, a.gene_id FROM probe2acc as a LEFT OUTER JOIN probe2gene as p ON a.probe_id=p.probe_id;")
        sqliteQuickSQL(db, "DROP TABLE other;")
        sqliteQuickSQL(db, "DROP TABLE probe2gene;")
        sqliteQuickSQL(db, "DROP TABLE probe2acc;")
        sqliteQuickSQL(db, "DROP TABLE probes_ori;")
        
        ##Now I need to attach the chipsrc and use verify that all the gene_IDs are in fact contained in the genes table of the corresponding chipsrc DB...
        sqliteQuickSQL(db, paste("ATTACH '", chipSrc,"' AS chipSrc;", sep="", collapse=""))
        sqliteQuickSQL(db, paste("UPDATE temp_probe_map SET gene_id = NULL WHERE gene_id NOT IN (SELECT gene_id FROM chipSrc.genes);",sep=""))

        
        ##Now I need to decide which probes have multiple mappings and flag those.
        probeData <- sqliteQuickSQL(db, "SELECT * FROM temp_probe_map;")
        modifiedData <- labelDuplicatedProbes(probeData)
        sqlIns <- "INSERT INTO probe_map (probe_id,gene_id,accession,is_multiple) VALUES (?,?,?,?);"
        dbBeginTransaction(db)
        rset <- dbSendPreparedQuery(db, sqlIns, modifiedData)
        dbClearResult(rset)
        dbCommit(db)

##         ##Drop that temp table
        sqliteQuickSQL(db, "DROP TABLE temp_probe_map;")

        dbDisconnect(db)
	pkgName
}


getMapForBiocChipPkg <- function(csvFileName, pkgName, chipMapSrc, chipSrc, 
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
               chipSrc=chipSrc,
               pkgName=pkgName,
               outputDir=outputDir,
               allowHomologyGene=allowHomologyGene)
}

getMapForOtherChipPkg <- function(filePath,
                                  pkgName,
                                  chipMapSrc,
                                  chipSrc,
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
               chipSrc=chipSrc,
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
		"CREATE TABLE probe_map (probe_id TEXT NOT NULL, systematic_name TEXT, is_multiple SMALLINT NOT NULL);");
    if(affy==TRUE){
        probeData <- as.data.frame(baseMaps[[1]])
        clnVals <- labelDuplicatedProbes(probeData)
        sqlIns <- "INSERT INTO probe_map VALUES(?,?,?);"
        dbBeginTransaction(db)
        rset <- dbSendPreparedQuery(db, sqlIns, clnVals)
        dbClearResult(rset)
        dbCommit(db)        
    }else
    {
        probeData <- cleanSrcMap(fileName)
        clnVals <- labelDuplicatedProbes(probeData)
        sqlIns <- "INSERT INTO probe_map VALUES(?,?,?);"
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
