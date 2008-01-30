
#Just a utility to prevent empty IDs from ever causing any more mayhem
cleanSrcMap <- function(file) {
    insVals <- read.delim(file=file, header=FALSE, sep="\t", quote="")
    blnkLines <- insVals[insVals[,1]=='',]
    if(dim(as.matrix(blnkLines))[1] > 0){
        print(paste("ID Source TABLE FLAW.  There were ",dim(as.matrix(blnkLines))[1],
        " blank values inside the file named ", file,'\n',sep="") )
    }
    insVals <- insVals[insVals[,1]!='',]
}


makeBaseFiles <- function(csvFileName, outputPrefix,
                          GenBankIDName="Representative.Public.ID",
                          EntrezGeneIDName="Entrez.Gene",
                          targetDir="."
                          ) {
    csvFile <- read.csv(csvFileName, as.is=TRUE, na.strings="---", 
		colClasses="character")
    probe <- csvFile[,1]
    gb <- csvFile[, GenBankIDName]
    eg <- csvFile[, EntrezGeneIDName]
    rm(csvFile)
    gb <- lapply(unlist(gb), function(x) toupper(strsplit(x,"\\.")[[1]][1]))
    gb_file <- paste(probe, gb, sep="\t", collapse="\n")
    gb_file <- paste(gb_file, "\n", sep="")

    eg <- strsplit(eg, split=" /// ", fix=T)
    eg_count <- sapply(eg, length)
    eg_probe <- rep(probe, eg_count)
    eg <- unlist(eg)
    eg_file <-  paste(eg_probe, eg, sep="\t", collapse="\n")
    eg_file <- paste(eg_file, "\n", sep="")
    
    gb_out <- file.path(targetDir, paste(outputPrefix,".GenBankID"  ,sep=""))
    eg_out <- file.path(targetDir,paste(outputPrefix,".EntrezGeneID",sep=""))
   
    cat(gb_file, file=gb_out)
    cat(eg_file, file=eg_out) 
}


probe2gene <- function(baseName, otherSrc,
			baseMapType=c("gb", "ug", "ll", "refseq", "gbNRef"), 
			chipMapSrc, pkgName, outputDir=".", allowHomologyGene=F) {
	baseMapType <- match.arg(baseMapType)
	drv <- dbDriver("SQLite")
	outputFile <- file.path(outputDir, paste(pkgName, "sqlite", sep="."))	
	db <- dbConnect(drv, outputFile)
	sqliteQuickSQL(db, 
		"CREATE TABLE metadata (name VARCHAR(80) PRIMARY KEY, value VARCHAR(255) );")
	metadata_sql <- paste("INSERT INTO metadata VALUES ('PKGNAME', '",
				pkgName, "');", sep="", collapse="")
	sqliteQuickSQL(db, metadata_sql)
	sqliteQuickSQL(db, "CREATE TABLE curr_map (probe_id TEXT, gene_id TEXT);")  #curr_map is the baseName file
	sqliteQuickSQL(db, "CREATE TEMP TABLE probes (probe_id TEXT);")             #all the probes from curr_map
	sqliteQuickSQL(db, "CREATE TABLE other (probe_id TEXT, gene_id TEXT);")     #This holds the otherSrc data
	sqliteQuickSQL(db, "CREATE TABLE other_rank (probe_id TEXT, gene_id TEXT, vote INTEGER,  row_id INTEGER);") 
	sqliteQuickSQL(db, "CREATE TABLE probe2gene (probe_id TEXT, gene_id TEXT);") 
	sqliteQuickSQL(db, "CREATE TABLE probe2acc (probe_id TEXT, gene_id TEXT);") 
	sqliteQuickSQL(db, "CREATE TABLE probe_map (probe_id TEXT, gene_id TEXT, accession TEXT);")

	RSQLite:::sqliteImportFile(db, "curr_map", baseName, header=F, append=T, 
			row.names=F, sep="\t") 
	sqliteQuickSQL(db, "INSERT INTO probes SELECT DISTINCT probe_id FROM curr_map;")
	sqliteQuickSQL(db,
		"DELETE FROM curr_map WHERE gene_id='NA' OR gene_id='';")
	attach_sql <- paste("ATTACH DATABASE '",chipMapSrc,"' AS src;", sep="", collapse="")
	sqliteQuickSQL(db, attach_sql)
	if(baseMapType=='ll') {
		sqliteQuickSQL(db, "INSERT INTO probe2acc SELECT probe_id, NULL FROM probes;")
		sql <- paste("INSERT INTO probe2gene",
			   "SELECT DISTINCT probe_id, gene_id",
			    "FROM curr_map GROUP BY probe_id;", sep=" ", collapse="")
		sqliteQuickSQL(db, sql)
	} else if (baseMapType=='ug') {
		sqliteQuickSQL(db, "INSERT INTO probe2acc SELECT probe_id, NULL FROM probes;")
		sql <- paste("INSERT INTO probe2gene",
			    "SELECT DISTINCT c.probe_id, u.gene_id",
			    "FROM curr_map as c, src.unigene as u",
			    "WHERE c.gene_id=u.unigene_id GROUP BY c.probe_id;", sep=" ", collapse="")
		sqliteQuickSQL(db, sql)
	} else if (baseMapType=='refseq') {
		sqliteQuickSQL(db, "CREATE INDEX cm1 ON curr_map(probe_id);")
		sqliteQuickSQL(db, "INSERT INTO probe2acc SELECT p.probe_id, c.gene_id FROM probes AS p LEFT OUTER JOIN curr_map AS c ON p.probe_id=c.pselecrobe_id GROUP BY p.probe_id;")
		sql <- paste("INSERT INTO probe2gene", 
			    "SELECT DISTINCT c.probe_id, a.gene_id",
			    "FROM curr_map as c, src.refseq as a",
			    "WHERE c.gene_id=a.accession GROUP BY c.probe_id;", sep=" ", collapse="") 
		sqliteQuickSQL(db, sql)
	} else 	{ ### type='gb' or 'gbNRef'
		sqliteQuickSQL(db, "CREATE INDEX cm1 ON curr_map(probe_id);")
		sqliteQuickSQL(db, "INSERT INTO probe2acc SELECT p.probe_id, c.gene_id FROM probes AS p LEFT OUTER JOIN curr_map AS c ON p.probe_id=c.probe_id GROUP BY p.probe_id;")
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
			    "FROM curr_map as c, src.accession_unigene as a",
			    "WHERE c.gene_id=a.accession GROUP BY c.probe_id;", sep=" ", collapse="") 
		sqliteQuickSQL(db, sql)
	}
	sqliteQuickSQL(db, "DETACH DATABASE src;")
	if (allowHomologyGene && baseMapType %in% c('refseq', 'gb', 'gbNRef')) {
		sqliteQuickSQL(db, "ATTACH DATABASE 'chipmapsrc_all.sqlite' AS src;")
		sqliteQuickSQL(db,
			"DELETE FROM curr_map WHERE probe_id IN (SELECT probe_id FROM probe2gene);")
		if (baseMapType=='refseq') { 
			sql <- paste("INSERT INTO probe2gene",
                            "SELECT DISTINCT c.probe_id, a.gene_id",
                            "FROM curr_map as c, src.refseq as a",
                            "WHERE c.gene_id=a.accession GROUP BY c.probe_id;", 
				sep=" ", collapse="")
		} else { ## type='gb' or 'gbNRef'
			sql <- paste("INSERT INTO probe2gene",
                            "SELECT DISTINCT c.probe_id, a.gene_id",
                            "FROM curr_map as c, src.accession as a",
                            "WHERE c.gene_id=a.accession GROUP BY c.probe_id;", 
				sep=" ", collapse="")
		}
		sqliteQuickSQL(db, sql)
		sqliteQuickSQL(db, "DETACH DATABASE src;")
	}
	sqliteQuickSQL(db, "DROP TABLE curr_map;")
	lapply(otherSrc, function(thisOtherName) {
		RSQLite:::sqliteImportFile(db, "other", thisOtherName, header=F, 
			append=T, row.names=F, sep="\t") 
	})
	sqliteQuickSQL(db, "DELETE FROM other WHERE gene_id IN ('NA', '');")
	sqliteQuickSQL(db, "DELETE FROM other WHERE probe_id IN (SELECT probe_id FROM probe2gene);")
	sqliteQuickSQL(db,
		"CREATE TEMP TABLE other_stat AS SELECT probe_id, gene_id, count(*) as vote FROM other GROUP BY probe_id, gene_id;")
	sqliteQuickSQL(db,
		"INSERT INTO other_rank (probe_id, gene_id, vote, row_id) SELECT DISTINCT o.probe_id AS Probe, o.gene_id AS Gene, s.vote AS Vote, o.rowid AS Row FROM other AS o, other_stat AS s WHERE o.probe_id=s.probe_id AND o.gene_id=s.gene_id ORDER BY Probe asc, Vote desc, Row asc;")
	sqliteQuickSQL(db,
		"INSERT INTO probe2gene SELECT DISTINCT probe_id, gene_id FROM other_rank WHERE rowid IN (SELECT min(rowid) FROM other_rank GROUP BY probe_id);")
	sqliteQuickSQL(db, "INSERT INTO probe2gene SELECT probe_id, NULL FROM probes WHERE probe_id NOT IN (SELECT probe_id FROM probe2gene);")
	sqliteQuickSQL(db, "CREATE INDEX p1 ON probe2gene(probe_id);")
	sqliteQuickSQL(db, "INSERT INTO probe_map SELECT DISTINCT p.probe_id, p.gene_id, a.gene_id FROM probe2acc as a LEFT OUTER JOIN probe2gene as p ON a.probe_id=p.probe_id;")
	sqliteQuickSQL(db, "DROP TABLE other_rank;")
        sqliteQuickSQL(db, "DROP TABLE other;")
        sqliteQuickSQL(db, "DROP TABLE probe2gene;")
        sqliteQuickSQL(db, "DROP TABLE probe2acc;")
        dbDisconnect(db)
	pkgName
}

getMapForBiocChipPkg <- function(csvFileName, pkgName, chipMapSrc, 
				otherSrc=character(0),
				baseMapType="gbNRef", 
				outputDir=".",
				allowHomologyGene=T) {
	makeBaseFiles (csvFileName=csvFileName,
			targetDir=outputDir,
			outputPrefix=pkgName)
	egMapFile <- file.path(outputDir, paste(pkgName, ".EntrezGeneID", sep=""))
	gbMapFile <- file.path(outputDir, paste(pkgName, ".GenBankID", sep=""))
	if (baseMapType == "ll"){
		baseName <- egMapFile
	} else {
		baseName <- gbMapFile
		otherSrc <- c(egMapFile, otherSrc)
	}
	probe2gene(baseName=baseName, 
			baseMapType=baseMapType, 
			otherSrc=otherSrc,
			chipMapSrc=chipMapSrc,
			pkgName=pkgName,
			outputDir=outputDir)
}

getMapForOtherChipPkg <- function(filePath,
                                  pkgName,
                                  chipMapSrc,
                                  otherSrc=character(0),
                                  baseMapType="gbNRef",
                                  outputDir=".") {
        baseName <- filePath
        # FIXME, when you rewrite probe2gene(), change it so that it does not take a file, but instead takes a matrix of values.
        mapFile = paste(pkgName,"BaseMap.txt",sep="")
        write.table(cleanSrcMap(baseName), file=mapFile,quote=F,sep="\t",row.names=F,col.names=F)
        probe2gene(baseName=mapFile,
                   baseMapType=baseMapType,
                   otherSrc=otherSrc,
                   chipMapSrc=chipMapSrc,
                   pkgName=pkgName,
                   outputDir=outputDir)
}


getMapForYeastChipPkg <- function(affy, fileName, pkgName, outputDir=".") {

    if(affy==TRUE){
        makeBaseFiles (csvFileName=fileName,
                        targetDir=outputDir,
                        outputPrefix=pkgName)
    }
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
	RSQLite:::sqliteImportFile(db, "probe_map", paste(pkgName, ".GenBankID",sep=""), 
			 header=F, append=T, row.names=F, sep="\t")
    }else
    {
        RSQLite:::sqliteImportFile(db, "probe_map", fileName, 
			 header=F, append=T, row.names=F, sep="\t")
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
    }else
    {        
        RSQLite:::sqliteImportFile(db, "probe_map", fileName, 
			 header=F, append=T, row.names=F, sep="\t")
    }
       	sqliteQuickSQL(db, insert_sql);
	sqliteQuickSQL(db, "DETACH src;");			
	dbDisconnect(db)
	pkgName
}


makeUniversalMapping <- function(pkgName,
                                chipSrc,
                                baseMapType="ll",
                                outputDir=".") {

	## The rest of this will just make a map of ALL the EGs

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
