
appendPreMeta <- function(db, subStrs, printSchema, metaDataSrc){

message(cat("Prepending Metadata"))

  sql<- paste("    CREATE TABLE IF NOT EXISTS metadata (
      name VARCHAR(80) PRIMARY KEY,
      value VARCHAR(255))
    ;")
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""))}
  sqliteQuickSQL(db, sql) ##This table was set up in the previous step (was always set up before this) ==> NO apparently it was NOT!
 # TODO: move this printschema statement to the makeBaseMaps section.

  ##This is where the version number for the schema is inserted.
  sql<- paste("
    INSERT INTO metadata VALUES('DBSCHEMAVERSION', '2.1');
     ") 
  sqliteQuickSQL(db, sql)

  ##This is where the Db type and package are set up
  sql<- paste("
    INSERT INTO metadata VALUES('Db type', '",subStrs[["Db_type"]],"');
     ", sep="") 
  sqliteQuickSQL(db, sql)
  ##This is where the version number for the schema is inserted.
  sql<- paste("
    INSERT INTO metadata VALUES('Supporting package', 'AnnotationDbi');
     ") 
  sqliteQuickSQL(db, sql)



  ## This handles the metadata for all the local packages
  if(length(names(metaDataSrc)) == 0){

    #message(cat("Using metaDataSrc.sqlite for Metadata \n"))
      
    sql <- paste("ATTACH DATABASE '",metaDataSrc,"' AS meta;",sep="")
    sqliteQuickSQL(db, sql)
      
    sql<- paste("
      INSERT INTO metadata
       SELECT 'DBSCHEMA', db_schema
       FROM meta.metadata
       WHERE package_name IN
          (SELECT value FROM metadata WHERE name='PKGNAME');
       ") 
    sqliteQuickSQL(db, sql)

    sql<- paste("
      INSERT INTO metadata
       SELECT 'ORGANISM', organism
       FROM meta.metadata
       WHERE package_name IN
          (SELECT value FROM metadata WHERE name='PKGNAME');
       ") 
    sqliteQuickSQL(db, sql)
  
    sql<- paste("
      INSERT INTO metadata
       SELECT 'SPECIES', species
       FROM meta.metadata
       WHERE package_name IN
          (SELECT value FROM metadata WHERE name='PKGNAME');
       ") 
    sqliteQuickSQL(db, sql)

    #these entries are only relevant for chip based packages
    if(subStrs[["coreTab"]]=="probes"){
      sql<- paste("
        INSERT INTO metadata
         SELECT 'MANUFACTURER', manufacturer
         FROM meta.metadata
         WHERE package_name IN
            (SELECT value FROM metadata WHERE name='PKGNAME');
         ") 
      sqliteQuickSQL(db, sql)

      sql<- paste("
        INSERT INTO metadata
         SELECT 'CHIPNAME', chip_name
         FROM meta.metadata
         WHERE package_name IN
            (SELECT value FROM metadata WHERE name='PKGNAME');
         ") 
      sqliteQuickSQL(db, sql)

      sql<- paste("
        INSERT INTO metadata
         SELECT 'MANUFACTURERURL', manufacture_url
         FROM meta.metadata
         WHERE package_name IN
          (SELECT value FROM metadata WHERE name='PKGNAME');
         ") 
      sqliteQuickSQL(db, sql)
    }
    
    sql<- paste("
      DETACH DATABASE meta;
       ")
      
    sqliteQuickSQL(db, sql)
  }
  else{  #user is using a named vector:
    #message(cat("Using named Vector for Metadata \n"))
    sql<- paste("
      INSERT INTO metadata VALUES('DBSCHEMA', '",metaDataSrc["DBSCHEMA"],"');
       ", sep="") 
    sqliteQuickSQL(db, sql)
    sql<- paste("
      INSERT INTO metadata VALUES('ORGANISM', '",metaDataSrc["ORGANISM"],"');
       ", sep="") 
    sqliteQuickSQL(db, sql)
    sql<- paste("
      INSERT INTO metadata VALUES('SPECIES', '",metaDataSrc["SPECIES"],"');
       ", sep="") 
    sqliteQuickSQL(db, sql)
    sql<- paste("
      INSERT INTO metadata VALUES('MANUFACTURER', '",metaDataSrc["MANUFACTURER"],"');
       ", sep="") 
    sqliteQuickSQL(db, sql)
    sql<- paste("
      INSERT INTO metadata VALUES('CHIPNAME', '",metaDataSrc["CHIPNAME"],"');
       ", sep="") 
    sqliteQuickSQL(db, sql)
    sql<- paste("
      INSERT INTO metadata VALUES('MANUFACTURERURL', '",metaDataSrc["MANUFACTURERURL"],"');
       ", sep="") 
    sqliteQuickSQL(db, sql)    
  }
    
  sql<- paste("
    DELETE FROM metadata WHERE name='PKGNAME';
     ") 
  sqliteQuickSQL(db, sql)


  sql<- paste("    CREATE TABLE IF NOT EXISTS map_metadata (
      map_name VARCHAR(80) NOT NULL,
      source_name VARCHAR(80) NOT NULL,
      source_url VARCHAR(255) NOT NULL,
      source_date VARCHAR(20) NOT NULL
    );")
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE TABLE IF NOT EXISTS map_counts (
      map_name VARCHAR(80) PRIMARY KEY,
      count INTEGER NOT NULL
    );")
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)
  
}





##  The genes table is (almost) always the central table (cntrTab)
appendGenes <- function(db, subStrs, printSchema){

  message(cat("Creating Genes table"))

  sql<- paste("    CREATE TABLE genes (
      _id INTEGER PRIMARY KEY,
      gene_id VARCHAR(10) NOT NULL UNIQUE           -- Entrez Gene ID
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO genes
     SELECT DISTINCT a._id, p.gene_id
     FROM probe_map as p CROSS JOIN anno.genes as a
     WHERE p.gene_id=a.gene_id;
     ") 
  sqliteQuickSQL(db, sql)

  
  sql<- paste("
    INSERT INTO genes(gene_id)
     SELECT DISTINCT gene_id
     FROM probe_map
     WHERE gene_id NOT IN (SELECT gene_id FROM genes);
     ") 
  sqliteQuickSQL(db, sql)


  ##sqliteQuickSQL(db, "ANALYZE;")

  #Get only map_metadata for this table at this time:
  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'ENTREZID';
     ") 
  sqliteQuickSQL(db, sql)
  
}



## Make the probes table (only used by chip packages)
appendProbes <- function(db, subStrs, printSchema){

  message(cat("Appending Probes"))
    
  sql<- paste("    CREATE TABLE probes (
      probe_id VARCHAR(80) NOT NULL,                         -- manufacturer ID
      accession VARCHAR(20),                        -- GenBank accession number
      is_multiple SMALLINT NOT NULL,
      _id INTEGER NULL,                             -- REFERENCES ", subStrs[["cntrTab"]],"
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );")
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO probes
     SELECT p.probe_id as probe_id, p.accession, p.is_multiple, g._id as _id
     FROM probe_map as p LEFT OUTER JOIN ", subStrs[["cntrTab"]]," as g
     ON p.gene_id=g.gene_id
     ORDER BY probe_id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fprobes ON probes (_id);") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)


  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'ACCNUM';
     ") 
  sqliteQuickSQL(db, sql)

  sqlCount<- paste("
    SELECT 'ACCNUM', count(DISTINCT probe_id)
     FROM probes
     WHERE accession NOT NULL;
    ")   
  
  sql<- paste("INSERT INTO map_counts",sqlCount)
  sqliteQuickSQL(db, sql)
  
##   count = makeMapCounts(db, "ACCNUM","probe_id","probes","","WHERE accession NOT NULL")

  count = as.integer(sqliteQuickSQL(db,sqlCount)[2])
  message(cat("Found",count,"Probe Accessions"))
}



## Make the accessions table (only used by the EG packages)
appendAccessions <- function(db, subStrs, printSchema){

  message(cat("Appending Accessions"))
    
  sql<- paste("    CREATE TABLE accessions (
      _id INTEGER NOT NULL,                         -- REFERENCES ", subStrs[["cntrTab"]],"
      accession VARCHAR(20) NOT NULL,               -- GenBank accession number
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO accessions
     SELECT DISTINCT g._id, a.accession
     FROM ", subStrs[["cntrTab"]]," as g CROSS JOIN anno.accessions as a
     WHERE g._id=a._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Faccessions ON accessions (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  #map_metadata
  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'ACCNUM';
    ") 
  sqliteQuickSQL(db, sql)

  #map_counts


  sql<- paste("
    INSERT INTO map_counts
     SELECT 'ACCNUM2EG', COUNT(DISTINCT accession)
     FROM accessions AS a INNER JOIN ", subStrs[["cntrTab"]]," AS g
     WHERE a._id=g._id;
    ") 
  sqliteQuickSQL(db, sql)


  sqlCount<- paste("
     SELECT 'ACCNUM', COUNT(DISTINCT gene_id)
     FROM ", subStrs[["cntrTab"]]," AS g INNER JOIN accessions AS a
     WHERE g._id=a._id;
    ")
  
  sql<- paste("INSERT INTO map_counts",sqlCount)
  sqliteQuickSQL(db, sql)

  count = as.integer(sqliteQuickSQL(db,sqlCount)[2])
  
##   count = makeMapCounts(db, "ACCNUM","accession","accessions AS a",paste(subStrs[["cntrTab"]],"AS g"),"WHERE g._id=a._id")
##   makeMapCounts(db, "ACCNUM2EG","gene_id",paste(subStrs[["cntrTab"]],"AS g"),"accessions AS a","WHERE g._id=a._id")
  message(cat("Found",count,"Entrez Gene Accessions"))  
}



## Make the gene info table
appendGeneInfo <- function(db, subStrs, printSchema){

  message(cat("Appending Gene Info"))
            
  sql<- paste("    CREATE TABLE gene_info (
      _id INTEGER NOT NULL UNIQUE,                  -- REFERENCES ", subStrs[["cntrTab"]],"
      gene_name VARCHAR(255) NOT NULL,              -- gene name
      symbol VARCHAR(80) NOT NULL,                  -- gene symbol
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO gene_info
     SELECT g._id as _id, i.gene_name, i.symbol
     FROM ", subStrs[["cntrTab"]]," as g, anno.gene_info as i
     WHERE g._id=i._id
     ORDER BY g._id;
     ") 
  sqliteQuickSQL(db, sql)


  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'GENENAME';
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'SYMBOL';
     ") 
  sqliteQuickSQL(db, sql)
  

  if(subStrs[["coreTab"]]=="genes"){
      makeMapCounts(db, paste("SYMBOL2",subStrs[["suffix"]],sep=""),"symbol","gene_info AS gi", paste(subStrs[["coreTab"]],"AS g"), "WHERE gi._id =g._id AND gi.symbol NOT NULL")
  }
  count = makeMapCounts(db, "GENENAME",subStrs[["coreID"]],subStrs[["coreTab"]],"gene_info",paste("WHERE ", subStrs[["coreTab"]],"._id=gene_info._id AND gene_info.gene_name NOT NULL",sep=""))
  message(cat("Found",count,"Gene Names"))

  count = makeMapCounts(db, "SYMBOL",subStrs[["coreID"]],subStrs[["coreTab"]],"gene_info",paste("WHERE ", subStrs[["coreTab"]],"._id=gene_info._id AND gene_info.symbol NOT NULL",sep=""))
  message(cat("Found",count,"Gene Symbols"))
}



## Make the chromosomes table
appendChromosomes <- function(db, subStrs, printSchema){
    
  message(cat("Appending Chromosomes"))

    sql<- paste("    CREATE TABLE chromosomes (
      _id INTEGER NOT NULL,                         -- REFERENCES ", subStrs[["cntrTab"]],"
      chromosome VARCHAR(2) NOT NULL,               -- chromosome name
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO chromosomes
     SELECT g._id as _id, i.chromosome
     FROM ", subStrs[["cntrTab"]]," as g, anno.chromosomes as i
     WHERE g._id=i._id
     ORDER BY g._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fchromosomes ON chromosomes (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'CHR';
     ") 
  sqliteQuickSQL(db, sql)
  
  makeMapCounts(db, "CHR",subStrs[["coreID"]],subStrs[["coreTab"]],"chromosomes",paste("WHERE ", subStrs[["coreTab"]],"._id=chromosomes._id",sep=""))
}



## Make the cytogenic locations table
appendCytogenicLocs <- function(db, subStrs, printSchema){
    
  message(cat("Appending Cytogenetic Locations"))
    
  sql<- paste("    CREATE TABLE cytogenetic_locations (
      _id INTEGER NOT NULL,                         -- REFERENCES ", subStrs[["cntrTab"]],"
      cytogenetic_location VARCHAR(20) NOT NULL,    -- cytoband location
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO cytogenetic_locations
     SELECT g._id as _id,  i.cytogenetic_location
     FROM ", subStrs[["cntrTab"]]," as g, anno.cytogenetic_locations as i
     WHERE g._id=i._id
     ORDER BY g._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fcytogenetic_locations ON cytogenetic_locations (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'MAP';
     ") 
  sqliteQuickSQL(db, sql)
  
  makeMapCounts(db, "MAP",subStrs[["coreID"]],subStrs[["coreTab"]],"cytogenetic_locations",paste("WHERE ",subStrs[["coreTab"]],"._id=cytogenetic_locations._id",sep=""))
  if(subStrs[["coreTab"]]=="genes"){
      makeMapCounts(db, paste("MAP2",subStrs[["suffix"]],sep=""),"cytogenetic_location","cytogenetic_locations AS cl", paste(subStrs[["coreTab"]],"AS g"),"WHERE cl._id=g._id")
  }
}



## Make the omim table
appendOmim <- function(db, subStrs, printSchema){

  message(cat("Appending Omim"))
        
  sql<- paste("    CREATE TABLE omim (
      _id INTEGER NOT NULL,                         -- REFERENCES ", subStrs[["cntrTab"]],"
      omim_id CHAR(6) NOT NULL,                     -- OMIM ID
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO omim
     SELECT g._id as _id, i.omim_id
     FROM ", subStrs[["cntrTab"]]," as g, anno.omim as i
     WHERE g._id=i._id
     ORDER BY g._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fomim ON omim (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'OMIM';
     ") 
  sqliteQuickSQL(db, sql)
  
  makeMapCounts(db, "OMIM",subStrs[["coreID"]],subStrs[["coreTab"]],"omim",paste("WHERE ",subStrs[["coreTab"]],"._id=omim._id",sep=""))
  if(subStrs[["coreTab"]]=="genes"){
      makeMapCounts(db, paste("OMIM2",subStrs[["suffix"]],sep=""),"omim_id","omim AS o", paste(subStrs[["coreTab"]],"AS g"),"WHERE o._id=g._id")
  }
}



## Make the refseq table
appendRefseq <- function(db, subStrs, printSchema){

  message(cat("Appending RefSeq"))
  
  sql<- paste("    CREATE TABLE refseq (
      _id INTEGER NOT NULL,                         -- REFERENCES ", subStrs[["cntrTab"]],"
      accession VARCHAR(20) NOT NULL,               -- RefSeq accession number
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO refseq
     SELECT g._id as _id, i.accession
     FROM ", subStrs[["cntrTab"]]," as g CROSS JOIN anno.refseq as i
     WHERE g._id=i._id
     ORDER BY g._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Frefseq ON refseq (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'REFSEQ';
     ") 
  sqliteQuickSQL(db, sql)
  
  makeMapCounts(db, "REFSEQ",subStrs[["coreID"]],subStrs[["coreTab"]],"refseq",paste("WHERE ",subStrs[["coreTab"]],"._id=refseq._id",sep=""))
  if(subStrs[["coreTab"]]=="genes"){    
      makeMapCounts(db, paste("REFSEQ2",subStrs[["suffix"]],sep=""),"r.accession","refseq AS r", paste(subStrs[["coreTab"]],"AS g"),"WHERE r._id=g._id")
  }
}


## Make the pubmed table
appendPubmed <- function(db, subStrs, printSchema){

  message(cat("Appending Pubmed"))
    
  sql<- paste("    CREATE TABLE pubmed (
      _id INTEGER NOT NULL,                         -- REFERENCES ", subStrs[["cntrTab"]],"
      pubmed_id VARCHAR(10) NOT NULL,               -- PubMed ID
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO pubmed
     SELECT g._id as _id, i.pubmed_id
     FROM ", subStrs[["cntrTab"]]," as g CROSS JOIN anno.pubmed as i
     WHERE g._id=i._id
     ORDER BY g._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fpubmed ON pubmed (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'PMID';
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT 'PMID2", subStrs[["suffix"]],"', source_name, source_url, source_date
     FROM anno.map_metadata
     WHERE map_name = 'PMID2GENE';
     ", sep="") 
  sqliteQuickSQL(db, sql)
  
  makeMapCounts(db, "PMID",subStrs[["coreID"]],subStrs[["coreTab"]],"pubmed",paste("WHERE ",subStrs[["coreTab"]],"._id=pubmed._id",sep=""))
  makeMapCounts(db, paste("PMID2",subStrs[["suffix"]],sep=""),"pubmed_id","pubmed as p", paste(subStrs[["coreTab"]],"AS g"),"WHERE p._id=g._id")

}


## Make the unigene table
appendUnigene <- function(db, subStrs, printSchema){

  message(cat("Appending Unigene"))
        
  sql<- paste("    CREATE TABLE unigene (
      _id INTEGER NOT NULL,                         -- REFERENCES ", subStrs[["cntrTab"]],"
      unigene_id VARCHAR(10) NOT NULL,              -- UniGene ID
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO unigene
     SELECT g._id as _id, i.unigene_id
     FROM ", subStrs[["cntrTab"]]," as g, anno.unigene as i
     WHERE g._id=i._id
     ORDER BY g._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Funigene ON unigene (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'UNIGENE';
     ") 
  sqliteQuickSQL(db, sql)
  
  makeMapCounts(db, "UNIGENE",subStrs[["coreID"]],subStrs[["coreTab"]],"unigene",paste("WHERE ",subStrs[["coreTab"]],"._id=unigene._id",sep=""))
  if(subStrs[["coreTab"]]=="genes"){  
      makeMapCounts(db, paste("UNIGENE2",subStrs[["suffix"]],sep=""),"unigene_id","unigene AS u", paste(subStrs[["coreTab"]],"AS g"),"WHERE u._id=g._id")
  }  

}


## Make the chrlengths table (table is tiny, so no need for an index)
appendChrlengths <- function(db, subStrs, printSchema){

message(cat("Appending ChrLengths"))
    
  sql<- paste("    CREATE TABLE chrlengths (
      chromosome VARCHAR(2) PRIMARY KEY,                   -- chromosome name
      length INTEGER NOT NULL
    );") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO chrlengths
     SELECT chromosome, length FROM anno.chrlengths;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'CHRLENGTHS';
     ") 
  sqliteQuickSQL(db, sql)
  
  makeMapCounts(db, "CHRLENGTHS","*","chrlengths")


}





## Make the go tables
appendGO <- function(db,subStrs, printSchema){

  message(cat("Appending 3 GO tables"))
            
  sql<- paste("    CREATE TABLE go_bp (
      _id INTEGER NOT NULL,                         -- REFERENCES ", subStrs[["cntrTab"]],"
      go_id CHAR(10) NOT NULL,                      -- GO ID
      evidence CHAR(3) NOT NULL,                    -- GO evidence code
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO go_bp
     SELECT g._id as _id, i.go_id, i.evidence
     FROM ", subStrs[["cntrTab"]]," as g, anno.go_bp as i
     WHERE g._id=i._id
     ORDER BY g._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fgo_bp ON go_bp (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fgo_bp_go_id ON go_bp (go_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)
  
  
  sql<- paste("    CREATE TABLE go_mf (
      _id INTEGER NOT NULL,                         -- REFERENCES ", subStrs[["cntrTab"]],"
      go_id CHAR(10) NOT NULL,                      -- GO ID
      evidence CHAR(3) NOT NULL,                    -- GO evidence code
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO go_mf
     SELECT g._id as _id, i.go_id, i.evidence
     FROM ", subStrs[["cntrTab"]]," as g, anno.go_mf as i
     WHERE g._id=i._id
     ORDER BY g._id;
    ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fgo_mf ON go_mf (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fgo_mf_go_id ON go_mf (go_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  
  sql<- paste("    CREATE TABLE go_cc (
      _id INTEGER NOT NULL,                         -- REFERENCES ", subStrs[["cntrTab"]],"
      go_id CHAR(10) NOT NULL,                      -- GO ID
      evidence CHAR(3) NOT NULL,                    -- GO evidence code
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO go_cc
     SELECT g._id as _id, i.go_id, i.evidence
     FROM ", subStrs[["cntrTab"]]," as g, anno.go_cc as i
     WHERE g._id=i._id
     ORDER BY g._id;
    ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fgo_cc ON go_cc (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fgo_cc_go_id ON go_cc (go_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)


  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'GO';
    ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'GO2GENE';
    ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
    UPDATE map_metadata
     SET map_name='GO2",subStrs[["suffix"]],"' WHERE map_name='GO2GENE';
    ", sep="") 
  sqliteQuickSQL(db, sql)


  makeMapCounts(db, "GO",subStrs[["coreID"]],subStrs[["coreTab"]],"","WHERE _id IN (SELECT _id FROM go_bp UNION SELECT _id FROM go_mf UNION SELECT _id FROM go_cc)")


  if(subStrs[["coreID"]]=="systematic_name" && subStrs[["org"]]=="yeast" ){

      subquery <- "(SELECT _id,go_id FROM sgd INNER JOIN go_bp USING (_id) WHERE systematic_name IS NOT NULL
                    UNION SELECT _id,go_id FROM sgd INNER JOIN go_mf USING (_id) WHERE systematic_name IS NOT NULL
                    UNION SELECT _id,go_id FROM sgd INNER JOIN go_cc USING (_id) WHERE systematic_name IS NOT NULL)"
  
      makeMapCounts(db, paste("GO2",subStrs[["suffix"]],sep=""),"go_id",paste(subquery,"AS s"), paste(subStrs[["coreTab"]],"AS g"),"WHERE s._id=g._id")
      
  }else{
      subquery <-"(SELECT * FROM go_bp UNION
                   SELECT * FROM go_mf UNION
                   SELECT * FROM go_cc)"
      makeMapCounts(db, paste("GO2",subStrs[["suffix"]],sep=""),"go_id",paste(subquery,"AS s"), paste(subStrs[["coreTab"]],"AS g"),"WHERE s._id=g._id")
  }   

  
}



## Make the go all tables
appendGOALL <- function(db, subStrs, printSchema){

  message(cat("Appending 3 GO ALL tables"))
        
  sql<- paste("    CREATE TABLE go_bp_all (
      _id INTEGER NOT NULL,                         -- REFERENCES ", subStrs[["cntrTab"]],"
      go_id CHAR(10) NOT NULL,                      -- GO ID
      evidence CHAR(3) NOT NULL,                    -- GO evidence code
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO go_bp_all
     SELECT g._id as _id, i.go_id, i.evidence
     FROM ", subStrs[["cntrTab"]]," as g CROSS JOIN anno.go_bp_all as i
     WHERE g._id=i._id
     ORDER BY g._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fgo_bp_all ON go_bp_all (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fgo_bp_all_go_id ON go_bp_all (go_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  
  sql<- paste("    CREATE TABLE go_mf_all (
      _id INTEGER NOT NULL,                         -- REFERENCES ", subStrs[["cntrTab"]],"
      go_id CHAR(10) NOT NULL,                      -- GO ID
      evidence CHAR(3) NOT NULL,                    -- GO evidence code
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO go_mf_all
     SELECT g._id as _id, i.go_id, i.evidence
     FROM ", subStrs[["cntrTab"]]," as g CROSS JOIN anno.go_mf_all as i
     WHERE g._id=i._id
     ORDER BY g._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fgo_mf_all ON go_mf_all (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fgo_mf_all_go_id ON go_mf_all (go_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  
  sql<- paste("    CREATE TABLE go_cc_all (
      _id INTEGER NOT NULL,                         -- REFERENCES ", subStrs[["cntrTab"]],"
      go_id CHAR(10) NOT NULL,                      -- GO ID
      evidence CHAR(3) NOT NULL,                    -- GO evidence code
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO go_cc_all
     SELECT g._id as _id, i.go_id, i.evidence
     FROM ", subStrs[["cntrTab"]]," as g CROSS JOIN anno.go_cc_all as i
     WHERE g._id=i._id
     ORDER BY g._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fgo_cc_all ON go_cc_all (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fgo_cc_all_go_id ON go_cc_all (go_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'GO2ALLGENES';
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
    UPDATE map_metadata
     SET map_name='GO2ALL",subStrs[["suffix"]],"S' WHERE map_name='GO2ALLGENES';
    ", sep="") 
  sqliteQuickSQL(db, sql)
  
  
  if(subStrs[["coreID"]]=="systematic_name" && subStrs[["org"]]=="yeast" ){
      subquery<- "(SELECT _id,go_id FROM sgd INNER JOIN go_bp_all USING (_id) WHERE systematic_name IS NOT NULL
                   UNION SELECT _id,go_id FROM sgd INNER JOIN go_mf_all USING (_id) WHERE systematic_name IS NOT NULL
                   UNION SELECT _id,go_id FROM sgd INNER JOIN go_cc_all USING (_id) WHERE systematic_name IS NOT NULL)" 
      makeMapCounts(db, paste("GO2ALL",subStrs[["suffix"]],"S",sep=""),"go_id",paste(subquery,"AS s"), paste(subStrs[["coreTab"]],"AS g"),"WHERE s._id=g._id")
  }else{
  subquery<- "(SELECT * FROM go_bp_all UNION
               SELECT * FROM go_mf_all UNION
               SELECT * FROM go_cc_all)" 
      makeMapCounts(db, paste("GO2ALL",subStrs[["suffix"]],"S",sep=""),"go_id",paste(subquery,"AS s"), paste(subStrs[["coreTab"]],"AS g"),"WHERE s._id=g._id")
  }
  
}


## Make the KEGG table
appendKEGG <- function(db, subStrs, printSchema){

  message(cat("Appending KEGG"))
        
  sql<- paste("    CREATE TABLE kegg (
      _id INTEGER NOT NULL,                         -- REFERENCES ", subStrs[["cntrTab"]],"
      path_id CHAR(5) NOT NULL,                     -- KEGG pathway short ID
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO kegg
     SELECT g._id as _id, i.path_id
     FROM ", subStrs[["cntrTab"]]," as g, anno.kegg as i
     WHERE g._id=i._id
     ORDER BY g._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fkegg ON kegg (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'PATH';
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'PATH2GENE';
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
    UPDATE map_metadata
     SET map_name='PATH2",subStrs[["suffix"]],"' WHERE map_name='PATH2GENE';
    ", sep="") 
  sqliteQuickSQL(db, sql)

  makeMapCounts(db, "PATH",subStrs[["coreID"]],subStrs[["coreTab"]],"kegg",paste("WHERE ",subStrs[["coreTab"]],"._id=kegg._id",sep=""))
  makeMapCounts(db, paste("PATH2",subStrs[["suffix"]],sep=""),"path_id","kegg as k", paste(subStrs[["coreTab"]],"AS g"),"WHERE k._id=g._id")

}


## Make the ec table
appendEC <- function(db, subStrs, printSchema){

  message(cat("Appending EC"))
        
  sql<- paste("    CREATE TABLE ec (
      _id INTEGER NOT NULL,                         -- REFERENCES ", subStrs[["cntrTab"]],"
      ec_number VARCHAR(13) NOT NULL,               -- EC number 
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO ec
     SELECT g._id as _id, i.ec_number
     FROM ", subStrs[["cntrTab"]]," as g, anno.ec as i
     WHERE g._id=i._id
     ORDER BY g._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fec ON ec (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'ENZYME';
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'ENZYME2GENE';
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
    UPDATE map_metadata
     SET map_name='ENZYME2",subStrs[["suffix"]],"' WHERE map_name='ENZYME2GENE';
    ", sep="") 
  sqliteQuickSQL(db, sql)

  makeMapCounts(db, "ENZYME",subStrs[["coreID"]],subStrs[["coreTab"]],"ec",paste("WHERE ",subStrs[["coreTab"]],"._id=ec._id",sep=""))
  makeMapCounts(db, paste("ENZYME2",subStrs[["suffix"]],sep=""),"ec_number","ec as e", paste(subStrs[["coreTab"]],"AS g"),"WHERE e._id=g._id")
}



## Make the chromosome_locations table
appendChromsomeLocs <- function(db, subStrs, printSchema){

  message(cat("Appending Chromosome Locations"))
    
  sql<- paste("    CREATE TABLE chromosome_locations (
      _id INTEGER NOT NULL,                      -- REFERENCES ", subStrs[["cntrTab"]],"
      seqname VARCHAR(20) NOT NULL,              -- sequence name
      start_location INTEGER NOT NULL,
      end_location INTEGER NOT NULL,
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO chromosome_locations
     SELECT g._id as _id, i.chromosome, i.start_location, i.end_location
     FROM ", subStrs[["cntrTab"]]," as g, anno.chromosome_locations as i
     WHERE g._id=i._id
     ORDER BY g._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fchromosome_locations ON chromosome_locations (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'CHRLOC';
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'CHRLOCEND';
     ") 
  sqliteQuickSQL(db, sql)

  makeMapCounts(db, "CHRLOC",subStrs[["coreID"]],subStrs[["coreTab"]],"chromosome_locations",paste("WHERE ",subStrs[["coreTab"]],"._id=chromosome_locations._id",sep=""))
  makeMapCounts(db, "CHRLOCEND",subStrs[["coreID"]],subStrs[["coreTab"]],"chromosome_locations",paste("WHERE ",subStrs[["coreTab"]],"._id=chromosome_locations._id",sep=""))
  
}


## Make the pfam table
appendPfam <- function(db, subStrs, printSchema){

  message(cat("Appending Pfam"))
    
  sql<- paste("    CREATE TABLE pfam (
      _id INTEGER NOT NULL,                         -- REFERENCES ", subStrs[["cntrTab"]],"
      ipi_id CHAR(11) NOT NULL,                     -- IPI accession number
      pfam_id CHAR(7) NULL,                         -- Pfam ID
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO pfam
     SELECT g._id as _id, i.ipi_id, i.pfam_id
     FROM ", subStrs[["cntrTab"]]," as g, anno.pfam as i
     WHERE g._id=i._id
     ORDER BY g._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fpfam ON pfam (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'PFAM';
     ") 
  sqliteQuickSQL(db, sql)

  makeMapCounts(db, "PFAM",subStrs[["coreID"]],subStrs[["coreTab"]],"pfam",paste("WHERE ",subStrs[["coreTab"]],"._id=pfam._id",sep=""))
  
}



## Make the prosite table
appendProsite <- function(db, subStrs, printSchema){

  message(cat("Appending Prosite"))
    
  sql<- paste("    CREATE TABLE prosite (
      _id INTEGER NOT NULL,                         -- REFERENCES ", subStrs[["cntrTab"]],"
      ipi_id CHAR(11) NOT NULL,                     -- IPI accession number
      prosite_id CHAR(7) NULL,                      -- PROSITE ID
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO prosite
     SELECT g._id as _id, i.ipi_id, i.prosite_id
     FROM ", subStrs[["cntrTab"]]," as g, anno.prosite as i
     WHERE g._id=i._id
     ORDER BY g._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fprosite ON prosite (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'PROSITE';
     ") 
  sqliteQuickSQL(db, sql)

  makeMapCounts(db, "PROSITE",subStrs[["coreID"]],subStrs[["coreTab"]],"prosite",paste("WHERE ",subStrs[["coreTab"]],"._id=prosite._id",sep=""))
  
}




## Make the alias table
appendAlias <- function(db, subStrs, printSchema){

  message(cat("Appending Alias"))
    
  sql<- paste("    CREATE TABLE alias (
      _id INTEGER NOT NULL,                         -- REFERENCES ", subStrs[["cntrTab"]],"
      alias_symbol VARCHAR(80) NOT NULL,            -- gene symbol or alias
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO alias
     SELECT DISTINCT a._id, a.symbol
     FROM ", subStrs[["cntrTab"]]," as g INNER JOIN anno.gene_synonyms as a
     WHERE g._id=a._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Falias ON alias (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'ALIAS2GENE';
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
    UPDATE map_metadata
     SET map_name='ALIAS2",subStrs[["suffix"]],"' WHERE map_name='ALIAS2GENE';
    ", sep="") 
  sqliteQuickSQL(db, sql)
  
  makeMapCounts(db, paste("ALIAS2",subStrs[["suffix"]],sep=""),"alias_symbol","alias AS a", paste(subStrs[["coreTab"]],"AS g"),"WHERE a._id=g._id")
  
}




## Make the ensembl table
appendEnsembl <- function(db, subStrs, printSchema){

  message(cat("Appending Ensembl"))
    
  sql<- paste("    CREATE TABLE ensembl (
      _id INTEGER NOT NULL,                         -- REFERENCES ", subStrs[["cntrTab"]],"
      ensembl_id VARCHAR(20) NOT NULL,              -- ensembl id
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO ensembl
     SELECT DISTINCT e._id, e.ensid
     FROM ", subStrs[["cntrTab"]]," as g INNER JOIN anno.ensembl as e
     WHERE g._id=e._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fensembl ON ensembl (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'ENSEMBL';
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'ENSEMBL2GENE';
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
    UPDATE map_metadata
     SET map_name='ENSEMBL2",subStrs[["suffix"]],"' WHERE map_name='ENSEMBL2GENE';
    ", sep="") 
  sqliteQuickSQL(db, sql)

  makeMapCounts(db, "ENSEMBL",subStrs[["coreID"]],subStrs[["coreTab"]],"ensembl",paste("WHERE ",subStrs[["coreTab"]],"._id=ensembl._id",sep=""))
  makeMapCounts(db, paste("ENSEMBL2",subStrs[["suffix"]],sep=""),"ensembl_id","ensembl AS e", paste(subStrs[["coreTab"]],"AS g"),"WHERE e._id=g._id")
  
}



## Make the ensembl2ncbi table (no metadata needed)
appendEnsembl2NCBI <- function(db, subStrs, printSchema){
    
  sql<- paste("    CREATE TABLE ensembl2ncbi (
      _id INTEGER NOT NULL,                         -- REFERENCES ", subStrs[["cntrTab"]],"
      ensembl_id VARCHAR(20) NOT NULL,              -- ensembl id
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO ensembl2ncbi
     SELECT DISTINCT e._id, e.ensid
     FROM ", subStrs[["cntrTab"]]," as g INNER JOIN anno.ensembl2ncbi as e
     WHERE g._id=e._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fensembl2ncbi ON ensembl2ncbi (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)
  
}

## Make the ncbi2ensembl table  (no metadata needed)
appendNCBI2Ensembl <- function(db, subStrs, printSchema){
    
  sql<- paste("    CREATE TABLE ncbi2ensembl (
      _id INTEGER NOT NULL,                         -- REFERENCES ", subStrs[["cntrTab"]],"
      ensembl_id VARCHAR(20) NOT NULL,              -- ensembl id
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO ncbi2ensembl
     SELECT DISTINCT e._id, e.ensid
     FROM ", subStrs[["cntrTab"]]," as g INNER JOIN anno.ncbi2ensembl as e
     WHERE g._id=e._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fncbi2ensembl ON ncbi2ensembl (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)
  
}



## Make the ensembl protein IDs table 
appendEnsemblProt <- function(db, subStrs, printSchema){

  message(cat("Appending Ensembl Protein IDs"))
    
  sql<- paste("    CREATE TABLE ensembl_prot (
      _id INTEGER NOT NULL,                          -- REFERENCES ", subStrs[["cntrTab"]],"
      prot_id VARCHAR(20) NOT NULL,                  -- Ensembl Protein ID
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO ensembl_prot
     SELECT a._id,a.prot_id
     FROM ", subStrs[["cntrTab"]]," as g INNER JOIN anno.ensembl_prot as a
     WHERE g._id=a._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fensemblp ON ensembl_prot (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  
  sql<- paste("
    INSERT INTO map_metadata
     SELECT 'ENSEMBLPROT', m1.value, m2.value, m3.value
     FROM anno.metadata AS m1, anno.metadata AS m2, anno.metadata AS m3
     WHERE m1.name='ENSOURCENAME' AND
           m2.name='ENSOURCEURL' AND
           m3.name='ENSOURCEDATE';
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT 'ENSEMBLPROT2GENE',source_name, source_url, source_date
     FROM map_metadata
     WHERE map_name='ENSEMBLPROT';
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
    UPDATE map_metadata
     SET map_name='ENSEMBLPROT2",subStrs[["suffix"]],"' WHERE map_name='ENSEMBLPROT2GENE';
    ", sep="") 
  sqliteQuickSQL(db, sql)

  makeMapCounts(db, "ENSEMBLPROT","e._id",paste(subStrs[["cntrTab"]]," AS g",sep=""),"ensembl_prot as e","WHERE g._id=e._id")
  makeMapCounts(db, paste("ENSEMBLPROT2",subStrs[["suffix"]],sep=""),"prot_id","ensembl_prot AS e", paste(subStrs[["cntrTab"]],"AS g"),"WHERE e._id=g._id")
  
  
}


## Make the ensembl transcript IDs table 
appendEnsemblTrans <- function(db, subStrs, printSchema){

  message(cat("Appending Ensembl Transcript IDs"))
    
  sql<- paste("    CREATE TABLE ensembl_trans (
      _id INTEGER NOT NULL,                          -- REFERENCES ", subStrs[["cntrTab"]],"
      trans_id VARCHAR(20) NOT NULL,                  -- Ensembl Transcript ID
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO ensembl_trans
     SELECT a._id,a.trans_id
     FROM ", subStrs[["cntrTab"]]," as g INNER JOIN anno.ensembl_trans as a
     WHERE g._id=a._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fensemblt ON ensembl_trans (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  
  sql<- paste("
    INSERT INTO map_metadata
     SELECT 'ENSEMBLTRANS', m1.value, m2.value, m3.value
     FROM anno.metadata AS m1, anno.metadata AS m2, anno.metadata AS m3
     WHERE m1.name='ENSOURCENAME' AND
           m2.name='ENSOURCEURL' AND
           m3.name='ENSOURCEDATE';
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT 'ENSEMBLTRANS2GENE',source_name, source_url, source_date
     FROM map_metadata
     WHERE map_name='ENSEMBLTRANS';
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
    UPDATE map_metadata
     SET map_name='ENSEMBLTRANS2",subStrs[["suffix"]],"' WHERE map_name='ENSEMBLTRANS2GENE';
    ", sep="") 
  sqliteQuickSQL(db, sql)

  makeMapCounts(db, "ENSEMBLTRANS","e._id",paste(subStrs[["cntrTab"]]," AS g",sep=""),"ensembl_trans as e","WHERE g._id=e._id")
  makeMapCounts(db, paste("ENSEMBLTRANS2",subStrs[["suffix"]],sep=""),"trans_id","ensembl_trans AS e", paste(subStrs[["cntrTab"]],"AS g"),"WHERE e._id=g._id")
  
}



## Make an MGI table
appendMGI <- function(db, subStrs, printSchema){

  message(cat("Appending MGI"))

  sql<- paste("    CREATE TABLE mgi (
      _id INTEGER NOT NULL,                     -- REFERENCES ", subStrs[["cntrTab"]],"
      mgi_id VARCHAR(20) NOT NULL,              -- ensembl id
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO mgi
     SELECT DISTINCT e._id, e.mgi_id
     FROM ", subStrs[["cntrTab"]]," as g INNER JOIN anno.mgi as e
     WHERE g._id=e._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fmgi ON mgi (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'MGI';
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'MGI2GENE';
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
    UPDATE map_metadata
     SET map_name='MGI2",subStrs[["suffix"]],"' WHERE map_name='MGI2GENE';
    ", sep="") 
  sqliteQuickSQL(db, sql)

  makeMapCounts(db, "MGI",subStrs[["coreID"]],subStrs[["coreTab"]],"mgi",paste("WHERE ",subStrs[["coreTab"]],"._id=mgi._id",sep=""))
  makeMapCounts(db, paste("MGI2",subStrs[["suffix"]],sep=""),"mgi_id","mgi AS m", paste(subStrs[["coreTab"]],"AS g"),"WHERE m._id=g._id")
  
}



## Make the flybase table
appendFlyBase <- function(db, subStrs, printSchema){

  message(cat("Appending FlyBase"))
    
  sql<- paste("    CREATE TABLE flybase (
      _id INTEGER NOT NULL,                         -- REFERENCES ", subStrs[["cntrTab"]],"
      flybase_id VARCHAR(80) NOT NULL,              -- FlyBase ID
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO flybase
     SELECT DISTINCT f._id, f.FBid
     FROM ", subStrs[["cntrTab"]]," as g INNER JOIN anno.flybase as f
     WHERE g._id=f._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fflybase ON flybase (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'FLYBASE';
     ") 
  sqliteQuickSQL(db, sql)

  makeMapCounts(db, "FLYBASE",subStrs[["coreID"]],subStrs[["coreTab"]],"flybase",paste("WHERE ",subStrs[["coreTab"]],"._id=flybase._id",sep=""))
  makeMapCounts(db, paste("FLYBASE2",subStrs[["suffix"]],sep=""),"flybase_id","flybase AS f", paste(subStrs[["coreTab"]],"AS g"),"WHERE f._id=g._id")
  
}


## Make the flybase_cg table
appendFlyBaseCG <- function(db, subStrs, printSchema){

  message(cat("Appending FlyBase CG IDs"))
    
  sql<- paste("    CREATE TABLE flybase_cg (
      _id INTEGER NOT NULL,                          -- REFERENCES ", subStrs[["cntrTab"]],"
      flybase_cg_id VARCHAR(10) NOT NULL,            -- FlyBase CG ID
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO flybase_cg
     SELECT DISTINCT f._id, f.CGid
     FROM ", subStrs[["cntrTab"]]," as g INNER JOIN anno.flybase_cg as f
     WHERE g._id=f._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fflybasecg ON flybase_cg (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  
  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'FLYBASECG';
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'FLYBASECG2GENE';
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
    UPDATE map_metadata
     SET map_name='FLYBASECG",subStrs[["suffix"]],"' WHERE map_name='FLYBASECG2GENE';
    ", sep="") 
  sqliteQuickSQL(db, sql)

  makeMapCounts(db, "FLYBASECG",subStrs[["coreID"]],subStrs[["coreTab"]],"flybase_cg",paste("WHERE ",subStrs[["coreTab"]],"._id=flybase_cg._id",sep=""))
  makeMapCounts(db, paste("FLYBASECG2",subStrs[["suffix"]],sep=""),"flybase_cg_id","flybase_cg AS f", paste(subStrs[["coreTab"]],"AS g"),"WHERE f._id=g._id")  
  
}


## Make the flybase_prot table
appendFlyBaseProt <- function(db, subStrs, printSchema){

  message(cat("Appending Flybase Protein IDs"))
    
  sql<- paste("    CREATE TABLE flybase_prot (
      _id INTEGER NOT NULL,                          -- REFERENCES ", subStrs[["cntrTab"]],"
      prot_id VARCHAR(20) NOT NULL,                  -- Flybase Protein ID
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]],"(_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO flybase_prot
     SELECT f._id,f.prot_id
     FROM ", subStrs[["cntrTab"]]," as g INNER JOIN anno.flybase_prot as f
     WHERE g._id=f._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fflybasep ON flybase_prot (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT 'FLYBASEPROT', m1.value, m2.value, m3.value
     FROM anno.metadata AS m1, anno.metadata AS m2, anno.metadata AS m3
     WHERE m1.name='FBSOURCENAME' AND
           m2.name='FBSOURCEURL' AND
           m3.name='FBSOURCEDATE';
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT 'FLYBASEPROT2GENE',source_name, source_url, source_date
     FROM map_metadata
     WHERE map_name='FLYBASEPROT';
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
    UPDATE map_metadata
     SET map_name='FLYBASEPROT2",subStrs[["suffix"]],"' WHERE map_name='FLYBASEPROT2GENE';
    ", sep="") 
  sqliteQuickSQL(db, sql)

  makeMapCounts(db, "FLYBASEPROT","f._id",paste(subStrs[["cntrTab"]]," AS g",sep=""),"flybase_prot as f","WHERE g._id=f._id")
  makeMapCounts(db, paste("FLYBASEPROT2",subStrs[["suffix"]],sep=""),"prot_id","flybase_prot AS f", paste(subStrs[["cntrTab"]],"AS g"),"WHERE f._id=g._id")
  
}


## Make the aracyc table
appendAraCyc <- function(db, subStrs, printSchema){

  message(cat("Appending AraCyc"))
    
  sql<- paste("    CREATE TABLE aracyc (
      _id INTEGER NOT NULL,                         -- REFERENCES ", subStrs[["cntrTab"]],"
      pathway_name VARCHAR(255) NOT NULL,           -- AraCyc pathway name
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO aracyc
     SELECT g._id, a.pathway_name
     FROM ", subStrs[["cntrTab"]]," as g CROSS JOIN anno.aracyc as a
     WHERE g._id=a._id;
    ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Faracyc ON aracyc (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'ARACYC';
     ") 
  sqliteQuickSQL(db, sql)

  if( subStrs[["coreTab"]] == "probes" ){
      makeMapCounts(db, "ARACYC",subStrs[["coreID"]],subStrs[["coreTab"]],"aracyc",paste("WHERE ",subStrs[["coreTab"]],"._id=aracyc._id",sep=""))
  }else{
      makeMapCounts(db, "ARACYC","pathway_name",subStrs[["coreTab"]],"aracyc",paste("WHERE ",subStrs[["coreTab"]],"._id=aracyc._id",sep=""))      
  }

}


## Make the "aracyc" enzyme table
appendAraCycEnzyme <- function(db, subStrs, printSchema){

  message(cat("Appending AraCyc Enzyme IDs"))
    
  sql<- paste("    CREATE TABLE enzyme (
      _id INTEGER NOT NULL,                          -- REFERENCES ", subStrs[["cntrTab"]],"
      ec_name VARCHAR(255) NOT NULL,                -- EC name
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO enzyme
     SELECT g._id, a.ec_name
     FROM ", subStrs[["cntrTab"]]," as g CROSS JOIN anno.enzyme as a
     WHERE g._id=a._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
    DELETE FROM enzyme
     WHERE rowid NOT IN (
      SELECT rowid FROM enzyme
      GROUP BY _id, ec_name
      HAVING min(rowid));
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fenzyme ON enzyme (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  makeMapCounts(db, "ARACYCENZYME",subStrs[["coreID"]],subStrs[["coreTab"]],"enzyme",paste("WHERE ",subStrs[["coreTab"]],"._id=enzyme._id",sep=""))
  
}



## For Arabidopsis the ", subStrs[["cntrTab"]]," table is still the central table
appendArabidopsisGenes <- function(db, subStrs, printSchema){

  message(cat("Creating Genes table"))

  sql<- paste("    CREATE TABLE genes (
      _id INTEGER PRIMARY KEY,
      gene_id CHAR(9) NOT NULL UNIQUE               -- AGI locus ID
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  if( subStrs[["coreTab"]] == "probes" ){
    sql<- paste("
      INSERT INTO genes
       SELECT DISTINCT a._id, p.gene_id
       FROM probe_map AS p CROSS JOIN anno.genes AS a
       WHERE p.gene_id=a.gene_id;
       ") 
    sqliteQuickSQL(db, sql)
  }
  else{
    sql<- paste("
      INSERT INTO genes
      SELECT * FROM anno.genes;
       ")
    sqliteQuickSQL(db, sql)
  }

  sqliteQuickSQL(db, "ANALYZE;")
}


## For Arabidopsis the ", subStrs[["cntrTab"]]," table is still the central table
appendArabidopsisEntrezGenes <- function(db, subStrs, printSchema){

  message(cat("Creating Entrez Genes table"))

  sql<- paste("    CREATE TABLE entrez_genes (
      _id INTEGER PRIMARY KEY,
      gene_id CHAR(9) NOT NULL UNIQUE               -- AGI locus ID
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  if( subStrs[["coreTab"]] == "probes" ){
    sql<- paste("
      INSERT INTO entrez_genes
       SELECT DISTINCT a._id, p.gene_id
       FROM probe_map AS p CROSS JOIN anno.entrez_genes AS a
       WHERE p.gene_id=a.gene_id;
       ") 
    sqliteQuickSQL(db, sql)
  }
  else{
    sql<- paste("
      INSERT INTO entrez_genes
      SELECT * FROM anno.entrez_genes;
       ")
    sqliteQuickSQL(db, sql)
  }
  
  sql<- paste("    CREATE INDEX Fentrez_genes ON entrez_genes(gene_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

}



## Make the probes table for Arabidopsis (only used by chip packages)
appendArabidopsisProbes <- function(db, subStrs, printSchema){

  message(cat("Appending Probes"))
    
  sql<- paste("    CREATE TEMP TABLE match_count (
      probe_id TEXT,
      is_multiple INTEGER
    );") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO match_count
     SELECT probe_id,
            CASE WHEN count(DISTINCT gene_id)>1 THEN 1 ELSE 0 END
     FROM probe_map
     GROUP BY probe_id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX mc1 ON match_count(probe_id);") 
  sqliteQuickSQL(db, sql)

  sql<- paste(" CREATE TEMP TABLE tprobes (
      probe_id VARCHAR(80) NOT NULL,                -- manufacturer ID
      is_multiple SMALLINT NOT NULL,                -- a silly and useless field
      _id INTEGER NULL,                             -- REFERENCES ", subStrs[["cntrTab"]],"
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO tprobes
     SELECT DISTINCT a.probe_id, a.is_multiple, b._id
     FROM (SELECT   m.probe_id as probe_id,
                    m.is_multiple as is_multiple,
                    p.gene_id as gene_id
            FROM    probe_map as p, match_count as m
            WHERE p.probe_id=m.probe_id) AS a
        LEFT OUTER JOIN ", subStrs[["cntrTab"]]," AS b
        ON a.gene_id=b.gene_id
     ORDER BY a.is_multiple, b._id;
     ") 
  sqliteQuickSQL(db, sql)

  ##We have to drop/cleanup probes that are multiple hits, where the 2nd hit does not map to a probe...
  ##So 1st we drop the 2ndary entries:
  sqliteQuickSQL(db, "DELETE FROM tprobes WHERE is_multiple = 1 and _id IS NULL;")

  ##Then we have to work on cleaning up, the 1st step is to make a temporary table to count the number
  ##of mapped probes that we have for each 
  sql<- paste(" CREATE TEMP TABLE probe_multi_counts (
      probe_id VARCHAR(80) NOT NULL,
      is_multiple SMALLINT NOT NULL,
      _id INTEGER NULL,
      count INTEGER NOT NULL,
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id));
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO probe_multi_counts (probe_id,
                                    is_multiple,
                                    _id,
                                    count)
     SELECT probe_id,
            is_multiple,
            _id,
            count(*)
     FROM tprobes GROUP BY probe_id;
     ")
  sqliteQuickSQL(db, sql)

  ##Update the is_multiple col where our counts indicate that we should
  sqliteQuickSQL(db, "UPDATE probe_multi_counts set is_multiple = 0 where count = 1;")

  ##Then we need a left join to combine this information 
  sql<- paste(" CREATE TEMP TABLE ljoin (probe_id VARCHAR(80) NOT NULL,
      p_is_multiple SMALLINT NOT NULL,
      _id INTEGER NULL,                       
      pmc_probe_id VARCHAR(80) NOT NULL,
      is_multiple SMALLINT NOT NULL,
      pmc_id INTEGER NULL,
      pmc_count INTEGER NULL,
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id));
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO ljoin (probe_id,
                       p_is_multiple,
                       _id,
                       pmc_probe_id,
                       is_multiple,
                       pmc_id,
                       pmc_count)
     SELECT p.probe_id,
            p.is_multiple,
            p._id,
            pmc.probe_id,
            pmc.is_multiple,
            pmc._id ,
            pmc.count
     FROM tprobes as p LEFT JOIN probe_multi_counts as pmc
     ON p.probe_id=pmc.probe_id;
     ") 
  sqliteQuickSQL(db, sql)
  
  ##And Finally, we can make our final table
  sql<- paste(" CREATE TABLE probes (
      probe_id VARCHAR(80) NOT NULL,                -- manufacturer ID
      is_multiple SMALLINT NOT NULL,                -- a silly and useless field
      _id INTEGER NULL,                             -- REFERENCES  genes
      FOREIGN KEY (_id) REFERENCES  ", subStrs[["cntrTab"]],"   (_id)
    );") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO probes (probe_id,
                        is_multiple,
                        _id)
     SELECT probe_id,
            is_multiple,
            _id
     FROM ljoin;
     ") 
  sqliteQuickSQL(db, sql)
  
  sql<- paste("    CREATE INDEX Fprobes ON probes (_id);") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fprobes_probe_id ON probes (probe_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  if(subStrs[["prefix"]] == "ag" || subStrs[["prefix"]] == "ath1121501"){
    sql<- paste("
      INSERT INTO map_metadata
       SELECT 'ACCNUM', source_name, source_url, source_date
       FROM anno.map_metadata
       WHERE map_name = '",subStrs[["prefix"]],"ACCNUM';
       ", sep="") 
    sqliteQuickSQL(db, sql)
  }
  else{
    sql<- paste("
      INSERT INTO map_metadata
       SELECT * FROM anno.map_metadata
       WHERE map_name = 'ACCNUM';
       ", sep="") 
    sqliteQuickSQL(db, sql)
  }
  
  makeMapCounts(db, "ACCNUM","probe_id","probes","","WHERE _id NOT NULL")
  
}



## Make the Arabidopsis gene info table
appendArabidopsisGeneInfo <- function(db, subStrs, printSchema){

  message(cat("Appending Gene Info"))
            
  sql<- paste("    CREATE TABLE gene_info (
      _id INTEGER NOT NULL,                         -- REFERENCES ", subStrs[["cntrTab"]],"
      gene_name VARCHAR(255) NULL,                  -- gene name
      symbol VARCHAR(80) NULL,                      -- gene symbol
      chromosome CHAR(1) NULL,                      -- Arabidopsis chromosome
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id))
    ;") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO gene_info
     SELECT DISTINCT g._id, a.gene_name, a.symbol, a.chromosome
     FROM ", subStrs[["cntrTab"]]," as g CROSS JOIN anno.gene_info as a
     WHERE g._id=a._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fgene_info ON gene_info (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'GENENAME';
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'SYMBOL';
     ") 
  sqliteQuickSQL(db, sql)
   
  makeMapCounts(db, "GENENAME",subStrs[["coreID"]],subStrs[["coreTab"]],"gene_info",paste("WHERE ", subStrs[["coreTab"]],"._id=gene_info._id AND gene_info.gene_name NOT NULL",sep=""))

  makeMapCounts(db, "SYMBOL",subStrs[["coreID"]],subStrs[["coreTab"]],"gene_info",paste("WHERE ", subStrs[["coreTab"]],"._id=gene_info._id AND gene_info.symbol NOT NULL",sep=""))

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'CHR';   ",sep="") 
  sqliteQuickSQL(db, sql)  
  
  makeMapCounts(db, "CHR",subStrs[["coreID"]],subStrs[["coreTab"]],"gene_info",paste("WHERE ", subStrs[["coreTab"]],"._id=gene_info._id AND chromosome NOT NULL",sep=""))
  
}



## For Yeast the ", subStrs[["cntrTab"]]," table is still the central table
appendYeastSGD <- function(db, subStrs, printSchema){

  message(cat("Creating SGD table"))

  sql<- paste("    CREATE TABLE sgd (
      _id INTEGER PRIMARY KEY,
      systematic_name VARCHAR(14) NULL UNIQUE,      -- Yeast gene systematic name
      gene_name VARCHAR(14) NULL UNIQUE,            -- Yeast gene name
      sgd_id CHAR(10) NOT NULL UNIQUE               -- SGD ID
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  if( subStrs[["coreTab"]] == "probes" ){
    sql<- paste("
      INSERT INTO sgd
       SELECT DISTINCT s._id, s.systematic_name, s.gene_name, s.sgd_id
       FROM probe_map as p CROSS JOIN anno.sgd as s
       WHERE p.systematic_name=s.systematic_name;
       ") 
    sqliteQuickSQL(db, sql) 
  }
  else{
    sql<- paste("
      INSERT INTO sgd
      SELECT * FROM anno.sgd;
       ") 
    sqliteQuickSQL(db, sql) 
  }

  sqliteQuickSQL(db, "ANALYZE;")

}



## Make the probes table for Yeast (only used by chip packages)
appendYeastProbes <- function(db, subStrs, printSchema){

  message(cat("Appending Probes"))
    
  sql<- paste("    CREATE TABLE probes (
      probe_id VARCHAR(80),              -- manufacturer ID
      is_multiple SMALLINT NOT NULL,
      _id INTEGER NULL,                              -- REFERENCES sgd
      FOREIGN KEY (_id) REFERENCES sgd (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO probes
     SELECT DISTINCT  p.probe_id as probe_id, p.is_multiple, s._id as _id
     FROM probe_map as p LEFT OUTER JOIN sgd as s
     ON p.systematic_name=s.systematic_name
     ORDER BY probe_id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fprobes ON probes (_id);") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)


  sql<- paste("
    INSERT INTO map_metadata
     SELECT 'ORF', source_name, source_url, source_date
     FROM anno.map_metadata
     WHERE map_name='ALIAS';
     ") 
  sqliteQuickSQL(db, sql)

  makeMapCounts(db, "ORF","probe_id","probes",subStrs[["cntrTab"]],paste("WHERE ", subStrs[["cntrTab"]],"._id=probes._id",sep=""))

}


## Make the Yeast chromosomes table
appendYeastOrphanMeta <- function(db, subStrs){
    
  message(cat("Appending Orphaned Meta Data"))

    sql<- paste("
    INSERT INTO map_metadata
     SELECT 'GENENAME', source_name, source_url, source_date
     FROM anno.map_metadata
     WHERE map_name='ALIAS';
    ") 
  sqliteQuickSQL(db, sql)

  if(subStrs[["coreTab"]]=="probes"){
      makeMapCounts(db, "GENENAME","probe_id","probes",subStrs[["cntrTab"]],paste("WHERE probes._id=", subStrs[["cntrTab"]],"._id AND ", subStrs[["cntrTab"]],".gene_name NOT NULL",sep=""))      
  }
  else{
      makeMapCounts(db, "GENENAME","systematic_name",subStrs[["cntrTab"]],"","WHERE gene_name NOT NULL")
  }
  
}


## Make the Yeast chromosomes table
appendYeastChromosomeFeatures <- function(db, subStrs, printSchema){
    
  message(cat("Appending Chromosome Features"))

    sql<- paste("      CREATE TABLE chromosome_features (
        _id INTEGER NOT NULL,                         -- REFERENCES sgd
        chromosome VARCHAR(2) NULL,                   -- chromosome name
        start INTEGER NULL,
        stop INTEGER NULL,
        feature_description TEXT NULL,                -- Yeast feature description
        FOREIGN KEY (_id) REFERENCES sgd (_id)
      );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO chromosome_features
     SELECT f._id, f.chromosome, f.start, f.stop, f.feature_description
     FROM sgd AS s CROSS JOIN anno.chromosome_features AS f
     WHERE s._id=f._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fchromosome_features ON chromosome_features (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'CHRLOC';
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'CHRLOCEND';
     ") 
  sqliteQuickSQL(db, sql)
  
  makeMapCounts(db, "CHRLOC",subStrs[["coreID"]],subStrs[["coreTab"]],"chromosome_features",paste("WHERE ", subStrs[["coreTab"]],"._id=chromosome_features._id AND start NOT NULL",sep=""))

  makeMapCounts(db, "CHRLOCEND",subStrs[["coreID"]],subStrs[["coreTab"]],"chromosome_features",paste("WHERE ", subStrs[["coreTab"]],"._id=chromosome_features._id AND stop NOT NULL",sep=""))
  
  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'CHR';
     ") 
  sqliteQuickSQL(db, sql)
  
  makeMapCounts(db, "CHR",subStrs[["coreID"]],subStrs[["coreTab"]],"chromosome_features",paste("WHERE ", subStrs[["coreTab"]],"._id=chromosome_features._id AND chromosome NOT NULL",sep=""))

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'DESCRIPTION';
     ") 
  sqliteQuickSQL(db, sql)
  
  makeMapCounts(db, "DESCRIPTION",subStrs[["coreID"]],subStrs[["coreTab"]],"chromosome_features",paste("WHERE ", subStrs[["coreTab"]],"._id=chromosome_features._id AND feature_description NOT NULL",sep=""))
  
}




## Make the Yeast alias table
appendYeastAlias <- function(db, subStrs, printSchema){

  message(cat("Appending Alias"))
    
  sql<- paste("    CREATE TABLE gene2alias (
      _id INTEGER NOT NULL,                         -- REFERENCES sgd
      alias VARCHAR(13) NOT NULL,                   -- Yeast gene alias
      FOREIGN KEY (_id) REFERENCES sgd (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO gene2alias
     SELECT a._id, a.alias
     FROM sgd AS s CROSS JOIN anno.gene2alias AS a
     WHERE s._id=a._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fgene2alias ON gene2alias(_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'ALIAS2GENE';
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT 'ALIAS', source_name, source_url, source_date
     FROM anno.map_metadata
     WHERE map_name='ALIAS';
    ", sep="") 
  sqliteQuickSQL(db, sql)
  
  makeMapCounts(db, "ALIAS",subStrs[["coreID"]],subStrs[["coreTab"]],"gene2alias",paste("WHERE ", subStrs[["coreTab"]],"._id=gene2alias._id",sep=""))
}


## Make the Yeast pfam table (different schema for table)
appendYeastPfam <- function(db, subStrs, printSchema){

  message(cat("Appending Pfam"))
    
  sql<- paste("    CREATE TABLE pfam (
      _id INTEGER NOT NULL,                         -- REFERENCES sgd
      pfam_id CHAR(7) NOT NULL,                     -- Pfam ID
      FOREIGN KEY (_id) REFERENCES sgd (_id))
    ;") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO pfam
     SELECT _id, pfam_id
     FROM anno.pfam;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fpfam ON pfam (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'PFAM';
     ") 
  sqliteQuickSQL(db, sql)

  makeMapCounts(db, "PFAM","_id","pfam")
  
}




## Make the Yeast Interpro table
appendYeastInterpro <- function(db, subStrs, printSchema){

  message(cat("Appending Interpro"))
    
  sql<- paste("    CREATE TABLE interpro (
      _id INTEGER NOT NULL,                         -- REFERENCES sgd
      interpro_id CHAR(9) NOT NULL,                 -- InterPro ID
      FOREIGN KEY (_id) REFERENCES sgd (_id))
    ;") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO interpro
     SELECT _id, interpro_id
     FROM anno.interpro;
    ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Finterpro ON interpro (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'INTERPRO';
     ") 
  sqliteQuickSQL(db, sql)

  makeMapCounts(db, "INTERPRO","_id","interpro")
}



## Make the Yeast Smart table
appendYeastSmart <- function(db, subStrs, printSchema){

  message(cat("Appending Smart"))
    
  sql<- paste("    CREATE TABLE smart (
      _id INTEGER NOT NULL,                         -- REFERENCES sgd 
      smart_id CHAR(7) NOT NULL,                    -- SMART ID
      FOREIGN KEY (_id) REFERENCES sgd (_id))\
    ;") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO smart
     SELECT _id, smart_id
     FROM anno.smart;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fsmart ON smart (_id);") 
  if(printSchema==TRUE){write(paste(paste(sql,"\n"),"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'SMART';
     ") 
  sqliteQuickSQL(db, sql)

  makeMapCounts(db, "SMART","_id","smart")
  
}



## Make the Yeast Smart table
appendYeastRejectORF <- function(db, subStrs, printSchema){

  message(cat("Appending RejectOrf"))
    
  sql<- paste("    CREATE TABLE reject_orf (
      systematic_name VARCHAR(14) PRIMARY KEY)     -- Yeast gene systematic name
    ;") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO reject_orf
     SELECT DISTINCT systematic_name
     FROM anno.reject_orf;
     ") 
  sqliteQuickSQL(db, sql)

  makeMapCounts(db, "REJECTORF","*","reject_orf")
  
}




## Make the Yeast Smart table
appendYeastGene2Systematic <- function(db, subStrs, printSchema){

  message(cat("Appending Gene2Systematic"))
    
  sql<- paste("    CREATE TABLE gene2systematic (
      gene_name VARCHAR(14) NULL,                     -- Yeast gene name
      systematic_name VARCHAR(14) NULL)               -- Yeast gene systematic name
    ;") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO gene2systematic
     SELECT gene_name, systematic_name
     FROM anno.gene2systematic;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT 'COMMON2",subStrs[["suffix"]],"', source_name, source_url, source_date
     FROM anno.map_metadata
     WHERE map_name='ALIAS';
     ", sep = "") 
  sqliteQuickSQL(db, sql)
  
  makeMapCounts(db, paste("COMMON2",subStrs[["suffix"]],sep=""),"gene_name","gene2systematic","","WHERE systematic_name IS NOT NULL")
  
}



## Make the uniprot table
appendUniprot <- function(db, subStrs, printSchema){

  message(cat("Appending Uniprot"))
    
  sql<- paste("    CREATE TABLE uniprot (
      _id INTEGER NOT NULL,                         -- REFERENCES ", subStrs[["cntrTab"]],"
      uniprot_id VARCHAR(20) NOT NULL,              -- uniprot id
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)
 
  sql<- paste("
    INSERT INTO uniprot
     SELECT DISTINCT u._id, u.uniprot_id
     FROM ", subStrs[["cntrTab"]]," as g INNER JOIN anno.uniprot as u
     WHERE g._id=u._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Funiprot ON uniprot (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'UNIPROT';
     ") 
  sqliteQuickSQL(db, sql)

  makeMapCounts(db, "UNIPROT",subStrs[["coreID"]],subStrs[["coreTab"]],"uniprot",paste("WHERE ",subStrs[["coreTab"]],"._id=uniprot._id",sep=""))

}

## Make the uniprot table
appendUCSCGenes <- function(db, subStrs, printSchema){

  message(cat("Appending UCSC Known Genes"))
    
  sql<- paste("    CREATE TABLE ucsc (
      _id INTEGER NOT NULL,                         -- REFERENCES ", subStrs[["cntrTab"]],"
      ucsc_id VARCHAR(20) NOT NULL,              -- uniprot id
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)
 
  sql<- paste("
    INSERT INTO ucsc
     SELECT DISTINCT u._id, u.ucsc_id
     FROM ", subStrs[["cntrTab"]]," as g INNER JOIN anno.ucsc as u
     WHERE g._id=u._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fucsc ON ucsc (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'UCSCKG';
     ") 
  sqliteQuickSQL(db, sql)

  makeMapCounts(db, "UCSCKG",subStrs[["coreID"]],subStrs[["coreTab"]],"ucsc",paste("WHERE ",subStrs[["coreTab"]],"._id=ucsc._id",sep=""))
  
}


## Make an external genes table to hold Entrez Gene IDs
appendExternalEG <- function(db, subStrs, printSchema){

  message(cat("Appending Entrez Gene IDs"))
    
  sql<- paste("    CREATE TABLE genes (
      _id INTEGER NOT NULL,                         -- REFERENCES ", subStrs[["cntrTab"]],"
      gene_id VARCHAR(20) NOT NULL,                 -- gene id
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)
 
  sql<- paste("
    INSERT INTO genes
     SELECT DISTINCT u._id, u.gene_id
     FROM ", subStrs[["cntrTab"]]," as g INNER JOIN anno.genes as u
     WHERE g._id=u._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fgene ON genes(_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'ENTREZID';
     ") 
  sqliteQuickSQL(db, sql)

  makeMapCounts(db, "ENTREZID",subStrs[["coreID"]],subStrs[["coreTab"]],"genes",paste("WHERE ",subStrs[["coreTab"]],"._id=genes._id",sep=""))

}




## Make the zfin table
appendZfin <- function(db, subStrs, printSchema){

  message(cat("Appending Zfin"))
    
  sql<- paste("    CREATE TABLE zfin (
      _id INTEGER NOT NULL,                         -- REFERENCES ", subStrs[["cntrTab"]],"
      zfin_id VARCHAR(20) NOT NULL,                 -- zfin id
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)
 
  sql<- paste("
    INSERT INTO zfin
     SELECT DISTINCT z._id, z.ZFid
     FROM ", subStrs[["cntrTab"]]," as g INNER JOIN anno.zfin as z
     WHERE g._id=z._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fzfin ON zfin (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'ZFIN';
     ") 
  sqliteQuickSQL(db, sql)

  makeMapCounts(db, "ZFIN",subStrs[["coreID"]],subStrs[["coreTab"]],"zfin",paste("WHERE ",subStrs[["coreTab"]],"._id=zfin._id",sep=""))

}



## Make the wormbase table
appendWormbase <- function(db, subStrs, printSchema){

  message(cat("Appending Wormbase"))
    
  sql<- paste("    CREATE TABLE wormbase (
      _id INTEGER NOT NULL,                         -- REFERENCES ", subStrs[["cntrTab"]],"
      wormbase_id VARCHAR(20) NOT NULL,             -- wormbase id
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)
 
  sql<- paste("
    INSERT INTO wormbase
     SELECT DISTINCT w._id, w.WBid
     FROM ", subStrs[["cntrTab"]]," as g INNER JOIN anno.wormbase as w
     WHERE g._id=w._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Fwormbase ON wormbase (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'WORMBASE';
     ") 
  sqliteQuickSQL(db, sql)

  makeMapCounts(db, "WORMBASE",subStrs[["coreID"]],subStrs[["coreTab"]],"wormbase",paste("WHERE ",subStrs[["coreTab"]],"._id=wormbase._id",sep=""))

}





## Make the locus_tag table
appendYeastNCBILocusTags <- function(db, subStrs, printSchema){

  message(cat("Appending locus tags"))
    
  sql<- paste("    CREATE TABLE locus_tag (
      _id INTEGER NOT NULL,                        -- REFERENCES ", subStrs[["cntrTab"]],"
      locus_tag VARCHAR(80) NOT NULL,              -- Locus_Tag
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO locus_tag
     SELECT DISTINCT g._id, lt.locus_tag
     FROM ", subStrs[["cntrTab"]]," as g INNER JOIN anno.locus_tags as lt
     WHERE g._id=lt._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX Flocus_tag ON locus_tag (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'ORF';
     ") 
  sqliteQuickSQL(db, sql)

  makeMapCounts(db, "ORF",subStrs[["coreID"]],subStrs[["coreTab"]],"locus_tag",paste("WHERE ",subStrs[["coreTab"]],"._id=locus_tag._id",sep=""))
  makeMapCounts(db, paste("ORF2",subStrs[["suffix"]],sep=""),"locus_tag","locust_tag AS lt", paste(subStrs[["coreTab"]],"AS g"),"WHERE lt._id=g._id")
  
}


## Make the sgd table
appendYeastNCBISGD <- function(db, subStrs, printSchema){

  message(cat("Appending sgd IDs"))
    
  sql<- paste("    CREATE TABLE sgd (
      _id INTEGER NOT NULL,                         -- REFERENCES ", subStrs[["cntrTab"]],"
      sgd_id VARCHAR(80) NOT NULL,              -- SGD ID
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO sgd
     SELECT DISTINCT g._id, s.sgd_id
     FROM ", subStrs[["cntrTab"]]," as g INNER JOIN anno.sgd_ids as s
     WHERE g._id=s._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX FSGD ON sgd (_id);") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO map_metadata
     SELECT * FROM anno.map_metadata
     WHERE map_name = 'SGD';
     ") 
  sqliteQuickSQL(db, sql)

  makeMapCounts(db, "SGD",subStrs[["coreID"]],subStrs[["coreTab"]],"sgd",paste("WHERE ",subStrs[["coreTab"]],"._id=sgd._id",sep=""))
  makeMapCounts(db, paste("SGD2",subStrs[["suffix"]],sep=""),"sgd_id","sgd AS s", paste(subStrs[["coreTab"]],"AS g"),"WHERE s._id=g._id")
  
}














## For the org packages, we want GO views that summarize the GO tables conveniently.
makeGOViews <- function(db){
  message(cat("Making GO views \n\n"))
  sql<- paste("
CREATE VIEW go AS
SELECT _id,go_id,evidence, 'BP' AS 'ontology' FROM go_bp
UNION
SELECT _id,go_id,evidence, 'CC' FROM go_cc
UNION
SELECT _id,go_id,evidence, 'MF' FROM go_mf;
   ",sep="") 
  sqliteQuickSQL(db, sql)

  sql<- paste("
CREATE VIEW go_all AS
SELECT _id,go_id,evidence, 'BP' AS 'ontology'  FROM go_bp_all
UNION
SELECT _id,go_id,evidence, 'CC' FROM go_cc_all
UNION
SELECT _id,go_id,evidence, 'MF' FROM go_mf_all;
   ",sep="")
  
sqliteQuickSQL(db, sql)
}






## Append on metadata that CANNOT easily go next to the table it describes.
appendPostMeta <- function(db, subStrs){

  message(cat("Appending Metadata \n\n"))
  
  sql<- paste("
    INSERT INTO metadata SELECT * FROM anno.metadata
     WHERE name!='DBSCHEMA' AND name!='ORGANISM' AND name!='SPECIES' AND name!='DBSCHEMAVERSION';
   ",sep="") 
  sqliteQuickSQL(db, sql)

  ##If its a chip package, then check to see what the metadata value is.  If that value is == "EG", then change it to be "ENTREZID"
  if(subStrs[["coreID"]]=="probe_id"){
      curVal <- sqliteQuickSQL(db, "SELECT value FROM anno.metadata WHERE name='CENTRALID';")
      if(curVal == "EG"){
          sqliteQuickSQL(db, "UPDATE metadata SET value = 'ENTREZID' WHERE name = 'CENTRALID';")
      }
  }  
  
  if(subStrs[["coreTab"]]=="probes" && subStrs[["org"]]!="arabidopsis" && subStrs[["org"]]!="yeast"){
      makeMapCounts(db, "ENTREZID",subStrs[["coreID"]],subStrs[["coreTab"]],"","WHERE _id NOT NULL")  
  }
  makeMapCounts(db, "TOTAL",subStrs[["coreID"]],subStrs[["coreTab"]])

  #we ALWAYS have to drop the base "probe map"
  sqliteQuickSQL(db, "DROP TABLE probe_map;")
  ##sqliteQuickSQL(db, "VACUUM probe_map;")
  ##sqliteQuickSQL(db, "ANALYZE;")  
}









##Generic Append function:
##This function necessarily makes several assumptions:
## 1) only one kinf of item can be in the final table
## 2) table will be of the most simple schema (_id and a value of some sort)
## 3) the data will be connected to the other data with an inner join
## 4) one of the collumns must match up to an existing field somewhere in the database and the name of this field MUST be passed in

##Things to specify:
##  1) table name
##  2) field (name of new field)
##  3) matchID (ID type to match to)
##  4) fileName (file with IDs to get put in, file has to have two cols where one is type 'field' and the other is type 'MatchID')
##  5) file is expected to be ORDERED, so in file matchID is listed FIRST and THEN the field that goes with that.  The idea is that matchID will be something like an entrez gene ID so this should be more intuitive.

appendGeneric <- function(db, subStrs, printSchema, table, matchID, field, fileName, mapCounts ){

  #CONSTRAINT: matchID *MUST* match the id of one of the fields in the DB
    
  message(cat("Appending ",table,""))

  #need to make a temp table to start
  sql<- paste("    CREATE TEMP TABLE tempTable (
      ",field," VARCHAR NOT NULL,
      ",matchID," VARCHAR NOT NULL
    );")
  sqliteQuickSQL(db, sql)

  #read in the file in fileName
  IDs = read.delim(file=fileName, header=FALSE, sep="\t", quote="")
  IDs = as.list(IDs)
  
  #Now its time to insert
  for( i in 1:length(IDs[[1]])){
      sqlIns <- paste("INSERT INTO tempTable ('",field,"','",matchID,"') VALUES ('",IDs[[2]][i],"','",IDs[[1]][i],"');
      ", sep="")
      #print(sqlIns)
      sqliteQuickSQL(db,sqlIns)
  }  
  
  sql<- paste("    CREATE TABLE ",table," (
      _id INTEGER NOT NULL,                         -- REFERENCES ", subStrs[["cntrTab"]],"
      ",field," VARCHAR NOT NULL,               -- ",field," accession number
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO ",table,"
     SELECT g._id as _id, t.",field,"
     FROM ", subStrs[["cntrTab"]]," as g INNER JOIN tempTable as t
     WHERE g.",matchID,"=t.",matchID,"
     ORDER BY g._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX F",table," ON ",table," (_id);", sep ="") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  #Put a forward and reverse map into map_counts
  #note, these queries REQUIRE that the coretable be MADE before they are invoked
  sql<- paste("
    INSERT INTO map_counts
     SELECT '",mapCounts,"', count(DISTINCT ", subStrs[["coreID"]],")
     FROM ",table," AS r INNER JOIN ", subStrs[["coreTab"]]," AS ct
     WHERE r._id = ct._id;
  ", sep="")
  #print(sql)
  sqliteQuickSQL(db, sql)
  
  sql<- paste("
    INSERT INTO map_counts
     SELECT '",mapCounts,"2", subStrs[["suffix"]],"', count(DISTINCT ",field,")
     FROM ",table," AS r INNER JOIN ", subStrs[["coreTab"]]," AS ct
     WHERE r._id = ct._id;
  ", sep="")
  #print(sql)
  sqliteQuickSQL(db, sql)
  
} 



#Append probes Generic is the same as appendGeneric, EXCEPT that it needs special mapcounts queries.
appendProbesGeneric <- function(db, subStrs, printSchema, table, matchID, field, fileName, mapCounts ){
    
  message(cat("Appending ",table,""))

  #need to make a temp table to start
  sql<- paste("    CREATE TEMP TABLE tempTable (
      ",field," VARCHAR NOT NULL,
      ",matchID," VARCHAR NOT NULL
    );")
  sqliteQuickSQL(db, sql)

  #read in the file in fileName
  IDs = read.delim(file=fileName, header=FALSE, sep="\t", quote="")
  IDs = as.list(IDs)
  
  #Now its time to insert
  for( i in 1:length(IDs[[1]])){
      sqlIns <- paste("INSERT INTO tempTable ('",field,"','",matchID,"') VALUES ('",IDs[[2]][i],"','",IDs[[1]][i],"');
      ", sep="")
      #print(sqlIns)
      sqliteQuickSQL(db,sqlIns)
  }  
  
  sql<- paste("    CREATE TABLE ",table," (
      _id INTEGER NOT NULL,                         -- REFERENCES ", subStrs[["cntrTab"]],"
      ",field," VARCHAR NOT NULL,               -- ",field," accession number
      FOREIGN KEY (_id) REFERENCES ", subStrs[["cntrTab"]]," (_id)
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO ",table,"
     SELECT g._id as _id, t.",field,"
     FROM ", subStrs[["cntrTab"]]," as g INNER JOIN tempTable as t
     WHERE g.",matchID,"=t.",matchID,"
     ORDER BY g._id;
     ") 
  sqliteQuickSQL(db, sql)

  sql<- paste("    CREATE INDEX F",table," ON ",table," (_id);", sep ="") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  #Put a forward and reverse map into map_counts
  #note, these queries REQUIRE that the coretable be MADE before they are invoked
  sql<- paste("
    INSERT INTO map_counts
     SELECT '",mapCounts,"', count(DISTINCT ", subStrs[["coreID"]],")
     FROM ",table," WHERE ", subStrs[["coreID"]]," NOT NULL ;
  ", sep="")
  #print(sql)
  sqliteQuickSQL(db, sql)

  #no reverse map this time - nobody ever wants probes to EG (not interesting except as a DB metric)
}





##Next I need a function so users can make a center table of their own specification.
##Similar assumptions to the appendGeneric() will apply here as well.  But additionally,
##there needs to be a constraint that 

##Things to specify:
##  1) table name
##  2) field (name of new field)
##  3) fileName (file with IDs to get put in, file has to have one col with the aim of having those IDs be the central IDs (to which all other things will get attached) in the DB)


createCntrTableGeneric <- function(db, subStrs, printSchema, table, field, fileName ){

  #CONSTRAINT: matchID *MUST* match the id of one of the fields in the DB
    
  message(cat("Appending ",table,""))
  
  sql<- paste("    CREATE TEMP TABLE tempTable (
      ",field," VARCHAR NOT NULL               -- ",field," accession number
    );")
  sqliteQuickSQL(db, sql)
  
  #read in the file in fileName
  IDs = read.delim(file=fileName, header=FALSE, sep="\t", quote="")
  #clean out the NAs (there just HAS to be a cleaner way to do this)
  IDs = IDs[!is.na(IDs)] #(get rid of NAs because they will cause the loop below to fail)  
  for(i in 1:length(IDs)){
      if(IDs[i]==""){
          IDs[i]=NA
      }
  }
  IDs = IDs[!is.na(IDs)]  #get rid of any new NAs here
  
  #Now its time to insert
  for( i in 1:length(IDs)){
      sqlIns <- paste("INSERT INTO tempTable ('",field,"') VALUES ('",IDs[i],"');
      ", sep="")
      #print(sqlIns)
      sqliteQuickSQL(db,sqlIns)
  }
  
  sql<- paste("    CREATE TABLE ",table," (
      _id INTEGER PRIMARY KEY,                         
     ",field," VARCHAR NOT NULL               -- ",field," accession number
    );") 
  if(printSchema==TRUE){write(sql, file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)

  sql<- paste("
    INSERT INTO ",table," (",field,")
     SELECT DISTINCT ",field,"
     FROM tempTable
     ORDER BY ",field,";
     ") 
  sqliteQuickSQL(db, sql)
  
  sql<- paste("    CREATE INDEX F",table," ON ",table," (",field,");", sep = "") 
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)
  
  #since we are making a central DB here we should probably check whether a map_counts table exists or not etc.
  sql<- paste("    CREATE TABLE IF NOT EXISTS map_counts (
      map_name VARCHAR(80) PRIMARY KEY,
      count INTEGER NOT NULL
    );")
  if(printSchema==TRUE){write(paste(sql,"\n"), file=paste(subStrs[["outDir"]],"/",subStrs[["prefix"]],".sql", sep=""), append=TRUE)}
  sqliteQuickSQL(db, sql)
  
}




## simplify the probes tables (for all packages except yeast and arabidopsis)
simplifyProbes <- function(db, subStrs){
  message(cat("simplifying probes table"))
  sqliteQuickSQL(db, "CREATE TEMP TABLE probehook (probe_id VARCHAR(80), gene_id VARCHAR(10) NULL, is_multiple SMALLINT NOT NULL);")  
  sqliteQuickSQL(db, "INSERT INTO probehook SELECT DISTINCT p.probe_id, g.gene_id, p.is_multiple FROM probes AS p LEFT JOIN genes AS g ON p._id = g._id;")
  sqliteQuickSQL(db, "CREATE TABLE accessions (probe_id VARCHAR(80),accession VARCHAR(20));")
  sqliteQuickSQL(db, "INSERT INTO accessions SELECT DISTINCT probe_id, accession FROM probes;") 
  sqliteQuickSQL(db, "CREATE INDEX Fgbprobes ON accessions (probe_id);")
  sqliteQuickSQL(db, "DROP TABLE probes;")
  sqliteQuickSQL(db, "CREATE TABLE probes (probe_id VARCHAR(80), gene_id VARCHAR(10) NULL, is_multiple SMALLINT NOT NULL);")
  sqliteQuickSQL(db, "INSERT INTO probes SELECT * FROM probehook;")
  sqliteQuickSQL(db, "CREATE INDEX Fprobes ON probes (probe_id);")
  sqliteQuickSQL(db, "CREATE INDEX Fgenes ON probes (gene_id);")
}


## simplify the probes tables (for arabidopsis)
simplifyArabidopsisProbes <- function(db, subStrs){
  message(cat("simplifying probes table"))
  sqliteQuickSQL(db, "CREATE TEMP TABLE probehook (probe_id VARCHAR(80) NOT NULL, is_multiple SMALLINT NOT NULL, gene_id VARCHAR(10) NULL);")
  sqliteQuickSQL(db, "INSERT INTO probehook SELECT p.probe_id, p.is_multiple, g.gene_id FROM probes AS p LEFT JOIN genes AS g ON p._id = g._id;")
  sqliteQuickSQL(db, "DROP TABLE probes;")
  sqliteQuickSQL(db, "CREATE TABLE probes (probe_id VARCHAR(80) NOT NULL, is_multiple SMALLINT NOT NULL, gene_id VARCHAR(10) NULL);")
  sqliteQuickSQL(db, "INSERT INTO probes SELECT * FROM probehook;")
  sqliteQuickSQL(db, "CREATE INDEX Fprobes ON probes (probe_id);")
  sqliteQuickSQL(db, "CREATE INDEX Fgenes ON probes (gene_id);")
}


## simplify the probes tables (for yeast)
simplifyYeastProbes <- function(db, subStrs){
  message(cat("simplifying probes table"))
  sqliteQuickSQL(db, "CREATE TEMP TABLE probehook (probe_id VARCHAR(80), systematic_name VARCHAR(14) NULL, gene_name VARCHAR(14) NULL, sgd_id CHAR(10) NULL, is_multiple SMALLINT NOT NULL);")
  sqliteQuickSQL(db, "INSERT INTO probehook SELECT p.probe_id, s.systematic_name, s.gene_name, s.sgd_id, p.is_multiple FROM probes AS p LEFT JOIN sgd AS s ON p._id = s._id;")
  sqliteQuickSQL(db, "DROP TABLE probes;")
  sqliteQuickSQL(db, "CREATE TABLE probes (probe_id VARCHAR(80), systematic_name VARCHAR(14) NULL, gene_name VARCHAR(14) NULL, sgd_id CHAR(10) NULL, is_multiple SMALLINT NOT NULL);")
  sqliteQuickSQL(db, "INSERT INTO probes SELECT * FROM probehook;")
  sqliteQuickSQL(db, "CREATE INDEX Fprobes ON probes (probe_id);")
  sqliteQuickSQL(db, "CREATE INDEX Fgenes ON probes (systematic_name);")
}



## drop unwanted tables 
dropRedundantTables <- function(db, subStrs){
  message(cat("dropping redundant data"))
  ##Define tables we should never drop (sqlite_stat1 is "undroppable")
  saveList = c("accessions", "probes", "map_counts", "metadata", "map_metadata", "sqlite_stat1")
  list = dbListTables(db)
  list = list[!(list %in% saveList)]
  for(i in seq_len(length(list))){
      sql <- paste("DROP TABLE ",list[[i]],";",sep="")
      sqliteQuickSQL(db, sql)
  }
  ## sqliteQuickSQL(db, "VACUUM;")
}





##makeMapCounts is for generic map_counts creation
makeMapCounts <- function(db, mapCount="", coreID="", coreTab="", otherTab="", whereClause=""){
    coreTab2 = coreTab  ##may need to append a comma, but we may also need the original...
  if(otherTab !=""){
        ## message(cat("placing comma to enable inner join."))
        coreTab2<-paste(coreTab2,", ",sep="")
        ## message(cat(coreTab2))
        ## message(cat(paste(coreTab2," ",otherTab,sep="")))
    }
  if(coreID !="*"){coreID<-paste("DISTINCT ",coreID,sep="")}
  sqlCount<- paste("
     SELECT '",mapCount,"', count(",coreID,")
     FROM ",coreTab2," ",otherTab,"
     ",whereClause,"
    ", sep="")

  ##If its a probes mapping, then we want to do something more.
  ##Since we need potential reverse maps to also be filtered, then I will have to also check if the otherTab is "probes".
    if(length(grep("probes", coreTab))>0 || length(grep("probes", otherTab))>0 ){
        if(coreTab=="probes" || otherTab=="probes"){
            if(whereClause==""){
                sqlCount<-paste(sqlCount," WHERE probes.is_multiple = 0", sep="")
            }else{
                sqlCount<-paste(sqlCount," AND probes.is_multiple = 0", sep="")
            }
        }else{
            if(length(grep("probes", coreTab))>0){
                abbrev = gsub("probes AS","",coreTab,perl=TRUE)
            }else{abbrev = gsub("probes AS","",otherTab,perl=TRUE)}
            abbrev = gsub(" ","",abbrev,perl=TRUE)
            if(whereClause==""){
                sqlCount<-paste(sqlCount,paste(" WHERE ",abbrev,".is_multiple = 0",sep=""), sep="")
            }else{
                sqlCount<-paste(sqlCount,paste(" AND ",abbrev,".is_multiple = 0",sep=""), sep="")
            }
        }
    }
      
  sql<- paste("INSERT INTO map_counts",sqlCount)
  ##Announce the insertion. (temp)
  ## message(cat(sqlCount))
  ##make the insertion.
  sqliteQuickSQL(db, sql)
  ##Then return the number of things inserted..
  return(as.integer(sqliteQuickSQL(db,sqlCount)[2]))
}


