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

generateAnnotTableObj <- function( objName, objClass, tableName, pkgName, col, rsProcessor=NULL) {
        if(!missing(pkgName))
                ns <- asNamespace(pkgName)
        else
                ns <- .GlobalEnv
	cat("generate ", objName, "\n")
        getDb <- get("getDb", ns)
        obj <- switch(objClass,
            "AnnotDbTable" = new("AnnotDbTable", 
                                            tableName, getDb, rsProcessor),
            "AnnotDbTableTwoWayMap" = new("AnnotDbTableTwoWayMap", 
                                            tableName, getDb, col[1], col[2], rsProcessor),
            "AnnotTwoColTable" = new("AnnotTwoColTable", 
                                            tableName, getDb, col[1], col[2], rsProcessor),
            "AnnotMultiColTable" = new("AnnotMultiColTable", 
                                            tableName, getDb, col[1], rsProcessor),
            "AnnotMultiColTwoKeyTable" = new("AnnotMultiColTwoKeyTable", 
                                            tableName, getDb, col[1], col[2], rsProcessor),
            "AnnotGOTermsTable" = new("AnnotGOTermsTable", 
                                            tableName, getDb, col[1], rsProcessor),
            "AnnotThreeColTable" = new("AnnotThreeColTable", 
                                            tableName, getDb, col[1], col[2], col[3], rsProcessor))
        assign(objName, obj, envir=ns)
}

# FIXME: this function need to be more general in the future...
generateQC <- function(pkgName, objPrefix=pkgName) {
	if(!missing(pkgName))
		ns <- asNamespace(pkgName)
	else
		ns <- .GlobalEnv
	tableNames <- ls(ns)
	tableCounts <- unlist(lapply(tableNames, function(x) {
			thisTable <- get(x, envir=ns)
			if (is(thisTable, "AnnotDbTable")) 
				length(ls(thisTable))
			else 
				 NA
		}))
	names(tableCounts) <- tableNames
	tableCounts <- tableCounts[!is.na(tableCounts)]
	assign(paste(objPrefix, "MAPCOUNTS", sep=""), tableCounts, envir=ns)
	tableQC <- paste("\t", names(tableCounts), "found", tableCounts,  
			sep=" ", collapse="\n")
	tableQC <- paste("\n\nQuality control information for ", pkgName, "\n",
			"\nMappings found for non-probe based rda files:\n", 
			tableQC, "\n\n", 
			sep="", collapse="")
	assign(paste(objPrefix, "QC", sep=""), tableQC, envir=ns)
}

compareAnnotation <- function(pkg1, pkg2, objs, nona2=FALSE, maxCompare=50, printSize=5) {
	if( ! paste("package", pkg1, sep=":") %in% search()) {
		cat("library(", pkg1, ")\n")
		library(pkg1, character.only=T, warn.conflicts=F)
	}
	if( ! paste("package", pkg2, sep=":") %in% search()) {
		cat("library(", pkg2, ")\n")
		library(pkg2, character.only=T, warn.conflicts=F)
	}
	pkgName1 <- paste(pkg1, packageDescription(pkg1, field="Version"), sep="_", collapse="")
	pkgName2 <- paste(pkg2, packageDescription(pkg2, field="Version"), sep="_", collapse="")
	ns1 <- as.environment(paste("package", pkg1, sep=":"))
	ns2 <- as.environment(paste("package", pkg2, sep=":"))
	cat("===============================\n",
	    "  ", pkgName1, " vs ", pkgName2, "\n",
	    "===============================\n")
	if ( missing(objs)) {
		objs1 <- ls(ns1); objs2 <- ls(ns2)
		objs <- objs1[objs1 %in% objs2]
		objs1 <- objs1[! objs1 %in% objs]; objs2 <- objs2[! objs2 %in% objs]
		cat("Available Objects:\n",
		    "\tThere are", length(objs), " objects appear in both packages: ", objs, ".\n",
		    "\t", length(objs1), " objects in ", pkgName1, " only: ", objs1, ".\n",
		    "\t", length(objs2), " objects in ", pkgName2, " only: ", objs2, ".\n")
	}
	objValid <- unlist(lapply(objs, function(x) {
			xo <- get(x, envir=ns1)
			is(xo, "AnnotDbTable")||is(xo, "environment")
         }))
	objs <- objs[objValid]
	lapply(objs, function(annObj) {
		cat("\n====== ", annObj, " ======\n")
		ao1 <- get(annObj, envir=ns1); ao2 <- get(annObj, envir=ns2)
		k1 <- ls(ao1); k2 <- ls(ao2)
		if (nona2) {
			k2na <- unlist(lapply(k2, function(x) 
					identical(get(x, envir=ao2), NA)
				))
			k2 <- k2[!k2na]
		}
		k <- k1[k1 %in% k2]
		k1 <- k1[! k1 %in% k]; k2 <-k2[! k2 %in% k]
		if (length(k1)>printSize) 
			ks1 <- c(k1[1:printSize], "...")
		else
			ks1 <- k1
		if (length(k2)>printSize)
			ks2 <- c(k2[1:printSize], "...")
		else
			ks2 <- k2
		cat("\tThe two ", annObj, " have ", length(k), " variables in common.\n",
		    "\t", length(k1), " objects in ", pkgName1, " only: ", ks1, ".\n",
		    "\t", length(k2), " objects in ", pkgName2, " only: ", ks2, ".\n")
		if(length(k)>0) {
		    if(length(k)>maxCompare) k <- sample(k, maxCompare)
		    identicalK <- unlist(lapply(k, function(x) {
			x1 <- get(x, envir=ao1)
			x2 <- get(x, envir=ao2)
			## x1/2 can be a list or an instances of class "GOTerms",
			## which can't be sort.
			if(typeof(x1)!= "list") x1 <- sort(x1)
			if(typeof(x2)!= "list") x2 <- sort(x2)
			identical(x1, x2) 
		    }))
		    diffK <- k[!identicalK]
		    if (length(diffK)>printSize)
			diffKS <- c(diffK[1:printSize], "...")
		    else
			diffKS <- diffK 
		    cat("\tAmong a random sample of ", length(k), "variables that are in common, ",
			length(k[identicalK]), 
		    " have identical values in the two packages, \n\tand ", 
		    length(diffK), " have different values: ", diffKS, ".\n")
		}
	})
	""
}
