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


compareAnnotation <- function(pkg1, pkg2, objs, nona1=FALSE, nona2=FALSE, maxCompare=50, printSize=5) {
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
		if (nona1) 
			k1 <- NoNaLs(ao1)
		else
			k1 <- ls(ao1)
		if (nona2)
			k2 <- NoNaLs(ao2)
		else
			k2 <- ls(ao2)	
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

comparePerf <- function(pkg1, pkg2, objs) {
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
	}
	objValid <- unlist(lapply(objs, function(x) {
			xo <- get(x, envir=ns1)
			is(xo, "AnnotDbTable")||is(xo, "environment")
         }))
	objs <- objs[objValid]
	resColNames <- c("Query#", pkgName1, pkgName2)
	lapply(objs, function(annObj) {
		cat("\n====== ", annObj, " ======\n")
		ao1 <- get(annObj, envir=ns1); ao2 <- get(annObj, envir=ns2)
		k1 <- ls(ao1); k2 <- ls(ao2)
		k <- k1[k1 %in% k2]
		kLength <- length(k)
		cat("\tThe two ", annObj, " have ", kLength, " variables in common.\n")
		tt <- numeric()
		while(kLength>0) {
		    tt<-c(tt, kLength)
		    tt <- c(tt, system.time(mget(k, ao1))[1])
		    tt <- c(tt, system.time(mget(k, ao2))[1])
		    kLength <- floor(kLength/10)
		    if (kLength>0)
		    	k <- sample(k, kLength)
		}
		tt <- matrix(data=tt, ncol=3, byrow=T)
		colnames(tt) <- resColNames
		print(tt) 
	})
	""
}
