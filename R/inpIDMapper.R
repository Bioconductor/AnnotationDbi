##inpMap is a map between an entrez gene and whatever thing inparanoid uses.
##You must ALWAYS have one of these for this function to work.
.getMappingData = function(species){
    switch(species,
           "HOMSA" = {srcSpcAb<-"Hs";
                      srcDBAb<-"eg";
                      require(paste("org.",srcSpcAb,".",srcDBAb,".db",sep=""),character.only=TRUE);
                      inpMap <- "org.Hs.egENSEMBLPROT";
		      centralID <- "EG"},
           "MUSMU"  = {srcSpcAb<-"Mm";
                       srcDBAb<-"eg";
                       require(paste("org.",srcSpcAb,".",srcDBAb,".db",sep=""),character.only=TRUE);
                       inpMap = "org.Mm.egENSEMBLPROT";  
		       centralID <- "EG"},
           "DROME"  = {srcSpcAb<-"Dm";
                       srcDBAb<-"eg";
                       require(paste("org.",srcSpcAb,".",srcDBAb,".db",sep=""),character.only=TRUE);
                       inpMap = "org.Dm.egFLYBASEPROT";
		       centralID <- "EG"},
           "RATNO"  = {srcSpcAb<-"Rn";
                       srcDBAb<-"eg";
                       require(paste("org.",srcSpcAb,".",srcDBAb,".db",sep=""),character.only=TRUE);
                       inpMap = "org.Rn.egENSEMBLPROT";
		       centralID <- "EG"},
           "CAEEL"  = {srcSpcAb<-"Ce";
                       srcDBAb<-"eg";
                       require(paste("org.",srcSpcAb,".",srcDBAb,".db",sep=""),character.only=TRUE);
                       inpMap = "org.Ce.egWORMBASE";
		       centralID <- "EG"},
           "ARATH"  = {srcSpcAb<-"At"; ##Won't work till we ltrim off the decimals off the TAIR IDs in the inp pkgs
                       srcDBAb<-"eg";
                       require(paste("org.",srcSpcAb,".",srcDBAb,".db",sep=""),character.only=TRUE);
                       ##inpMap = NA;  ##Should not be needed since this one should be like yeast
		       centralID <- "TAIR"},
           "DANRE"  = {srcSpcAb<-"Dr";
                       srcDBAb<-"eg";
                       require(paste("org.",srcSpcAb,".",srcDBAb,".db",sep=""),character.only=TRUE);
                       inpMap = "org.Dr.egZFIN";
		       centralID <- "EG"},
           "SACCE"  = {srcSpcAb<-"Sc";
                       srcDBAb<-"sgd"; 
                       require(paste("org.",srcSpcAb,".",srcDBAb,".db",sep=""),character.only=TRUE);
                       inpMap = NA; ##Not needed for yeast (uses the same ID in inparanoid as the central ID in the org package)
		       centralID <- "ORF"}  
           )
    return(c(srcSpcAb, srcDBAb, inpMap, centralID))    
}


##helper function to remove NAs and drop any duplicates that may emerge from mapping.
##a fair amount of this is needed since if you have any NAs that you feed into mget, you can end up matching EVERYTHING.
.cleanup = function(ids){
    ids = ids[!is.na(ids)]
    dupIndex = duplicated(ids)
    ids = ids[!dupIndex]
}

##helper function to rename things
.reLabel = function(srcIDs, destIDs, renameString){
    if(length(destIDs)==length(srcIDs)){
        names(destIDs) = names(srcIDs)
    }else{
        stop(paste("Cannot rename ", renameString, sep=""))
    }
    return(destIDs)
}

##helper function to decide what to do with multiple values.
.handleMultipleMatches = function(ids, keepMultiples){
    if(keepMultiples==FALSE){
        ids = lapply(ids, function(x){if(length(x)>1) x=NA else x=x})
    }else{
        ids = lapply(ids, function(x){if(length(x)>1) x=as.character(x)[1] #return the 1st thing
                                      else if(length(x)<1) x=NA
                                      else if(length(x)==1) x=as.character(x) }) 
    }
    return(ids)
}

##TODO: rename the internal variables.  They should be named better.

inpIDMapper = function(ids, srcSpecies, destSpecies, srcIDType="UNIPROT", destIDType="EG", keepMultGeneMatches=FALSE, keepMultProtMatches=FALSE, keepMultDestIDMatches=TRUE){

    ##Argument checking
    if(srcSpecies==destSpecies){stop("The srcSpecies and destSpecies should be different. 'No matter where you go, there you are.' - Bucakaroo Banzai")}    
    if(srcIDType=="SYMBOL"){stop("I refuse to attempt this on the grounds that SYMBOLS are nearly worthless as IDs, and it would be irresponsible to proceed.  If you must use symbols, you will have to process them one step at a time and double check your work, because they cannot be relied upon to be uniquely mapped onto a single gene.")}
    
    ##I need to set up the right thing based on what the package is
    setupVals = .getMappingData(srcSpecies)
    srcSpcAbrv = setupVals[1]
    srcDBAbrv = setupVals[2]
    if(!is.na(setupVals[3])){protMap = get(setupVals[3])}
    centralID1 = setupVals[4]
    #require the hom package and map
    require(paste("hom.",srcSpcAbrv,".inp.db",sep=""),character.only=TRUE)
    homMap = get(paste("hom.",srcSpcAbrv,".inp",destSpecies,sep=""))
    if(srcIDType!=centralID1){toSrcEGMap = get(paste("org.",srcSpcAbrv,".",srcDBAbrv,srcIDType,sep=""))}
    ##more info to map back out to an entrez gene ID at the end.
    mapBackVals = .getMappingData(destSpecies)
    destSpcAbrv = mapBackVals[1]
    destDBAbrv = mapBackVals[2]
    centralID2 = mapBackVals[4]
    if(!is.na(mapBackVals[3])){geneMap = get(mapBackVals[3])}

    ###MGET#1 Get mapped into the initial EGs
    if(srcIDType==centralID1){
        genes = ids
        names(genes) = ids
    }else{
        ids = .cleanup(ids)
        genes = mget(as.character(ids), revmap(toSrcEGMap), ifnotfound=NA)
        genes = .cleanup(genes)
    }

    ##Before the 2nd mgt, we need to have ONLY ONE answer for each element.
    genes = .handleMultipleMatches(genes, keepMultGeneMatches)
    genes = .cleanup(genes)

    ###MGET#2 map to the ID type used by inparanoid.
    if(exists("protMap")){
	inpIDs = mget(as.character(genes), protMap, ifnotfound=NA)
    	##Carry the names for the uniprot IDs over...
    	inpIDs = .reLabel(genes, inpIDs, "inpIDs")
    	inpIDs = .cleanup(inpIDs)
    }else{
        inpIDs = genes
    }
    
    ###MGET#3 map across species with inparanoid
    ##I have to check each one of these possible mappings for a match...    
    destList = lapply(inpIDs, function(x){mget(as.character(x), homMap, ifnotfound=NA)})
    ##and then drop the NA elements of each of those sub-lists 
    destIDs = lapply(destList, function(x){x = x[!is.na(x)]})    

    ##after the "tricks" to map the inparanoid mappings, the IDs list is double "nested" so we have to clean that up here...
    destIDs = lapply(destIDs, unlist)

    
    ##quick?? hack to make this work:
    dindex = vector()
    for(i in seq_len(length(destIDs))){
        dindex = c(dindex, !is.null(destIDs[[i]]) )
    }
    dnames = names(destList)[dindex]
    destIDs = destIDs[dindex]
    ##I have to (MUST!) drop the extras at this stage for this to work...
    destIDs = .handleMultipleMatches(destIDs, keepMultProtMatches)
    destIDs = unlist(destIDs)
    if(length(destIDs)==length(dnames)){names(destIDs)=dnames}else{stop("Names are not congruent")}

    
    
    ##Before the 4th mgt, we need to be down to ONLY ONE answer per element again.
    ##The hacking above probably makes this step unecessary at this point 
    finIDs = .handleMultipleMatches(destIDs, keepMultProtMatches)    
    uniqIDs = .cleanup(finIDs)
        
    ###MGET#4 dest species EG don't do this if we are already "there"
    if(exists("geneMap")){
        EGIDs = mget(as.character(uniqIDs), revmap(geneMap), ifnotfound=NA) 
        EGIDs = .reLabel(uniqIDs, EGIDs, "EGIDs")
        EGIDs = .cleanup(EGIDs) #do before the rename, because there may be duplicates)
    }else{
        EGIDs = uniqIDs
    }
    
    ###MGET#5 final mapping (may not be needed if they wanted EGs)
    ##Finally give the user the ID type that they asked for (ORF, EntrezID = (which is the default), UNIPROT etc.).
    if(destIDType != centralID2 && toupper(destDBAbrv)!=destIDType){
        ##We will need a special case here if we decide to unify the eg IDs into the SGD org DB...
        resultMap = get(paste("org.",destSpcAbrv,".",destDBAbrv,destIDType,sep=""))    
        resultIDs = mget(as.character(EGIDs), resultMap, ifnotfound=NA)
    }else{resultIDs = EGIDs}
    
    resultIDs = .reLabel(EGIDs, resultIDs, "resultIDs")
    if(keepMultDestIDMatches==FALSE){resultIDs = .handleMultipleMatches(resultIDs, keepMultiples=TRUE)}
    resultIDs = .cleanup(resultIDs)
    
    resultIDs
}



#######################################################################################################
## Here we need to define a function to remap things for us
## (assemble package names, load them etc.
## This might really be useful to someone who does not want to use the mappings.
######################################################################################################

intraIDMapper = function(ids,
                         species,
                         srcIDType="UNIPROT",
                         destIDType="EG",
                         keepMultGeneMatches=FALSE){
    ##Argument checking
    if(srcIDType==destIDType){stop("The SrcIDType and destIDType should be different. 'No matter where you go, there you are.' - Bucakaroo Banzai")}
    if(srcIDType=="SYMBOL"){stop("I refuse to attempt this on the grounds that SYMBOLS are nearly worthless as IDs, and it would be irresponsible to proceed.  If you must use symbols, you will have to use them one step at a time and double check your work, because they cannot be relied upon to be uniquely mapped onto a single gene.")}

    ##Need to set things up and invoke the library etc.
    setupVals = .getMappingData(species)
    srcSpcAbrv = setupVals[1]
    srcDBAbrv = setupVals[2]
    centralID = setupVals[4]    
    ##cleanup the source IDs in case there are missing values.
    ids = .cleanup(ids)
    
    if(srcIDType==centralID){##srcIDType is a central ID (EG, ORF etc.)
        map = get(paste("org.",srcSpcAbrv,".",srcDBAbrv,destIDType,sep="")) ##eg. org.Hs.eg.UNIPROT and we have an "EG"
        ##Get the new names
        resultIDs <- unlist2(mget(as.character(ids), map, ifnotfound=NA))
    }
    else if(destIDType==centralID){##destIDType is a central ID (EG, ORF etc.)
        map = get(paste("org.",srcSpcAbrv,".",srcDBAbrv,srcIDType,sep=""))  ##eg.  org.Hs.eg.UNIPROT and we have an "UNIPROT"
        map = revmap(map) ##So In this case it has to be revmapped
        ##Get the new names
        resultIDs <- unlist2(mget(as.character(ids), map, ifnotfound=NA))
    }
    else if(srcIDType!=centralID && destIDType!=centralID){
        ##Get the central IDs
        map1 = get(paste("org.",srcSpcAbrv,".",srcDBAbrv,srcIDType,sep=""))  ##This one will be the map that goes TO the central ID
        map1 = revmap(map1) ##Therefore this one has to be reversed
        resultIDs1 = unlist2(mget(as.character(ids), map1, ifnotfound=NA))
        ##Before the 2nd mget, we need to have ONLY ONE answer for each element.
        resultIDs1 = .handleMultipleMatches(resultIDs1, keepMultGeneMatches)
        resultIDs1 = .cleanup(resultIDs1) ##in case there are NAs
        ##Then get the dest IDs.
        map2 = get(paste("org.",srcSpcAbrv,".",srcDBAbrv,destIDType,sep=""))
        resultIDs = unlist2(mget(as.character(resultIDs1), map2, ifnotfound=NA))
        resultIDs = .handleMultipleMatches(resultIDs, keepMultGeneMatches)        
        resultIDs = .reLabel(resultIDs1, resultIDs, "resultIDs") ##two mappings so we want to keep initial names
    }
    resultIDs = .cleanup(resultIDs)
    resultIDs
}


#######################################################################################################
## Finally lets just make a wrapper function to tie it all together.
## The purpose of this is really just for programmatic access.
## That is, it's just to allow people to set things up generically
## when then need a conversion to be done elsewhere
#######################################################################################################

idConverter = function(ids,
                       srcSpecies,
                       destSpecies, 
                       srcIDType = "UNIPROT",
                       destIDType = "EG",
                       keepMultGeneMatches=FALSE,
                       keepMultProtMatches=FALSE,
                       keepMultDestIDMatches=TRUE){
    if(srcSpecies != destSpecies){
        inpIDMapper(ids=ids,
                    srcSpecies=srcSpecies,
                    destSpecies=destSpecies,
                    srcIDType=srcIDType,
                    destIDType=destIDType,
                    keepMultGeneMatches=keepMultGeneMatches,
                    keepMultProtMatches=keepMultProtMatches,
                    keepMultDestIDMatches=keepMultDestIDMatches)
    }else{##else it's 'intra'
        intraIDMapper(ids=ids,
                      species = srcSpecies,
                      srcIDType=srcIDType,
                      destIDType=destIDType,
                      keepMultGeneMatches=keepMultGeneMatches)         
    }
}
