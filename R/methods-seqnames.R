###############################################################################
## Code to access the special chromosome name conventions DB.
## The methods in this file are deprecated and now belong to GenomeInfoDb


supportedSeqnameMappings <- 
    function()
{
    txt <- "'supportedSeqnameMappings' is deprecated.
           Use 'genomeStyles()' in 'GenomeInfoDb' instead." 
    .Deprecated("genomeStyles", msg=paste(strwrap(txt), collapse="\n"))
    genomeStyles()
}

# same information displayed
# in addition - auto, linear and sex chr mapping shown
# displayed as one list for each organism (old: one list per style per organim)
listAllSupportedStylesBySpecies <- function(species){
    txt <- "'supportedSeqnameMappings' is deprecated.
           Use 'genomeStyles(species)' in 'GenomeInfoDb' instead." 
    .Deprecated("genomeStyles", msg=paste(strwrap(txt), collapse="\n"))
    genomeStyles(species)
}

# same information displayed
# in addition - auto, linear and sex chr mapping shown
# displayed as one list for each organism (old: one list per style per organim)
listAllSupportedSeqnameStyles <- function(){
    txt <- "'supportedSeqnameMappings' is deprecated.
           Use 'genomeStyles()' in 'GenomeInfoDb' instead." 
    .Deprecated("genomeStyles", msg=paste(strwrap(txt), collapse="\n"))
    genomeStyles()
}

## A discovery method for users to learn the supported seqname styles
supportedSeqnameStyles <- 
    function()
{
    txt <- "'supportedSeqnameStyles' is deprecated.
       Use 'genomeStyles()' in 'GenomeInfoDb' instead." 
    .Deprecated("genomeStyles", msg=paste(strwrap(txt), collapse="\n"))
    genomeStyles()
}



## This helper takes no arguments and just returns all the possible seqnames in the whole DB (in no particular order, just the unique set).
supportedSeqnames <- function(){
    txt <- "'isSupportedSeqnamesStyle' is deprecated."
    .Deprecated("genomeStyles", msg=paste(strwrap(txt), collapse="\n"))
    genomeStyles()
}

## check whether or not a style is really a supported seqnameStyle
isSupportedSeqnamesStyle <- 
    function(style, species)
{
    txt <- "'isSupportedSeqnamesStyle' is deprecated."
    .Deprecated( msg=paste(strwrap(txt), collapse="\n"))
  
}

testSeqnames <- 
    function(styles=c("ensembl", "UCSC"), seqnames, species="Homo sapiens")
{
    txt <- "'testSeqnames' is deprecated."
    .Deprecated( msg=paste(strwrap(txt), collapse="\n"))
}


findSequenceRenamingMaps <- 
    function(seqnames, style, best.only=TRUE, drop=TRUE)
{
    txt <- "'findSequenceRenamingMaps' is deprecated.
           Use 'mapSeqlevels' in 'GenomeInfoDb' instead." 
    .Deprecated("mapSeqlevels", msg=paste(strwrap(txt), collapse="\n"))
    mapSeqlevels(seqnames, style, best.only=TRUE, drop=TRUE)
}

extractSeqnameSet <- 
    function(style="UCSC", species="Homo sapiens")
{
    txt <- "'extractSeqnameSet' is deprecated.
           Use 'extractSeqlevels()' in 'GenomeInfoDb' instead." 
    .Deprecated("extractSeqlevels", msg=paste(strwrap(txt), collapse="\n"))
    extractSeqlevels(style="UCSC", species="Homo sapiens")
}




## Testing:
## seqnames = as.character(c(1:5,"Pt","Mt"))
## styles = c("NCBI","ensembl")
## species = "Arabidopsis thaliana"
## testSeqnames(styles,seqnames, species)

## seqnames = as.character(c(paste0("chr", c(1:22,"M","Y","X")),"chr6_apd_hap1"))
## styles = c("UCSC","NCBI","ensembl")
## species = "Homo sapiens"
## testSeqnames(styles,seqnames, species)

## seqnames2 = (paste0("chr",1:22))

## testSeqnames(c("ensembl","NCBI"),seqnames=c(1:22,"X","Y","MT"),"Rattus norvegicus")


















