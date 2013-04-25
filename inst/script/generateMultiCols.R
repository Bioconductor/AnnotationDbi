## This script is used to pre-compute information about which cols()
## values have a many to one relationship with the _id cols in each
## table, and how bad that relationship is.  The values are scanned
## across all the org packages and then collected into a vector so
## that they can be used by AnntoationDbi to make sensible
## recommendations about what cols() a user should ask for at once, or
## if multiple cols should be requested together.

## The final output is a named integer vector.  The names of this
## vector are the values for cols().  And the values of this vector is
## a number that represents the max value ever seen for any given
## field, and the names are the names used by cols().  So this number
## is meant to be an indicator for how bad something can be when
## cols() is used.

## If cols() indicates potential danger, then the plan is to message()
## the user (or use warning("",immediately=TRUE)) and tell them that
## what they are doing is going to maybe take a long time.  Herve has
## suggested that I could just use count() at the front of the same
## query and just pre-call that to get a message that tells them how
## bad it is (so that they can escape if they wish).  Paul asked me to
## add a parameter (in that case) so that users who know what they are
## doing can switch that safety off.  Martin mentioned that being able
## to hit Ctrl-C is probably sufficient.  I agree with Martin in this
## case.



## ## sample query:

## SELECT  genes.gene_id,pfam.pfam_id,pfam.ipi_id,prosite.prosite_id,accessions.accession,alias.alias_symbol,chromosomes.chromosome,chromosome_locations.start_location,chromosome_locations.seqname,chromosome_locations.end_location,ec.ec_number,cytogenetic_locations.cytogenetic_location,kegg.path_id,pubmed.pubmed_id,refseq.accession,gene_info.symbol,unigene.unigene_id,ensembl.ensembl_id,ensembl_prot.prot_id,ensembl_trans.trans_id,gene_info.gene_name,uniprot.uniprot_id,go.go_id,go.evidence,go.ontology,go_all.go_id,go_all.evidence,go_all.ontology,omim.omim_id,ucsc.ucsc_id  FROM genes LEFT JOIN  pfam USING ( _id ) LEFT JOIN  prosite USING ( _id ) LEFT JOIN  accessions USING ( _id ) LEFT JOIN  alias USING ( _id ) LEFT JOIN  chromosomes USING ( _id ) LEFT JOIN  chromosome_locations USING ( _id ) LEFT JOIN  ec USING ( _id ) LEFT JOIN  cytogenetic_locations USING ( _id ) LEFT JOIN  kegg USING ( _id ) LEFT JOIN  pubmed USING ( _id ) LEFT JOIN  refseq USING ( _id ) LEFT JOIN  gene_info USING ( _id ) LEFT JOIN  unigene USING ( _id ) LEFT JOIN  ensembl USING ( _id ) LEFT JOIN  ensembl_prot USING ( _id ) LEFT JOIN  ensembl_trans USING ( _id ) LEFT JOIN  uniprot USING ( _id ) LEFT JOIN  go USING ( _id ) LEFT JOIN  go_all USING ( _id ) LEFT JOIN  omim USING ( _id ) LEFT JOIN  ucsc USING ( _id ) WHERE  genes.gene_id in ( '100008586' );


## ## shorter query to just get counts up:

## SELECT  count(*)  FROM genes LEFT JOIN  pfam USING ( _id ) LEFT JOIN  prosite USING ( _id ) LEFT JOIN  accessions USING ( _id ) LEFT JOIN  alias USING ( _id ) LEFT JOIN  chromosomes USING ( _id ) LEFT JOIN  chromosome_locations USING ( _id ) LEFT JOIN  ec USING ( _id ) LEFT JOIN  cytogenetic_locations USING ( _id ) LEFT JOIN  kegg USING ( _id ) LEFT JOIN  pubmed USING ( _id ) LEFT JOIN  refseq USING ( _id ) LEFT JOIN  gene_info USING ( _id ) LEFT JOIN  unigene USING ( _id ) LEFT JOIN  ensembl USING ( _id ) LEFT JOIN  ensembl_prot USING ( _id ) LEFT JOIN  ensembl_trans USING ( _id ) LEFT JOIN  uniprot USING ( _id ) LEFT JOIN  go USING ( _id ) LEFT JOIN  go_all USING ( _id ) LEFT JOIN  omim USING ( _id ) LEFT JOIN  ucsc USING ( _id ) WHERE  genes.gene_id in ( '100008586' );


## ## put these into temp files.
## time sqlite3 -bail org.Hs.eg.sqlite < temp_queryLong.sql

## real	5m56.135s
## user	5m10.627s
## sys	0m28.350s

## time sqlite3 -bail org.Hs.eg.sqlite < temp_queryShort.sql

## real	6m6.410s
## user	5m13.880s
## sys	0m33.182s




## ## To test from R:
## library(org.Hs.eg.db)
## con = org.Hs.eg_dbconn()
## system.time(sqliteQuickSQL(con, "SELECT  count(*)  FROM genes LEFT JOIN  pfam USING ( _id ) LEFT JOIN  prosite USING ( _id ) LEFT JOIN  accessions USING ( _id ) LEFT JOIN  alias USING ( _id ) LEFT JOIN  chromosomes USING ( _id ) LEFT JOIN  chromosome_locations USING ( _id ) LEFT JOIN  ec USING ( _id ) LEFT JOIN  cytogenetic_locations USING ( _id ) LEFT JOIN  kegg USING ( _id ) LEFT JOIN  pubmed USING ( _id ) LEFT JOIN  refseq USING ( _id ) LEFT JOIN  gene_info USING ( _id ) LEFT JOIN  unigene USING ( _id ) LEFT JOIN  ensembl USING ( _id ) LEFT JOIN  ensembl_prot USING ( _id ) LEFT JOIN  ensembl_trans USING ( _id ) LEFT JOIN  uniprot USING ( _id ) LEFT JOIN  go USING ( _id ) LEFT JOIN  go_all USING ( _id ) LEFT JOIN  omim USING ( _id ) LEFT JOIN  ucsc USING ( _id ) WHERE  genes.gene_id in ( '100008586' )"))


## ##    user  system elapsed 
## ##  18.373   2.084  32.379 


## system.time(sqliteQuickSQL(con, "SELECT  genes.gene_id,pfam.pfam_id,pfam.ipi_id,prosite.prosite_id,accessions.accession,alias.alias_symbol,chromosomes.chromosome,chromosome_locations.start_location,chromosome_locations.seqname,chromosome_locations.end_location,ec.ec_number,cytogenetic_locations.cytogenetic_location,kegg.path_id,pubmed.pubmed_id,refseq.accession,gene_info.symbol,unigene.unigene_id,ensembl.ensembl_id,ensembl_prot.prot_id,ensembl_trans.trans_id,gene_info.gene_name,uniprot.uniprot_id,go.go_id,go.evidence,go.ontology,go_all.go_id,go_all.evidence,go_all.ontology,omim.omim_id,ucsc.ucsc_id  FROM genes LEFT JOIN  pfam USING ( _id ) LEFT JOIN  prosite USING ( _id ) LEFT JOIN  accessions USING ( _id ) LEFT JOIN  alias USING ( _id ) LEFT JOIN  chromosomes USING ( _id ) LEFT JOIN  chromosome_locations USING ( _id ) LEFT JOIN  ec USING ( _id ) LEFT JOIN  cytogenetic_locations USING ( _id ) LEFT JOIN  kegg USING ( _id ) LEFT JOIN  pubmed USING ( _id ) LEFT JOIN  refseq USING ( _id ) LEFT JOIN  gene_info USING ( _id ) LEFT JOIN  unigene USING ( _id ) LEFT JOIN  ensembl USING ( _id ) LEFT JOIN  ensembl_prot USING ( _id ) LEFT JOIN  ensembl_trans USING ( _id ) LEFT JOIN  uniprot USING ( _id ) LEFT JOIN  go USING ( _id ) LEFT JOIN  go_all USING ( _id ) LEFT JOIN  omim USING ( _id ) LEFT JOIN  ucsc USING ( _id ) WHERE  genes.gene_id in ( '100008586' )"))

## ##    user  system elapsed 
## ##  27.878   2.268  50.224 


## so all this just proved to be an indictment of doing things on old
## gladstone.  We will have to upgrade it I guess...


##############################################################################
##  ACK Combinatorics!
##############################################################################
## As for the actual problem.  I am just going to put together a
## hand-made blacklist of cols that I know have many to one
## relationships based on this script here.  This script will just
## learn which fields do that (not try to rate them).


## Then if the user uses more than a few of them, I will issue a
## warning as described above...  The test can trap this warning maybe
## to make sure it goes out?



## So lets just do it for HUMAN
## library(org.Hs.eg.db)
## x <- org.Hs.eg.db


getManyToOneStatus <- function(x){
    cols <- cols(x)
    testManyToOne <- function(c, x){
        k <- keys(x,"ENTREZID")
        res <- select(x, cols=c, keys=k, keytype="ENTREZID")
        if(length(unique(res[["ENTREZID"]])) <
           length(res[["ENTREZID"]])){
            return(TRUE)
        }else{
            return(FALSE)
        }
    }
    res <- unlist(lapply(cols, testManyToOne, x))
    names(res) <- cols
    res
}

## Tests
## This should be TRUE
## testManyToOne("PFAM", x)
## This should be FALSE:
## testManyToOne("SYMBOL", x)

## res <- getManyToOneStatus(x)


## so then do the above for all the org packages.
require("org.Hs.eg.db")
require("org.Mm.eg.db")
require("org.At.tair.db")
require("org.Bt.eg.db")
require("org.Cf.eg.db")
require("org.Gg.eg.db")
require("org.Dm.eg.db")
require("org.Rn.eg.db")
require("org.Ce.eg.db")
require("org.Xl.eg.db")
require("org.Sc.sgd.db")
require("org.Ss.eg.db")
require("org.Dr.eg.db")
require("org.EcK12.eg.db")
require("org.EcSakai.eg.db")


pkgs <- c(org.Hs.eg.db,
          org.Mm.eg.db,
          ##           org.At.tair.db, ## exclude b/c tair is (atypical)
          org.Bt.eg.db,
          org.Cf.eg.db,
          org.Gg.eg.db,
          org.Dm.eg.db,
          org.Rn.eg.db,
          org.Ce.eg.db,
          org.Xl.eg.db,
          ##        org.Sc.sgd.db,   ## There is a problem with this one.
          org.Ss.eg.db,
          org.Dr.eg.db,
          org.EcK12.eg.db,
          org.EcSakai.eg.db)

res <- lapply(pkgs, getManyToOneStatus)
many2Ones = res
save(many2Ones, file="many2Ones.Rda")


## then combine all these vectors into one vector.  We want to have
## the vectors added together and then call unique, but we ALSO want
## to give preference to things being TRUE.  So if you are true for
## one, you are always true from then on...

## So we just do this to sort so that the trues are in front (and will
## get grabbed by the final filtering)
blackList <- sort(unlist(res), decreasing=TRUE)
## And then we call unique (which grabs the ones it sees 1st)
blackList <- blackList[unique(names(blackList))]
## Then we just keep the ones that are marked as TRUE.
blackList <- names(blackList[blackList])
## and save it...
save(blackList, file="manyToOneBlackList.Rda")
