\name{ACCNUM}

\alias{ACCNUM}
\alias{ALIAS}
\alias{ARACYC}
\alias{ARACYCENZYME}
\alias{CHR}
\alias{CHRLOC}
\alias{CHRLOCEND}
\alias{COMMON}
\alias{DESCRIPTION}
\alias{ENSEMBL}
\alias{ENSEMBLPROT}
\alias{ENSEMBLTRANS}
\alias{ENTREZID}
\alias{ENZYME}
\alias{EVIDENCE}
\alias{EVIDENCEALL}
\alias{GENENAME}
\alias{GO}
\alias{GOALL}
\alias{INTERPRO}
\alias{IPI}
\alias{MAP}
\alias{OMIM}
% \alias{ONTOLOGY}
\alias{ONTOLOGYALL}
\alias{ORF}
\alias{PATH}
\alias{PFAM}
\alias{PMID}
\alias{PROBEID}
\alias{PROSITE}
\alias{REFSEQ}
\alias{SGD}
\alias{SMART}
\alias{SYMBOL}
\alias{TAIR}
\alias{UNIGENE}
\alias{UNIPROT}

\title{Descriptions of available values for \code{columns} and \code{keytypes}.}


\description{
  This manual page enumerates the kinds of data represented by the
  values returned when the user calls \code{columns} or \code{keytypes}
}


\details{  
  All the possible values for \code{columns} and \code{keytypes} are listed
  below.  Users will have to actually use these methods to learn which
  of the following possible values actually apply in their case.
  
  \describe{
    \item{ACCNUM:}{GenBank accession numbers}
    \item{ALIAS:}{Commonly used gene symbols}
    \item{ARACYC:}{KEGG Identifiers for arabidopsis as indicated by aracyc}
    \item{ARACYCENZYME:}{Aracyc enzyme names as indicated by aracyc}
    \item{CHR:}{Chromosome (deprecated for Bioc > 3.1) For this
    information you should look at a TxDb or OrganismDb object and
    search for an appropriate field like TXCHROM, EXONCHROM or
    CDSCHROM.  This information can also be retrieved from these objects
    using an appropriate range based accesor like transcripts,
    transcriptsBy etc.}
    \item{CHRLOC:}{Chromosome and starting base of associated gene
    (deprecated for Bioc > 3.1) For this information you should look at
    a TxDb or OrganismDb object and search for an appropriate field like
    TXSTART etc. or even better use the associated range based accessors
    like transcripts or transcriptsBy to get back GRanges objects.}
    \item{CHRLOCEND:}{Chromosome and ending base of associated gene
    (deprecated for Bioc > 3.1) For this information you should look at
    a TxDb or OrganismDb object and search for an appropriate field like
    TXEND etc. or even better use the associated range based accessors
    like transcripts or transcriptsBy to get back GRanges objects.}
    \item{COMMON:}{Common name}
    \item{DESCRIPTION:}{The description of the associated gene}
    \item{ENSEMBL:}{The ensembl ID as indicated by ensembl}
    \item{ENSEMBLPROT:}{The ensembl protein ID as indicated by ensembl}
    \item{ENSEMBLTRANS:}{The ensembl transcript ID as indicated by ensembl}
    \item{ENTREZID:}{Entrez gene Identifiers}
    \item{ENZYME:}{Enzyme Commission numbers}
    \item{EVIDENCE:}{Evidence codes for GO associations with a gene of interest}
    \item{EVIDENCEALL:}{Evidence codes for GO (includes less specific terms)}
    \item{GENENAME:}{The full gene name}
    \item{GO:}{GO Identifiers associated with a gene of interest}
    \item{GOALL:}{GO Identifiers (includes less specific terms)}
    \item{INTERPRO:}{InterPro identifiers}
    \item{IPI:}{IPI accession numbers}
    \item{MAP:}{cytoband locations}
    \item{OMIM:}{Online Mendelian Inheritance in Man identifiers}
    \item{ONTOLOGY:}{For GO Identifiers, which Gene Ontology (BP, CC, or MF)}
    \item{ONTOLOGYALL:}{Which Gene Ontology (BP, CC, or MF), (includes less specific terms)}
    \item{ORF:}{Yeast ORF Identifiers}
    \item{PATH:}{KEGG Pathway Identifiers}
    \item{PFAM:}{PFAM Identifiers}
    \item{PMID:}{Pubmed Identifiers}
    \item{PROBEID:}{Probe or manufacturer Identifiers for a chip package}
    \item{PROSITE:}{Prosite Identifiers}
    \item{REFSEQ:}{Refseq Identifiers}
    \item{SGD:}{Saccharomyces Genome Database Identifiers}
    \item{SMART:}{Smart Identifiers}
    \item{SYMBOL:}{The official gene symbol}
    \item{TAIR:}{TAIR Identifiers}
    \item{UNIGENE:}{Unigene Identifiers}
    \item{UNIPROT:}{Uniprot Identifiers}
  }

  To get the latest  information about the date stamps and source URLS
  for the data used to make an annotation package, please use the
  metadata method as shown in the example below.

  Unless otherwise indicated above, the majority of the data for any one
  package is taken from the source indicated by either it's name (if
  it's an org package) OR from the name of it's associated org package.
  So for example, org.Hs.eg.db is using "eg" in the name to indicate
  that most of the data in that package comes from NCBI entrez gene
  based data.  And org.At.tair.db uses data that primarily comes from
  tair.  For chip packages, the relevant information is the organism
  package that they depend on.  So for example, hgu95av2.db depends on
  org.Hs.eg.db, and is thus primarily based on NCBI entrez gene ID
  information.
  
}


\author{Marc Carlson}


\examples{
  library(hgu95av2.db)
  ## List the possible values for columns
  columns(hgu95av2.db)
  ## List the possible values for keytypes
  keytypes(hgu95av2.db)
  ## get some values back
  keys <- head(keys(hgu95av2.db))
  keys
  select(hgu95av2.db, keys=keys, columns=c("SYMBOL","PFAM"),
  keytype="PROBEID")

  ## More infomation about the dates and original sources for these data:
  metadata(hgu95av2.db)
}


\keyword{utilities}
\keyword{manip}
