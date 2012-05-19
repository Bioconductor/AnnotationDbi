\name{supportedSeqnameStyles}

\alias{supportedSeqnameStyles}
\alias{extractSeqnameSet}
\alias{isSupportedSeqnamesStyle}
\alias{listAllSupportedSeqnameStyles}
\alias{listAllSupportedStylesBySpecies}
\alias{supportedSeqnames}
\alias{testSeqnames}

\title{List the supported seqname styles for all supported organisms}

\description{
  Lists out the available seqname styles for all organisms supported by
  the seqnames.db database.  
}

\usage{
  supportedSeqnameStyles()
  isSupportedSeqnamesStyle(style, species)
  extractSeqnameSet(style, species)
  listAllSupportedSeqnameStyles()
  listAllSupportedStylesBySpecies(species)
  supportedSeqnames()
  testSeqnames(styles, seqnames, species)
}


\details{
    \code{supportedSeqnameStyles}:
    Different organizations have different naming conventions for how they
    name the biologically defined sequence elements (usually chromosomes)
    for each organism they support.  The seqnames.db package contains a
    database that defines these different conventions.  The
    \code{supportedSeqnameStyles} function allows users to discover
    which conventions are currently encoded by the seqnames DB.
    
    \code{isSupportedSeqnamesStyle}:
    Returns TRUE or FALSE depending on whether or not a suspected style
    and species are legitimately defined in seqnames.db.

    \code{extractSeqnameSet}:
    Extracts the seqnames for a single style and species.

    \code{listAllSupportedSeqnameStyles}:
    Returns a list of character vectors where each character vector
    contains the seqnames for a particular style.  This effectively dumps
    the whole seqnames.db DB into a single list object.

    \code{listAllSupportedStylesBySpecies}:
    List out only the supported styles for a particular species.

    \code{supportedSeqnames}:
    This just lists all possible seqnames for the database as a single
    unique character vector.

    \code{testSeqnames}:
    Takes a character vector of styles, a set of seqnames and a species
    and determines for each style whether or not the seqnames given are
    a match.  Returns a TRUE/FALSE vector of the same length as styles.

}

\arguments{
  \item{species}{The genus and species of the organism in question
  separated by a single space. Don't forget to capitalize the genus.}
  \item{style}{a character vector with a single element to specify the style.}
  \item{styles}{same as style, but expecting a character vector which
  could have multiple elements.}
  \item{seqnames}{the seqnames.  These are given as a character vector
  and are the labels attached to the chromosomes in a genome.}
}

\value{
  A character list containing character vectors with the names of all
  seqname styles for each supported species.  The names of the list
  are the species names for each supported organism.
}

\author{Marc Carlson}

\examples{
  library("TxDb.Hsapiens.UCSC.hg19.knownGene")
  txdb = TxDb.Hsapiens.UCSC.hg19.knownGene
  species = species(txdb)
  style = "NCBI"

  supportedSeqnameStyles()

  isSupportedSeqnamesStyle(style, species)

  extractSeqnameSet(style=style,species=species)

  listAllSupportedSeqnameStyles()

  supportedSeqnames()


  styles = c("NCBI","ensembl")
  seqnames = as.character(c(1:5,"Pt","Mt"))

  testSeqnames(styles, seqnames, species)
}

\keyword{manip}
