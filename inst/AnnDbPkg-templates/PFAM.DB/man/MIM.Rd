\name{pfamAC2MIM}
\alias{pfamAC2MIM}
\title{Mappings from Accession number to MIM ID}
\description{
  Mapping function from Accession number (AC) to the associated MIM ID (MIM)
}
\usage{
pfamAC2MIM(ac=NULL)
}
\arguments{
  \item{ac}{A character vector of Accession number or NULL (default)}
}
\value{
  A list of MIM ID.
}
\details{
  If \code{ac} is a vector of Accession number,  pfamAC2MIM will
  return a list of associated MIM ID for the given \code{ac}.  If
  \code{ac} is NULL,  pfamAC2MIM(ac=NULL) (or pfamAC2MIM())
  will return a list of associated MIM ID for all the Accession number in
  the database.
  Extra information:  None
  More Details: MIM (a.k.a. OMIM, \url{http://www.ncbi.nlm.nih.gov/omim/}) is a catalog of human genes and genetic disorders authored and edited by Dr. Victor A. McKusick and his colleagues at Johns Hopkins and elsewhere.  Reference:  <MIM> MIM: McKusick, V.A.: Mendelian Inheritance in Man. A Catalog of Human Genes and Genetic Disorders. Baltimore: Johns Hopkins University Press, 1998 (12th edition). <OMIM> Online Mendelian Inheritance in Man, OMIM (TM). McKusick-Nathans Institute for Genetic Medicine, Johns Hopkins University (Baltimore, MD) and National Center for Biotechnology Information, National Library of Medicine (Bethesda, MD), 2000
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    AC2MIM <- pfamAC2MIM()
    head(AC2MIM)
    pfamAC2MIM(ac=sample(names(AC2MIM), 3))
}
}
\keyword{datasets}
