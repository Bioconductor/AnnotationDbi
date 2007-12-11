\name{pfamMIM2AC}
\alias{pfamMIM2AC}
\title{Mappings from MIM ID to Accession number}
\description{
  Mapping function from MIM ID (MIM) to the associated Accession number (AC)
}
\usage{
pfamMIM2AC(mim=NULL)
}
\arguments{
  \item{mim}{A character vector of MIM ID or NULL (default)}
}
\value{
  A list of Accession number.
}
\details{
  If \code{mim} is a vector of MIM ID,  pfamMIM2AC will
  return a list of associated Accession number for the given \code{mim}.  If
  \code{mim} is NULL,  pfamMIM2AC(mim=NULL) (or pfamMIM2AC())
  will return a list of associated Accession number for all the MIM ID in
  the database.
  Extra information:  None
  More Details: MIM (a.k.a. OMIM, \url{http://www.ncbi.nlm.nih.gov/omim/}) is a catalog of human genes and genetic disorders authored and edited by Dr. Victor A. McKusick and his colleagues at Johns Hopkins and elsewhere.  Reference:  <MIM> MIM: McKusick, V.A.: Mendelian Inheritance in Man. A Catalog of Human Genes and Genetic Disorders. Baltimore: Johns Hopkins University Press, 1998 (12th edition). <OMIM> Online Mendelian Inheritance in Man, OMIM (TM). McKusick-Nathans Institute for Genetic Medicine, Johns Hopkins University (Baltimore, MD) and National Center for Biotechnology Information, National Library of Medicine (Bethesda, MD), 2000
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    MIM2AC <- pfamMIM2AC()
    head(MIM2AC)
    pfamMIM2AC(mim=sample(names(MIM2AC), 3))
}
}
\keyword{datasets}
