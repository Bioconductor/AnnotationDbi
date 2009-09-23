\name{@ANNOBJPREFIX@CAZY2AC}
\alias{@ANNOBJPREFIX@CAZY2AC}
\alias{@ANNOBJPREFIX@DE2AC}
\alias{@ANNOBJPREFIX@ID2AC}
\alias{@ANNOBJPREFIX@HOMSTRAD2AC}
\alias{@ANNOBJPREFIX@INTERPRO2AC}
\alias{@ANNOBJPREFIX@LOAD2AC}
\alias{@ANNOBJPREFIX@MEROPS2AC}
\alias{@ANNOBJPREFIX@MIM2AC}
\alias{@ANNOBJPREFIX@PRINTS2AC}
\alias{@ANNOBJPREFIX@PROSITE2AC}
\alias{@ANNOBJPREFIX@PROSITEPROFILE2AC}
\alias{@ANNOBJPREFIX@RM2AC}
\alias{@ANNOBJPREFIX@SMART2AC}
\alias{@ANNOBJPREFIX@TC2AC}
\alias{@ANNOBJPREFIX@TP2AC}
\alias{@ANNOBJPREFIX@URL2AC}

\title{Mappings from an ID to a PFAM Accession number}
\description{
  This is an R object that contains mapppings from an ID to its
  PFAM Accession number (AC)
}
\details{
  For each of the aliases listed above, there is a mapping object that
  corresponds which will map you from the ID type in the environments
  name to the appropriate PFAM accession number.  So for example
  @ANNOBJPREFIX@CAZY2AC will map from CAZY IDs to PFAM IDs while
  @ANNOBJPREFIX@DE2AC will map from Descriptions to PFAM IDs.

  Details on supported things that are mapped to:

  CAZY: The CAZy database (<URL: http://afmb.cnrs-mrs.fr/CAZY/>)
     describes the families of structurally-related catalytic and
     carbohydrate-binding modules (or functional domains) of enzymes
     that degrade, modify, or create glycosidic bonds.
     
  DE:  Definition for all the Accession number in the database.

  ID:  Associated Identification for all the Accession number in the
     database.

  HOMSTRAD:  HOMSTRAD (HOMologous STRucture Alignment Database, <URL:
     http://www-cryst.bioc.cam.ac.uk/homstrad/>) is a curated database
     of structure-based alignments for homologous protein families.
     Reference: Mizuguchi K, Deane CM, Blundell TL, Overington JP.
     (1998) HOMSTRAD: a database of protein structure alignments for
     homologous families. Protein Science 7:2469-2471.

  INTERPRO:  Associated INTERPRO ID for all the Accession number in the
     database.  <URL: http://www.ebi.ac.uk/interpro/> 

  LOAD:  LOAD ID for all the Accession number in the database.

  MEROPS:  The MEROPS database (<URL: http://merops.sanger.ac.uk/>) is
     an information resource for peptidases (also termed proteases,
     proteinases and proteolytic enzymes) and the proteins that inhibit
     them. Reference: Rawlings, N.D., Tolle, D.P. & Barrett, A.J. (2004)
     MEROPS: the peptidase database. Nucleic Acids Res. 32 Database
     issue, D160-D164

  MIM:  MIM (a.k.a. OMIM, <URL: http://www.ncbi.nlm.nih.gov/omim/>) is a
     catalog of human genes and genetic disorders authored and edited by
     Dr. Victor A. McKusick and his colleagues at Johns Hopkins and
     elsewhere. Reference:  <MIM> MIM: McKusick, V.A.: Mendelian
     Inheritance in Man. A Catalog of Human Genes and Genetic
     Disorders. Baltimore: Johns Hopkins University Press, 1998 (12th
     edition). <OMIM> Online Mendelian Inheritance in Man, OMIM
     (TM). McKusick-Nathans Institute for Genetic Medicine, Johns
     Hopkins University (Baltimore, MD) and National Center for
     Biotechnology Information, National Library of Medicine (Bethesda,
     MD), 2000

  PRINTS:  PRINTS (<URL: http://umber.sbs.man.ac.uk/dbbrowser/PRINTS/>)
    is a compendium of protein fingerprints.

  PROSITEPROFILE:  A list of associated PROSITE PROFILE ID.

  RM:  Reference Medline (<URL: http://www.ncbi.nlm.nih.gov/PubMed/>)

  SMART:  SMART (a Simple Modular Architecture Research Tool, <URL:
    http://smart.embl-heidelberg.de/>) allows the identification and
    annotation of genetically mobile domains and the analysis of domain
    architectures.  Reference: (1) Schultz et al. (1998)
    Proc. Natl. Acad. Sci. USA 95, 5857-5864.  (2) Letunic et al. (2004)
    Nucleic Acids Res 32, D142-D144

  TC:  Trusted cutoff for all the Accession number in the database.

  TP:  A list of associated Type field for the given Accession.

  URL:  A list of associated URL for all the Accession number in the
    database.
  
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\examples{
        #To map from CAZY to PFAM IDs:
        x <- @ANNOBJPREFIX@CAZY2AC
        # Get the CAZY identifiers that are mapped to a PFAM ID
        mapped_keys <- mappedkeys(x)
        # Convert to a list
        xx <- as.list(x[mapped_keys])
        if(length(xx) > 0) {
          # Get the PFAM ID for the first five CAZYs
          xx[1:5]
          # Get the first one
          xx[[1]]
        }

        #Or to use the DE2AC mapping:
        x <- @ANNOBJPREFIX@DE2AC
        # Get the Descriptions that are mapped to a PFAM ID
        mapped_keys <- mappedkeys(x)
        # Convert to a list
        xx <- as.list(x[mapped_keys])
        if(length(xx) > 0) {
          # Get the PFAM ID for the first five DEs
          xx[1:5]
          # Get the first one
          xx[[1]]
        }

        #etc.
}
\keyword{datasets}
