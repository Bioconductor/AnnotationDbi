\name{inpIDMapper}

\alias{inpIDMapper}

\title{convenience function for mapping IDs through an appropriate set of
  inparanoid packages}

\description{
  This is a convenience function that attempts to take a list of IDs
  along with some addional information about what those IDs are, what
  type of ID you would like them to be, as well as some information
  about what species they are from and what species you would like them
  to be from and then attempts to the simplest possible conversion using
  an inparanoid package.  By default, this function will drop ambiguous
  matches from the results.  Please see the details section for more
  information about these last two parameters.
}

\usage{
  inpIDMapper(ids, srcSpecies, destSpecies, srcIDType="UNIPROT",
  destIDType="EG", keepMultGeneMatches=FALSE, keepMultProtMatches=FALSE,
  keepMultDestIDMatches = TRUE)
}

\arguments{
  \item{ids}{the list of original IDs to match}
  \item{srcSpecies}{The original source species in in paranoid
  format. In other words, the 3 letters of the genus followed by 2
  letters of the species in all caps.  Ie. 'HOMSA' is for Homo sapiens etc.}
  \item{destSpecies}{the destination species in inparanoid format}
  \item{srcIDType}{The source ID type written exactly as it would be
  used in a mapping name for an eg package.  So for example, 'UNIPROT'
  is how the uniprot mappings are always written, so we keep that
  convention here.}
  \item{destIDType}{the destination ID, written the same way as you
  would write the srcIDType. By default this is set to "EG" for entrez
  gene IDs}
  \item{keepMultGeneMatches}{Do you want to try and keep the 1st ID in
  those ambiguous cases where more than one protein is suggested?  (You
  probably want to filter them out - hence the default is FALSE)}
  \item{keepMultProtMatches}{Do you want to try and keep the 1st ID in
  those ambiguous cases where more than one protein is suggested?
  (default = FALSE)}
  \item{keepMultDestIDMatches}{If you have mapped to a destination ID OTHER
  than an entrez gene ID, then it is possible that there may be multiple
  answers.  Do you want to keep all of these or only return the 1st one?
  (default = TRUE)}
}

\value{
  a list where the names of each element are the elements of the
  original list you passed in, and the values are the matching results.
  Elements that do not have a match are not returned.  If you want
  things to align you can do some bookeeping.
}
  
\details{
  This is a convenience function for getting an ID from one species
  mapped to an ID type of your choice from another organism of your
  choice.  The only mappings used to do this are the mappings that are
  scored as 100 according to the inparanoid algorithm.  This function
  automatically tries to join IDs by using FIVE different mappings in
  the sequence that follows:

  1) initial IDs -> src organism Entrez Gene IDs
  2) src organism Entrez Gene IDs -> sre organism Inparanoid ID
  3) src organism Inparanoid ID -> dest organism Inparanoid ID
  4) dest organism Inparanoid ID -> dest organism Entrez Gene ID
  5) dest organism Entrez Gene ID -> final destination organism ID

  You can simplify this mapping as a series of steps like this:

  srcIDs ---> srcEGs ---> srcInp ---> destInp ---> destEGs ---> destIDs
         (1)         (2)         (3)          (4)          (5) 
  
  There are two steps in this process where multiple mappings can really
  interfere with getting a clear answer.  It's no coincidence that these
  are also adjacent to the two places where we have to tie the identity
  to a single gene for each organism.  When this happens, any ambiguity
  is confounding.  Preceding step \#2, it is critical that we only have
  ONE entrez gene ID per initial ID, and the parameter
  keepMultGeneMatches can be used to toggle whether to drop any
  ambiguous matches (the default) or to keep the 1st one in the hope of
  getting an additional hit.  A similar thing is done preceding step \#4,
  where we have to be sure that the protein IDs we are getting back have
  all mapped to only one gene.  We allow you to use the
  keepMultProtMatches parameter to make the same kind of decision as in
  step \#2, again, the default is to drop anything that is ambiguous.

}

\author{Marc Carlson}

\examples{
\dontrun{
  ## This has to be in a dontrun block because otherwise I would have to
  ## expand the DEPENDS field for AnnotationDbi
  ## library("org.Hs.eg.db")
  ## library("org.Mm.eg.db")
  ## library("org.Sc.eg.db")
  ## library("hom.Hs.inp.db")
  ## library("hom.Mm.inp.db")
  ## library("hom.Sc.inp.db")

  ##Some IDs just for the example
  library("org.Hs.eg.db")
  ids = as.list(org.Hs.egUNIPROT)[10000:10500]
  ## Get entrez gene IDs (default) for uniprot IDs mapping from human to mouse.
  MouseEGs = inpIDMapper(ids, "HOMSA", "MUSMU")
  ##Get yeast uniprot IDs in exchange for uniprot IDs from human
  YeastUPs = inpIDMapper(ids, "HOMSA", "SACCE", destIDType="UNIPROT")
  ##Get yeast uniprot IDs but only return one ID per initial ID
  YeastUPSingles = inpIDMapper(ids, "HOMSA", "SACCE", destIDType="UNIPROT", keepMultDestIDMatches = FALSE)
  ##Now we have to go from yeast uniprot IDs to Human entrez gene IDs.
  HumanEGs = inpIDMapper(YeastUPSingles, "SACCE", "HOMSA")
}
}

\keyword{manip}
