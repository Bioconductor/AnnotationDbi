\name{@ANNOBJPREFIX@HOMSA}
\alias{@ANNOBJPREFIX@ACYPI}
\alias{@ANNOBJPREFIX@AEDAE}
\alias{@ANNOBJPREFIX@ANOGA}
\alias{@ANNOBJPREFIX@APIME}
\alias{@ANNOBJPREFIX@ARATH}
\alias{@ANNOBJPREFIX@ASPFU}
\alias{@ANNOBJPREFIX@BATDE}
\alias{@ANNOBJPREFIX@BOMMO}
\alias{@ANNOBJPREFIX@BOSTA}
\alias{@ANNOBJPREFIX@BRAFL}
\alias{@ANNOBJPREFIX@BRUMA}
\alias{@ANNOBJPREFIX@CAEBRE}
\alias{@ANNOBJPREFIX@CAEBR}
\alias{@ANNOBJPREFIX@CAEEL}
\alias{@ANNOBJPREFIX@CAEJA}
\alias{@ANNOBJPREFIX@CAERE}
\alias{@ANNOBJPREFIX@CANAL}
\alias{@ANNOBJPREFIX@CANGL}
\alias{@ANNOBJPREFIX@CANFA}
\alias{@ANNOBJPREFIX@CAPSP}
\alias{@ANNOBJPREFIX@CAVPO}
\alias{@ANNOBJPREFIX@CHLRE}
\alias{@ANNOBJPREFIX@CIOIN}
\alias{@ANNOBJPREFIX@CIOSA}
\alias{@ANNOBJPREFIX@COCIM}
\alias{@ANNOBJPREFIX@COPCI}
\alias{@ANNOBJPREFIX@CRYNE}
\alias{@ANNOBJPREFIX@CRYHO}
\alias{@ANNOBJPREFIX@CRYPA}
\alias{@ANNOBJPREFIX@CULPI}
\alias{@ANNOBJPREFIX@CYAME}
\alias{@ANNOBJPREFIX@DANRE}
\alias{@ANNOBJPREFIX@DAPPU}
\alias{@ANNOBJPREFIX@DEBHA}
\alias{@ANNOBJPREFIX@DICDI}
\alias{@ANNOBJPREFIX@DROAN}
\alias{@ANNOBJPREFIX@DROGR}
\alias{@ANNOBJPREFIX@DROME}
\alias{@ANNOBJPREFIX@DROMO}
\alias{@ANNOBJPREFIX@DROPS}
\alias{@ANNOBJPREFIX@DROVI}
\alias{@ANNOBJPREFIX@DROWI}
\alias{@ANNOBJPREFIX@ENTHI}
\alias{@ANNOBJPREFIX@EQUCA}
\alias{@ANNOBJPREFIX@ESCCO}
\alias{@ANNOBJPREFIX@FUSGR}
\alias{@ANNOBJPREFIX@GALGA}
\alias{@ANNOBJPREFIX@GASAC}
\alias{@ANNOBJPREFIX@GIALA}
\alias{@ANNOBJPREFIX@HELRO}
\alias{@ANNOBJPREFIX@HOMSA}
\alias{@ANNOBJPREFIX@IXOSC}
\alias{@ANNOBJPREFIX@KLULA}
\alias{@ANNOBJPREFIX@LEIMA}
\alias{@ANNOBJPREFIX@LOTGI}
\alias{@ANNOBJPREFIX@MACMU}
\alias{@ANNOBJPREFIX@MAGGR}
\alias{@ANNOBJPREFIX@MONDO}
\alias{@ANNOBJPREFIX@MONBR}
\alias{@ANNOBJPREFIX@MUSMU}
\alias{@ANNOBJPREFIX@NASVI}
\alias{@ANNOBJPREFIX@NEMVE}
\alias{@ANNOBJPREFIX@NEUCR}
\alias{@ANNOBJPREFIX@ORNAN}
\alias{@ANNOBJPREFIX@ORYSA}
\alias{@ANNOBJPREFIX@ORYLA}
\alias{@ANNOBJPREFIX@OSTTA}
\alias{@ANNOBJPREFIX@PANTR}
\alias{@ANNOBJPREFIX@PEDPA}
\alias{@ANNOBJPREFIX@PHYPA}
\alias{@ANNOBJPREFIX@PHYRA}
\alias{@ANNOBJPREFIX@PHYSO}
\alias{@ANNOBJPREFIX@PLAFA}
\alias{@ANNOBJPREFIX@PLAVI}
\alias{@ANNOBJPREFIX@PONPY}
\alias{@ANNOBJPREFIX@POPTR}
\alias{@ANNOBJPREFIX@PRIPA}
\alias{@ANNOBJPREFIX@PUCGR}
\alias{@ANNOBJPREFIX@RATNO}
\alias{@ANNOBJPREFIX@RHIOR}
\alias{@ANNOBJPREFIX@SACCE}
\alias{@ANNOBJPREFIX@SCHMA}
\alias{@ANNOBJPREFIX@SCHPO}
\alias{@ANNOBJPREFIX@SCLSC}
\alias{@ANNOBJPREFIX@SORBI}
\alias{@ANNOBJPREFIX@STANO}
\alias{@ANNOBJPREFIX@STRPU}
\alias{@ANNOBJPREFIX@TAKRU}
\alias{@ANNOBJPREFIX@TETTH}
\alias{@ANNOBJPREFIX@TETNI}
\alias{@ANNOBJPREFIX@THAPS}
\alias{@ANNOBJPREFIX@THEAN}
\alias{@ANNOBJPREFIX@THEPA}
\alias{@ANNOBJPREFIX@TRICA}
\alias{@ANNOBJPREFIX@TRIVA}
\alias{@ANNOBJPREFIX@TRIAD}
\alias{@ANNOBJPREFIX@TRYCR}
\alias{@ANNOBJPREFIX@USTMA}
\alias{@ANNOBJPREFIX@XENTR}
\alias{@ANNOBJPREFIX@YARLI}
\title{Map between IDs for genes in one organism to their predicted
  paralogs in another}
\description{ 
  A map of this type is an R object that provides mappings between
  identifiers for genes in the package organism and their predicted
  paralogs in the map that the organism is named after.  So for example,
  if the inparanoid package is the human package, then the
  @ANNOBJPREFIX@RATNO map would provide mappings between human and rat.
}
\details{
    Mappings between gene identifiers and their paralogs as predicted by
    the Inparanoid algorithm.  The map filters out paralogs that have an
    Inparanoid score less than 100%.

    Mappings are normally given from the ID of the organism in the
    package to the IDs of the organism listed in the map name.

    Reversal can be made of ANY map by using the function revmap (see
    examples below).

    Names for these maps are done in the "INPARANOID style" which means
    that they are normally the 1st three letters of the genus followed
    by the 1st two letters of the species.  For example: "Mus musculus"
    becomes "MUSMU", "Homo sapiens" becomes "HOMSA", "Monodelphis
    domestica" becomes "MONDO" etc. This means that for most of these
    organisms it will be possible to easily guess the abbreviations
    used.  An exception may occur in the future if a new model organism
    has a very similar genus and species name to an existing one.
}
\references{
\url{http://inparanoid.sbc.su.se/download/current/sqltables}
}
\examples{
        x <- @ANNOBJPREFIX@APIME
        # Get honeybee IDs that are paralogous to the pkg IDs
        mapped_IDs <- mappedkeys(x)
        # Convert to a list
        xx <- as.list(x[mapped_IDs])
        if(length(xx) > 0) {
          # Get the paralogs for the first five genes
          xx[1:5]
          # Get the first one
          xx[[1]]
        }

        #Now for the reverse map (honeybee IDs back to pkg paralog)
        x <- revmap(@ANNOBJPREFIX@APIME)
        mapped_IDs <- mappedkeys(x)
        # Convert to a list
        xx <- as.list(x[mapped_IDs])
        if(length(xx) > 0) {
          # Get the paralogs for the first five IDs
          xx[1:5]
          # Get the first one
          xx[[1]]
        }         

    \dontrun{
        #For the most common organisms, we try to ensure that you can
        #map back to an Entrez Gene ID by providing you with necessary
        #maps in the related organism based annotation packages.  The
        #following example shows how to get from an Entrez Gene ID for
        #Human to Entrez Gene IDs for Mouse even though inparanoid does
        #not map to Entrez Gene IDs for either of these species.

        #You will have to include the appropriate packages for
        #humans:
        library("org.Hs.eg.db")
        #and for mouse:
        library("org.Mm.eg.db")
        #And of course you will need the inparanoid package:
        library("hom.Hs.inp.db")

        #Start with some Human Entrez Gene IDs
        humanEGIds <- c("4488","4487")

        #Inparanoid uses ensembl protein IDs so start with
        #those. Notice that there will be many protein IDs returned for
        #a typical gene since there are many possible translations.
        humanProtIds <- mget(humanEGIds,org.Hs.egENSEMBLPROT)

        #Map the IDs that we can from inparanoid. Notice that by design,
        #inparanoid only represents each gene product with a single
        #translation product. Therefore your list could slim down a lot
        #during this step.  Also, if the thing you are trying to match
        #up at this step has less than 100\% seed status, you will not
        #find it in this step.
        rawMouseProtIds <- mget(unlist(humanProtIds),hom.Hs.inpMUSMU,ifnotfound=NA)
        #This also means that we need to clean up the NAs from our result
        mouseProtIds <- rawMouseProtIds[!is.na(rawMouseProtIds)]

        #Then use the mouse organism based packages to convert these IDs
        #back to an Entrez Gene ID again (this time for mouse). 
        mouseEGIds <- mget(unlist(mouseProtIds),
                           org.Mm.egENSEMBLPROT2EG,ifnotfound=NA)
        #Now go ahead and have a look at the output
        mouseEGIds
    }

}
\keyword{datasets}

