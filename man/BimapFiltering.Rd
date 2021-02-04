\name{toggleProbes}

% \alias{exposeAllProbes}
% \alias{exposeAllProbes,AnnDbBimap-method}
% \alias{maskMultiProbes}
% \alias{maskMultiProbes,AnnDbBimap-method}
% \alias{maskSingleProbes}
% \alias{maskSingleProbes,AnnDbBimap-method}

\alias{toggleProbes}
\alias{toggleProbes,ProbeAnnDbBimap-method}
\alias{toggleProbes,ProbeAnnDbMap-method}
\alias{toggleProbes,ProbeIpiAnnDbMap-method}
\alias{toggleProbes,ProbeGo3AnnDbBimap-method}

\alias{hasMultiProbes}
\alias{hasMultiProbes,ProbeAnnDbBimap-method}
\alias{hasMultiProbes,ProbeAnnDbMap-method}
\alias{hasMultiProbes,ProbeIpiAnnDbMap-method}
\alias{hasMultiProbes,ProbeGo3AnnDbBimap-method}

\alias{hasSingleProbes}
\alias{hasSingleProbes,ProbeAnnDbBimap-method}
\alias{hasSingleProbes,ProbeAnnDbMap-method}
\alias{hasSingleProbes,ProbeIpiAnnDbMap-method}
\alias{hasSingleProbes,ProbeGo3AnnDbBimap-method}

\alias{getBimapFilters}
\alias{getBimapFilters,AnnDbBimap-method}


\title{Methods for getting/setting the filters on a Bimap object}

\description{
  These methods are part of the \link{Bimap} interface (see
  \code{?\link{Bimap}} for a quick overview of the \link{Bimap} objects
  and their interface).

  Some of these methods are for getting or setting the filtering status
  on a \link{Bimap} object so that the mapping object can toggle between
  displaying all probes, only single probes (the defualt) or only
  multiply matching probes.

  Other methods are for viewing or setting the filter threshold value on
  a InpAnnDbBimap object.
}

\usage{
  ## Making a Bimap object that does not prefilter to remove probes that
  ## match multiple genes:
  toggleProbes(x, value)
  hasMultiProbes(x) ##T/F test for exposure of single probes
  hasSingleProbes(x) ##T/F test for exposure of mulitply matched probes

  ## Looking at the SQL filter values for a Bimap
  getBimapFilters(x)
}

\arguments{
  \item{x}{
    A \link{Bimap} object.
  }
  \item{value}{    
    A character vector containing the new value that the Bimap should
    use as the filter.  Or the value to toggle a probe mapping to:
    "all", "single", or "multiple".
  }
}

\details{  
  \code{toggleProbes(x)} is a methods for creating Bimaps that have an
  alternate filter for which probes get exposed based upon whether these
  probes map to multiple genes or not.

  \code{hasMultiProbes(x)} and \code{hasSingleProbes(x)} are provided to
  give a quick test about whether or not such probes are exposed in a
  given mapping.

  \code{getBimapFilters(x)} will list all the SQL filters applied to a
  Bimap object.
}

\value{
  A \link{Bimap} object of the same subtype as \code{x} for
  \code{exposeAllProbes(x)}, \code{maskMultiProbes(x)} and
  \code{maskSingleProbes(x)}.

  A TRUE or FALSE value in the case of \code{hasMultiProbes(x)} and
  \code{hasSingleProbes(x)}.
}

\author{M. Carlson}

\seealso{
  \link{Bimap},
  \link{Bimap-keys},
  \link{Bimap-direction},
  \link{BimapFormatting},
  \link{Bimap-envirAPI},
  \code{\link{nhit}}
}

\examples{
  ## Make a Bimap that contains all the probes
  require("hgu95av2.db")
  mapWithMultiProbes <- toggleProbes(hgu95av2ENTREZID, "all")
  count.mappedLkeys(hgu95av2ENTREZID)
  count.mappedLkeys(mapWithMultiProbes)

  ## Check that it has both multiply and singly matching probes:
  hasMultiProbes(mapWithMultiProbes)
  hasSingleProbes(mapWithMultiProbes)

  ## Make it have Multi probes ONLY:
  OnlyMultiProbes = toggleProbes(mapWithMultiProbes, "multiple")
  hasMultiProbes(OnlyMultiProbes)
  hasSingleProbes(OnlyMultiProbes)

  ## Convert back to a default map with only single probes exposed
  OnlySingleProbes = toggleProbes(OnlyMultiProbes, "single")
  hasMultiProbes(OnlySingleProbes)
  hasSingleProbes(OnlySingleProbes)
}

\keyword{methods}
