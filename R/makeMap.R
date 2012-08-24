make_eg_to_go_map <- function(chip) {
    
    if (length(grep("\\.db$", chip)))
      chip <- substr(chip, 1, nchar(chip) - 3)
    chipNS <- getNamespace(paste0(chip, ".db"))
    chip2GO <- get(paste0(chip, "GO"), chipNS)
    chipGO2PROBE <- get(paste0(chip, "GO2PROBE"), chipNS)
    L2Rchain <- list(new("L2Rlink", tablename="probes", Lcolname="gene_id", Rcolname="gene_id"),
                    chip2GO@L2Rchain[[2]],
                    chip2GO@L2Rchain[[3]])
    eg2go.map <- new("Go3AnnDbBimap",
                     rightTables=chipGO2PROBE@rightTables,
                     L2Rchain=L2Rchain,
                     datacache=chipGO2PROBE@datacache,
                     objName="ENTREZID2GO",
                     objTarget=paste("chip", chip))
    eg2go.map
}

make_go_to_eg_map <- function(chip) {
    map <- make_eg_to_go_map(chip)
    revmap(map)
}

## Notes from Herve:
## 1) First define the left-to-right path that you need to take to go from
##    the ENZYME space to the ENTREZID space. You can look at the
##    hgu95av2ENZYME2PROBE/hgu95av2ENTREZID slots for this or consult
##    the schemas definitions in AnnotationDbi:

##      L2Rchain <- list(
##        new("L2Rlink", tablename="ec", Lcolname="ec_number", Rcolname="_id"),
##        new("L2Rlink", tablename="genes", Lcolname="_id", Rcolname="gene_id")
##      )

## 2) Create a new AnnDbBimap instance:

##      hgu95av2ENZYME2ENTREZID <- new("AnnDbBimap",
##        L2Rchain=L2Rchain,
##        datacache=hgu95av2ENZYME@datacache,
##        objName="ENZYME2ENTREZID",
##        objTarget="chip hgu95av2"
##      )

