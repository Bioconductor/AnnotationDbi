make_eg_to_go_map <- function(chip) {
    if (length(grep("\\.db$", chip)))
      chip <- substr(chip, 1, nchar(chip) - 3)
    chipNS <- getNamespace(paste(chip, ".db", sep=""))
    chip2GO <- get(paste(chip, "GO", sep=""), chipNS)
    chipGO2PROBE <- get(paste(chip, "GO2PROBE", sep=""), chipNS)
    db_conn <- get("db_conn", chipNS)
    L2Rchain <- list(new("L2Rlink", tablename="genes",
                        Lcolname="gene_id", Rcolname="id"),
                    chip2GO@L2Rchain[[2]])
    eg2go.map <- new("Go3AnnDbBimap",
                     rightTables=chipGO2PROBE@rightTables,
                     L2Rchain=L2Rchain,
                     conn=db_conn,
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
##        new("L2Rlink", tablename="ec", Lcolname="ec_number", Rcolname="id"),
##        new("L2Rlink", tablename="genes", Lcolname="id", Rcolname="gene_id")
##      )

## 2) Create a new AtomicAnnDbBimap instance:

##      hgu95av2ENZYME2ENTREZID <- new("AtomicAnnDbBimap",
##        L2Rchain=L2Rchain,
##        conn=db_conn,
##        objName="ENZYME2ENTREZID",
##        objTarget="chip hgu95av2"
##      )

