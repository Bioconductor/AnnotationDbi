make_eg_to_go_map <- function(chip) {
    if (length(grep("\\.db$", chip)))
      chip <- substr(chip, 1, nchar(chip) - 3)
    chipNS <- getNamespace(paste(chip, ".db", sep=""))
    chip2GO <- get(paste(chip, "GO", sep=""), chipNS)
    chipGO2PROBE <- get(paste(chip, "GO2PROBE", sep=""), chipNS)
    db_conn <- get("db_conn", chipNS)
    L2Rpath <- list(new("L2Rbrick", table="genes",
                        Lcolname="gene_id", Rcolname="id"),
                    chip2GO@L2Rpath[[2]])
    eg2go.map <- new("Go3AnnDbMap",
                     rightTables=chipGO2PROBE@rightTables,
                     L2Rpath=L2Rpath,
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

##      L2Rpath <- list(
##        new("L2Rbrick", table="ec", Lcolname="ec_number", Rcolname="id"),
##        new("L2Rbrick", table="genes", Lcolname="id", Rcolname="gene_id")
##      )

## 2) Create a new AtomicAnnDbMap instance:

##      hgu95av2ENZYME2ENTREZID <- new("AtomicAnnDbMap",
##        L2Rpath=L2Rpath,
##        conn=db_conn,
##        objName="ENZYME2ENTREZID",
##        objTarget="chip hgu95av2"
##      )

## One thing that needs to happen before the plugin feature can be
## implemented is a change in the current class hierarchy. More
## precisely, the AtomicAnnDbMap/RevAtomicAnnDbMap classes need to be
## replaced by a single AnnDbBimap class that will be like the
## AtomicAnnDbMap but with an additional slot to indicate its direction
## (left-to-right or right-to-left).  For now having RevAtomicAnnDbMap
## being a subclass of AtomicAnnDbMap is a bad thing and leads to all
## sort of problems. This change in the class hierarchy should happen
## very soon (probably this week).

