setMethod("show", signature(object="AnnotDbTable"),
          function(object) {
              cat("Annotation Data Table", sQuote(object@tableName),
                  "with", nrow(object), "rows\n")
              rowOne <- paste("SELECT * FROM", object@tableName,
                              "LIMIT 1")
              rs <- object@firstRow
              fldNames <- names(object)
              fldNameLens <- sapply(fldNames, nchar)
              fldHead <- "Field Name"
              maxLen <- max(fldNameLens, nchar(fldHead))
              cat(rep(" ", maxLen - nchar(fldHead)),
                  fldHead, "    ",
                  "Data From First Row\n", sep="")
              for (i in seq(length=length(fldNameLens))) {
                  nm <- fldNames[i]
                  val <- rs[[nm]]
                  pad <- rep(" ", maxLen - nchar(nm))
                  cat(pad, nm, "    ", val, "\n", sep="")
              }
          })

