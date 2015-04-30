## if(!suppressWarnings(library('org.testing.db',logical.return=TRUE))){
##     install.packages(system.file('extdata','org.testing.db', package='AnnotationDbi'), repos=NULL)
## }

.test <- function() BiocGenerics:::testPackage("AnnotationDbi")
