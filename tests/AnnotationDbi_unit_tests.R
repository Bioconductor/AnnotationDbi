require("AnnotationDbi") || stop("unable to load AnnotationDbi package")
    if (!require(org.testing.db))
    {
        install.packages(system.file("extdata", "org.testing.db",
          package="AnnotationDbi"), repos=NULL,
          type="source", INSTALL_opts="--no-test-load")
        library(org.testing.db)
    }

AnnotationDbi:::.test()
