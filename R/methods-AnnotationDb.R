AnnotationDb <-
    function(conn,  ...)
{
    .AnnotationDb$new(conn=conn,  ...)
}

setMethod(show, "AnnotationDb",
   function(object)
{
    cat("class:", class(object), "\n")
})
