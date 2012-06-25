print.probetable = function(x, maxrows = 3, ...) {
  nr = nrow(x)
  np = min(nr, maxrows)
  cat("Object of class", class(x), "with", nr, "rows and", ncol(x), "columns.\n")
  if(nr>maxrows) cat("First", np, "rows are:\n")
  print.data.frame(x[seq_len(np), ])
}
