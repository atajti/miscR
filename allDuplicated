allDuplicated <- function(x){
  # This function returns TRUE for the first occurrence, too.
  x.pasted <- do.call("paste0", as.list(x))
  d <- x.pasted[duplicated(x.pasted)]
  return(x.pasted %in% d) 
}
