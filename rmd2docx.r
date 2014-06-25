rmd2docx <- function(source){
  # this function is to create a docx from an Rmarkdown document with code chunks in it.
  
  # getting the directory
  setwd(dirname(source))
  # getting the filename 
  FILE <- basename(source)
  # strip the extension
  NAME <- strsplit(FILE, ".", fixed=TRUE)[[1]][1]
  ext <- strsplit(FILE, ".", fixed=TRUE)[[1]][2]
  # check if it is an R markdown file
  if(!(tolower(ext)=="rmd")){
    message("Are you sure you use an R markdown file?")
  }
  
  ## give proper settings to the RMD:
  
  # settings of the knitr options:
  opts.for.docx <- '```{r set_knitr_chunk_options}
  opts_chunk$set(echo=FALSE,message=FALSE,results = "asis") # important for making sure the output will be well formatted.
  ```
  ```{r load_pander_methods}
  require(pander)
  replace.print.methods <- function(PKG_name = "pander") {
     PKG_methods <- as.character(methods(PKG_name))
     print_methods <- gsub(PKG_name, "print", PKG_methods)
     for(i in seq_along(PKG_methods)) {
        f <- eval(parse(text=paste(PKG_name,":::", PKG_methods[i], sep = ""))) # the new function to use for print
        assign(print_methods[i], f, ".GlobalEnv")
     }   
  }
  replace.print.methods()
  ## The following might work with some tweaks:
  ## print <- function (x, ...) UseMethod("pander")
  ```'
  # append them to the file:
  tempfile <- readLines(FILE)
  tempfile <- c(opts.for.docx, tempfile)
  writeLines(tempfile, "tempfile.rmd")
  
  # load knitr
  require("knitr", quietly=TRUE)
  
  # create an html and a markdown file
  knit2html("tempfile.rmd")
  # create docx from markdown
  system(paste0("pandoc -o ", NAME, ".docx ", "tempfile", ".md"))
  # delete html and markdown
  unlink(c("tempfile.html", "tempfile.md"))
  # return nothing
}
