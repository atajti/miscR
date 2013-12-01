dl_postdata <- function(site="http://criticalmass.hu", nodes, file){
# this downloads data of criticalmass.hu posts.
# nodes: the number of posts to read
# file: a .csv file to write the data

  data <- data.frame(node=1:nodes,
                     time=NA, author=NA)
  # creating the header if it's not there
  if(!file.exists(file)){
    write.csv(data.frame(node=NULL, time=NULL, author=NULL))
    i.min <- 1
  } else{
    dat <- read.csv(file)
    i.min <- max(dat[1])
  }
#  pb <- txtProgressBar(min=0, max=nodes, style=3)
  for(i in i.min:nodes){
    cat(i)
    postdata  <- getTimeAuthor(paste0(site, "/node/", i, collapse=""))
    cat(": downloaded\n")
    cat(paste0(" time=", postdata$time))
    cat(paste0(" author=", postdata$author, "\n"))
    write.table(data.frame(node=i, time=postdata$time, author=postdata$author),
      file=file, append=TRUE, col.names=FALSE, row.names=FALSE, sep=",")
    data[i, "time"] <- as.numeric(postdata$time)
    data[i, "author"] <- postdata$author
 #   setTxtProgressBar(pb, i)
  }
 # close(pb)
  return(data)
} 
