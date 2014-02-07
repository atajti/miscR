widen <- function(data, id.vars, na.rm=TRUE){
  # This function is for transform a df from multiple row cases
  # to single row cases, with the multiple measurments numbered.
  # It will not let multiple data types, so better to use character data
  #
  # data: a data.frame
  # id.vars: character vector of names of the variables used as id
  # na.rm: logical, if TRUE, gets rid of drows with ids as NA

  if(!is.data.frame(data)){
    data <- as.data.frame(data)
  }
  # getting rid of NAs
  if(na.rm){
    all.id.na <- rowSums(is.na(data[id.vars]))==length(id.vars)
    if(sum(all.id.na)){
      data <- data[-which(all.id.na),]
    }
  }

  # create id:
  data.ids <- data[id.vars]
  data.ids.unique <- unique(data.ids)
  data <- data[-which(names(data) %in% id.vars)]
  data$id <- do.call(paste0, c(data.ids))

  # map how many replica each id has and get the max
  repeat.number <- max(table(data$id))

  # get the other varnames, and number them each from 1:repeat.number
  multied.names <- names(data)[-length(data)] #  'cos "id" is the last one
  new.varnames <- paste(rep(multied.names,repeat.number),
                        rep(1:repeat.number, each=length(multied.names)),
                        sep="_")
  # create new df
  result.df <- data.frame(id=do.call(paste0, c(data.ids.unique)))
  result.df[new.varnames] <- NA  
  # fill with data
  for(i in 1:NROW(result.df)){
    filler <- as.vector(t(data[which(data$id == result.df$id[i]),
                               multied.names]))
    result.df[i,1:length(filler)+1]<- filler
  }
  result.df <- cbind(data.ids.unique, result.df[-1])
  return(result.df)
}
