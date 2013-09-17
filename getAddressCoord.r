getAddressCoord <- function(address){
	address <- as.character(address)
	address <- paste(unlist(strsplit(address, split=" ")), collapse="+")
	base <- "http://maps.googleapis.com/maps/api/geocode/json?address="
	ending <- "&sensor=false"
	download.file(url=paste(base, address, ending, sep=""),
	              destfile="temp_address_info.json")
	address.info <- fromJSON(file="temp_address_info.json")
	unlink("temp_address_info.json")

	if(address.info[["status"]] == "OK"){
	    long.lat <- unlist(address.info$results[[1]]$geometry$location)
	    return(long.lat)
	} else {
		return("Result is not appropriate!")
	}
}
