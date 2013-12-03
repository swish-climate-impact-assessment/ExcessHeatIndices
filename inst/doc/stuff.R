# a function to download
getStuff <- function(station_id, outdir = "C:/temp")
{
url <- sprintf("http://www.bom.gov.au/fwo/IDN60901/IDN60901.%s.axf", station_id)
output_file <- file.path(outdir, paste(station_id, ".csv", sep = ""))
download.file(url, 
output_file, mode = "wb")
d <- read.csv(output_file, stringsAsFactors = F, skip = 19)
write.csv(d,output_file, row.names = F)
return(d)
}

# TEST
station_ids <- c("94768", "94762", "94675")
for(st in station_ids)
{ 
  print(st)
  d <- getStuff(st)
  print(head(d))
}
# warning
#http://www.bom.gov.au/fwo/IDS60901/IDS60901.94675.axf
#http://www.bom.gov.au/fwo/IDN60901/IDN60901.94675.axf
#str(d)
#head(d)


# appendix fail
#install.packages("RCurl")
#require(RCurl)
#d <- read.csv(textConnection(getURLContent(url)))
	