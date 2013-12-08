
# this takes data in directories from bom_download.r
 
# first get list of directories
filelist <- dir(pattern = "axf", recursive = T)
filelist
 
# next get directories for days we haven't done yet
if(file.exists("complete_dataset.csv"))
{
complete_data <- read.csv("complete_dataset.csv", stringsAsFactors = F)
#str(complete_data)
last_collated <- max(as.Date(complete_data$date_downloaded))
#max(complete_data$local_hrmin)
 
days_downloaded <- dirname(filelist)
filelist <- filelist[which(as.Date(days_downloaded) > as.Date(last_collated))]
}
 
# for these collate them into the complete file
for(f in filelist)
{
  #f <- filelist[2]
  print(f)
  fin <- read.csv(f, colClasses = c("local_date_time_full.80." = "character"), 
    stringsAsFactors = F, skip = 19)
  fin <- fin[1:(nrow(fin) - 1),]
  fin$date_downloaded <- dirname(f)
  fin$local_year <- substr(fin$local_date_time_full.80., 1, 4)
  fin$local_month <- substr(fin$local_date_time_full.80., 5, 6)
  fin$local_day <- substr(fin$local_date_time_full.80., 7, 8)
  fin$local_hrmin <- substr(fin$local_date_time_full.80., 9, 12)
  fin$local_date <- paste(fin$local_year, fin$local_month, fin$local_day, sep = "-")
  if(file.exists("complete_dataset.csv"))
  {
  write.table(fin, "complete_dataset.csv", row.names = F, sep = ",", append = T, col.names = F)
  } else {
  write.table(fin, "complete_dataset.csv", row.names = F, sep = ",")
  }
}
