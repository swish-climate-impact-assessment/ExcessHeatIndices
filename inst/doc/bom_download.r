
filename = "~/data/ExcessHeatIndices/inst/doc/weather_stations.csv"
output_directory = "~/bom-downloads"
setwd(output_directory)

urls <- read.csv(filename)
urls_list <- paste(sep = "", "http://www.bom.gov.au/fwo/ID",
                  urls$State,
                  "60", 
                  urls$City_9_or_regional_8_,
                  "01/ID",
                  urls$State,
                  "60",
                  urls$City_9_or_regional_8_,
                  "01.",
                  urls$Station_ID,
                  ".axf")

output_directory <- file.path(output_directory,Sys.Date())
dir.create(output_directory)

for(url in urls_list)
{
  output_file <- file.path(output_directory,basename(url))
  download.file(url, output_file, mode = "wb")

}
print("SUCCESS")
