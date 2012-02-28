###############################################################################
# newnode load
	
 # load 
 # get the station number you want.   Nairn paper states they use this station.
 stn <- '023090'
 inurl <- paste('http://www.bom.gov.au/climate/change/hqsites/data/temp/meanT.',stn,'.daily.txt',sep='')
 header <- read.table(inurl, nrow = 1)
# header
 stnlabl <- paste(stn,header$V6,sep='')
 analyte <- read.table(inurl, skip = 1)
 
 # check
#  head(analyte)
#  str(analyte)
#  head(analyte)
 names(analyte) <- c('bomdates','avtemp')
#  summary(analyte)
 
 # clean
 analyte$dates <- as.Date(paste(substr(analyte$bomdates,1,4),substr(analyte$bomdates,5,6),substr(analyte$bomdates,7,8), sep = '-'))
 # missing data = 99999.9
 analyte$avtemp <- ifelse(analyte$avtemp == 99999.9, NA, analyte$avtemp)
 
 
