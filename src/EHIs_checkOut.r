###############################################################################
# newnode choose station
	
 
 
 # download high quality temperature BoM station data
 outdir <- 'data'
 if(!file.exists(outdir)){
 dir.create(outdir)
 download.file('ftp://ftp.bom.gov.au/anon/home/ncc/www/change/HQdailyT/HQ_daily_tmean_txt.tar',file.path(outdir,'HQ_daily_tmean_txt.tar'),mode='wb')
 untar(file.path(outdir,'HQ_daily_tmean_txt.tar'), exdir= file.path(outdir,'HQ_daily_tmean_txt'))
 }
 # check
 d <- read.table(file.path(outdir,'HQ_daily_tmean_txt','HQDT_stations.txt'),header=F,skip=0,nrow=1,as.is=T)
 #d
 # ok fixed width
 # nchar(d)
 
 # V1 V2 V3 V4 V5 V6 
  # 4  6  6  2  9 11 
 # actually not correct
 widths <- c(7,7,7,7,41)
 
 d2 <- read.fwf(file.path(outdir,'HQ_daily_tmean_txt','HQDT_stations.txt'),widths=widths,header=F,skip=0,as.is=T,comment.char='|',strip.white=T)
 #str(d2)
 #head(d2)
 #tail(d2)

 ## Treat data frame as spatial points
 
 pt.stations <- SpatialPointsDataFrame(cbind(d2$V3,d2$V2),d2,
   proj4string=CRS(epsg$prj4[epsg$code %in% '4283']))
 

 # get distances of all stations within 150km
 coords <- c(147.216667, -33.916667)
 #summary(pt.stations)
 dist2pt <- distVincentyEllipsoid(pt.stations,coords)
 pt.stations@data <- cbind(pt.stations@data, dist2pt)
 d <- pt.stations[which(dist2pt<150000),]
 d <- d@data
 d <- arrange(d,d$dist2pt)
 #d@data
 
 
  
 
###############################################################################
# newnode download data
	
 


 # load 
 # get the station number you want.  in this case it is the closest in the list
 stn <- d$V1[1]
 if(nchar(stn) == 5) {stn <- paste(0,stn, sep='')}        
 inurl <- paste('http://www.bom.gov.au/climate/change/hqsites/data/temp/meanT.',stn,'.daily.txt',sep='')
 header <- read.table(inurl, nrow = 1)
 stnlabl <- paste(stn,header$V6,sep='')
 analyte <- read.table(inurl, skip = 1)
 names(analyte) <- c('bomdates','avtemp')
 analyte$dates <- as.Date(paste(substr(analyte$bomdates,1,4),substr(analyte$bomdates,5,6),substr(analyte$bomdates,7,8), sep = '-'))
 analyte$avtemp <- ifelse(analyte$avtemp == 99999.9, NA, analyte$avtemp)
 #head(analyte)
 # make sure all dates are there
 fulldates <- seq(min(analyte$dates), max(analyte$dates), by = 1)
 if(length(fulldates) != nrow(analyte)) {print('check for missing days')}

 
###############################################################################
# newnode run function
	

 # do EHIs function for this station

 analyte2 <- EHIs(analyte = analyte,
  exposurename = 'avtemp',
  datename = 'dates',
  referencePeriodStart = as.Date('1971-1-1'),
  referencePeriodEnd = as.Date('2000-12-31'),
  nlags = 32)
  
  # flag upper quintile of non-negative EHF
  analyte2 <- na.omit(analyte2)        
  analyte2$EHFpctile[analyte2$EHF < 1] <- 0
  analyte2$EHFpctile[analyte2$EHF >= 1] <- (rank(analyte2$EHF[analyte2$EHF >= 1]) -1)/(length(analyte2$EHF[analyte2$EHF >= 1])-1) 
  analyte2$EHFextreme <- ifelse(analyte2$EHFpctile >= .8, 1, 0) 
       
 # deal with missings
 fulldates<-data.frame(fulldates)
 names(fulldates) <- 'dates'
 analyte2 <- merge(subset(fulldates, dates >= min(analyte2$dates)), analyte2, all.x=T)
 #str(analyte2)
        
 
###############################################################################
# newnode Check output of running function
	
  
 png('reports/WestWyalongHeatwaves19861988.png')
 with(subset(analyte2, dates >= as.Date('1986-1-1') & dates <= as.Date('1989-1-1')), plot(dates, avtemp, type = 'l'))        
 with(subset(analyte2, EHIsigCount >= 2), points(dates, avtemp, col = 'orange', pch=16, cex= .8))        
 with(subset(analyte2, EHFextreme == 1), points(dates, avtemp, col = 'red', pch=16, cex= .8))        
 title(paste(stnlabl, 'heatwaves'))
 legend('bottomright', c('EHFextreme','EHIsig for 2 or more days'), pch = 16, cex=.8, col=c('red','orange'))
 dev.off()

 
