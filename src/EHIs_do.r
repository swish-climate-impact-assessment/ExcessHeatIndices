###############################################################################
# newnode EHI{accl}
	
  # TASK SHOULD WE IMPUTE MISSING DAYS?
 
 # first get lags
 # TASK THERE IS PROBABLY A VECTORISED VERSION THAT IS QUICKER?
 analyte$temp_lag0 <- analyte$avtemp
 exposuresList <- 'temp_lag0'
 nlags <- 32 
 # lag0 is not needed
 for(lagi in 1:nlags){
 	# lagi <- 1
 	exposuresList <- c(exposuresList, gsub('lag0',paste('lag', lagi,sep=''), exposuresList[1]))
 	analyte[,(ncol(analyte)+1)] <- Lag(analyte[,exposuresList[1]],lagi)
 	}
 exposuresList <- exposuresList[-1]
 names(analyte) <- c(names(analyte[,1:(ncol(analyte)-nlags)]),exposuresList)
#  head(analyte)
 # now 3 day av
 analyte$temp_movav <- rowMeans(analyte[,c('temp_lag0','temp_lag1','temp_lag2')], na.rm =FALSE)

 # now 30 day av
 analyte$temp30_movav <- rowMeans(analyte[,7:36], na.rm =FALSE)
 analyte <- na.omit(analyte)
#  head(analyte)
 
 # now calculate the EHI
 analyte$EHIaccl <- analyte$temp_movav - analyte$temp30_movav
 
 # check
 qc <- subset(analyte, dates >= as.Date('2009-1-1') & dates <= as.Date('2009-2-28'))
 png(paste('reports/',stnlabl, 'movavs.png', sep = ''))
 # par(mfrow=c(1,2))
 with(qc, 
  plot(dates, temp_lag0,type = 'l', ylim = c(0,40), main = stnlabl)
  )
 with(qc, 
  lines(dates, temp_movav,col = 'red')
  )
 with(qc, 
  lines(dates, temp30_movav,col = 'orange')
  )
 legend('topright',c('temp','3movav','30movav'), lty = 1, col = c('black','red','orange'))  
 
 # with(qc, 
  # plot(dates, EHIaccl,type = 'l', ylim = c(-15,20), main = stnlabl)
  # )
  
 dev.off()
 
 
###############################################################################
# newnode EHI{sig}
	
 
 # first calculate the 95th centile
 referencestart <- as.Date('1971-1-1')
 referenceend <- as.Date('2000-12-31')
 reference <- subset(analyte, dates >= referencestart & dates <= referenceend, select = c('dates', 'avtemp'))
#  head(reference);tail(reference)
 T95 <- quantile(reference$avtemp, 0.95, na.rm = T)
#  T95
 
 # now calculate the EHIsig
 analyte$EHIsig <- analyte$temp_movav - T95
 
 # check
 qc <- subset(analyte, dates >= as.Date('2009-1-1') & dates <= as.Date('2009-2-28'))
#  with(qc, 
#   plot(dates, EHIaccl,type = 'l', ylim = c(-15,20), main = stnlabl)
#   )
#  with(qc, 
#   lines(dates, EHIsig,col = 'grey')
#   )
#  segments(as.Date('2009-1-1') , seq(-15, 20,5) , as.Date('2009-2-28'),  seq(-15, 20,5))  
#  dev.off()
 
 
###############################################################################
# newnode EHFS
	
 
 # now calculate the EHF
 analyte$EHF <- abs(analyte$EHIaccl) * analyte$EHIsig
 
 
###############################################################################
# newnode summary
	
 
 qc <- subset(analyte, dates >= as.Date('2009-1-1') & dates <= as.Date('2009-2-28'))
 png(paste('reports/Figure1.png', sep = ''))
 par(mfrow=c(2,2))
 with(qc, 
  plot(dates, temp_lag0,type = 'l', ylim = c(0,100), main = stnlabl)
  )
 with(qc, 
  lines(dates, temp_movav,col = 'red')
  )
 with(qc, 
  lines(dates, temp30_movav,col = 'orange')
  )
 segments(as.Date('2009-1-1') , seq(0, 50,5) , as.Date('2009-2-28'),  seq(0, 50,5))  
 segments(as.Date('2009-1-1') , T95 , as.Date('2009-2-28'),  T95, col = 'blue', lwd = 2)  
 legend('topright',c('temp','3movav','30movav', 'T95'), lty = 1, col = c('black','red','orange', 'blue'), bg = 'white')  
 
 with(qc, 
  plot(dates, EHIaccl,type = 'l', ylim = c(-15,20), main = stnlabl, ylab = 'EHIs')
  )
 with(qc, 
  lines(dates, EHIsig,col = 'grey')
  )
 segments(as.Date('2009-1-1') , seq(-15, 20,5) , as.Date('2009-2-28'),  seq(-15, 20,5))  
 legend('topright',c('EHIaccl','EHIsig'), lty = 1, col = c('black','grey'), bg = 'white')  
 
 with(qc, 
  plot(dates, EHF,type = 'l', ylim = c(-150,200), main = stnlabl)
  )
 segments(as.Date('2009-1-1') , seq(-150, 200,50) , as.Date('2009-2-28'),  seq(-150, 200,50))  
  
 dev.off()
 
 
