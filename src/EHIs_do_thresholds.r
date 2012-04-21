###############################################################################
# newnode Ask BoM what they think is a good threshold
	
 
 # first question is if they say EHF greater than 1 is this related to a ADT percentile?
#  head(reference)
 reference$pctile <- (rank(reference$avtemp) -1)/(length(reference$avtemp)-1) 
 with(reference, plot(avtemp, pctile))
 segments(0,.95,40,.95)
 segments(T95, 0, T95, 1)
 
#  head(analyte)
 checkEHFagainstQuantiles <- merge(analyte[,c('dates','avtemp','EHF')], reference)
 checkEHFagainstQuantiles$heatwave <- ifelse(checkEHFagainstQuantiles$EHF >=1, 1, 0)
 checkEHFagainstQuantiles$heatwave <- as.factor(checkEHFagainstQuantiles$heatwave)
 
 with(checkEHFagainstQuantiles, plot(EHF, pctile)) 
 # hangon how is it possible to have EHF > 0  but pctile < 95?
 hangon <- subset(checkEHFagainstQuantiles, pctile < .95 & EHF > 0)
#  head(hangon[order(hangon$pctile),] )
#  subset(analyte, dates <= as.Date('1982-01-25') & dates >= as.Date('1982-01-20'), select = c('dates','avtemp','temp_movav','temp30_movav', 'EHIaccl','EHIsig','EHF'))
 # oh the 3 day movav has two days at 37+
#  subset(checkEHFagainstQuantiles, dates <= as.Date('1982-01-25') & dates >= as.Date('1982-01-20'))
 # infact the day before was the highest on record at 37.8
 
 png(paste('reports/','checkEHFagainstQuantiles.png', sep = ''))
 par(mfrow = c(2,2))
 with(checkEHFagainstQuantiles, plot(EHF, pctile, ylab = 'percentile of ADT'))
 with(checkEHFagainstQuantiles, boxplot(pctile ~ heatwave))
 segments(0,.95, 3, .95) 
 with(checkEHFagainstQuantiles, plot(EHF, avtemp))
 # and what about the upper quintile?
 checkEHFagainstQuantiles$EHFpctile[checkEHFagainstQuantiles$EHF < 1] <- NA
 checkEHFagainstQuantiles$EHFpctile[checkEHFagainstQuantiles$EHF >= 1] <- (rank(checkEHFagainstQuantiles$EHF[checkEHFagainstQuantiles$EHF >= 1]) -1)/(length(checkEHFagainstQuantiles$EHF[checkEHFagainstQuantiles$EHF >= 1])-1) 
 with(checkEHFagainstQuantiles, plot(EHF, EHFpctile))
 segments(-150,.8, 100, .8) 
 EHF80 <- min(checkEHFagainstQuantiles[which(checkEHFagainstQuantiles$EHFpctile >= .8),'EHF'])
 segments( EHF80,0, EHF80, 1) 
 
 dev.off()
 
 
###############################################################################
# newnode How would the Adelaide heatwave look
	
 png(paste('reports/','Figure1withHeatwaves.png', sep = ''))
 with(qc, 
  plot(dates, EHF,type = 'l', ylim = c(-150,200), main = stnlabl)
  )
 segments(as.Date('2009-1-1') , seq(-150, 200,50) , as.Date('2009-2-28'),  seq(-150, 200,50))  
 segments(as.Date('2009-1-1') , 1, as.Date('2009-2-28'),  1, col = 'orange', lwd = 2)  
 segments(as.Date('2009-1-1') , EHF80, as.Date('2009-2-28'),  EHF80, col = 'red', lwd = 2)  
 legend('topright',c('extreme heatwave','heatwave'), lty = 1, lwd = 2, col = c('red','orange'), bg = 'white')  
 
 dev.off()
 
 
###############################################################################
# newnode Propose a threshold method based on Hutchinson
	
 
 # newnode counts
 # counts can be done quicker with this
#  names(analyte)
 x <- analyte$EHIaccl >= 0
 xx <- (cumsum(!x) + 1) * x 
 x2<-(seq_along(x) - match(xx, xx) + 1) * x 
 analyte$EHIacclCount <- x2

 
 # alternately, slower but more interpretable
 analyte$EHIacclCount2<-as.numeric(0)
 # 
#  which(analyte$dates == as.Date('2009-1-1'))
#  which(analyte$dates == as.Date('2009-3-1'))
#  
 for(j in 43034:43093){
  # j=43034
  analyte$EHIacclCount2[j] <- ifelse(analyte$EHIaccl[j] < 0, 0,
  ifelse(analyte$EHIaccl[j-1] >= 0, 1 + analyte$EHIacclCount2[j-1],
  1)
  )
  }
	
 # check 
 qc <- subset(analyte, dates >= as.Date('2009-1-1') & dates <= as.Date('2009-2-28'),
	select = c('dates','temp_lag0', 'EHIaccl', 'EHIacclCount', 'EHIacclCount2', 'EHIsig', 'EHF'))
#  qc	
 
 # good do the sig
 x <- analyte$EHIsig >= 0
 xx <- (cumsum(!x) + 1) * x 
 x2<-(seq_along(x) - match(xx, xx) + 1) * x 
 analyte$EHIsigCount <- x2
 
 #newnode sums workings
 # analyte$EHF2  <- analyte$EHF * -1 
 # y <- ifelse(analyte$EHF2 >= 0, 0, analyte$EHF)
 # f <- analyte$EHF2 < 0
 # cbind(analyte$bomdates,y,f)[43034:43093,]
 # f <- (cumsum(!f) + 1) * f 
 # cbind(analyte$bomdates,y,f)[43034:43093,]
 # z <- unsplit(lapply(split(y,f),cumsum),f)
 # qc <- as.data.frame(cbind(analyte$dates,analyte$EHF, y,f,z)[43034:43093,])
 # names(qc) <- c('bomdates','EHF', 'y','f','z')
 # par(mfrow = c(2,1))
 # with(qc, plot(bomdates,EHF, type = 'l'))
 # with(qc, plot(bomdates,z, type = 'l', col = 'red'))
 
 # sums
 EHFinverted  <- analyte$EHF * -1 
 y <- ifelse(EHFinverted >= 0, 0, analyte$EHF)
 f <- EHFinverted < 0
 f <- (cumsum(!f) + 1) * f 
 z <- unsplit(lapply(split(y,f),cumsum),f)
 analyte$EHFintegrated <- z
 
 # alternately, slower but more interpretable
 analyte$EHFintegrated2 <- as.numeric(0)
 for(j in 43034:43093){
 # j = 43034
	analyte$EHFintegrated2[j] <- ifelse(analyte$EHF[j] < 0,0,
	 ifelse(analyte$EHF[j-1] >= 0,
	 analyte$EHF[j] + analyte$EHFintegrated2[j-1],
	 analyte$EHF[j])
	 )
	}
 # check 
 qc <- subset(analyte, dates >= as.Date('2009-1-1') & dates <= as.Date('2009-2-28'),
	select = c('dates','temp_lag0', 'EHIaccl', 'EHIacclCount', 'EHIacclCount2', 'EHIsig', 'EHF', 'EHFintegrated', 'EHFintegrated2'))
#  qc	
 
 
###############################################################################
# newnode Add this to the summary
	
 
 qc <- subset(analyte, dates >= as.Date('2009-1-1') & dates <= as.Date('2009-2-28'))
 png(paste('reports/','thresholds.png', sep = ''))
 par(mfrow=c(3,1))
 
 with(qc, 
  plot(dates, EHIaccl,type = 'l', ylim = c(-15,20), main = 'EHIs', ylab = 'EHIs')
  )
  with(qc, 
  points(dates, EHIaccl)
  )
 with(qc, 
  lines(dates, EHIsig,col = 'grey')
  )
 with(qc, 
  points(dates, EHIsig,col = 'grey')
  )
 segments(as.Date('2009-1-1') , seq(-15, 20,5) , as.Date('2009-2-28'),  seq(-15, 20,5),col = 'grey', lty=2)  
 abline(0,0)
 legend('topright',c('EHIaccl','EHIsig'), lty = 1, col = c('black','grey'), bg = 'white')  
 
with(qc, 
  plot(dates, EHIacclCount,type = 'l', ylim = c(-5,20), main = 'consecutive days whith positive EHIs (duration)', ylab = 'EHIs')
  )
 with(qc, 
  lines(dates, EHIsigCount,col = 'grey')
  )
 segments(as.Date('2009-1-1') , seq(-15, 20,5) , as.Date('2009-2-28'),  seq(-15, 20,5), col = 'grey', lty=2)  
 legend('topright',c('EHIacclCount','EHIsigCount'), lty = 1, col = c('black','grey'), bg = 'white')  
 
 
 with(qc, 
  plot(dates, EHFintegrated,type = 'l', ylim = c(-50,800), main = 'EHF summed across consecutive days where positive (intensity)')
  )
 segments(as.Date('2009-1-1') , seq(0,1000,250) , as.Date('2009-2-28'),  seq(0,1000,250),col = 'grey', lty=2)  

 dev.off()
 
 
