###############################################################################
# newnode tools
	
 if (!require(Hmisc)) install.packages('Hmisc', repos='http://cran.csiro.au'); require(Hmisc)
 # TASK this package is used for the Lag function
 # I'd like to convert the approach taken here to something more sensible using zoo package, rollapply?
 if (!require(rgdal)) install.packages('rgdal', repos='http://cran.csiro.au'); require(rgdal)
 epsg <- make_EPSG() 
 if (!require(geosphere)) install.packages('geosphere', repos='http://cran.csiro.au'); require(geosphere)
 if (!require(plyr)) install.packages('plyr', repos='http://cran.csiro.au'); require(plyr)
 
###############################################################################
# newnode Make an R function to do the work
	
 
 EHIs <- function(analyte = data_subset,
  exposurename = 'air_temperature_in_degrees_c_max_climatezone_av',
  datename = 'date',
  referencePeriodStart = as.Date('1971-1-1'),
  referencePeriodEnd = as.Date('2000-12-31'),
  nlags = 32) {
  # TASK SHOULD WE IMPUTE MISSING DAYS?
 
  # first get lags
  # TASK THERE IS PROBABLY A VECTORISED VERSION THAT IS QUICKER?
  # TASK it is rollmean from the zoo package
  # ALTHOUGH THAT DOESNT HANDLE NAs SO TRY ROLLAPPLY?
  analyte$temp_lag0 <- analyte[,exposurename]
  exposuresList <- 'temp_lag0'
  # make sure in order
  analyte <- arrange(analyte,  analyte[,datename])
  # lag0 is not needed
  for(lagi in 1:nlags){
 	# lagi <- 1
 	exposuresList <- c(exposuresList, gsub('lag0',paste('lag', lagi,sep=''), exposuresList[1]))
 	analyte[,(ncol(analyte)+1)] <- Lag(analyte[,exposuresList[1]],lagi)
 	}
  exposuresList <- exposuresList[-1]
  names(analyte) <- c(names(analyte[,1:(ncol(analyte)-nlags)]),exposuresList)
  # head(analyte)
  # now 3 day av
  analyte$temp_movav <- rowMeans(analyte[,c('temp_lag0','temp_lag1','temp_lag2')], na.rm =FALSE)

  # now 30 day av
  # paste('temp_lag',3:32, sep = '', collapse = \"','\")
  analyte$temp30_movav <- rowMeans(analyte[,c('temp_lag3','temp_lag4','temp_lag5','temp_lag6','temp_lag7','temp_lag8','temp_lag9','temp_lag10','temp_lag11','temp_lag12','temp_lag13','temp_lag14','temp_lag15','temp_lag16','temp_lag17','temp_lag18','temp_lag19','temp_lag20','temp_lag21','temp_lag22','temp_lag23','temp_lag24','temp_lag25','temp_lag26','temp_lag27','temp_lag28','temp_lag29','temp_lag30','temp_lag31','temp_lag32')], na.rm =FALSE)
  # TASK note that this removes any missing days which could be imputed
  analyte <- na.omit(analyte)
  # head(analyte)
 
  # now calculate the EHI
  analyte$EHIaccl <- analyte$temp_movav - analyte$temp30_movav
  
  # first calculate the 95th centile
  referencestart <- referencePeriodStart
  referenceend <- referencePeriodEnd
  analyte$dateidCol <- analyte[,datename]
  reference <- subset(analyte, dateidCol >= referencestart & dateidCol <= referenceend, select = c('dateidCol', exposurename))
  head(reference);tail(reference)
  T95 <- quantile(reference[,exposurename], 0.95, na.rm = T)
  T95
 
  # now calculate the EHIsig
  analyte$EHIsig <- analyte$temp_movav - T95
  
  # now calculate the EHF
  analyte$EHF <- abs(analyte$EHIaccl) * analyte$EHIsig
  
  # proposed integrations
  # counts can be done quicker with this
  x <- analyte$EHIaccl >= 0
  xx <- (cumsum(!x) + 1) * x 
  x2<-(seq_along(x) - match(xx, xx) + 1) * x 
  analyte$EHIacclCount <- x2

  # alternately, slower but more interpretable
  # analyte$EHIacclCount2<-as.numeric(0)
  # # 
  # which(analyte$dates == as.Date('2009-1-1'))
  # which(analyte$dates == as.Date('2009-3-1'))
  
  # for(j in 43034:43093){
  # # j=43034
  # analyte$EHIacclCount2[j] <- ifelse(analyte$EHIaccl[j] < 0, 0,
  # ifelse(analyte$EHIaccl[j-1] >= 0, 1 + analyte$EHIacclCount2[j-1],
  # 1)
  # )
  # }
  
  x <- analyte$EHIsig >= 0
  xx <- (cumsum(!x) + 1) * x 
  x2<-(seq_along(x) - match(xx, xx) + 1) * x 
  analyte$EHIsigCount <- x2
  
  # sums
  EHFinverted  <- analyte$EHF * -1 
  y <- ifelse(EHFinverted >= 0, 0, analyte$EHF)
  f <- EHFinverted < 0
  f <- (cumsum(!f) + 1) * f 
  z <- unsplit(lapply(split(y,f),cumsum),f)
  analyte$EHFintegrated <- z
  
  # alternately, slower but more interpretable
  # analyte$EHFintegrated2 <- as.numeric(0)
  # for(j in 43034:43093){
  # # j = 43034
	# analyte$EHFintegrated2[j] <- ifelse(analyte$EHF[j] < 0,0,
	 # ifelse(analyte$EHF[j-1] >= 0,
	 # analyte$EHF[j] + analyte$EHFintegrated2[j-1],
	 # analyte$EHF[j])
	 # )
	# }
  
  return(analyte)
  }
 
