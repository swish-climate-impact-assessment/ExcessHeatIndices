
EHF <- function(
                ta
               ,
                t95 = NULL
                ) {

 t3 <- rollapplyr(ta, width = 3, FUN = mean, fill = NA)
 t30 <- rollapplyr(lag(ta, 3), width = 30, FUN = mean, fill = NA)
   

 # now calculate the EHI
 
 # calculate the 95th centile?
 if(is.null(t95)){
    t95 <- quantile(ta, 0.95, na.rm = T)
 }

 EHIsig <- t3 - t95
 EHIaccl <- t3 - t30
 EHF <- EHIsig * pmax(1, EHIaccl)

  analyte <- data.frame(EHIaccl, EHIsig, EHF)

  analyte <- na.omit(analyte)  
  # proposed integrations
  # counts can be done quicker with this
  x <- analyte$EHF >= 0
  xx <- (cumsum(!x) + 1) * x 
  x2<-(seq_along(x) - match(xx, xx) + 1) * x 
  analyte$EHFcount <- x2

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
  #head(analyte[680:nrow(analyte),], 50)  
  return(analyte)

    

 }
