
## From Nairn and Fawcett 2013 CAWCR report: In the case of a heatwave
## extending over more than one three-day period, we will define the
## heat load of the event as the sum of the consecutive positive EHF
## values. This definition has the benefit of simplicity, but it comes
## at the slight cost of down-weighting the DMT contributions from the
## first and last days of the multi-day (n > 3) period
EHFload <- function(EHF, EHFduration = NULL){
    if(is.null(EHFduration)) stop("EHFload depends on EHFduration")
    # conditional cumulitive sums
    index <- 1:length(EHF)
    analyte <- data.frame(index, EHF, EHFduration)
    analyte <- na.omit(analyte)
    which(analyte$EHFduration > 0)
#head(data.frame(analyte[1080:1420,]), 33)
    EHFinverted  <- analyte$EHF * -1 
    y <- ifelse(EHFinverted >= 0, 0, analyte$EHF)
    f <- EHFinverted < 0
    f <- (cumsum(!f) + 1) * f
    z <- unsplit(lapply(split(y,f),cumsum),f)

    analyte$EHFload <- z
    id <- as.data.frame(index)
    # str(analyte)
    analyte <- merge(id, analyte, all.x = T, by = "index")
    # head(analyte, 50)  
    return(analyte$EHFload)
    }

    
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
  #    str(id)
