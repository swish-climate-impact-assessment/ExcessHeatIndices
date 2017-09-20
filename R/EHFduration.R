
EHFduration <- function(EHF){
    # conditional cumulative count
    index <- 1:length(EHF)
    analyte <- data.frame(index, EHF)

    analyte <- na.omit(analyte)  
    # proposed integrations
    # counts can be done quicker with this
    x <- analyte$EHF >= 0
    xx <- (cumsum(!x) + 1) * x 
    x2<-(seq_along(x) - match(xx, xx) + 1) * x 
    analyte$EHFduration <- x2
    id <- as.data.frame(index)
    # str(analyte)
    analyte <- merge(id, analyte, all.x = T, by = "index")
    # head(analyte, 50)  
    return(analyte$EHFduration)
}

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
