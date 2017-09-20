
## calculate daily excess heat factor (ehf)
## arguments:
## ta: ideally of avg(tmax, tmin). Orig suggests same 0900-0900 period. No missings 
## t95: historical 95th percentile (optional, if missing this is computed). Orig suggests 1971-2000
## forward: the day aggregatopms are sampled either 1) as a
## foward-looking period (for the current day and following two days
## (i.e., days i, i+1 and i+2), or 2) the retrospective view (i.e., days
## i, i–1 and i–2). This is motivated by the intention to apply these
## ideas in the context of a weather forecasting service looking
## forward in time, but as Scalley points out a lag of at least three
## days from the threshold event to the peak health effect would be
## expected in the forward version.
## returns daily ehf series


EHF <- function(ta, t95 = NULL, forward = F) {
if(forward){
    t3 <- zoo::rollapply(ta, width = 3, FUN = mean, fill = NA, align = "left")
    t30 <- zoo::rollapplyr(
                    c(rep(NA, 1),ta[1:(length(ta)-1)])
                  , width = 30, FUN = mean, fill = NA)

} else {
    t3 <- zoo::rollapplyr(ta, width = 3, FUN = mean, fill = NA)
    t30 <- zoo::rollapplyr(
                  c(rep(NA, 3), ta[1:(length(ta)-3)])
                  , width = 30, FUN = mean, fill = NA)
    
}   
# calculate the 95th centile?
if(is.null(t95)){
   t95 <- quantile(ta, 0.95, na.rm = T)
}
# now calculate the EHI
EHIsig <- t3 - t95
EHIaccl <- t3 - t30
EHF <- EHIsig * pmax(1, EHIaccl)
return(EHF)
}
