
# calculate daily excess heat factor (ehf)
# arguments:
#       ta: ideally of avg(tmax, tmin). Orig suggests same 0900-0900 period. No missings 
#       t95: historical 95th percentile (optional, if missing this is computed). Orig suggests 1971-2000
# returns daily ehf series


EHF <- function(ta, t95 = NULL) {

   t3 <- zoo::rollapplyr(ta, width = 3, FUN = mean, fill = NA)
   t30 <- zoo::rollapplyr(zoo::lag(ta, 3), width = 30, FUN = mean, fill = NA)
     
   
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
