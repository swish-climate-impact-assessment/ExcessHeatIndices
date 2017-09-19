
'name:dewpt'
# https://github.com/martinluther/meteo/blob/master/wunderground/meteo-wu-fmt.pl
# orig formulas from http://www.usatoday.com/weather/whumcalc.htm
#
# returns a dewpoint given a temp (celsius) and relative humidity
#
# Logic: First, obtain the saturation vapor pressure(Es) using formula (5)
# from air temperature Tc. 
#
# (5) Es=6.11*10.0**(7.5*Tc/(237.7+Tc)) 
#
# The next step is to use the saturation vapor pressure and the relative humidity
# to compute the actual vapor pressure(E) of the air. This can be done with
# the following formula. 
#
# (9) E=(RH*Es)/100 
#
# RH=relative humidity of air expressed as a percent.(i.e. 80%) 
#
# Now you are ready to use the following formula to obtain the
# dewpoint temperature. 
#
# Note: ln( ) means to take the natural log of the variable in the parentheses 
#
# (10) Tdc=(-430.22+237.7*ln(E))/(-ln(E)+19.08) 
#
dewpt <- function(Tc, RH){

  Es <- 6.11 * 10.0^(7.5 * Tc / (237.7 + Tc))
  E <- (RH * Es) / 100
  Tdc <- (-430.22 + 237.7 * log(E)) / (-log(E) +19.08)

  return(Tdc)
}
