
# steadman 1994 (and BoM Ta + 0.33×e − 0.70×ws − 4.00)
tapparent <- function(ta, vprph, ws = 0){

  tapparent <- ta + (0.33 * vprph) - (0.70 * ws) - 4.00

  return(tapparent)
}
