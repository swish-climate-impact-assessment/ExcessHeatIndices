
# steadman 1994 (and BoM Ta + 0.33×e − 0.70×ws − 4.00)
tapparent <- function(dat, ta = "maxave", vprph = "vprph15", ws = 0){

  dat$tappmax <- dat[,ta] + (0.33 * dat[,vprph]) - (0.70 * ws) - 4.00

  return(dat)
}
