###############################################################################
# newnode qc graph
	
 # do graph
 png(paste('reports/',stnlabl, '.png', sep = ''))
 with(analyte, plot(dates, avtemp, type = 'l', main =  stnlabl))
 dev.off()
 
