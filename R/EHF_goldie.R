
# calculate daily excess heat factor (ehf)
# arguments:
#       tx: tmax time series
#       tn: tmin time series
#       t95: historical 95th percentile (ideally of avg(tx, tn) 0900-0900 1971-2000)
# returns daily ehf series

ehf_g <- function(tx, tn, t95)
{
        message('Calculating ehf')
    
        # use filter() to quickly calculate the moving averages required
        t3 = rowMeans(cbind(
                filter(tx, c(rep(1/3, 3), rep(0, 2)), method = 'convolution', sides = 2, circular = FALSE),
                filter(tn, c(rep(1/3, 3), rep(0, 4)), method = 'convolution', sides = 2, circular = FALSE)),
                na.rm = TRUE)
        t30 = rowMeans(cbind(
                filter(tx, c(rep(0, 31), rep(1/30, 30)), method = 'convolution', sides = 2, circular = FALSE),
                filter(tn, c(rep(0, 29), rep(1/30, 30)), method = 'convolution', sides = 2, circular = FALSE)),
                na.rm = TRUE)
        
        # bring it all together and return ehf
        ehi.sig = t3 - t95
        ehi.accl = t3 - t30
        ehf = ehi.sig * pmax(1, ehi.accl)

  message('Filling in missing ehf values')
    
  # which values are missing? (except for the edges that can't be done)
  missing.vals = which(is.na(ehf))
  missing.vals =
  missing.vals[! missing.vals %in% c(1:30, (length(tx) - 2):length(tx))]
    
        # fill in missing data manually
        for (i in missing.vals)
        {
                # get rolling tx, tn windows
                t3x = tx[i:(i + 2)]
                t3n = tn[(i + 1):(i + 3)]
                t30x = tx[(i - 30):(i - 1)]
                t30n = tn[(i - 29):i]

                # calc ehf if there's enough data
                if (length(which(is.na(t3x))) <= 1 ||
                                length(which(is.na(t3n))) <= 1 ||
                                length(which(is.na(t30x))) <= 5 ||
                                length(which(is.na(t30n))) <= 5)
                {
                                t3 = mean(c(
                                                mean(t3x, na.rm = TRUE),
                                                mean(t3n, na.rm = TRUE)))
                                t30 = mean(c(
                                                mean(t30x, na.rm = TRUE),
                                                mean(t30n, na.rm = TRUE)))
                                ehf[i] = (t3 - t95) * pmax(1, t3 - t30)
                }
        }
        return(ehf)
}

# threedmt: lowest maximum of today and the next two days
threedmt <- function(tx)
{
        message('Calculating threedmt')

        # quick version with rollapply
        threedmt = rollapply(tx, width = 3, FUN = min, fill = NA, align = 'left')

        message('Filling in missing 3dmt values')

        # which values are missing? (except for the edges that can't be done)
        missing.vals = which(is.na(threedmt))
        missing.vals =
                missing.vals[! missing.vals %in% (length(tx) - 1):length(tx)]

        # fill missing data in manually
        for (i in missing.vals)
        {
                # get rolling tx window
                txi = tx[i:(i + 2)]

                # calc 3dmt if there's enough data
                if (length(which(is.na(txi))) <= 1)
                        threedmt[i] = min(txi, na.rm = TRUE)
        }
        return(threedmt)
}

# today's dat is the mean of tx today and tn tomorrow.
# threedat is the mean of dat for today + next two days
threedat <- function(tx, tn)
{
        message('Calculating threedat')
        # do the initial work quickly with filter()
        threedat =
                rowMeans(cbind(
                        filter(tx, c(rep(1/3, 3), rep(0, 2)), method = 'convolution', sides = 2, circular = FALSE),
                        filter(tn, c(rep(1/3, 3), rep(0, 4)), method = 'convolution', sides = 2, circular = FALSE)),
                        na.rm = TRUE)

        message(run.time(), city, ': filling in missing 3dat values')

        # which values are missing? (except for the edges that can't be done)
        missing.vals = which(is.na(threedat))
        missing.vals =
                        missing.vals[! missing.vals %in% (length(tx) - 2):length(tx)]

        # fill missing data in manually
        for (i in missing.vals)
        {
                # get rolling tx, tn windows 
                txi = tx[i:(i + 2)]
                tni = tn[(i + 1):(i + 3)]

                # cal if there's enough data
                if (length(which(is.na(txi))) <= 1 &&
                                length(which(is.na(tni))) <= 1)
                                threedat[i] = mean(c(
                                                mean(txi, na.rm = TRUE),
                                                mean(tni, na.rm = TRUE)),
                                                na.rm = TRUE)
        }
        return(threedat)
}

# returns a vector lagged by n elements (last n elements are lost)
# use a negative n to bring the series forward (now commented out)
# nb: dplyr::lag is equivalent and a little more defensive
lag <- function(x, n)
{
        if (n == 0) 
        {
                return(x)
        } else if (n > 0)
        {
                return(c(rep(NA, n), x[1:(length(x) - n)]))
        } else if (n < 0)
        {
                stop('Error in lag: must provide n >= 0')
        }

}
