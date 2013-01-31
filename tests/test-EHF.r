
# first test
dir()
source('R/EHF.r')
require(swishdbtools)
require(plyr)
if(!exists('p'))
  {
    p <- getPassword()
  }
ch <- connect2postgres('115.146.84.135', 'ewedb', 'gislibrary', p)

df <- dbGetQuery(ch,
  "select cityname, date, tmean, tmax, tmin, tmpd
  from nmmaps.exposure
  where cityname = 'Chicago'
  ")
head(df)
tail(df)
with(df, plot(date, tmpd))
str(df)
df2 <- EHF(df, 'tmpd', "date", min(df$date), max(df$date))
names(df2)
hist(subset(df2, EHF >= 1)[,'EHF'])
threshold <- quantile(subset(df2, EHF >= 1)[,'EHF'], probs=0.9)

with(df, plot(date, tmpd, type = 'l'))
with(subset(df2, EHF > threshold), points(date, tmpd, col = 'red', pch = 16))
