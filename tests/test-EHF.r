
# first test
dir()
source('./R/EHF.r')
require(swishdbtools)
require(plyr)
# access to ewedb is password restricted
ch <- connect2postgres2('ewedb')
slacode <- sql_subset(ch,"abs_sla.aussla01", subset = "sla_name = 'Scullin'",
           select = c("sla_code, sla_name"), eval=T)
sql <- sql_subset(ch,"weather_sla.weather_sla",
                 subset=paste("sla_code = '",slacode$sla_code,"'",sep=""), eval = F)
cat(sql)
# this might take some minutes
df <- dbGetQuery(ch, sql)
head(df)
tail(df)
with(df, plot(date, maxave))
str(df)
df2 <- EHF(df, 'maxave', "date", min(df$date), max(df$date))
names(df2)
hist(subset(df2, EHF >= 1)[,'EHF'])
threshold <- quantile(subset(df2, EHF >= 1)[,'EHF'], probs=0.9)

with(df, plot(date, maxave, type = 'l'))
with(subset(df2, EHF > threshold), points(date, maxave, col = 'red', pch = 16))
